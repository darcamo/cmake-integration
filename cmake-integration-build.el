;;; cmake-integration-build.el --- Build targets -*- lexical-binding: t -*-

;; Copyright (C) 2025 Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs

;; This file is part of cmake-integration.

;; cmake-integration is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cmake-integration is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cmake-integration. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cmake-integration-variables)
(require 'cmake-integration-core)
(require 'cmake-integration-configure)


(defun ci--adjust-build-preset ()
  "Adjust the build preset when changing the configure preset.

This function is added to `cmake-integration-after-set-configure-preset-hook'."
  (if ci-configure-preset
      (let ((presets (ci-get-build-presets)))
        ;; Only change the build preset if there is excactly one
        ;; build preset for the conffigure preset
        (if (= (length presets) 1)
            (setq ci-build-preset (car presets))
          (setq ci-build-preset nil)))
    (setq ci-build-preset nil)))


(add-hook 'ci-after-set-configure-preset-hook 'ci--adjust-build-preset)


(defun ci-get-last-build-preset-name ()
  "Get the `name' field of the last preset used for build."
  (ci--get-preset-name ci-build-preset))


;;;###autoload (autoload 'cmake-integration-get-build-presets "cmake-integration")
(defun ci-get-build-presets (&optional configure-preset)
  "Get the build presets associated with CONFIGURE-PRESET.

Get the build presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files whose
configure preset is CONFIGURE-PRESET. If CONFIGURE-PRESET is not
provided, then the value in the `cmake-integration-configure-preset'
variable will be used."
  (ci-get-presets-of-type 'buildPresets configure-preset))


;;;###autoload (autoload 'cmake-integration-select-build-preset "cmake-integration")
(defun ci-select-build-preset ()
  "Select a build preset for CMake."
  (interactive)
  (when (not ci-configure-preset)
    (error "Please, select a configure preset first"))

  (let ((all-presets (ci-get-build-presets)))
    (setq ci-build-preset
          (ci-select-preset all-presets "Build preset: "))))


(defun ci--create-target-fullname (target-name &optional config-name)
  "Return target constructed from TARGET-NAME and CONFIG-NAME."
  (if config-name
      (concat target-name ci--multi-config-separator config-name)
    target-name))


(defun ci--create-target (target-name &optional config-name)
  "Create a simple target containing only its TARGET-NAME for config CONFIG-NAME.

This is only used for the `all', `clean', and `install' targets."
  (let ((target-full-name (ci--create-target-fullname target-name config-name)))
    (list target-full-name)))


(defun ci--delete-build-folder-no-confirm ()
  "Delete the current build folder."
  (delete-directory (ci-get-build-folder) t))


(defun ci-delete-build-folder ()
  "Delete the current build folder.

It's useful in case you changed the generator, since CMake would
complain in that case."
  (interactive)
  (let ((folder (ci--get-build-folder-relative-to-project)))
    (if (yes-or-no-p (format "Delete folder '%s'?" folder))
        (ci--delete-build-folder-no-confirm)
      (message "Build folder was not deleted"))))


(defun ci--check-if-build-folder-exists-and-throws-if-not ()
  "Check that the build folder exists and throws an error if not."
  (unless (file-exists-p (ci-get-build-folder))
    (error "The build folder is missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset' to configure the project")))


(defun ci--save-and-compile-no-completion (target &optional extra-args)
  "Save the buffer and compile TARGET also passing EXTRA-ARGS.

See the documentation of `cmake-integration-get-build-command' for the
EXTRA-ARGS parameter."
  (if (buffer-file-name)
      (save-buffer 0))
  (pcase-let* ((`(,run-dir ,cmd)
                (ci-get-build-command target extra-args)))
    (let ((default-directory run-dir))
      (compile cmd))))


(defun ci--get-target-type-from-name (target-name all-targets)
  "Get the type of the target with name TARGET-NAME from ALL-TARGETS.
ALL-TARGETS is an alist like the one returned by
`cmake-integration--get-annotated-targets-from-codemodel-json-file'."
  (let ((target (alist-get target-name all-targets nil nil 'equal)))
    (alist-get 'type target)))


(defun ci--get-propertized-target-type-from-name (target-name all-targets)
  "Get the type of the target with name TARGET-NAME from ALL-TARGETS.

This is the same as `ci--get-target-type-from-name', with the difference
that the returned type is propertized with a face that depends on the
target type."
  (let ((type (ci--get-target-type-from-name target-name all-targets)))
    (cond
     ((string-match-p "executable" type) (propertize type 'face 'ci-executable-target-face))
     ((string-match-p "library" type) (propertize type 'face 'ci-library-target-face))
     ((string-match-p "utility" type) (propertize type 'face 'ci-utility-target-face))
     ((string-match-p "unknown" type) (propertize type 'face 'ci-unknown-target-face))
     (t type))))


(defun ci--target-annotation-function (target-name)
  "Annotation function that takes a TARGET-NAME and return an annotation for it.

This is used in `cmake-integration--get-target-using-completions'
when completing a target name to generate an annotation for that
target, which is shown during the completions if you are using
the marginalia package, or in Emacs standard completion buffer."
  (let ((spaces (ci--get-annotation-initial-spaces target-name)))
    (pcase (car (split-string target-name ci--multi-config-separator))
      ("clean" (concat spaces (propertize "Clean all compiled targets" 'face 'ci-phony-target-face)))
      ("all" (concat spaces (propertize "Compile all targets" 'face 'ci-phony-target-face)))
      ("install" (concat spaces (propertize "Install targets" 'face 'ci-phony-target-face)))
      (_ (concat spaces (ci--get-propertized-target-type-from-name target-name minibuffer-completion-table))))))


(defun ci--target-completion-group-function (completion transform)
  "Group function used during completion for COMPLETION and TRANSFORM.

If TRANSFORM is nil, the function must return the group title of the
group to which the COMPLETION candidate belongs. Otherwise the function
must return the transformed candidate, where the only transformation is
changing the face to `bold' if COMPLETION is the current target.

See \"Programmed Completion\" in Emacs info for more."
  (if transform
      ;; If transform is non-nil, return the transformed completion candidate
      (if (equal completion ci-current-target)
          (propertize completion 'face 'bold)
        completion)

    ;; If transform is nil, return the group title
    (let ((type (ci--get-target-type-from-name completion minibuffer-completion-table)))
      (cond
       ((ci--is-phony-target completion) (propertize "Phony" 'face 'ci-phony-target-face))
       ((string-match-p "executable" type) (propertize "Executable" 'face 'ci-executable-target-face))
       ((string-match-p "library" type) (propertize "Library" 'face 'ci-library-target-face))
       ((string-match-p "utility" type) (propertize "Utility" 'face 'ci-utility-target-face))
       ((string-match-p "unknown" type) (propertize type 'face 'ci-unknown-target-face))
       (t type)))))


(defun ci--get-target-using-completions (list-of-targets)
  "Ask the user to choose one of the targets in LIST-OF-TARGETS using completions."
  (let ((completion-extra-properties '(:annotation-function ci--target-annotation-function :group-function ci--target-completion-group-function)))
    (completing-read "Target: " list-of-targets nil t)))


(defun ci--get-all-targets (json-filename)
  "Get all targets for completion specified in JSON-FILENAME.

Get the name of all targets for completion, respecting the value
of the `*-targets-during-completion' variables.

If a prefix argument is provided, then the value of
`cmake-integration-include-subproject-targets-during-completion' is
temporarily considered as t.

If two prefix arguments are provided, then all targets are included."
  (when (or (null json-filename) (not (file-exists-p json-filename)))
    (error "Could not find the CMake codemodel reply file. Please run either `cmake-integration-cmake-reconfigure' or `cmake-integration-cmake-configure-with-preset'"))

  (let* ((include-subprojects (or current-prefix-arg ci-include-subproject-targets-during-completion))
         (list-of-targets (if include-subprojects
                              (ci--get-annotated-targets-from-codemodel-json-file
                               json-filename)
                            (ci--get-annotated-targets-from-codemodel-json-file
                             json-filename
                             'ci--target-is-in-projectIndex0-p))))

    ;; Filter the list of targets
    (cond
     ;; If two C-u are passed, include all targets
     ((equal current-prefix-arg '(16)) list-of-targets)

     ;; Do not include utility and library targets
     ((and ci-hide-utility-targets-during-completion
           ci-hide-library-targets-during-completion)
      (seq-filter #'(lambda (target) (and
                                 (ci--target-is-not-utility-p target)
                                 (ci--target-is-not-library-p target)))
                  list-of-targets))

     ;; Do not include only utility targets
     (ci-hide-utility-targets-during-completion
      ;; Do not include utility targets
      (seq-filter 'ci--target-is-not-utility-p list-of-targets))

                                        ; Do not include only library targets
     (ci-hide-library-targets-during-completion
      (seq-filter 'ci--target-is-not-library-p list-of-targets))

     ;; Include all targets
     (t list-of-targets))))


(defun ci-select-current-target ()
  "Ask for a target to build and return the target name."

  ;; If the build folder is missing we should stop with an error
  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)

  (if-let* ((json-filename (ci--get-codemodel-reply-json-filename))
            (list-of-targets (ci--get-all-targets json-filename))
            (target (ci--get-target-using-completions list-of-targets)))
      (setq ci-current-target target)

    ;; If `json-filename' is nil that means we could not find the
    ;; CMake reply with the file API, which means the query file is
    ;; missing.
    (display-warning 'cmake-integration "Could not find list of targets due to CMake file API file
missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset'.")
    (setq ci-current-target nil)))


;;;###autoload (autoload 'cmake-integration-save-and-compile "cmake-integration")
(defun ci-save-and-compile ()
  "Ask for a target name and compile it.

A list of target names is obtained from the project using CMake's
file API and completion is used to choose the desired target. If
that is not possible, ask for the target name without
completions."

  (interactive)
  ;; Ask the user for a target and set the
  ;; cmake-integration-current-target variable with the chosen target
  ;; name
  (ci-select-current-target)

  (ci--save-and-compile-no-completion ci-current-target))


;;;###autoload (autoload 'cmake-integration-save-and-compile-last-target "cmake-integration")
(defun ci-save-and-compile-last-target (&optional extra-args)
  "Recompile the last target that was compiled (or `all') also passing EXTRA-ARGS.

See the documentation of `cmake-integration-get-build-command' for the
EXTRA-ARGS parameter."
  (interactive)
  (ci--save-and-compile-no-completion
   (or ci-current-target "all")
   extra-args))


(defun ci-get-build-command (target &optional extra-args)
  "Get the command to compile target TARGET passing EXTRA-ARGS to cmake.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run.

EXTRA-ARGS is a list of strings, which will be joined with a space as
separation and then passed to cmake command to build the target."
  (pcase-let* ((`(,target-name ,config-name)
                (split-string target ci--multi-config-separator))
               (project-root (ci--get-project-root-folder))
               (preset-arg-or-build-folder (if ci-build-preset
                                               (format "--preset %s" (ci-get-last-build-preset-name))
                                             (ci--get-build-folder-relative-to-project)))
               (build-args extra-args))

    ;; Add configuration argument if available
    (when config-name (push (format "--config %s" config-name) build-args))

    ;; Add the target
    (push (format "--target %s" target-name) build-args)

    ;; Add build folder or preset part
    (push preset-arg-or-build-folder build-args)

    ;; Return (run-dir command)
    (list project-root (format "cmake --build %s" (string-join build-args " ")))))


;; See CMake file API documentation for what projectIndex is
;; https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html
(defun ci--target-is-in-projectIndex0-p (target-info)
  "Return t if the projectIndex field of TARGET-INFO is 0."
  (eq (alist-get 'projectIndex target-info) 0))


(defun ci--target-is-not-utility-p (target)
  "Return t if TARGET type is not `UTILITY'."
  (not (equal (alist-get 'type target) "UTILITY")))


(defun ci--target-is-not-library-p (target)
  "Return t if TARGET type does not contain `LIBRARY'."
  (if-let* ((type (alist-get 'type target)))
      (not (equal (car (cdr (split-string type "_"))) "LIBRARY"))
    t))


(defun ci--get-targets-from-configuration (config &optional predicate)
  "Get all targets in CONFIG that match PREDICATE.

CONFIG is a subset of the codemodel JSON file. One of the keys in it is
`targets', whose value is an array of \"targets\". This function will
return all targets in that array that match PREDICATE.

PREDICATE is a function that takes one argument, which is an alist, such
like the example below:

  ((directoryIndex . 1)
  (id . \"some-id\")
  (jsonFile . \"target-...-....json\")
  (name . \"target-name\")
  (projectIndex . 0))."
  (if predicate
      (seq-filter predicate (alist-get 'targets config))
    (alist-get 'targets config)))


(defun ci--get-target-name (target-info &optional config-name)
  "Get the full name for TARGET-INFO, including CONFIG-NAME (if not nil).

TARGET-INFO is an alist with the information about a given target,
including its name."
  (let* ((target-name (alist-get 'name target-info)))
    (ci--create-target-fullname target-name config-name)))


(defun ci--add-name-to-target (target-info &optional config-name)
  "Add the target name to TARGET-INFO for CONFIG-NAME.

TARGET-INFO is an alist, which includes amond other things the the
`name` of the target. This function will extract the name field and
prepend it to the list. In other words, this function returns a cons
cell containing the target-name as car and TARGET-INFO as cdr."
  (let ((target-name (ci--get-target-name target-info config-name)))
    (cons target-name target-info)))


(defun ci--get-prepared-targets-from-configuration (config include-config-name &optional predicate)
  "Get all targets in CONFIG matching PREDICATE.

 If INCLUDE-CONFIG-NAME is t, the configuration name is added after the
 target name.

CONFIG is a subset of the codemodel JSON file. One of the keys in it is
`targets', whose value is an array of \"targets\". This function will
return all targets in that array that match PREDICATE.

The implicit targets `all', `clean' and optional `install' targets will
be returned as well."
  (let ((install-rule? (cl-some (lambda (dir) (alist-get 'hasInstallRule dir))
                                (alist-get 'directories config)))
        (targets-in-config (ci--get-targets-from-configuration config predicate))
        (config-name (when include-config-name (alist-get 'name config)))
        (targets-and-name))
    (setq targets-and-name (mapcar (lambda (target-info) (ci--add-name-to-target target-info config-name))
                                   targets-in-config))
    ;; Add implicit 'all', 'clean' and optional 'install' targets
    (ci--add-all-clean-install-targets
     targets-and-name
     config-name
     install-rule?)))


(defun ci--is-phony-target (target-name)
  "Return t if TARGET-NAME is one of the phony targets.

The phony targets are all, clean and install."
  (member target-name '("all" "clean" "install")))


;; TODO: Rename to add-phony-targets
(defun ci--add-all-clean-install-targets (targets config-name has-install-rule)
  "Return TARGETS with extra `all', `clean' and `install' for CONFIG-NAME.

The `install' target is only included if HAS-INSTALL-RULE is true."
  (let ((all-target (list (ci--create-target "all" config-name)))
        (clean-target (list (ci--create-target "clean" config-name)))
        (install-target (when has-install-rule (list (ci--create-target "install" config-name)))))
    (nconc all-target clean-target install-target targets)))


(defun ci--get-targets-from-codemodel-json-file (&optional json-filename predicate)
  "Return the targets found in JSON-FILENAME that match PREDICATE.

Return an alist of (target-name . target-info) elements for
targets found in JSON-FILENAME.

JSON-FILENAME must be a CMake API codemodel file. If is not provided,
`cmake-integration--get-codemodel-reply-json-filename' is used.

The returned alist includes:
  - Explicit targets from the JSON file.
  - Implicit targets: `all', `clean', and `install' (if applicable).

Each entry maps `target-name` to `target-info`."

  (let* ((json-filename (or json-filename
                            ;; If json-filename was not provided, get it from
                            ;; 'cmake-integration--get-codemodel-reply-json-filename'.
                            (ci--get-codemodel-reply-json-filename)))
         ;; On single-configuration generators there is one entry for the value
         ;; of the CMAKE_BUILD_TYPE variable. For multi-configuration generators
         ;; there is an entry for each configuration listed in the
         ;; CMAKE_CONFIGURATION_TYPES variable.
         (all-config (alist-get 'configurations (json-read-file json-filename))))
    ;; The result of the nested `mapcar's below is a list of list of alists.
    ;; What we need a list of alists so remove one level and combine the next
    ;; level lists (of alists) of into a single list (of alists).
    (apply #'nconc
           ;; process all-config vector
           (mapcar (lambda (config)
                     ;; config-name is non-nil only when using ninja
                     ;; multi-config generator, where we have more
                     ;; than one configuration

                     (let ((config-name (and (> (length all-config) 1)
                                             (alist-get 'name config))))
                       (ci--get-prepared-targets-from-configuration config config-name predicate)))
                   ;; Sequence mapped in the mapcar
                   all-config))))


(defvar ci--target-type-cache (make-hash-table :test 'equal)
  "Cache for target types. Maps target names to their types.")


(defun ci-refresh-target-type-cache ()
  "Clear the target type cache."
  (interactive)
  (clrhash ci--target-type-cache))


(defun ci--add-type-field-to-target (target)
  "Add a `type' field to a TARGET, using cache if available.

NOTE: This will modify the input TARGET.

TARGET is a list containing the target name followed by many cons, with
each cons having some information about the TARGET. Particularly, TARGET
has a cons cell with a `jsonFile' car and a `\"someJsonFile.json\"'
filename. This json file will be read to extract the type that should be
added to TARGET."
  (let ((target-name
         (car (split-string (car target) ci--multi-config-separator))))
    (unless (ci--is-phony-target target-name)
      (if-let* ((target-type
                 (when ci--target-type-cache
                   (gethash target-name ci--target-type-cache))))
        (setf (alist-get 'type (cdr target)) target-type)

        (if ci-annotate-targets
            (let* ((json-file (alist-get 'jsonFile target))
                   (json-full-filename
                    (file-name-concat (ci--get-reply-folder) json-file))
                   (target-json-data (json-read-file json-full-filename)))

              ;; Update the cache with the type of the target
              (setq target-type (alist-get 'type target-json-data)))

          (setq target-type "<UNKNOWN>"))

        (when ci--target-type-cache
          (puthash target-name target-type ci--target-type-cache))

        ;; Set the value from the json data to `type' field
        (setf (alist-get 'type (cdr target)) target-type)))))


(defun ci--get-annotated-targets-from-codemodel-json-file (&optional json-filename predicate)
  "Return the targets found in JSON-FILENAME that respect PREDICATE.

This function is the same as
`cmake-integration--get-targets-from-codemodel-json-file',
with the exception that it adds the type of each target to a
`type' field in the target. The main use for this information is
during completion of target names, where this type information is
shown as an annotation."
  (let ((list-of-targets (ci--get-targets-from-codemodel-json-file json-filename predicate)))
    ;; Create the progress reporter
    (let* ((total (length list-of-targets))
           (progress-reporter
            (make-progress-reporter "Processing targets..." 0 total)))
      ;; Process each target and update the progress reporter
      (cl-loop for target in list-of-targets
               for idx from 0
               do (ci--add-type-field-to-target target)
               do (progress-reporter-update progress-reporter idx))
      (progress-reporter-done progress-reporter))
    list-of-targets))


(provide 'cmake-integration-build)

;;; cmake-integration-build.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
