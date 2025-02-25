;;; cmake-integration-build.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cmake-integration-variables)
(require 'cmake-integration-core)
(require 'cmake-integration-configure)


(defun cmake-integration--adjust-build-preset ()
  "Adjust the build preset when changing the configure preset.

This function is added to `cmake-integration-after-set-configure-preset-hook'."
  (when cmake-integration-build-preset
    (if cmake-integration-configure-preset
        (let ((presets (cmake-integration-get-build-presets)))
          ;; Only change the build preset if there is excactly one
          ;; build preset for the conffigure preset
          (if (= (length presets) 1)
              (setq cmake-integration-build-preset (car presets))
            (setq cmake-integration-build-preset nil)))
      (setq cmake-integration-build-preset nil))))


(add-hook 'cmake-integration-after-set-configure-preset-hook 'cmake-integration--adjust-build-preset)


(defun cmake-integration-get-last-build-preset-name ()
  "Get the `name' field of the last preset used for build."
  (cmake-integration--get-preset-name cmake-integration-build-preset))


;;;###autoload (autoload 'cmake-integration-get-build-presets "cmake-integration")
(defun cmake-integration-get-build-presets (&optional configure-preset)
  "Get the build presets associated with CONFIGURE-PRESET.

Get the build presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files whose
configure preset is CONFIGURE-PRESET. If CONFIGURE-PRESET is not
provided, then the value in the `cmake-integration-configure-preset'
variable will be used."
  (cmake-integration-get-presets-of-type 'buildPresets configure-preset))


;;;###autoload (autoload 'cmake-integration-select-build-preset "cmake-integration")
(defun cmake-integration-select-build-preset ()
  "Select a build preset for CMake."
  (interactive)
  (when (not cmake-integration-configure-preset)
    (error "Please, select a configure preset first"))
  (let ((all-presets (cmake-integration-get-build-presets)))
    (when all-presets
      (setq cmake-integration-build-preset
            (cmake-integration-select-preset all-presets "Build preset: ")))))

(defun cmake-integration--create-target-fullname (target-name &optional config-name)
  "Return target constructed from TARGET-NAME and CONFIG-NAME."
  (if config-name
      (concat target-name cmake-integration--multi-config-separator config-name)
    target-name))


(defun cmake-integration--create-target (target-name &optional config-name)
  "Create a simple target containing only its TARGET-NAME for config CONFIG-NAME.

This is only used for the `all', `clean', and `install' targets."
  (let ((target-full-name (cmake-integration--create-target-fullname target-name config-name)))
    (list target-full-name)))


(defun cmake-integration-delete-build-folder ()
  "Delete the current build folder.

It's useful in case you changed the generator, since CMake would
complain in that case."
  (interactive)
  (delete-directory (cmake-integration-get-build-folder) t))


(defun check-if-build-folder-exists-and-throws-if-not ()
  "Check that the build folder exists and throws an error if not."
  (unless (file-exists-p (cmake-integration-get-build-folder))
    (error "The build folder is missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset' to configure the project")))


;;;###autoload (autoload 'cmake-integration-save-and-compile-no-completion "cmake-integration")
(defun cmake-integration-save-and-compile-no-completion (target)
  "Save the buffer and compile TARGET."
  (interactive "sTarget: ")
  (save-buffer 0)

  (check-if-build-folder-exists-and-throws-if-not)

  (setq cmake-integration-current-target target)
  (let ((compile-command (cmake-integration-get-build-command target)))
    (compile compile-command)))


(defun cmake-integration--get-target-type-from-name (target-name all-targets)
  "Get the type of the target with name TARGET-NAME from ALL-TARGETS.
ALL-TARGETS is an alist like the one returned by
`cmake-integration--get-targets-from-codemodel-json-file-2'."
  (let ((target (alist-get target-name all-targets nil nil 'equal)))
    ;; (cmake-integration--get-target-type target)
    (alist-get 'type target)
    ))


(defun cmake-integration--target-annotation-function (target-name)
  "Annotation function that takes a TARGET-NAME and return an annotation for it.

This is used in `cmake-integration--get-target-using-completions'
when completing a target name to generate an annotation for that
target, which is shown during the completions if you are using
the marginalia package, or in Emacs standard completion buffer."
  (pcase (car (split-string target-name cmake-integration--multi-config-separator))
    ("clean" (concat (cmake-integration--get-annotation-initial-spaces target-name) "Clean all compiled targets"))
    ("all" (concat (cmake-integration--get-annotation-initial-spaces target-name) "Compile all targets"))
    (_ (concat (cmake-integration--get-annotation-initial-spaces target-name) (cmake-integration--get-target-type-from-name target-name minibuffer-completion-table)))
    ))


(defun cmake-integration--get-target-using-completions (list-of-targets)
  "Ask the user to choose one of the targets in LIST-OF-TARGETS using completions."
  (let ((completion-extra-properties '(:annotation-function cmake-integration--target-annotation-function)))
    (completing-read "Target: " list-of-targets nil t)))


(defun cmake-integration--get-all-targets (json-filename)
  "Get all targets for completion specified in JSON-FILENAME.

Get the name of all targets for completion, respecting the value
of the `*-targets-during-completion' variables.

If a prefix argument is provided, then the value of
`cmake-integration-include-subproject-targets-during-completion'
will be ignored."
  (let* ((include-subprojects (or current-prefix-arg cmake-integration-include-subproject-targets-during-completion))
         (list-of-targets (if include-subprojects
                              (cmake-integration--get-targets-from-codemodel-json-file-2
                               json-filename)
                            (cmake-integration--get-targets-from-codemodel-json-file-2
                             json-filename
                             'cmake-integration--target-is-in-projectIndex0-p))))

    ;; Filter the list of targets
    (cond
     ;; Do not include utility and library targets
     ((and cmake-integration-hide-utility-targets-during-completion
           cmake-integration-hide-library-targets-during-completion)
      (seq-filter #'(lambda (target) (and
                                      (cmake-integration--target-is-not-utility-p target)
                                      (cmake-integration--target-is-not-library-p target)))
                  list-of-targets))

     ;; Do not include only utility targets
     (cmake-integration-hide-utility-targets-during-completion
      ;; Do not include utility targets
      (seq-filter 'cmake-integration--target-is-not-utility-p list-of-targets))

                                        ; Do not include only library targets
     (cmake-integration-hide-library-targets-during-completion
      (seq-filter 'cmake-integration--target-is-not-library-p list-of-targets))

     ;; Include all targets
     (t list-of-targets))))


;;;###autoload (autoload 'cmake-integration-save-and-compile "cmake-integration")
(defun cmake-integration-save-and-compile ()
  "Ask for a target name and compile it.

A list of target names is obtained from the project using CMake's
file API and completion is used to choose the desired target. If
that is not possible, ask for the target name without
completions."

  (interactive)
  ;; If the build folder is missing we should stop with an error
  (check-if-build-folder-exists-and-throws-if-not)

  (if-let* ((json-filename (cmake-integration--get-codemodel-reply-json-filename))
            ;; The list of targets also includes the convenience
            ;; targets 'all', 'clean' and optional 'install' target
            (list-of-targets (cmake-integration--get-all-targets json-filename))
            (chosen-target (cmake-integration--get-target-using-completions list-of-targets)))
      (cmake-integration-save-and-compile-no-completion chosen-target)

    (unless json-filename
      ;; If `json-filename' is nil that means we could not find the
      ;; CMake reply with the file API, which means the query file is
      ;; missing. All we need to do is to configure using either
      ;; `cmake-integration-cmake-reconfigure' or
      ;; `cmake-integration-cmake-configure-with-preset', which created
      ;; the query file.
      (display-warning 'cmake-integration "Could not find list of targets due to CMake file API file
missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset'."))

    (command-execute 'cmake-integration-save-and-compile-no-completion)))


;;;###autoload (autoload 'cmake-integration-save-and-compile-last-target "cmake-integration")
(defun cmake-integration-save-and-compile-last-target ()
  "Recompile the last target that was compiled (or `all')."
  (interactive)
  (cmake-integration-save-and-compile-no-completion
   (or cmake-integration-current-target "all")))


(defun cmake-integration-get-build-command (target)
  "Get the command to compile target TARGET."
  (pcase-let* ((`(,target-name ,config-name)
                (split-string target cmake-integration--multi-config-separator))
               (project-root (cmake-integration--get-project-root-folder))
               (preset-arg-or-build-folder (if cmake-integration-build-preset
                                               (format "--preset %s" (cmake-integration-get-last-build-preset-name))
                                             (cmake-integration--get-build-folder-relative-to-project)))
               (config-option (if config-name
                                  (format " --config %s" config-name)
                                "")))
    (format "cd %s && cmake --build %s%s --target %s"
            project-root
            preset-arg-or-build-folder
            config-option
            target-name)))


;; See CMake file API documentation for what projectIndex is
;; https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html
(defun cmake-integration--target-is-in-projectIndex0-p (target)
  "Return t if the projectIndex field of TARGET is 0."
  (eq (alist-get 'projectIndex target) 0))


(defun cmake-integration--target-is-not-utility-p (target)
  "Return t if TARGET type is not `UTILITY'."
  (not (equal (alist-get 'type target) "UTILITY"))
  )


(defun cmake-integration--target-is-not-library-p (target)
  "Return t if TARGET type is not `UTILITY'."
  (let ((type (alist-get 'type target)))
    (when type
      (not (equal (car (cdr (split-string type "_"))) "LIBRARY"))
      )))


(defun cmake-integration--get-targets-from-configuration (config &optional predicate)
  "Get all targets in CONFIG that match PREDICATE."
  (if predicate
      (seq-filter predicate (alist-get 'targets config))
    (alist-get 'targets config)))


(defun cmake-integration--get-target-name (target config-name)
  "Get name for TARGET, including CONFIG-NAME (if not nil)."
  (let* ((target-name (alist-get 'name target)))
    (cmake-integration--create-target-fullname target-name config-name)))


(defun cmake-integration--add-name-to-target (target config-name)
  "Add the target name to TARGET for CONFIG-NAME.

TARGET is a list of cons cells, including one with `name` field. This
function will extract the value in the name field and prepend it to the
list."
  (let ((target-name (cmake-integration--get-target-name target config-name)))
    (cons target-name target)))


(defun cmake-integration--get-prepared-targets-from-configuration (config config-name predicate)
  "Get all targets in CONFIG with name CONFIG-NAME that match PREDICATE.

The implicit targets `all', `clean' and optional `install' targets will
be returned as well.

CONFIG-NAME is non-nil only when using ninja multi-config generator,
where we have more than one configuration."
  (let ((install-rule? (cl-some (lambda (dir) (alist-get 'hasInstallRule dir))
                                (alist-get 'directories config)))
        (targets-in-config (cmake-integration--get-targets-from-configuration config predicate))
        (targets-and-name))
    (setq targets-and-name (mapcar (lambda (target-info) (cmake-integration--add-name-to-target target-info config-name))
                                   targets-in-config))
    ;; Add implicit 'all', 'clean' and optional 'install' targets
    (cmake-integration--add-all-clean-install-targets
     targets-and-name
     config-name
     install-rule?)))


(defun cmake-integration--add-all-clean-install-targets (targets config-name has-install-rule)
  "Return TARGETS with extra `all', `clean' and `install' for CONFIG-NAME.

The `install' target is only included if HAS-INSTALL-RULE is true."
  (let ((all-target (list (cmake-integration--create-target "all" config-name)))
        (clean-target (list (cmake-integration--create-target "clean" config-name)))
        (install-target (when has-install-rule (list (cmake-integration--create-target "install" config-name)))))
    (nconc all-target clean-target install-target targets)))


(defun cmake-integration--get-targets-from-codemodel-json-file (&optional json-filename predicate)
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
                            (cmake-integration--get-codemodel-reply-json-filename)))
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
                       (cmake-integration--get-prepared-targets-from-configuration config config-name predicate)))
                   ;; Sequence mapped in the mapcar
                   all-config))))


(defun cmake-integration--add-type-field-to-target (target)
  "Add a `type' field to a TARGET.

This will modify TARGET.

TARGET is a list containing the target name followend by many cons, with
each cons having some information about the TARGET. Particularly, TARGET
has a cons cell with a `jsonFile' car and a `\"someJsonFile.json\"'
filename. This json file will be read to extract the type that should be
added to TARGET."

  ;; ("target-name" (directoryIndex . 2) (id . "Continuous::@a44f0ac069e85531cdee") (jsonFile . "target-Continuous-Debug-32dbfb99931b31bc9c8f.json") (name . #1#) (projectIndex . 0))
  (let ((target-name (car (split-string (car target) cmake-integration--multi-config-separator))))
    (unless (or (equal target-name "all") (equal target-name "clean") (equal target-name "install"))
      (let* ((json-file (alist-get 'jsonFile target))
             (json-full-filename (file-name-concat (cmake-integration--get-reply-folder) json-file))
             (target-json-data (json-read-file json-full-filename)))
        ;; Set the value from the json data to `type' field
        (setf (alist-get 'type (cdr target)) (alist-get 'type target-json-data))))))


(defun cmake-integration--get-targets-from-codemodel-json-file-2 (&optional json-filename predicate)
  "Return the targets found in JSON-FILENAME that respect PREDICATE.

This function is the same as
`cmake-integration--get-targets-from-codemodel-json-file',
with the exception that it adds the type of each target to a
`type' field in the target. The main use for this information is
during completion of target names, where this type information is
shown as an annotation."

  ;; Start with the list of targets returned by
  ;; `cmake-integration--get-targets-from-codemodel-json-file',
  ;; then loop over each target to add the type information, skipping
  ;; the "all", "clean" and "install" targets.
  (let ((list-of-targets (cmake-integration--get-targets-from-codemodel-json-file json-filename predicate)))
    (mapc 'cmake-integration--add-type-field-to-target list-of-targets)
    list-of-targets))



(provide 'cmake-integration-build)

;;; cmake-integration-build.el ends here
