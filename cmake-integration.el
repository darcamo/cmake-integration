;;; cmake-integration.el --- Integrates with CMake to easily configure, compile and run -*- lexical-binding: t -*-

;; Author: Darlan Cavalcante Moreira
;; Maintainer: Darlan Cavalcante Moreira
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") f s json)
;; Homepage: homepage
;; Keywords: keywords
;; URL: https://github.com/darcamo/cmake-integration/


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Provide functions to configure cmake projects, and to compile and
;; run a given target. Completions are provided with targets obtained
;; from cmake. It also support cmake presets.

;;; Code:
(require 'project)
(require 'f)
(require 's)
(require 'json)
(require 'cl-extra)


;; TODO: Only show the target names with
;; `cmake-integration-save-and-compile' if there is more than one
;; target.

(defcustom cmake-integration-build-dir "build"
  "The build folder to use when no presets are used.

If this is nil, then using presets is required." :type '(string) :group 'cmake-integration)

;; Column where annotation during completion should start
(defvar cmake-integration-annotation-column 30)

(defvar cmake-integration-current-target nil)
(defvar cmake-integration-current-target-run-arguments "")

(defvar cmake-integration-last-configure-preset nil)

;; Working directory when running a target This can be 'root, to run
;; from the project root, 'build, to run from the build folder, 'bin,
;; to run from the folder containing the executable, or a string with
;; a custom folder (relative to the project root)
(defcustom cmake-integration-run-working-directory 'bin
  "Working directory when running the executable.

Possible values are the symbols 'bin (to run from the folder
containing the executable), 'build (to run from the build folder)
and 'root (to run from the project root), as well as any string.
In the case of a string, it should match an existing subfolder of
the project root." :type '(choice symbol string)
  :group 'cmake-integration
  :safe 'cmake-integration--run-working-directory-p
  :local t
  )

(defconst cmake-integration--multi-config-separator "/"
  "Character used to separate target name from config name.

In case of of multi-config generators, target names have the
special form <target-name><sep><config-name> (e.g. 'all/Debug'
with '/' as configured separator).

Note: The selected separator shall be a character that it is not
a valid component of a CMake target name (see
https://cmake.org/cmake/help/latest/policy/CMP0037.html).")


;; BUG: This function seems to work correctly, but when used as the
;; ":safe" predicate in the defcustom Emacs still asks for confirming
;; if the variable is safe for the symbol values
(defun cmake-integration--run-working-directory-p (val)
  "Check if VAL is safe as a local variable.

Function to verify is VAL is save as a value for the
`cmake-integration-run-working-directory' variable"

  (if (stringp val)
      ;; Return t if VAL is a valid subfolder of the project root
      (file-exists-p (file-name-concat (cmake-integration-get-project-root-folder) val))
    ;; Return t if VAL is one of the accepted symbols
    (pcase val
      ('bin t)
      ('build t)
      ('root t)
      (_ nil)
      )
    )
  )


(defun cmake-integration--mktarget (target-name &optional config-name)
  "Return target constructed from from TARGET-NAME and CONFIG-NAME."
  (if config-name
      (concat target-name cmake-integration--multi-config-separator config-name)
    target-name))


(defun cmake-integration-get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (project-root (project-current)))


(defun cmake-integration-get-build-folder ()
  "Get the project build folder.

This is just the project folder concatenated with
'cmake-integration-build-dir' (when presets are not used) or with
the binaryDir field of the preset. It returns 'nil' if not in a
project."
  (let ((project-root-folder (cmake-integration-get-project-root-folder)))
    (when project-root-folder
      (if cmake-integration-last-configure-preset
          ;; We have a configure preset -> let's use build folder pointed by it
          (s-replace "${sourceDir}/" (cmake-integration-get-project-root-folder) (cmake-integration-get-binaryDir cmake-integration-last-configure-preset))

        ;; We don't have a configure preset
        (if cmake-integration-build-dir
            ;; If cmake-integration-build-dir is set, we can use it as the project
            (file-name-concat project-root-folder cmake-integration-build-dir)
          ;; If we do not have cmake-integration-build-dir set (it is
          ;; nil) throw an error asking the user to select a preset
          (error "Please call `cmake-integration-cmake-configure-with-preset' first and select a \"configure preset\", or set `cmake-integration-build-dir' to be the build folder"))))))


(defun cmake-integration-get-query-folder ()
  "Get the query folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/query/client-emacs"))


(defun cmake-integration-get-reply-folder ()
  "Get the reply folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/reply/"))


(defun cmake-integration-get-path-of-codemodel-query-file ()
  "Get the full path of the codemodel-query-file."
  (file-name-concat (cmake-integration-get-query-folder) "codemodel-v2"))


(defun cmake-integration-get-compile-command (target)
  "Get the command to compile target TARGET."
  (pcase-let ((`(,target-name ,config-name)
               (split-string target
                             cmake-integration--multi-config-separator)))
    (format "cd %s && cmake --build %s%s --target %s"
            (cmake-integration-get-project-root-folder)
            (if cmake-integration-last-configure-preset
                (format "--preset %s" (cmake-integration-get-last-configure-preset-name))
              (file-relative-name (cmake-integration-get-build-folder)
                                  (cmake-integration-get-project-root-folder)))
            (if config-name (format " --config %s" config-name) "")
            target-name)))


(defun cmake-integration-get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."

  (elt (f-glob "codemodel-v2*json" (cmake-integration-get-reply-folder)) 0))


(defun cmake-integration--get-preset-name (preset)
  "Get the name of the preset PRESET.

PRESET is an alist obtained from reading the cmake presets file
and getting one of the configure presets in it."
  (when preset (alist-get 'name preset)))


(defun cmake-integration-get-last-configure-preset-name ()
  "Get the 'name' field of the last preset used for configure."
  (cmake-integration--get-preset-name cmake-integration-last-configure-preset))


(defun cmake-integration-create-empty-codemodel-file ()
  "Create an empty codemodel query file for CMake's file API."
  ;; Only do something if the file does not exist
  (unless (file-exists-p (cmake-integration-get-path-of-codemodel-query-file))
    ;; Create the folder if it does not exists yet
    (unless (file-exists-p (cmake-integration-get-query-folder))
      (shell-command (concat "mkdir -p " (cmake-integration-get-query-folder))))
    ;; Create the codemodel file
    (shell-command (concat "touch " (cmake-integration-get-path-of-codemodel-query-file)))))


(defun cmake-integration-get-cmake-targets-from-codemodel-json-file (&optional json-filename)
  "Return the targets found in JSON-FILENAME.

Return an alist of (target-name . target-info) elements for
targets found in JSON-FILENAME.

JSON-FILENAME must be a CMake API codemodel file.

If JSON-FILENAME is not provided, use the value obtained with
'cmake-integration-get-codemodel-reply-json-filename'.

In addition to the targets defined in JSON-FILENAME, the returned
alist also contains elements for the implicit targets 'all' and
'clean' plus optional 'install' targets. These special targets
don't have the 'target-info' data."

  (let* ((json-filename (or json-filename
                            ;; If json-filename was not provided, get it from
                            ;; 'cmake-integration-get-codemodel-reply-json-filename'.
                            (cmake-integration-get-codemodel-reply-json-filename)))
         (configurations (alist-get 'configurations (json-read-file json-filename))))
    ;; The result of the nested `mapcar's below is a list of list of alists.
    ;; What we need a list of alists so remove one level and combine the next
    ;; level lists (of alists) of into a single list (of alists).
    (apply #'nconc
           ;; process configurations vector
           (mapcar (lambda (config-data)
                     (let ((config-name (and (> (length configurations) 1)
                                             (alist-get 'name config-data)))
                           (has-install-rule (cl-some (lambda (dir) (alist-get 'hasInstallRule dir))
                                                      (alist-get 'directories config-data))))
                       ;; add implicit 'all', 'clean' and optional 'install' targets
                       ;; to the list
                       (nconc `((,(cmake-integration--mktarget "all" config-name)))
                              `((,(cmake-integration--mktarget "clean" config-name)))
                              (when has-install-rule
                                `((,(cmake-integration--mktarget "install" config-name))))
                              ;; process targets vector, return an alist of (target .
                              ;; target-info) elements
                              (mapcar (lambda (target-info)
                                        (let ((target-name (alist-get 'name target-info)))
                                          (cons (cmake-integration--mktarget target-name config-name)
                                                target-info)))
                                      (alist-get 'targets config-data)))))
                   configurations))))


(defun cmake-integration-get-cmake-configure-presets ()
  "Get the configure presets.

Get the configure presets in both 'CMakePresets.json' and
'CMakeUserPresets.json' files."
  (let ((cmake-system-presets-filename (file-name-concat (cmake-integration-get-project-root-folder) "CMakePresets.json"))
        (cmake-user-presets-filename (file-name-concat (cmake-integration-get-project-root-folder) "CMakeUserPresets.json"))
        )

    ;; The mapcar will turn the list of presets (each preset is an
    ;; alist) into an alist where the key is the preset name. This
    ;; will allow us to use the returned alist as the COLLECTION
    ;; argument of completing-read and to also retrieve information of
    ;; the chosen preset.
    (mapcar (lambda (preset) (cons (alist-get 'name preset) preset))
            ;; We will read both the system and the user preset files to find configure presets there
            (append
             ;; Presets names in the CMakePresets.json file
             (when (file-exists-p cmake-system-presets-filename)
               (alist-get 'configurePresets (json-read-file cmake-system-presets-filename)))

             ;; Presets names in CMakeUserPresets.json
             (when (file-exists-p cmake-user-presets-filename)
               (alist-get 'configurePresets (json-read-file cmake-user-presets-filename)))))))


(defun cmake-integration--cmake-configure-with-preset (preset)
  "Configure CMake using the preset PRESET."
  (compile (format "cd %s && cmake . --preset %s"
                   (cmake-integration-get-project-root-folder)
                   (cmake-integration--get-preset-name preset))))


(defun cmake-integration--get-annotation-initial-spaces (annotated-string)
  "Get a string of spaces that should be added after ANNOTATED-STRING.

The number of spaces is enough such that when added after
ANNOTATED-STRING the annotation after that starts in the column
indicated by `cmake-integration-annotation-column'."
  (make-string
   (max 1
        (- cmake-integration-annotation-column (length annotated-string)))
   ;; 32 is the space character
   32))


(defun cmake-integration--configure-annotation-function (preset)
  "Annotation function that takes a PRESET and return an annotation for it.

This is used in `cmake-integration-cmake-configure-with-preset'
when completing a preset name to generate an annotation for that
preset, which is shown during the completions if you are using
the marginalia package, or in Emacs standard completion buffer."

  (if (equal preset "No Preset")
      (concat (cmake-integration--get-annotation-initial-spaces "No Preset") "Don't use any preset. The build folder will be the value of `cmake-integration-build-dir'.")
    ;; Note that `minibuffer-completion-table' has the list of
    ;; completions currently in use, from which we know PRESET is one
    ;; of them
    (concat (cmake-integration--get-annotation-initial-spaces preset) (alist-get 'displayName (alist-get preset minibuffer-completion-table nil nil 'equal)))))


;;;###autoload
(defun cmake-integration-cmake-configure-with-preset ()
  "Configure CMake using one of the availeble presets.

A list of preset names if obtained from 'CMakePresets.json' and
'CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion) and CMake is configured with
the chosen preset."
  ;; TODO: Pass a predicate function to completing-read that remove any preset with "hidden" value of true
  (interactive)

  ;; Since we are changing the configure preset, the last target (if
  ;; any) might not be valid anymore. Thus, we unset the current
  ;; target.
  (setq cmake-integration-current-target nil)

  (let ((all-presets
         ;; Add a "No Preset" option to all-presets to allow a user to
         ;; remove the preset and use default build folder
         (append (cmake-integration-get-cmake-configure-presets) '("No Preset")))
        choice)

    (let ((completion-extra-properties '(:annotation-function cmake-integration--configure-annotation-function )))
      (setq choice (completing-read "Build preset: " all-presets nil t))
      )

    ;; If "No Preset" was selected, then we are not using any preset
    ;; and thus cmake-integration-last-configure-preset should be nil
    (if (equal choice "No Preset")
        (setq cmake-integration-last-configure-preset nil)
      (setq cmake-integration-last-configure-preset (alist-get choice all-presets nil nil 'equal))
      )

    ;; Configure the project -> This will do the right thing in both
    ;; cases when 'cmake-integration-last-configure-preset' is nil and when it
    ;; has a specific preset
    (cmake-integration-cmake-reconfigure)))


;;;###autoload
(defun cmake-integration-cmake-reconfigure ()
  "Call cmake again to re-configure the project."
  (interactive)
  (cmake-integration-create-empty-codemodel-file)

  (if cmake-integration-last-configure-preset
      (cmake-integration--cmake-configure-with-preset cmake-integration-last-configure-preset)
    (compile (format "cd %s && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s" (cmake-integration-get-build-folder) (cmake-integration-get-project-root-folder)))))


(defun cmake-integration-get-target-executable-filename (&optional target)
  "Get the executable filename for the target TARGET.

The name is relative to the build folder. This is usually
something like just <target-name>, or bin/<target-name>.

Throws an error if the target is not an executable.

If TARGET-NAME is not provided use the last target (saved in a
'cmake-integration-current-target')."

  ;; If both `target' and `cmake-integration-current-target' are nil,
  ;; throw an error asking the UE to select a target first by calling
  ;; `cmake-integration-save-and-compile'
  (unless (or target cmake-integration-current-target)
    (error "Please select a target first `cmake-integration-save-and-compile' first")
    )

  ;; The `target-info' variable inside the 'let' has the data from the
  ;; codemodel json file for TARGET-NAME. This data is an alist and
  ;; includes a 'jsonFile' field, which has the name of another json
  ;; file with more data about the target. We read this json file and
  ;; save the data in the `target-data' variable. From there we can
  ;; get the executable name from its 'artifacts' field.
  (let* ((target (or target cmake-integration-current-target))
         (target-name (car (split-string target cmake-integration--multi-config-separator)))
         (target-info (alist-get
                       target
                       (cmake-integration-get-cmake-targets-from-codemodel-json-file)
                       nil nil 'equal)))

    (unless (cdr target-info)
      (if (member target-name '("all" "clean" "install"))
          (error "Target '%s' is not a valid executable target" target-name)
        (error "Unknown target: '%s'" target-name)))

    (let* ((target-json-file (file-name-concat
                              (cmake-integration-get-reply-folder)
                              (alist-get 'jsonFile target-info)))
           (target-data (json-read-file target-json-file)))

      (unless (equal (alist-get 'type target-data) "EXECUTABLE")
        (error "Target '%s' is not an executable" target-name))

      ;; Note that target-artifacts is a vector, but with a single
      ;; element in our case
      (let ((target-artifacts (alist-get 'artifacts target-data)))
        ;; We assume the vector has just one element
        (alist-get 'path (elt target-artifacts 0))))))


(defun check-if-build-folder-exists-and-throws-if-not ()
  "Check that the build folder exists and throws an error if not."
  (unless (file-exists-p (cmake-integration-get-build-folder))
    (error "The build folder is missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset' to configure the project")))


;;;###autoload
(defun cmake-integration-save-and-compile-no-completion (target)
  "Save the buffer and compile TARGET."
  (interactive "sTarget: ")
  (save-buffer 0)

  (check-if-build-folder-exists-and-throws-if-not)

  (setq cmake-integration-current-target target)
  (let ((compile-command (cmake-integration-get-compile-command target)))
    (compile compile-command)))


;;;###autoload
(defun cmake-integration-save-and-compile ()
  "Ask for a target name and compile it.

A list of target names is obtained from the project using CMake's
file API and completion is used to choose the desired target. If
that is not possible, ask for the target name without
completions."

  (interactive)
  ;; If the build folder is missing we should stop with an error
  (check-if-build-folder-exists-and-throws-if-not)

  (if-let* ((json-filename (cmake-integration-get-codemodel-reply-json-filename))
            ;; The list of targets includes all targets found in the codemodel
            ;; file, as well as the 'all', 'clean' and optional 'install' target
            (list-of-targets (cmake-integration-get-cmake-targets-from-codemodel-json-file json-filename))
            ;; TODO: Annotate each target during completion with the target 'type' (executable, library, meta target)
            ;;       See how it was done to add annotations to preset names
            (chosen-target (completing-read "Target: " list-of-targets)))
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


;;;###autoload
(defun cmake-integration-save-and-compile-last-target ()
  "Recompile the last target that was compiled (or 'all')."
  (interactive)
  (cmake-integration-save-and-compile-no-completion
   (or cmake-integration-current-target "all")))


(defun cmake-integration--get-run-command-project-root-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the project root folder."
  (format "cd %s && %s %s"
          (cmake-integration-get-project-root-folder)
          (file-name-concat (cmake-integration-get-build-folder) executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-build-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the build folder."
  (format "cd %s && %s %s"
          (cmake-integration-get-build-folder)
          executable-filename
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-bin-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the binary folder.

The binary folder is the folder containing the executable."
  (format "cd %s && ./%s %s"
          (file-name-concat (cmake-integration-get-build-folder) (file-name-directory executable-filename))
          (file-name-nondirectory executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-custom-cwd (executable-filename project-subfolder)
  "Get the correct run command EXECUTABLE-FILENAME from a PROJECT-SUBFOLDER."
  (format "cd %s && %s %s"
          (file-name-concat (cmake-integration-get-project-root-folder) project-subfolder)
          (file-name-concat (cmake-integration-get-build-folder) executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command (executable-filename)
  "Get the correct run command for EXECUTABLE-FILENAME.

Get the correct run command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable."
  (pcase cmake-integration-run-working-directory
    ('root (cmake-integration--get-run-command-project-root-cwd executable-filename))
    ('build (cmake-integration--get-run-command-build-folder-cwd executable-filename))
    ('bin (cmake-integration--get-run-command-bin-folder-cwd executable-filename))
    (_ (cmake-integration--get-run-command-custom-cwd executable-filename cmake-integration-run-working-directory))))


;;;###autoload
(defun cmake-integration-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  ;; Run the target
  (compile (cmake-integration--get-run-command (cmake-integration-get-target-executable-filename))))


;;;###autoload
(defun cmake-integration-run-last-target-with-arguments (run-arguments)
  "Run the last compiled target passing RUN-ARGUMENTS as arguments."
  (interactive "sArguments: ")
  (setq cmake-integration-current-target-run-arguments run-arguments)
  (cmake-integration-run-last-target))


(defun cmake-integration-get-parent-preset-name (preset)
  "Get the name in the `inherits' field of the preset PRESET."
  (alist-get 'inherits preset))


(defun cmake-integration-get-parent-preset (preset)
  "Get the parent preset of PRESET."
  (alist-get
   (cmake-integration-get-parent-preset-name preset)
   (cmake-integration-get-cmake-configure-presets)
   nil nil 'equal))


(defun cmake-integration-get-binaryDir (preset)
  "Get the `binaryDir' field of a preset PRESET.

If not available, get the binaryDir or a parent preset."
  (when preset
    (let (binary-dir)
      (setq binary-dir (alist-get 'binaryDir preset))
      (unless binary-dir
        (setq binary-dir (cmake-integration-get-binaryDir (cmake-integration-get-parent-preset preset)))
        )
      binary-dir)))


(provide 'cmake-integration)

;;; cmake-integration.el ends here
