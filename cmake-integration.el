;;; cmake-integration.el --- Integrates with CMake to easily configure, compile and run -*- lexical-binding: t -*-

;; Author: Darlan Cavalcante Moreira
;; Maintainer: Darlan Cavalcante Moreira
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") f s json)
;; Homepage: homepage
;; Keywords: keywords


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

;; Provide functions to configure cmake projects, compile and run a
;; given target. Completions are provided with targets obtained from
;; cmake.

;;; Code:
(require 'project)
(require 'f)
(require 'json)


;; TODO: Only show the target names with
;; cmake-integration-save-and-compile if there is more than one
;; target.

;; TODO: Change this to a custom variable
(defvar cmake-integration-build-dir "build" "The build folder to use when no presets are used. If this is nil, then using presets is required")

(defvar cmake-integration-current-target-name nil)
(defvar cmake-integration-current-target-run-arguments "")

(defvar cmake-integration-last-configure-preset nil)


(defun cmake-integration-get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (project-root (project-current))
  )


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
          (error "Please call `cmake-integration-cmake-configure-with-preset` first and select a \"configure preset\", or set \"cmake-integration-build-dir\" to be the build folder")
          )
        )
      )
    ))


(defun cmake-integration-get-query-folder ()
  "Get the query folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/query/client-emacs")
  )


(defun cmake-integration-get-reply-folder ()
  "Get the reply folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/reply/")
  )


(defun cmake-integration-get-path-of-codemodel-query-file ()
  "Get the full path of the codemodel-query-file."
  (file-name-concat (cmake-integration-get-query-folder) "codemodel-v2")
  )


;; TODO: Check the "artifacts" in the target json file to get the executable path
(defun cmake-integration-get-compile-command (target-name)
  "Get the command to compile target TARGET-NAME."
  (if cmake-integration-last-configure-preset
      (format "cd %s && cmake --build --preset %s --target %s" (cmake-integration-get-project-root-folder) (cmake-integration-get-last-configure-preset-name) target-name)
    (format "cd %s && cmake --build . --target %s" (cmake-integration-get-build-folder) target-name)
    )
  )

(defun cmake-integration-get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."

  (elt (f-glob "codemodel-v2*json" (cmake-integration-get-reply-folder)) 0)
  )


(defun cmake-integration--get-preset-name (preset)
  "Get the name of the preset PRESET.

PRESET is an alist obtained from reading the cmake presets file
and getting one of the configure presets in it."
  (when preset (alist-get 'name preset))
  )


(defun cmake-integration-get-last-configure-preset-name ()
  "Get the 'name' field of the last preset used for configure."
  (cmake-integration--get-preset-name cmake-integration-last-configure-preset)
  )


(defun cmake-integration-create-empty-codemodel-file ()
  "Create an empty codemodel query file for CMake's file API."
  ;; Only do something if the file does not exist
  (unless (file-exists-p (cmake-integration-get-path-of-codemodel-query-file))
    ;; Create the folder if it does not exists yet
    (unless (file-exists-p (cmake-integration-get-query-folder))
      (shell-command (concat "mkdir -p " (cmake-integration-get-query-folder))))
    ;; Create the codemodel file
    (shell-command (concat "touch " (cmake-integration-get-path-of-codemodel-query-file)))
    )
  )


(defun cmake-integration-get-cmake-targets-from-codemodel-json-file (&optional json-filename)
  "Get a list of tartget names from the json file with name JSON-FILENAME.

If JSON-FILENAME is not provided, use the value obtained with
'cmake-integration-get-codemodel-reply-json-filename'."

  ;; If json-filename was not provided, get it from
  ;; 'cmake-integration-get-codemodel-reply-json-filename'.
  (unless json-filename
    (setq json-filename (cmake-integration-get-codemodel-reply-json-filename))
    )

  (let (cmake-configurations cmake-configuration-0 cmake-targets)
    ;; Get the list of configurations in the json file (this is a vector)
    ;; In my case I'll only have one, since I don't use a multi-config generator
    (setq cmake-configurations (alist-get 'configurations (json-read-file json-filename)))

    ;; Get the first configuration (we are assuming there is only one)
    (setq cmake-configuration-0 (elt cmake-configurations 0))

    ;; From that configuration we get the vector of targets
    (setq cmake-targets (alist-get 'targets cmake-configuration-0))

    ;; Then all we need is to get the name of each target and we will
    ;; have our vector of target names
    (mapcar (lambda (target) (let ((target-name (alist-get 'name target)))
                          (cons target-name target)
                          ))
            cmake-targets)
    ;; (mapcar (lambda (target) (alist-get 'name target)) cmake-targets)
    )
  )


(defun cmake-integration-get-cmake-configure-presets ()
  "Get the configure presets in both 'CMakePresets.json' and 'CMakeUserPresets.json' files."
  (let ((cmake-system-presets-filename (file-name-concat (cmake-integration-get-project-root-folder) "CMakePresets.json"))
        (cmake-user-presets-filename (file-name-concat (cmake-integration-get-project-root-folder) "CMakeUserPresets.json"))
        )

    ;; The mapcar will turn the list of presets (each preset is an
    ;; alist) into an alist where the key is the preset name. This
    ;; will allow us to use the returned alist as the COLLECTION
    ;; argument of completing-read and to also retrieve information of
    ;; the chosen preset.
    ;;
    ;; TODO: Use "displayName" field (if available) as the preset key
    ;;
    (mapcar (lambda (preset) (cons (alist-get 'name preset) preset))
            ;; We will read both the system and the user preset files to find configure presets there
            (append
             ;; Presets names in the CMakePresets.json file
             (when (file-exists-p cmake-system-presets-filename)
               (alist-get 'configurePresets (json-read-file cmake-system-presets-filename)))

             ;; Presets names in CMakeUserPresets.json
             (when (file-exists-p cmake-user-presets-filename)
               (alist-get 'configurePresets (json-read-file cmake-user-presets-filename)))
             )))
  )


(defun cmake-integration--cmake-configure-with-preset (preset)
  "Configure CMake using the preset PRESET."
  (compile (format "cd %s && cmake . --preset %s"
                   (cmake-integration-get-project-root-folder)
                   (cmake-integration--get-preset-name preset)))
  )


;;;###autoload
(defun cmake-integration-cmake-configure-with-preset ()
  "Configure CMake using one of the availeble presets.

A list of preset names if obtained from 'CMakePresets.json' and
'CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion) and CMake is configured with
the chosen preset."
  ;; TODO: Pass a predicate function to completing-read that remove any preset with "hidden" value of true
  (interactive)
  (let ((all-presets (cmake-integration-get-cmake-configure-presets))
        choice ;; this will be the preset "displayName" (if available) of the preset "name"
        )

    ;; Add a "No Preset" option to all-presets to allow a user to
    ;; remove the preset and use default build folder
    (setq all-presets (append all-presets '("No Preset")))
    (setq choice (completing-read "Build preset: " all-presets nil t))

    ;; If "No Preset" was selected, then we are not using any preset
    ;; and thus cmake-integration-last-configure-preset should be nil
    (if (equal choice "No Preset")
        (setq cmake-integration-last-configure-preset nil)
      (setq cmake-integration-last-configure-preset (alist-get choice all-presets nil nil 'equal))
      )

    ;; Configure the project -> This will do the right thing in both
    ;; cases when 'cmake-integration-last-configure-preset' is nil and when it
    ;; has a specific preset
    (cmake-integration-cmake-reconfigure)
    )
  )


;;;###autoload
(defun cmake-integration-cmake-reconfigure ()
  "Call cmake again to re-configure the project."
  (interactive)
  (cmake-integration-create-empty-codemodel-file)

  (if cmake-integration-last-configure-preset
      (cmake-integration--cmake-configure-with-preset cmake-integration-last-configure-preset)
    (compile (format "cd %s && cmake %s" (cmake-integration-get-build-folder) (cmake-integration-get-project-root-folder)))
    )
  )


(defun cmake-integration-get-target-executable-filename (&optional target-name)
  "Get the executable filename for the target TARGET-NAME.

The name is relative to the build folder. This is usually
something like just <target-name>, or bin/<target-name>.

Throws an error if the target is not an executable.

If TARGET-NAME is not provided use the last target (saved in a
'cmake-integration-current-target-name')."

  (unless target-name
    (setq target-name cmake-integration-current-target-name)
    )

  ;; The 'target' variable inside the 'let' has the data from the
  ;; codemodel json file for TARGET-NAME. This data is an alist and
  ;; includes a 'jsonFile' field, which has the name of another json
  ;; file with more data about the target. We read this json file and
  ;; save the data in the 'target-data' variable. From there we can
  ;; get the executable name from its 'artifacts' field.
  (let ((target (alist-get target-name (cmake-integration-get-cmake-targets-from-codemodel-json-file) nil nil 'equal))
        target-json-file
        target-data
        target-artifacts
        )

    ;;
    (unless target
      (if (equal target-name "all")
          (error "Target 'all' is not a valid executable target")
        (error "Unknown target '%s'" target-name)
        )

      )

    (setq target-json-file
          (file-name-concat (cmake-integration-get-reply-folder) (alist-get 'jsonFile target))
          )

    (setq target-data (json-read-file target-json-file))

    (unless (equal (alist-get 'type target-data) "EXECUTABLE")
      (error "Target '%s' is not an executable" target-name)
      )

    ;; Note that target-artifacts is a vector, but with a single
    ;; element in our case
    (setq target-artifacts (alist-get 'artifacts target-data))
    ;; We assume the vector has just one element
    (alist-get 'path (elt target-artifacts 0))
    )
  )

(defun check-if-build-folder-exists-and-throws-if-not ()
  "Check that the build folder exists and throws an error if not."
  (unless (file-exists-p (cmake-integration-get-build-folder))
    (error "The build folder is missing. Please run either cmake-integration-cmake-reconfigure or
cmake-integration-cmake-configure-with-preset to configure the project.")
    )
  )

    ;;;###autoload
(defun cmake-integration-save-and-compile-no-completion (target-name)
  "Save the buffer and compile TARGET-NAME."
  (interactive "sTarget name: ")
  (save-buffer 0)

  (check-if-build-folder-exists-and-throws-if-not)

  (setq cmake-integration-current-target-name target-name)
  (let ((compile-command (cmake-integration-get-compile-command target-name)))
    (compile compile-command)
    )
  )


;;;###autoload
(defun cmake-integration-save-and-compile ()
  "Ask for a target name and compile it.

A list of target names is obtained from the project using CMake's
file API and completion is used to choose the desired target. If
that is not possible, ask for the target name without
completions."

  (interactive)
  (let (list-of-targets
        chosen-target-name
        (json-filename (cmake-integration-get-codemodel-reply-json-filename)))
    (if json-filename
        ;; The list of targets include all targets found in the json file, as well as the "all" target
        (progn (setq list-of-targets (append
                                      (cmake-integration-get-cmake-targets-from-codemodel-json-file json-filename)
                                      '("all")
                                      ))
               (setq chosen-target-name (completing-read "Target Name: " list-of-targets))
               (cmake-integration-save-and-compile-no-completion chosen-target-name))

      ;; If the build folder is missing we should stop with an error
      (check-if-build-folder-exists-and-throws-if-not)

      ;; If json-filename is nil that means we could not find the
      ;; CMake reply with the file API, which means the query file is
      ;; missing. All we need to do is to configure using either
      ;; cmake-integration-cmake-reconfigure or
      ;; cmake-integration-cmake-configure-with-preset, which created
      ;; the query file.
      (display-warning 'cmake-integration "Could not find list of targets due to CMake file API file
missing. Please run either cmake-integration-cmake-reconfigure or
cmake-integration-cmake-configure-with-preset.")

      (command-execute 'cmake-integration-save-and-compile-no-completion)
      )
    )
  )


;;;###autoload
(defun cmake-integration-save-and-compile-last-target ( )
  "Recompile the last target that was compiled."
  (interactive)
  (if cmake-integration-current-target-name
      (cmake-integration-save-and-compile-no-completion cmake-integration-current-target-name)
    (cmake-integration-save-and-compile-no-completion "all")
    ))


;;;###autoload
(defun cmake-integration-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (let (run-command)
    ;; We have a configure preset
    (let* ((executable-filename (cmake-integration-get-target-executable-filename))
           (target-bin-folder (file-name-directory executable-filename))
           (target-executable-name (file-name-nondirectory executable-filename)))

      (setq run-command (format "cd %s && ./%s %s"
                                (file-name-concat (cmake-integration-get-build-folder) target-bin-folder )
                                target-executable-name
                                cmake-integration-current-target-run-arguments))
      )

    ;; Run the target
    (compile run-command)
    )
  )


;;;###autoload
(defun cmake-integration-run-last-target-with-arguments (run-arguments)
  "Run the last compiled target passing RUN-ARGUMENTS as arguments."
  (interactive "sArguments: ")
  (setq cmake-integration-current-target-run-arguments run-arguments)
  (cmake-integration-run-last-target)
  )


(defun cmake-integration-get-parent-preset-name (preset)
  "Get the name in the 'inherits' field of the preset PRESET."
  (alist-get 'inherits preset)
  )


(defun cmake-integration-get-parent-preset (preset)
  "docstring"
  (alist-get
   (cmake-integration-get-parent-preset-name preset)
   (cmake-integration-get-cmake-configure-presets)
   nil nil 'equal)
  )


(defun cmake-integration-get-binaryDir (preset)
  "Get the binaryDir field of a preset PRESET.

If not available, get the binaryDir or a parent preset."
  (when preset
    (let (binary-dir)
      (setq binary-dir (alist-get 'binaryDir preset))
      (unless binary-dir
        (setq binary-dir (cmake-integration-get-binaryDir (cmake-integration-get-parent-preset preset)))
        )
      binary-dir
      ))
  )

(provide 'cmake-integration)

;;; cmake-integration.el ends here
