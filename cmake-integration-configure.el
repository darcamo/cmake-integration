;;; cmake-integration-configure.el --- Configure functionality -*- lexical-binding: t -*-

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
(require 'cmake-integration-core)
(require 'cmake-integration-core-presets)


(defvar ci-after-set-configure-preset-hook nil "A hook run after changing the configure preset.")
(defvar ci-after-configure-hook nil "A hook run after calling cmake to configure the project.")


(defun ci--get-available-generators ()
  "Get the available CMake generators in the system."
  (let ((json-content
         (json-parse-string (shell-command-to-string "cmake -E capabilities")))
        (get-name-func (lambda (elem) (gethash "name" elem))))
    (mapcar get-name-func (gethash "generators" json-content))))


(defun ci-select-generator ()
  "Select a CMake generator from the available ones in the system."
  (interactive)
  (let* ((all-generators (ci--get-available-generators))
         (chosen-generator
          (completing-read "Select CMake generator: " all-generators nil t)))
    (setq ci-generator chosen-generator)))


(defun ci--perform-binaryDir-replacements (binaryDir project-root-folder preset-name)
  "Replace `${sourceDir}' and `${presetName}' in a BINARYDIR string.

They will be replaced with PROJECT-ROOT-FOLDER and PRESET-NAME, respectively.

NOTE: `project-root-folder' must end with a slash."
  (let* ((source-dir-replacement (cons "${sourceDir}/" project-root-folder))
         (preset-name-replacement (cons "${presetName}" preset-name))
         (replacements (list source-dir-replacement preset-name-replacement)))
    (s-replace-all replacements binaryDir)))


(defun ci--get-binaryDir (preset)
  "Get the `binaryDir' field of a preset PRESET.

If not available, get the binaryDir or a parent preset."
  (when preset
    (if-let* ((binary-dir (alist-get 'binaryDir preset)))
        binary-dir
      (let ((parents (ci--get-configure-parent-preset preset)))
        (if (vectorp parents)
            (let ((binaryDirs (mapcar 'ci--get-binaryDir parents)))
              (seq-find 'stringp binaryDirs))
          (ci--get-binaryDir parents))))))


(defun ci--extract-remote-prefix (path)
  "Extrtact a tramp remote prefix from PATH."
  (if (string-match "^\\(/[^:]+:[^:]+:\\)" path)
      (match-string 1 path)))


(defun ci--get-binaryDir-with-replacements (preset)
  "Get the `binaryDir' field of a preset PRESET, and also perform the replacements."
  (if-let* ((binaryDir (ci--get-binaryDir preset))
            (project-root (ci--get-project-root-folder))
            (preset-name (ci--get-preset-name preset)))
      (ci--perform-binaryDir-replacements binaryDir project-root preset-name)))


(defun ci--get-configure-preset-by-name (preset-name)
  "Get the preset from the list of presets with name PRESET-NAME."
  (let ((list-of-presets (ci-get-configure-presets t)))
    (ci--get-preset-by-name preset-name list-of-presets)))


(defun ci-get-configure-presets (&optional show-hidden)
  "Get all the configure presets.

If SHOW-HIDDEN is t, include hidden presets.

Get the configure presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files."
  (ci-get-all-presets-of-type 'configurePresets show-hidden))


(defun ci--get-configure-parent-preset (preset)
  "Get the parent preset of PRESET.

If the `inherits' field in the presets file is a single string, then a
single preset is returned. If the `inherits' is an array of names, then
a vector of presets is returned."
  (let ((parent-name (ci--get-parent-preset-name preset)))
    (if (vectorp parent-name)
        (let* ((parent-presets (mapcar 'ci--get-configure-preset-by-name parent-name))
               (parent-presets-no-nil (remq nil parent-presets))
               (presets (cl-coerce parent-presets-no-nil 'vector)))
          (if (seq-empty-p presets)
              nil
            presets))
      (ci--get-configure-preset-by-name parent-name))))


(defun ci-get-last-configure-preset-name ()
  "Get the `name' field of the last preset used for configure."
  (ci--get-preset-name ci-configure-preset))


(defun ci-add-cmake-cache-variables ()
  "Add a CMake cache variable.

The variable and its value are used when configuring the project with CMake."
  (interactive)
  ;; Ask the user for a cache variable name and value and add to the list in
  ;; ci-cache-variables
  (let* ((var-name (read-string "CMake Cache Variable Name: "))
         (var-value (read-string (format "Value for %s: " var-name))))
    (if-let* ((existing (assoc var-name ci-cache-variables)))
        (setcdr existing var-value)
      (push (cons var-name var-value) ci-cache-variables)))
  (ci--maybe-refresh-cache-variables-buffer))


(defun ci-remove-cmake-cache-variable ()
  "Remove a CMake cache variable."
  (interactive)
  ;; Ask the user for a cache variable name and remove from the list. Use
  ;; completing-read.
  (let* ((var-names (mapcar 'car ci-cache-variables))
         (var-to-remove
          (completing-read "CMake Cache Variable to remove: " var-names nil t)))
    (setq ci-cache-variables
          (cl-remove-if
           (lambda (pair) (string-equal (car pair) var-to-remove))
           ci-cache-variables)))
  (ci--maybe-refresh-cache-variables-buffer))


(defun ci-remove-all-cmake-cache-variables ()
  "Remove all CMake cache variables."
  (interactive)
  ;; Ask for confirmation when called interactively. If the user confirms,
  ;; clear the list of cache variables.
  (when (or (not (called-interactively-p 'any))
            (yes-or-no-p
             "Are you sure you want to remove all CMake cache variables? "))
    (setq ci-cache-variables nil))
  (ci--maybe-refresh-cache-variables-buffer))


(defun ci-display-cmake-cache-variables ()
  "Display the CMake cache variables in a separated buffer.

The variables are shown as `var1=value1', one per line."
  (interactive)
  (let ((buffer-name "*CMake Cache Variables*"))
    (with-output-to-temp-buffer buffer-name
      (princ "CMake Cache Variables:\n\n")
      (if ci-cache-variables
          (dolist (pair ci-cache-variables)
            (princ (format "%s=%s\n" (car pair) (cdr pair))))
        (princ "No CMake cache variables defined.")))
    (with-current-buffer buffer-name
      (read-only-mode 1))))


(defun ci--maybe-refresh-cache-variables-buffer ()
  "Refresh the CMake cache variables buffer, if it is open."
  (when (get-buffer-window "*CMake Cache Variables*" 0)
    (ci-display-cmake-cache-variables)))


(defun ci--maybe-quit-cache-variables-window ()
  "Quit the CMake cache variables window if it is open."
  (when-let* ((buffer-window (get-buffer-window "*CMake Cache Variables*" 0)))
    (quit-window t buffer-window)))


(defun ci--get-cmake-cache-variables-command-line-args ()
  "Get the CMake cache variables as command line arguments.

This is the \"-Dvar1=value1 -Dvar2=value2 ...\" part of the command
line."
  (mapconcat
   (lambda (pair) (format "-D%s=%s" (car pair) (cdr pair))) ci-cache-variables
   " "))


(defun ci--get-configure-command-with-preset (preset)
  "Get the directory and command to configure with CMake using the preset PRESET.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run (without any `cd`)."
  (list (ci--get-project-root-folder)
        (format "cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s --preset %s"
                (ci--get-cmake-cache-variables-command-line-args)
                (ci--get-preset-name preset))))


(defun ci--get-configure-command-without-preset ()
  "Get the directory and command to configure with CMake when presets are not used.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run (without any `cd`)."
  (let* ((build-folder (ci-get-build-folder))
         (cmakelists-location
          (file-relative-name (ci--get-project-root-folder) build-folder))
         (cache-variables-string
          (ci--get-cmake-cache-variables-command-line-args))
         (cmd
          (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s %s"
                  cache-variables-string
                  cmakelists-location))
         (final-cmd
          (if ci-generator
              (format "%s -G \"%s\"" cmd ci-generator)
            cmd)))
    (list build-folder final-cmd)))


;;;###autoload (autoload 'cmake-integration-select-configure-preset "cmake-integration")
(defun ci-select-configure-preset ()
  "Select a configure preset for CMake.

A list of preset names if obtained from `CMakePresets.json' and
`CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion)."
  (interactive)

  ;; Invalidate build folder cache
  (setq ci--build-folder-cache nil)

  (let ((all-presets (ci-get-configure-presets)))
    (setq ci-configure-preset
          (ci-select-preset all-presets "Configure preset: "))
    (run-hooks 'ci-after-set-configure-preset-hook)))


;;;###autoload (autoload 'cmake-integration-cmake-configure-with-preset "cmake-integration")
(defun ci-cmake-configure-with-preset ()
  "Configure CMake using one of the availeble presets.

A list of preset names if obtained from `CMakePresets.json' and
`CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion) and CMake is configured with
the chosen preset."
  (interactive)
  (ci-select-configure-preset)
  ;; Configure the project -> This will do the right thing in both
  ;; cases when 'cmake-integration-configure-preset' is nil and when it
  ;; has a specific preset
  (ci-cmake-reconfigure))


(defun ci--get-reconfigure-command ()
  "Get the directory and command to configure the project.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run (without any `cd`)."
  (if ci-configure-preset
      (ci--get-configure-command-with-preset
       ci-configure-preset)
    (ci--get-configure-command-without-preset)))


;;;###autoload (autoload 'cmake-integration-cmake-reconfigure "cmake-integration")
(defun ci-cmake-reconfigure (&optional extra-args)
  "Call cmake to configure the project passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command. Example for EXTRA-ARGS: `'(\"--fresh\")`

Note: If no preset is used then `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON' is
passed to cmake."
  (interactive)

  (ci--create-empty-codemodel-file)

  (pcase-let* ((`(,run-dir ,cmake-command) (ci--get-reconfigure-command))
               (extra-args-string (string-join extra-args " "))
               (cmake-command-with-extra-args (format "%s %s" cmake-command extra-args-string)))
    (let ((default-directory run-dir))
      (compile cmake-command-with-extra-args)))

  ;; Run the after configure hook
  ;; See cmake-integration-language-servers.el
  (run-hooks 'ci-after-configure-hook))


(provide 'cmake-integration-configure)

;;; cmake-integration-configure.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
