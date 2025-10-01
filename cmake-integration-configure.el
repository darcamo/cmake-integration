;;; cmake-integration-configure.el --- Configure functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cmake-integration-core)
(require 'cmake-integration-core-presets)


(defvar ci-after-set-configure-preset-hook nil "A hook run after changing the configure preset.")


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


(defun ci--get-binaryDir-with-replacements (preset)
  "Get the `binaryDir' field of a preset PRESET, and also perform the replacements."
  (let ((binaryDir (ci--get-binaryDir preset)))
    (when binaryDir
      (let ((project-root (ci--get-project-root-folder))
            (preset-name (ci--get-preset-name preset))
            (binaryDir (ci--get-binaryDir preset)))
        (ci--perform-binaryDir-replacements binaryDir project-root preset-name)))))


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


(defun ci--get-configure-command-with-preset (preset)
  "Get the directory and command to configure with CMake using the preset PRESET.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run (without any `cd`)."
  (list (ci--get-project-root-folder)
        (format "cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON --preset %s"
                (ci--get-preset-name preset))))


(defun ci--get-configure-command-without-preset ()
  "Get the directory and command to configure with CMake when presets are not used.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run (without any `cd`)."
  (let* ((build-folder (ci-get-build-folder))
         (cmakelists-location (file-relative-name (ci--get-project-root-folder) build-folder))
         (cmd (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s" cmakelists-location))
         (final-cmd (if ci-generator
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


(defun ci--create-link-to-compile-commands ( )
  "Create a symbolic link to the compile_commands file in the project root.

This will only create the link if
`cmake-integration-create-compile-commands-link' is t and the
`compile_commands.json' file exists in the build folder."
  (let* ((compile-commands-file (file-name-concat (ci-get-build-folder) "compile_commands.json"))
         (should-create-link (and ci-create-compile-commands-link
                                  (file-exists-p compile-commands-file)))
         (destination-directory (ci--get-project-root-folder))
         (target (file-relative-name compile-commands-file destination-directory)))
    (when should-create-link
      (make-symbolic-link target destination-directory t))))


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

  ;; Make a link of the compile_commands.json file to the project root
  (ci--create-link-to-compile-commands))


(provide 'cmake-integration-configure)

;;; cmake-integration-configure.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
