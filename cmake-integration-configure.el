;;; cmake-integration-configure.el --- Configure functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cmake-integration-core)
(require 'cmake-integration-core-presets)


(declare-function cmake-integration-get-conan-run-command "cmake-integration-conan.el")


(defvar cmake-integration-after-set-configure-preset-hook nil "A hook run after changing the configure preset.")


(defun cmake-integration--perform-binaryDir-replacements (binaryDir project-root-folder preset-name)
  "Replace `${sourceDir}' and `${presetName}' in a BINARYDIR string.

They will be replaced with PROJECT-ROOT-FOLDER and PRESET-NAME, respectively.

NOTE: `project-root-folder' must end with a slash."
  (let* ((source-dir-replacement (cons "${sourceDir}/" project-root-folder))
         (preset-name-replacement (cons "${presetName}" preset-name))
         (replacements (list source-dir-replacement preset-name-replacement)))
    (s-replace-all replacements binaryDir)))


(defun cmake-integration--get-binaryDir (preset)
  "Get the `binaryDir' field of a preset PRESET.

If not available, get the binaryDir or a parent preset."
  (when preset
    (if-let* ((binary-dir (alist-get 'binaryDir preset)))
        binary-dir
      (let ((parents (cmake-integration--get-configure-parent-preset preset)))
        (if (vectorp parents)
            (let ((binaryDirs (mapcar 'cmake-integration--get-binaryDir parents)))
              (seq-find 'stringp binaryDirs))
          (cmake-integration--get-binaryDir parents))))))


(defun cmake-integration--get-binaryDir-with-replacements (preset)
  "Get the `binaryDir' field of a preset PRESET, and also perform the replacements."
  (let ((binaryDir (cmake-integration--get-binaryDir preset)))
    (when binaryDir
      (let ((project-root (cmake-integration--get-project-root-folder))
            (preset-name (cmake-integration--get-preset-name preset))
            (binaryDir (cmake-integration--get-binaryDir preset)))
        (cmake-integration--perform-binaryDir-replacements binaryDir project-root preset-name)))))


(defun cmake-integration--get-configure-preset-by-name (preset-name)
  "Get the preset from the list of presets with name PRESET-NAME."
  (let ((list-of-presets (cmake-integration-get-configure-presets)))
    (cmake-integration--get-preset-by-name preset-name list-of-presets)))


(defun cmake-integration-get-configure-presets ()
  "Get all the non-hidden configure presets.

Get the configure presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files."
  (cmake-integration-get-all-presets-of-type 'configurePresets))


(defun cmake-integration--get-configure-parent-preset (preset)
  "Get the parent preset of PRESET.

If the `inherits' field in the presets file is a single string, then a
single preset is returned. If the `inherits' is an array of names, then
a vector of presets is returned."
  (let ((parent-name (cmake-integration--get-parent-preset-name preset)))
    (if (vectorp parent-name)
        (let ((presets (cl-coerce (remq nil (mapcar 'cmake-integration--get-configure-preset-by-name parent-name)) 'vector)))
          (if (seq-empty-p presets)
              nil
            presets))
      (cmake-integration--get-configure-preset-by-name parent-name))))


(defun cmake-integration-get-last-configure-preset-name ()
  "Get the `name' field of the last preset used for configure."
  (cmake-integration--get-preset-name cmake-integration-configure-preset))


(defun cmake-integration--get-configure-command-with-preset (preset)
  "Get the command to configure with CMake using the preset PRESET."
  (format "cd %s && cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON --preset %s"
          (cmake-integration--get-project-root-folder)
          (cmake-integration--get-preset-name preset)))


(defun cmake-integration--get-configure-command-without-preset ()
  "Get the command to configure with CMake when presets are not used."
  (if cmake-integration-generator
      ;; case with generator set
      (format "cd %s && cmake -G \"%s\" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s"
              (cmake-integration-get-build-folder)
              cmake-integration-generator
              (cmake-integration--get-project-root-folder))

    ;; case without generator set
    (format "cd %s && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s"
            (cmake-integration-get-build-folder)
            (cmake-integration--get-project-root-folder))))


;;;###autoload (autoload 'cmake-integration-select-configure-preset "cmake-integration")
(defun cmake-integration-select-configure-preset ()
  "Select a configure preset for CMake.

A list of preset names if obtained from `CMakePresets.json' and
`CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion)."
  (interactive)

  (let ((all-presets (cmake-integration-get-configure-presets)))
    (setq cmake-integration-configure-preset
          (cmake-integration-select-preset all-presets "Configure preset: "))
    (run-hooks 'cmake-integration-after-set-configure-preset-hook)))


;;;###autoload (autoload 'cmake-integration-cmake-configure-with-preset "cmake-integration")
(defun cmake-integration-cmake-configure-with-preset ()
  "Configure CMake using one of the availeble presets.

A list of preset names if obtained from `CMakePresets.json' and
`CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion) and CMake is configured with
the chosen preset."
  (interactive)
  (cmake-integration-select-configure-preset)
  ;; Configure the project -> This will do the right thing in both
  ;; cases when 'cmake-integration-configure-preset' is nil and when it
  ;; has a specific preset
  (cmake-integration-cmake-reconfigure))


(defun cmake-integration--create-link-to-compile-commands ( )
  "Create a symbolic link to the compile_commands file in the project root.

This will only create the link if
`cmake-integration-create-compile-commands-link' is t and the
`compile_commands.json' file exists in the build folder."
  (let* ((compile-commands-file (file-name-concat (cmake-integration-get-build-folder) "compile_commands.json"))
         (should-create-link (and cmake-integration-create-compile-commands-link
                                  (file-exists-p compile-commands-file)))
         (destination-directory (cmake-integration--get-project-root-folder))
         (target (file-relative-name compile-commands-file destination-directory)))
    (when should-create-link
      (make-symbolic-link target destination-directory t))))


(defun cmake-integration--get-reconfigure-command ()
  "Get the cmake command to configure the project."
  (if cmake-integration-configure-preset
      (cmake-integration--get-configure-command-with-preset
       cmake-integration-configure-preset)
    (cmake-integration--get-configure-command-without-preset)))


;;;###autoload (autoload 'cmake-integration-cmake-reconfigure "cmake-integration")
(defun cmake-integration-cmake-reconfigure ()
  "Call cmake again to re-configure the project.

Note: If no preset is used then
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON' is passed to cmake."
  (interactive)

  (cmake-integration--create-empty-codemodel-file)

  (let* ((cmake-command (cmake-integration--get-reconfigure-command))
         ;; If a prefix argument was passed we will call conan before cmake
         (cmake-and-conan-command (if current-prefix-arg
                                      (cmake-integration--prepend-conan-command cmake-command)
                                    cmake-command)))
    (compile cmake-and-conan-command))

  ;; Make a link of the compile_commands.json file to the project root
  (cmake-integration--create-link-to-compile-commands))


(provide 'cmake-integration-configure)

;;; cmake-integration-configure.el ends here
