;;; cmake-integration-configure.el --- Configure functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cmake-integration-core)
(require 'cmake-integration-core-presets)


(declare-function cmake-integration-get-conan-run-command "cmake-integration-conan.el")

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


(defun cmake-integration--get-configure-presets-from-filename (json-filename)
  "Get the configure presets from the JSON-FILENAME.

Return nil if the file does not exist."
  (cmake-integration--get-presets-of-given-type json-filename 'configurePresets))


(defun cmake-integration--get-configure-presets-from-filename-2 (json-filename)
  "Get the configure presets from the JSON-FILENAME.

It also considering included presets."
  (when (file-exists-p json-filename)
    ;; The mapcar will turn the list of presets (each preset is an
    ;; alist) into an alist where the key is the preset name. This
    ;; will allow us to use the returned alist as the COLLECTION
    ;; argument of completing-read and to also retrieve information of
    ;; the chosen preset.
    (let* ((expanded-preset-files (cmake-integration--expand-included-presets json-filename))
           (all-configure-presets (-mapcat
                                   #'cmake-integration--get-configure-presets-from-filename
                                   expanded-preset-files)))
      (seq-filter 'cmake-integration--is-preset-visible all-configure-presets))))


(defun cmake-integration-get-configure-presets ()
  "Get the configure presets.

Get the configure presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files."
  (let ((system-presets-file (cmake-integration--get-system-presets-file))
        (user-presets-file (cmake-integration--get-user-presets-file)))

    (append (cmake-integration--get-configure-presets-from-filename-2 system-presets-file)
            (cmake-integration--get-configure-presets-from-filename-2 user-presets-file))))


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


(defun cmake-integration--configure-annotation-function (preset)
  "Annotation function that takes a PRESET and return an annotation for it.

This is used in `cmake-integration-select-configure-preset'
when completing a preset name to generate an annotation for that
preset, which is shown during the completions if you are using
the marginalia package, or in Emacs standard completion buffer."

  (if (equal preset "No Preset")
      (concat (cmake-integration--get-annotation-initial-spaces "No Preset")
              "Don't use any preset."
              (cond
               ((and cmake-integration-build-dir cmake-integration-generator)
                (format "The build folder is '%s' and the generator is '%s'."
                        cmake-integration-build-dir
                        cmake-integration-generator))
               (t (format "The build folder is '%s'."
                          cmake-integration-build-dir))))
    ;; Note that `minibuffer-completion-table' has the list of
    ;; completions currently in use, from which we know PRESET is one
    ;; of them
    (concat (cmake-integration--get-annotation-initial-spaces preset) (alist-get 'displayName (alist-get preset minibuffer-completion-table nil nil 'equal)))))


(defun cmake-integration--prepare-for-completing-read (list-of-presets)
  "Transform LIST-OF-PRESETS to be used with `completing-read'.

This will create a transformed list of presets which maps each preset in
LIST-OF-PRESETS into a cons with the preset name and the preset itself.
It will also append the string 'No Preset' to this transformed list.
This makes it suitable to be used as the collection argument in
`completing-read'."
  (let ((transformed-list-of-presets (mapcar
   (lambda (preset) (cons (cmake-integration--get-preset-name preset) preset))
   list-of-presets)))
    ;; Add a "No Preset" option to all-presets to allow a user to
    ;; remove the preset and use default build folder
    (append transformed-list-of-presets '("No Preset"))))


;;;###autoload
(defun cmake-integration-select-configure-preset ()
  "Select a configure preset for CMake.

A list of preset names if obtained from `CMakePresets.json' and
`CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion)."
  (interactive)

  ;; Since we are changing the configure preset, the last target (if
  ;; any) might not be valid anymore. Thus, we unset the current
  ;; target.
  (setq cmake-integration-current-target nil)

  (let* ((all-presets (cmake-integration-get-configure-presets))
         (collection (cmake-integration--prepare-for-completing-read all-presets))
         (completion-extra-properties '(:annotation-function cmake-integration--configure-annotation-function))
         (choice (completing-read "Build preset: " collection nil t)))

    ;; If "No Preset" was selected, then we are not using any preset
    ;; and thus cmake-integration-configure-preset should be nil
    (if (equal choice "No Preset")
        (setq cmake-integration-configure-preset nil)
      (setq cmake-integration-configure-preset (cmake-integration--get-preset-by-name choice all-presets)))))


;;;###autoload
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


;;;###autoload
(defun cmake-integration-cmake-reconfigure ()
  "Call cmake again to re-configure the project.

Note: If no preset is used then
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON' is passed to cmake."
  (interactive)

  (cmake-integration--create-empty-codemodel-file)

  (let ((cmake-command (if cmake-integration-configure-preset
                           (cmake-integration--get-configure-command-with-preset
                            cmake-integration-configure-preset)
                         (cmake-integration--get-configure-command-without-preset)))
        ;; If a prefix argument was passed we will call conan before cmake
        (conan-command (if current-prefix-arg
                           (format "%s && " (cmake-integration-get-conan-run-command))
                         "")))

    (if cmake-integration-include-conan-toolchain-file
        (compile (format "%s%s --toolchain conan_toolchain.cmake" conan-command cmake-command))
      (compile (format "%s%s" conan-command cmake-command))))

  ;; Make a link of the compile_commands.json file to the project root
  (let ((compile-commands-file (file-name-concat (cmake-integration-get-build-folder) "compile_commands.json")))
    (when (and cmake-integration-create-compile-commands-link
               (file-exists-p compile-commands-file))
      (make-symbolic-link (file-relative-name compile-commands-file (cmake-integration--get-project-root-folder))  (cmake-integration--get-project-root-folder) t))))


(provide 'cmake-integration-configure)

;;; cmake-integration-configure.el ends here
