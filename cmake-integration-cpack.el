;;; cmake-integration-cpack.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun cmake-integration-get-last-package-preset-name ()
  "Get the `name' field of the last preset used for package."
  (cmake-integration--get-preset-name cmake-integration-package-preset))


;; TODO: Allow specifying other arguments to "cmake --install", such
;; as "--config", "--component", "--prefix" and "--strip".
(defun cmake-integration--get-install-command (&optional extra-args)
  "Get the command to run cmake install passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (let ((build-folder (cmake-integration-get-build-folder))
        (extra-args-string (string-join extra-args " ")))
    (format "cmake --install %s %s" build-folder extra-args-string)))


;;;###autoload (autoload 'cmake-integration-run-cmake-install "cmake-integration")
(defun cmake-integration-run-cmake-install (&optional extra-args)
  "Run `cmake --install' in the current build folder passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (interactive)
  (compile (cmake-integration--get-install-command extra-args)))


(defun cmake-integration--get-cpack-command ()
  "Get the command to run cpack."
  (let* ((preset-name (cmake-integration-get-last-package-preset-name))
         (project-folder (cmake-integration--get-project-root-folder))
         (build-folder (cmake-integration-get-build-folder)))
    (if preset-name
        (format "cd %s && cpack --preset %s" project-folder preset-name)
      (format "cd %s && cpack ." build-folder))))


;;;###autoload (autoload 'cmake-integration-run-cpack "cmake-integration")
(defun cmake-integration-run-cpack ()
  "Run cpack."
  (interactive)
  (compile (cmake-integration--get-cpack-command)))


;;;###autoload (autoload 'cmake-integration-get-package-presets "cmake-integration")
(defun cmake-integration-get-package-presets (&optional configure-preset)
  "Get the package presets associated with CONFIGURE-PRESET.

Get the package presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files whose
configure preset is CONFIGURE-PRESET. If CONFIGURE-PRESET is not
provided, then the value in the `cmake-integration-configure-preset'
variable will be used."
  (cmake-integration-get-presets-of-type 'packagePresets configure-preset))


;;;###autoload (autoload 'cmake-integration-select-package-preset "cmake-integration")
(defun cmake-integration-select-package-preset ()
  "Select a package preset for CMake."
  (interactive)
  (when (not cmake-integration-configure-preset)
    (error "Please, select a configure preset first"))

  (let ((all-presets (cmake-integration-get-package-presets)))
    (setq cmake-integration-package-preset
          (cmake-integration-select-preset all-presets "Package preset: "))))


(defun cmake-integration--adjust-package-preset ()
  "Adjust the package preset when changing the configure preset.

This function is added to `cmake-integration-after-set-configure-preset-hook'."
  (if cmake-integration-configure-preset
      (let ((presets (cmake-integration-get-package-presets)))
        ;; Only change the package preset if there is excactly one
        ;; package preset for the conffigure preset
        (if (= (length presets) 1)
            (setq cmake-integration-package-preset (car presets))
          (setq cmake-integration-package-preset nil)))
    (setq cmake-integration-package-preset nil)))


(add-hook 'cmake-integration-after-set-configure-preset-hook 'cmake-integration--adjust-package-preset)



(provide 'cmake-integration-cpack)

;;; cmake-integration-cpack.el ends here
