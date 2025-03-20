;;; cmake-integration-ctest.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun cmake-integration-get-last-test-preset-name ()
  "Get the `name' field of the last preset used for test."
  (cmake-integration--get-preset-name cmake-integration-test-preset))


(defun cmake-integration--get-ctest-command (&optional extra-args)
  "Get the command to run the tests passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (let* ((preset-name (cmake-integration-get-last-test-preset-name))
         (project-root (cmake-integration--get-project-root-folder))
         (build-folder (cmake-integration-get-build-folder))
         (extra-args-string (string-join extra-args " ")))
    (if preset-name
        (format "cd %s && ctest --preset %s %s" project-root preset-name extra-args-string)
      (format "cd %s && ctest . %s" build-folder extra-args-string))))


;;;###autoload (autoload 'cmake-integration-run-ctest "cmake-integration")
(defun cmake-integration-run-ctest (&optional extra-args)
  "Run ctest passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (interactive)

  (when current-prefix-arg
    (push "--output-on-failure" extra-args)
    (push "--rerun-failed" extra-args))
  
  (compile (cmake-integration--get-ctest-command extra-args)))


;;;###autoload (autoload 'cmake-integration-get-test-presets "cmake-integration")
(defun cmake-integration-get-test-presets (&optional configure-preset)
  "Get the test presets associated with CONFIGURE-PRESET.

Get the test presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files whose
configure preset is CONFIGURE-PRESET. If CONFIGURE-PRESET is not
provided, then the value in the `cmake-integration-configure-preset'
variable will be used."
  (cmake-integration-get-presets-of-type 'testPresets configure-preset))



;;;###autoload (autoload 'cmake-integration-select-test-preset "cmake-integration")
(defun cmake-integration-select-test-preset ()
  "Select a test preset for CMake."
  (interactive)
  (when (not cmake-integration-configure-preset)
    (error "Please, select a configure preset first"))

  (let ((all-presets (cmake-integration-get-test-presets)))
    (setq cmake-integration-test-preset
          (cmake-integration-select-preset all-presets "Test preset: "))))


(defun cmake-integration--adjust-test-preset ()
  "Adjust the test preset when changing the configure preset.

This function is added to `cmake-integration-after-set-configure-preset-hook'."
  (if cmake-integration-configure-preset
      (let ((presets (cmake-integration-get-test-presets)))
        ;; Only change the test preset if there is excactly one
        ;; test preset for the conffigure preset
        (if (= (length presets) 1)
            (setq cmake-integration-test-preset (car presets))
          (setq cmake-integration-test-preset nil)))
    (setq cmake-integration-test-preset nil)))


(add-hook 'cmake-integration-after-set-configure-preset-hook 'cmake-integration--adjust-test-preset)



(provide 'cmake-integration-ctest)

;;; cmake-integration-ctest.el ends here
