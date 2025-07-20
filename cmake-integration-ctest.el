;;; cmake-integration-ctest.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun ci-get-last-test-preset-name ()
  "Get the `name' field of the last preset used for test."
  (ci--get-preset-name ci-test-preset))


(defun ci--get-ctest-command (&optional extra-args)
  "Get the command to run the tests passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (let* ((preset-name (ci-get-last-test-preset-name))
         (project-root (ci--get-project-root-folder))
         (build-folder (ci-get-build-folder))
         (extra-args-string (string-join extra-args " ")))
    (if preset-name
        (format "cd %s && ctest --preset %s %s" project-root preset-name extra-args-string)
      (format "cd %s && ctest . %s" build-folder extra-args-string))))


;;;###autoload (autoload 'cmake-integration-run-ctest "cmake-integration")
(defun ci-run-ctest (&optional extra-args)
  "Run ctest passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (interactive)

  (when current-prefix-arg
    (push "--output-on-failure" extra-args)
    (push "--rerun-failed" extra-args))

  ;; Append the result of calling ci--ctest-get-include-labels-command-line-string and ci--ctest-get-exclude-labels-command-line-string to extra-args list
    (let ((include-labels (ci--ctest-get-include-labels-command-line-string))
            (exclude-labels (ci--ctest-get-exclude-labels-command-line-string)))
        (when (not (string-empty-p include-labels))
        (push include-labels extra-args))
        (when (not (string-empty-p exclude-labels))
        (push exclude-labels extra-args)))
  
  (compile (ci--get-ctest-command extra-args)))


;;;###autoload (autoload 'cmake-integration-get-test-presets "cmake-integration")
(defun ci-get-test-presets (&optional configure-preset)
  "Get the test presets associated with CONFIGURE-PRESET.

Get the test presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files whose
configure preset is CONFIGURE-PRESET. If CONFIGURE-PRESET is not
provided, then the value in the `cmake-integration-configure-preset'
variable will be used."
  (ci-get-presets-of-type 'testPresets configure-preset))



;;;###autoload (autoload 'cmake-integration-select-test-preset "cmake-integration")
(defun ci-select-test-preset ()
  "Select a test preset for CMake."
  (interactive)
  (when (not ci-configure-preset)
    (error "Please, select a configure preset first"))

  (let ((all-presets (ci-get-test-presets)))
    (setq ci-test-preset
          (ci-select-preset all-presets "Test preset: "))))


(defun ci--adjust-test-preset ()
  "Adjust the test preset when changing the configure preset.

This function is added to `cmake-integration-after-set-configure-preset-hook'."
  (if ci-configure-preset
      (let ((presets (ci-get-test-presets)))
        ;; Only change the test preset if there is excactly one
        ;; test preset for the conffigure preset
        (if (= (length presets) 1)
            (setq ci-test-preset (car presets))
          (setq ci-test-preset nil)))
    (setq ci-test-preset nil)))


(defun ci-print-ctest-labels ()
  "Get all the test labels from the current CMake project."
  (interactive)
  (ci-run-ctest '("--print-labels")))


(defun ci--get-all-ctest-labels ()
  "Get all the test labels from the current CMake project."
  (let* ((build-folder (ci-get-build-folder))
         (command (format "cd %s && ctest --print-labels" build-folder))
         (output (shell-command-to-string command))
         (labels (nthcdr 2 (split-string output "\n" t))))
    (mapcar #'string-trim-left (mapcar #'string-trim-right labels))))


(defun ci--select-ctest-labels ()
  "Select and returns one or more ctest labels."
  (let* ((labels (ci--get-all-ctest-labels))
         (selected-labels (completing-read-multiple
                           "Select labels to include (separated by comma): "
                           labels nil t)))
    (delete-dups selected-labels)))


(defun ci-select-ctest-labels-to-include ()
  "Select one or more labels to include in the ctest run."
  (interactive)
  (if current-prefix-arg
      (progn
        (setq ci--ctest-label-include-regexp nil)
        (message "Cleared ctest label include regexp."))
    (setq ci--ctest-label-include-regexp (ci--select-ctest-labels))))


(defun ci-select-ctest-labels-to-exclude ()
  "Select one or more labels to exclude from the ctest run."
  (interactive)
  (if current-prefix-arg
      (progn
        (setq ci--ctest-label-exclude-regexp nil)
        (message "Cleared ctest label exclude regexp."))
    (setq ci--ctest-label-exclude-regexp (ci--select-ctest-labels))))


(defun ci--ctest-get-include-labels-command-line-string ()
  "Get the command line options specifying the test labels to include."
  (if ci--ctest-label-include-regexp
      (s-join " " (mapcar
                   #'(lambda (s) (format "-L %s" s))
                   ci--ctest-label-include-regexp))
    ""))


(defun ci--ctest-get-exclude-labels-command-line-string ()
  "Get the command line options specifying the test labels to exclude."
  (if ci--ctest-label-exclude-regexp
      (s-join " " (mapcar
                   #'(lambda (s) (format "-LE %s" s))
                   ci--ctest-label-exclude-regexp))
    ""))


(add-hook 'ci-after-set-configure-preset-hook 'ci--adjust-test-preset)



(provide 'cmake-integration-ctest)

;;; cmake-integration-ctest.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
