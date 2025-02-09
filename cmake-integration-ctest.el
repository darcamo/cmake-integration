;;; cmake-integration-ctest.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun cmake-integration--get-ctest-command (&optional only-failed?)
  "Get the command to run all tests, or only failed ones if ONLY-FAILED?.

If only-failed? is t, then \"--rerun-failed --output-on-failure\" is
added to the ctest command."
  (let* ((preset-name (cmake-integration-get-last-configure-preset-name))
         (build-folder (cmake-integration-get-build-folder))
         (ctest-command (if preset-name
                            (format "ctest --preset %s" preset-name)
                          (format "cd %s && ctest ." build-folder))))
    (if only-failed?
        (format "%s %s" ctest-command "--rerun-failed --output-on-failure")
      ctest-command)))


;;;###autoload
(defun cmake-integration-run-ctest ()
  "Run ctest."
  (interactive)
  (compile (cmake-integration--get-ctest-command current-prefix-arg)))



(provide 'cmake-integration-ctest)

;;; cmake-integration-ctest.el ends here
