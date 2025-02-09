;;; cmake-integration-cpack.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun cmake-integration--get-cpack-command ()
  "Get the command to run cpack."
  (let* ((preset-name (cmake-integration-get-last-configure-preset-name))
         (build-folder (cmake-integration-get-build-folder)))
    (if preset-name
        (format "cpack --preset %s" preset-name)
      (format "cd %s && cpack ." build-folder))))


;;;###autoload
(defun cmake-integration-run-cpack ()
  "Run cpack."
  (interactive)
  (compile (cmake-integration--get-cpack-command)))



(provide 'cmake-integration-cpack)

;;; cmake-integration-cpack.el ends here
