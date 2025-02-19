;;; cmake-integration-cpack.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; TODO: Allow specifying other arguments to "cmake --install", such
;; as "--config", "--component", "--prefix" and "--strip".
(defun cmake-integration--get-install-command ()
  "Get the command to run cmake install."
  (let ((build-folder (cmake-integration-get-build-folder)))
    (format "cmake --install %s" build-folder)))


(defun cmake-integration-run-cmake-install ()
  "Run `cmake --install' in the current build folder."
  (interactive)
  (compile (cmake-integration--get-install-command)))


(defun cmake-integration--get-cpack-command ()
  "Get the command to run cpack."
  (let* ((preset-name (cmake-integration-get-last-configure-preset-name))
         (build-folder (cmake-integration-get-build-folder))
         (project-folder (cmake-integration--get-project-root-folder)))
    (if preset-name
        (format "cd %s && cpack --preset %s" project-folder preset-name)
      (format "cd %s && cpack ." build-folder))))


;;;###autoload
(defun cmake-integration-run-cpack ()
  "Run cpack."
  (interactive)
  (compile (cmake-integration--get-cpack-command)))



(provide 'cmake-integration-cpack)

;;; cmake-integration-cpack.el ends here
