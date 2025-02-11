;;; cmake-integration-extra.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun cmake-integration-open-shell-in-build-folder ()
  "Open eshell in the build folder."
  (interactive)
  (let ((default-directory (cmake-integration-get-build-folder)))
    (eshell)))


(defun cmake-integration-open-dired-in-build-folder ()
  "Open dired in the buiild folder."
  (interactive)
  (dired (cmake-integration-get-build-folder)))


(provide 'cmake-integration-extra)

;;; cmake-integration-extra.el ends here
