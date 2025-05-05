;;; cmake-integration-extra.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;###autoload (autoload 'cmake-integration-open-shell-in-build-folder "cmake-integration")
(defun ci-open-shell-in-build-folder ()
  "Open eshell in the build folder."
  (interactive)
  (let ((default-directory (ci-get-build-folder)))
    (eshell)))


;;;###autoload (autoload 'cmake-integration-open-dired-in-build-folder "cmake-integration")
(defun ci-open-dired-in-build-folder ()
  "Open Dired in the buiild folder."
  (interactive)
  (dired (ci-get-build-folder)))


(provide 'cmake-integration-extra)

;;; cmake-integration-extra.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
