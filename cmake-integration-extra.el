;;; cmake-integration-extra.el --- Extra miscellaneous functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cmake-integration-launch)

;;;###autoload (autoload 'cmake-integration-open-shell-in-build-folder "cmake-integration")
(defun ci-open-shell-in-build-folder ()
  "Open eshell in the build folder."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)
  (let ((default-directory (ci-get-build-folder)))
    (eshell)))


;;;###autoload (autoload 'cmake-integration-open-dired-in-build-folder "cmake-integration")
(defun ci-open-dired-in-build-folder ()
  "Open Dired in the buiild folder."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)
  (dired (ci-get-build-folder)))


(defun ci-open-dired-in-target-folder (&optional target-name)
  "Open Dired in the target folder for TARGET-NAME."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  (let* ((target-name (or target-name ci-current-target))
         (executable-relative-path (ci-get-target-executable-filename target-name))
         (target-full-path (ci-get-target-executable-full-path executable-relative-path))
         (target-folder (file-name-directory target-full-path)))

    (if (not (file-exists-p target-folder))
        (message "The target folder does not exist.")
      ;; Go to the target folder
      (dired target-folder)

      (if (not (file-exists-p target-full-path))
          (message "Note: Opened the target folder, but the target executable does not exist yet.")
        ;; Move cursor to the line containing the target executable in the dired buffer
        (goto-char (point-min))
        (search-forward (file-name-nondirectory target-full-path) nil t)))))


(provide 'cmake-integration-extra)

;;; cmake-integration-extra.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
