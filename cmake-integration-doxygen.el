;;; cmake-integration-doxygen.el --- Doxygen related commands -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun ci--get-docs-folder ()
  "Get the folder with the Doxyfile."
  (s-replace "${sourceDir}/" (ci--get-project-root-folder) ci-docs-folder))


;;;###autoload (autoload 'cmake-integration-generate-project-documentation "cmake-integration")
(defun ci-generate-project-documentation ( )
  "Generate the documentation in a cmake based project using Doxygen.

This assume that there is a `doc' folder in the project root,
from where the doxygen command will be run."
  (interactive)
  (let ((docs-folder (expand-file-name (ci--get-docs-folder))))
    (unless (file-directory-p docs-folder)
      (user-error "Documentation folder does not exist: %s" docs-folder))
    (unless (file-exists-p (expand-file-name "Doxyfile" docs-folder))
      (user-error "No Doxyfile found in %s" docs-folder))
    (let ((default-directory docs-folder))
      (compile "doxygen"))))


;;;###autoload (autoload 'cmake-integration-view-project-documentation "cmake-integration")
(defun ci-view-project-documentation ()
  "Open generated doxygen documentation."
  (interactive)
  (browse-url-of-file (file-name-concat (expand-file-name (ci--get-docs-folder)) "html/index.html")))


;;;###autoload (autoload 'cmake-integration-view-project-documentation-in-eww "cmake-integration")
(defun ci-view-project-documentation-in-eww ()
  "Open generated doxygen documentation in eww."
  (interactive)
  (eww-open-file (file-name-concat (expand-file-name (ci--get-docs-folder)) "html/index.html")))


(provide 'cmake-integration-doxygen)

;;; cmake-integration-doxygen.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
