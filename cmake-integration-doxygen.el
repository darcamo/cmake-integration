;;; cmake-integration-doxygen.el --- Retrieve targets -*- lexical-binding: t -*-

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
  (let ((doxygen-command (format "cd %s && doxygen" (ci--get-docs-folder))))
    (compile doxygen-command)))


;;;###autoload (autoload 'cmake-integration-view-project-documentation "cmake-integration")
(defun ci-view-project-documentation ()
  "Open generated doxygen documentation."
  (interactive)
  (browse-url (file-name-concat (expand-file-name (ci--get-docs-folder)) "html/index.html")))



(provide 'cmake-integration-doxygen)

;;; cmake-integration-doxygen.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
