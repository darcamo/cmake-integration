;;; cmake-integration-doxygen.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun cmake-integration--get-docs-folder ()
  "Get the folder with the Doxyfile."
  (s-replace "${sourceDir}/" (cmake-integration--get-project-root-folder) cmake-integration-docs-folder)
  )

(defun cmake-integration-generate-project-documentation ( )
  "Generate the documentation in a cmake based project using Doxygen.

This assume that there is a `doc' folder in the project root,
from where the doxygen command will be run."
  (interactive)
  (let ((doxygen-command (format "cd %s && doxygen" (cmake-integration--get-docs-folder))))
    (compile doxygen-command)
    )
  )


(defun cmake-integration-view-project-documentation ()
  "Open generated doxygen documentation."
  (interactive)
  (browse-url (file-name-concat (expand-file-name (cmake-integration--get-docs-folder)) "html/index.html"))
  )




(provide 'cmake-integration-doxygen)

;;; cmake-integration-doxygen.el ends here
