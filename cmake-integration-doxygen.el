;;; cmake-integration-doxygen.el --- Doxygen related commands -*- lexical-binding: t -*-

;; Copyright (C) 2025 Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs

;; This file is part of cmake-integration.

;; cmake-integration is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cmake-integration is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cmake-integration. If not, see <https://www.gnu.org/licenses/>.

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
  (let ((uri (file-name-concat (expand-file-name (ci--get-docs-folder)) "html/index.html")))
    (if (file-remote-p uri)
        (error "Cannot view remote documentation using a local browser")
      (browse-url-of-file uri))))


;;;###autoload (autoload 'cmake-integration-view-project-documentation-in-eww "cmake-integration")
(defun ci-view-project-documentation-in-eww ()
  "Open generated doxygen documentation in eww."
  (interactive)
  (let ((uri (file-name-concat (expand-file-name (ci--get-docs-folder)) "html/index.html")))
    (eww-open-file uri)))


(provide 'cmake-integration-doxygen)

;;; cmake-integration-doxygen.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
