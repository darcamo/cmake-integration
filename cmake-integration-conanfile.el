;;; cmake-integration-conanfile.el --- Interact with conanfile.txt -*- lexical-binding: t -*-

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

(require 'cmake-integration-core)


(defun ci--find-conanfile ()
  "Return the path to the conanfile.txt or conanfile.py in the project root."
  (if-let* ((project-root (ci--get-project-root-folder)))
      (let* ((conanfile-txt (f-join (ci--get-project-root-folder) "conanfile.txt"))
             (conanfile-py (f-join (ci--get-project-root-folder) "conanfile.py")))
        (cond
         ((f-exists? conanfile-txt) conanfile-txt)
         ((f-exists? conanfile-py) conanfile-py)
         (t nil)))
    nil))  ;; No project root found


(defun ci--add-requirement-to-conanfile-txt (filepath package)
  "Add PACKAGE to the [requires] section of the conanfile.txt at FILEPATH."
  (with-temp-file filepath
    (insert-file-contents-literally filepath)
    (goto-char (point-min))
    (if-let* ((point (re-search-forward "^\\[requires\\]" nil t))
              ;; Forward to the next line after "[requires]". The value of "n"
              ;; will be zero if there was a new line. If there was no new line,
              ;; it will be 1 and pointer will be in the same line of the
              ;; "[requires]" section.
              (n (forward-line)))
        (if (> n 0)
            (insert "\n" package "\n")
          (insert package "\n"))
      (message (format "There is no '[requires]' section in '%s'" filepath)))))


(defun ci-add-requirement-to-project-conanfile-txt (package)
  "Add PACKAGE to the conanfile.txt in the project root."
  (let ((conanfile (ci--find-conanfile)))
    (if conanfile
        (ci--add-requirement-to-conanfile-txt conanfile package)
      (message "No conanfile.txt found in the project root.")))
  )


(provide 'cmake-integration-conanfile)

;;; cmake-integration-conanfile.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
