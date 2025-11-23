;;; cmake-integration-extra.el --- Extra miscellaneous functionality -*- lexical-binding: t -*-

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
(require 'cmake-integration-launch)

;;;###autoload (autoload 'cmake-integration-open-shell-in-build-folder "cmake-integration")
(defun ci-open-shell-in-build-folder ()
  "Open eshell in the build folder."
  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)
  (let ((default-directory (ci-get-build-folder)))
    (eshell)))


;;;###autoload (autoload 'cmake-integration-open-dired-in-build-folder "cmake-integration")
(defun ci-open-dired-in-build-folder ()
  "Open Dired in the buiild folder."
  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)
  (dired (ci-get-build-folder)))


(defmacro ci--with-target-folder (target-name &rest body)
  "Execute BODY with TARGET-NAME's folder.
Checks if the build folder exists and gets the target folder for
TARGET-NAME. Executes BODY in the context where `target-folder' is bound
to the target directory."
  `(progn
     (ci--check-if-build-folder-exists-and-throws-if-not)
     (let* ((target-name (or ,target-name ci-current-target))
            (executable-relative-path (ci-get-target-executable-filename target-name))
            (target-full-path (ci-get-target-executable-full-path executable-relative-path))
            (target-folder (file-name-directory target-full-path)))
       (if (not (file-exists-p target-folder))
           (message "The target folder does not exist.")

         ;; @body has access to `target-folder'
         ,@body))))


(defun ci-open-dired-in-target-folder (&optional target-name)
  "Open eshell in the target folder for TARGET-NAME.

If TARGET-NAME is not provided, then the value in
`cmake-integration-current-target' is used."
  (interactive)
  (ci--with-target-folder
   target-name

   ;; Go to the target folder
   (dired target-folder)

   (if (not (file-exists-p target-full-path))
       (message "Note: Opened the target folder, but the target executable does not exist yet.")
     ;; Move cursor to the line containing the target executable in the dired buffer
     (goto-char (point-min))
     (search-forward (file-name-nondirectory target-full-path) nil t))))


(defun ci-open-eshell-in-target-folder (&optional target-name)
  "Open eshell in the target folder for TARGET-NAME.

If TARGET-NAME is not provided, then the value in
`cmake-integration-current-target' is used."
  (interactive)
  (ci--with-target-folder
   target-name
   (let ((default-directory target-folder))
     (eshell))))


(provide 'cmake-integration-extra)

;;; cmake-integration-extra.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
