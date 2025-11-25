;;; cmake-integration-persistence.el --- Save and restore the state of cmake integration -*- lexical-binding: t -*-

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

;; Functions to save and restore the state of cmake-integration.

;;; Code:

(require 'project nil t)
(require 'cmake-integration-variables nil t)
(require 'cmake-integration-core nil t)


(defvar ci-last-save-or-restore-state nil
  "Store the location of the state file used in the last save or restore.")


(defconst ci--state-variables
  '(cmake-integration--target-type-cache
    cmake-integration-current-target
    cmake-integration-run-arguments
    cmake-integration-configure-preset
    cmake-integration-build-preset
    cmake-integration-test-preset
    cmake-integration-package-preset)
  "List of relevante variables in cmake-integration to save current state.")


(defconst ci--state-file-name "cmake-integration-state.el"
  "Filename used to persist cmake-integration state.")


(defun ci--get-persist-location-in-emacs-directory ()
  "Return the directory in `user-emacs-directory' to save state.

Return nil if not in a project."
  (if-let* ((project (project-current))
            (project-identifier
             (file-name-nondirectory
              (directory-file-name (project-root project)))))
    (file-name-concat user-emacs-directory
                      "cmake-integration"
                      project-identifier)))


(defun ci--get-persist-location-in-project-directory ()
  "Return the directory in project to save state.

Return nil if not in a project."
  (if-let* ((root-folder (ci--get-project-root-folder)))
    (file-name-concat root-folder "cmake-integration/")))


(defun ci--persist-directory (&optional ensure)
  "Return the directory where the state should be stored.

If ENSURE is non-nil, create the directory when necessary.

Return nil if not in a project."
  (if-let* ((dir
             (cond
              ((eq ci-persist-location 'user-directory)
               (ci--get-persist-location-in-emacs-directory))
              ((eq ci-persist-location 'project-directory)
               (ci--get-persist-location-in-project-directory))
              ((stringp ci-persist-location)
               (expand-file-name ci-persist-location))
              (t
               (user-error
                "Invalid value for 'cmake-integration-persist-location': %S"
                ci-persist-location)))))
    (let ((expanded-dir (expand-file-name dir)))
      (when ensure
        (make-directory expanded-dir t))
      expanded-dir)))


(defun ci--get-persist-file (&optional ensure-directory)
  "Return the absolute path to the state file.

If ENSURE-DIRECTORY is non-nil, create the directory when necessary."
  (when-let* ((dir (ci--persist-directory ensure-directory)))
    (expand-file-name ci--state-file-name dir)))


(defun ci--has-state-been-saved-or-restored-p ()
  "Return non-nil if the state has been saved or restored for the current project."
  (let ((state-file (ci--get-persist-file)))
    (equal state-file ci-last-save-or-restore-state)))


(defun ci--state-file-exists-p ()
  "Returns non-nil if the cmake-integration state file exists."
  (if-let* ((state-file (ci--get-persist-file)))
    (file-exists-p state-file)))


;;;###autoload (autoload 'cmake-integration-should-restore-state-p "cmake-integration")
(defun ci-should-restore-state-p ()
  "Return non-nil if the state should be restored automatically."
  (interactive)
  (and (ci--state-file-exists-p) (not (ci--has-state-been-saved-or-restored-p))))


;;;###autoload (autoload 'cmake-integration-save-state "cmake-integration")
(defun ci-save-state ()
  "Save the current state of cmake-integration to persistent storage.

The location is determined by `cmake-integration-persist-location'.

If not in a CMake project, no state is saved."
  (interactive)
  (if (ci-is-cmake-project-p)
      (let ((state-file (ci--get-persist-file t)))
        (when state-file
          (let ((state
                 (mapcar
                  (lambda (var)
                    (cons
                     var
                     (when (boundp var)
                       (symbol-value var))))
                  ci--state-variables))
                (print-length nil)
                (print-level nil))
            (with-temp-file state-file
              (prin1 state (current-buffer))))
          (when (called-interactively-p 'interactive)
            (message "cmake-integration state saved to %s" state-file))

          (setq ci-last-save-or-restore-state state-file)))

    (message "Current project in '%s' is not a CMake project: cmake-integration state was not saved" (ci--get-project-root-folder))))


;;;###autoload (autoload 'cmake-integration-restore-state "cmake-integration")
(defun ci-restore-state ()
  "Restore the state of cmake-integration from persistent storage."
  (interactive)
  (let ((state-file (ci--get-persist-file)))
    (cond
     ((null state-file)
      (when (called-interactively-p 'interactive)
        (message "No state file location resolved.")))
     ((not (file-readable-p state-file))
      (when (called-interactively-p 'interactive)
        (message "No cmake-integration state file found at %s" state-file)))
     (t
      (condition-case err
          (with-temp-buffer
            (insert-file-contents state-file)
            (let ((state (read (current-buffer))))
              (dolist (entry state)
                (let ((var (car entry))
                      (value (cdr entry)))
                  (when (memq var ci--state-variables)
                    (set var value))))))
        (error
         (signal (car err) (cdr err))))
      (when (called-interactively-p 'interactive)
        (message "cmake-integration state restored from %s" state-file))

      (setq ci-last-save-or-restore-state state-file)))))


(defun ci-maybe-restore-state ()
  "Restore the state of cmake-integration if appropriate.

This function checks whether the state should be restored by calling
`cmake-integration-should-restore-state-p'. If it returns non-nil, the
state is restored by calling `cmake-integration-restore-state'."
  (interactive)
  (when (ci-should-restore-state-p)
    (ci-restore-state)))


(provide 'cmake-integration-persistence)

;;; cmake-integration-persistence.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
