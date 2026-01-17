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
(require 'cmake-integration-project-mode nil t)


(defvar ci-last-save-or-restore-state nil
  "Store the location of the state file used in the last save or restore.")


(defconst ci--state-variables
  '(;; Cache variables
    ci-cache-variables

    ;; Target
    ci--target-extra-data-cache
    ci-current-target
    ci-run-arguments

    ;; Presets
    ci-configure-preset
    ci-build-preset
    ci-test-preset
    ci-package-preset
    ci--build-folder-cache

    ;; CTest
    ci--ctest-label-include-regexp
    ci--ctest-label-exclude-regexp
    )
  "List of relevante variables in cmake-integration to save current state.")


(defconst ci-functions-to-save-state
  '(ci-select-current-target
    ci-select-configure-preset
    ci-add-cmake-cache-variables
    ci-remove-cmake-cache-variable
    ci-remove-all-cmake-cache-variables
    ci-select-test-preset
    ci-select-build-preset
    ci-select-package-preset
    ci-select-conan-profile
    ;; We also want to save state if the build folder cache is modified
    ci--set-build-folder-cache
    )
  "Functions which automatically save cmake-intregration state.

State is only saved if `ci-automatic-persistence-mode' is enabled."
)


(defconst ci-functions-to-restore-state
  '(ci-transient
    ci-run-ctest
    ci-save-and-compile-last-target
    ci-run-last-target
    ci-debug-last-target
    ci-cmake-reconfigure
    ci-run-cpack
    ci-run-cmake-install
    ci-open-dired-in-target-folder
    ci-open-eshell-in-target-folder
    ci-delete-build-folder
    ci--check-if-build-folder-exists-and-throws-if-not
    )
  "Functions which automatically restore cmake-intregration state.

State is only restored if `ci-automatic-persistence-mode' is enabled.")


(defconst ci-functions-to-restore-and-save-state
  '(ci-save-and-compile ci-run-last-target-with-arguments)
  "Functions which automatically restore and save state.

The state is restored before the function is called, and then saved
after the function is called.")



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


(defun ci--build-current-state ()
  "Return an alist with the current state of cmake-integration.

See also `ci--state-variables' for the list of variables used to build
the STATE.

The inverse of this function is performed by
`ci--restore-variables-from-state'."
  (mapcar
   (lambda (var)
     (cons
      var
      (when (boundp var)
        (symbol-value var))))
   ci--state-variables))


(defun ci--serialize-state (state state-file-name)
  "Serialize STATE to STATE-FILE-NAME.

STATE is an alist where each entry is (VARIABLE . VALUE). This is
exactly what `cmake-integration--build-current-state' returns."
  (with-temp-file state-file-name
    (prin1 state (current-buffer))))


;; TODO Add a test for this
;;;###autoload (autoload 'cmake-integration-save-state "cmake-integration")
(defun ci-save-state (&optional _)
  "Save the current state of cmake-integration to persistent storage.

The location is determined by `cmake-integration-persist-location'.

If not in a CMake project, no state is saved.

Note: The optional argument is not used. It is only present so this
function can be used as an advice."
  (interactive)
  (if (ci-is-cmake-project-p)
      (when-let* ((state-file (ci--get-persist-file t))
                  (state (ci--build-current-state)))
        (ci--serialize-state state state-file)
        (when (called-interactively-p 'interactive)
          (message "cmake-integration state saved to %s" state-file))
        (setq ci-last-save-or-restore-state state-file))

    (message
     "Current project in '%s' is not a CMake project: cmake-integration state was not saved"
     (ci--get-project-root-folder))))


(defun ci--deserialize-state (state-file-name)
  "Deserialize the state from STATE-FILE-NAME."
    (with-temp-buffer
        (insert-file-contents state-file-name)
        (read (current-buffer))))


(defun ci--restore-variables-from-state (state)
  "Restore the cmake-integration variables from STATE.

See also `ci--state-variables' for the list of variables that are
restored from STATE.

This is the inverse of the `ci--build-current-state' function."
  (dolist (entry state)
    (let ((var (car entry))
          (value (cdr entry)))
      (when (memq var ci--state-variables)
        (set var value)))))


;;;###autoload (autoload 'cmake-integration-restore-state "cmake-integration")
(defun ci-restore-state ()
  "Restore the state of cmake-integration from persistent storage."
  (interactive)
  (if-let* ((state-file (ci--get-persist-file)))
    (cond
     ((not (ci-is-cmake-project-p))
      (message "Current project in '%s' is not a CMake project"
               (ci--get-project-root-folder)))
     ((not (file-readable-p state-file))
      (when (called-interactively-p 'interactive)
        (message "No cmake-integration state file found at %s" state-file)))
     (t
      (condition-case err
          (let ((state (ci--deserialize-state state-file)))
            (ci--restore-variables-from-state state))

        (error
         (message
          "An error occurred while restoring cmake-integration state from %s: %s"
          state-file (error-message-string err))))
      (when (called-interactively-p 'interactive)
        (message "cmake-integration state restored from %s" state-file))

      (setq ci-last-save-or-restore-state state-file)))

    (when (called-interactively-p 'interactive)
      (message "No state file location resolved."))))


;; Note: This function can be called interactively, but it is also used as an
;; advice. That is the reason why it has the optional argument.
(defun ci-maybe-restore-state (&optional _)
  "Restore the state of cmake-integration if appropriate.

This function checks whether the state should be restored by calling
`cmake-integration-should-restore-state-p'. If it returns non-nil, the
state is restored by calling `cmake-integration-restore-state'.

Note: If the state has not been restored for the current project, but
there is no state file for the current project, the relevant variables
in `ci--state-variables' are cleared to avoid using stale data."
  (interactive)
  (if (ci-should-restore-state-p)
      (ci-restore-state)
    (when (and ci-last-save-or-restore-state
               (not (ci--has-state-been-saved-or-restored-p)))
      ;; If ci-last-save-or-restore-state is set and it's different from the
      ;; current project, but there is no state file for the current project,
      ;; then we clear the variables in ci--state-variables to avoid using stale
      ;; data.
      (dolist (var ci--state-variables)
        (when (boundp var)
          (set var nil)))
      (message "cmake-integration state was cleared")
      (setq ci-last-save-or-restore-state nil))))


(defun ci--advice-restore-and-save-state (orig-func &rest args)
  "Advice to restore state before calling ORIG-FUNC and save state after.

ORIG-FUNC is the original function being advised, while ARGS is the
arguments that will be passed to it."
  (ci-maybe-restore-state)
  (apply orig-func args)
  (ci-save-state))


(defun ci--add-persistence-advices ()
  "Add advices to functions to automatically save and restore state."
  (dolist (func ci-functions-to-restore-state)
    (unless (advice-member-p #'ci-maybe-restore-state func)
      (advice-add func :before #'ci-maybe-restore-state)))

  (dolist (func ci-functions-to-save-state)
    (unless (advice-member-p #'ci-save-state func)
      (advice-add func :after #'ci-save-state)))

  (dolist (func ci-functions-to-restore-and-save-state)
    (unless (advice-member-p #'ci--advice-restore-and-save-state func)
      (advice-add func :around #'ci--advice-restore-and-save-state))))


(defun ci--remove-persistence-advices ()
  "Remove advices added to functions to automatically save and restore state."
  (dolist (func ci-functions-to-restore-state)
    (advice-remove func #'ci-maybe-restore-state))

  (dolist (func ci-functions-to-save-state)
    (advice-remove func #'ci-save-state))

  (dolist (func ci-functions-to-restore-and-save-state)
    (advice-remove func #'ci--advice-restore-and-save-state)))


;;;###autoload (autoload 'cmake-integration-automatic-persistence-mode "cmake-integration")
(define-minor-mode ci-automatic-persistence-mode
  "Toggle automatic persistence of cmake-integration state.

When enabled, advices are installed so state is restored and saved
automatically around relevant commands."
  :global t
  :init-value nil
  :lighter "cip"
  :group 'cmake-integration-persistence
  (if ci-automatic-persistence-mode
      (ci--add-persistence-advices)
    (ci--remove-persistence-advices)))


(provide 'cmake-integration-persistence)

;;; cmake-integration-persistence.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
