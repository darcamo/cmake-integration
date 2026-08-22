;;; cmake-integration-launch.el --- Run a target (with or without debugging) -*- lexical-binding: t -*-

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
(require 'cmake-integration-build)
(require 'cmake-integration-launch-functions)
(require 'cmake-integration-logging)

(defun ci--executable-targets ()
  "Return the names of all executable targets in the project.

Reads them from the CMake file-api codemodel, ignoring which
build targets are currently selected."

  (when-let* ((json-filename (ci--get-codemodel-reply-json-filename)))
    (mapcar #'car
            (seq-filter (lambda(entry)
                          (equal (alist-get 'type (cdr entry)) "EXECUTABLE"))
                        (ci--get-annotated-targets-from-codemodel-json-file json-filename)))))

(defun ci--current-executable-build-targets ()
  "Return the selected build targets that are executables.

Filters `ci-current-build-targets' down to executable targets,
keeping their selection order."

  (let ((executables (ci--executable-targets)))
    (seq-filter (lambda (name) (member name executables)) ci-current-build-targets)))

(defun ci--resolve-runnable-target (remembered-var &optional action)
  "Return the target stored in REMEMBERED-VAR for running/debugging.

If REMEMBERED-VAR (`ci-current-run-target' or
`ci-current-debug-target') is non-nil it is honored blindly;
launch-time checks remain the only validation.  Otherwise the
first best candidate is auto-picked, stored in REMEMBERED-VAR and
returned: executables among the selected build targets first,
falling back to any project executable.  ACTION labels log
messages and defaults to \"Run\"."

  (or (symbol-value remembered-var)
      (let* ((action (or action "Run"))
             (choice (car (or (ci--current-executable-build-targets)
                              (ci--executable-targets)))))
        (unless choice
          (error "%s: no executable targets found in the project" action))
        (set remembered-var choice)
        (ci-log-info "%s target auto-selected: %s" action choice)
        choice)))


(defun ci--select-runnable-target (remembered-var action)
  "Prompt for an executable target and store it in REMEMBERED-VAR.

Candidates come from all project executables, with the current
value of REMEMBERED-VAR offered as default.  ACTION labels the
prompt and log messages (\"Run\" or \"Debug\").  This is an
explicit user request, so it always overwrites."

  (let* ((candidates (or (ci--executable-targets)
                         (user-error "%s: no executable targets found in the project"
                                     action)))
         (target (completing-read
                  (format "%s target: " action)
                  candidates
                  nil
                  t
                  nil
                  nil
                  (symbol-value remembered-var))))
    (set remembered-var target)
    (ci-log-info "%s target selected: %s" action target)))

;;;###autoload (autoload 'cmake-integration-select-run-target "cmake-integration")
(defun ci-select-run-target ()
  "Select the target to use when running, from the project executables.

Overwrites `ci-current-run-target'; `ci-current-debug-target' is
left untouched. Unless you have an explicit reason to use different
run and debug targets, use `ci-select-run-and-debug-target' instead."
  (interactive)
  (ci--select-runnable-target 'ci-current-run-target "Run"))

;;;###autoload (autoload 'cmake-integration-select-debug-target "cmake-integration")
(defun ci-select-debug-target ()
  "Select the target to use when debugging, from the project executables.

Overwrites `ci-current-debug-target'; `ci-current-run-target' is
left untouched. Unless you have an explicit reason to use different
run and debug targets, use `ci-select-run-and-debug-target' instead."
  (interactive)
  (ci--select-runnable-target 'ci-current-debug-target "Debug"))

;;;###autoload (autoload 'cmake-integration-select-run-and-debug-target "cmake-integration")
(defun ci-select-run-and-debug-target ()
  "Select one executable target to use for both running and debugging.

Prompts once and stores the choice in both
`ci-current-run-target' and `ci-current-debug-target'.  This is an
explicit user request, so both are always overwritten."
  (interactive)
  (ci--select-runnable-target 'ci-current-run-target "Run")
  (set 'ci-current-debug-target ci-current-run-target))


(defun ci-get-target-executable-filename (&optional target)
  "Get the executable filename for the target TARGET.

The name is relative to the build folder. This is usually
something like just <target-name>, or bin/<target-name>.

Throws an error if the target is not an executable.

If TARGET is not provided, `ci-current-run-target' is used. If
that is nil as well, an error is signaled."

  ;; The `target-info' variable inside the `let' has the data from the
  ;; codemodel json file for TARGET-NAME. This data is an alist and
  ;; includes a `jsonFile' field, which has the name of another json
  ;; file with more data about the target. We read this json file and
  ;; save the data in the `target-data' variable. From there we can
  ;; get the executable name from its `artifacts' field.
  (let* ((target (or target ci-current-run-target))
         (_ (unless target
              (user-error "No run target selected. Select build targets and run targets accordingly!")))
         (target-name (car (split-string target ci--multi-config-separator)))
         (target-info (alist-get
                       target
                       (ci--get-targets-from-codemodel-json-file)
                       nil nil 'equal)))

    (unless (cdr target-info)
      (if (ci--is-phony-target target-name)
          (error "Target '%s' is not a valid executable target" target-name)
        (error "Unknown target: '%s'" target-name)))

    (let* ((target-json-file (file-name-concat
                              (ci--get-reply-folder)
                              (alist-get 'jsonFile target-info)))
           (target-data (json-read-file target-json-file)))

      (unless (equal (alist-get 'type target-data) "EXECUTABLE")
        (error "Target '%s' is not an executable" target-name))

      ;; Note that target-artifacts is a vector, but with a single
      ;; element in our case
      (let ((target-artifacts (alist-get 'artifacts target-data)))
        ;; We assume the vector has just one element
        (alist-get 'path (elt target-artifacts 0))))))

(defun ci--get-working-directory (&optional executable-filename)
  "Get the working directory to run EXECUTABLE-FILENAME.

If EXECUTABLE-FILENAME is not provided, it is derived from
`ci-current-run-target' via `ci-get-target-executable-filename',
which signals an error when no target is selected."
  (let* ((executable-filename (or executable-filename (ci-get-target-executable-filename))))
    (pcase ci-run-working-directory
      ('root (ci--get-project-root-folder))
      ('build (ci-get-build-folder))
      ('bin (file-name-concat (ci-get-build-folder) (file-name-directory executable-filename)))
      (_ (file-name-concat (ci--get-project-root-folder) ci-run-working-directory)))))


(defun ci-get-target-executable-full-path (&optional executable-filename)
  "Get the full path of EXECUTABLE-FILENAME.

EXECUTABLE-FILENAME must be relative to the build folder.

If it is not provided the executable for the target in
`cmake-integration-current-target' is used.

If called interactively, the result is copied to the `kill-ring`."
  (interactive)
  (let* ((executable-filename (or executable-filename (ci-get-target-executable-filename)))
         (full-path (file-name-concat (ci-get-build-folder) executable-filename)))
    (when (called-interactively-p 'any)
      (kill-new full-path)
      (ci-log-info "Copied to kill-ring: %s" full-path))
    full-path))


(defun ci--get-program-launch-buffer-name (target-name)
  "Get the compilation buffer name for TARGET-NAME."

  (format "*Running - %s*" target-name))


(defun ci--get-run-command (executable-filename)
  "Get the directory and the run command for EXECUTABLE-FILENAME.

Note: EXECUTABLE-FILENAME must be relative to the build folder.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run."
  (let* ((run-dir (ci--get-working-directory executable-filename))
         (executable-relative-path (file-relative-name (ci-get-target-executable-full-path executable-filename) run-dir))
         (run-command (format "./%s %s" executable-relative-path ci-run-arguments)))
    (list run-dir run-command)))


;;;###autoload (autoload 'cmake-integration-run-last-target "cmake-integration")
(defun ci-run-last-target ()
  "Run the last compiled target."

  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)

  (let* ((target (ci--resolve-runnable-target 'ci-current-run-target))
         (bufer-name
          (when ci-use-separated-compilation-buffer-for-each-target
            (ci--get-program-launch-buffer-name target))))
    (pcase-let* ((`(,run-dir ,cmd)
                  (ci--get-run-command (ci-get-target-executable-filename target))))
      (let ((default-directory run-dir))
        (cond
         ((eq ci-program-launcher-function 'compilation)
          ;; Use compile to run the command in a compilation buffer
          (funcall 'ci-default-program-launch-function cmd bufer-name))
         ((eq ci-program-launcher-function 'comint)
          ;; Use compile with `t` arg to run the command in a comint buffer
          (funcall 'ci-comint-program-launch-function cmd bufer-name))
         ((eq ci-program-launcher-function 'eshell)
          ;; Use eshell to run the command
          (funcall 'ci-eshell-program-launch-function cmd bufer-name))
         (t
          ;; Assume it is a function
          (funcall ci-program-launcher-function cmd bufer-name)))))))


;;;###autoload (autoload 'cmake-integration-debug-last-target "cmake-integration")
(defun ci-debug-last-target ()
  "Run debugger with the current debug target."

  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)
  (let* ((target (ci--resolve-runnable-target 'ci-current-debug-target "Debug"))
         (executable-filename (ci-get-target-executable-filename target))
         (run-dir (ci--get-working-directory executable-filename))
         (executable-path
          (file-relative-name
           (ci-get-target-executable-full-path executable-filename)
           run-dir)))

    ;; Call the debug launcher function
    (cond
     ((eq ci-debug-launcher-function 'classic-gdb)
      ;; Use native gdb Emacs integration
      (funcall 'ci-default-debug-launch-function
               executable-path
               ci-run-arguments
               run-dir))
     ((eq ci-debug-launcher-function 'dape)
      ;; Use dape with gdb's Debugger Adapter Protocol
      (funcall 'ci-dape-debug-launch-function
               executable-path
               ci-run-arguments
               run-dir))
     (t
      ;; Assume it is a function
      (funcall ci-debug-launcher-function
               executable-path
               ci-run-arguments
               run-dir)))))


(defun ci--set-runtime-arguments (run-arguments)
  "Set arguments passed to the executable to RUN-ARGUMENTS."
  (interactive "sArguments: ")
  (setq ci-run-arguments run-arguments))


;;;###autoload (autoload 'cmake-integration-run-last-target-with-arguments "cmake-integration")
(defun ci-run-last-target-with-arguments (run-arguments)
  "Run the last compiled target passing RUN-ARGUMENTS as arguments."
  (interactive (list (read-string "Arguments: " ci-run-arguments)))
  (setq ci-run-arguments run-arguments)
  (ci-run-last-target))



(provide 'cmake-integration-launch)

;;; cmake-integration-launch.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
