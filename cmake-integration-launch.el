;;; cmake-integration-launch.el --- Run a target (with or without debugging) -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'cmake-integration-launch-functions)


(defun ci-get-target-executable-filename (&optional target)
  "Get the executable filename for the target TARGET.

The name is relative to the build folder. This is usually
something like just <target-name>, or bin/<target-name>.

Throws an error if the target is not an executable.

If TARGET-NAME is not provided use the last target (saved in a
`cmake-integration-current-target')."

  ;; If both `target' and `cmake-integration-current-target' are nil,
  ;; throw an error asking the UE to select a target first by calling
  ;; `cmake-integration-save-and-compile'
  (unless (or target ci-current-target)
    (error "Please select a target first by calling `cmake-integration-select-current-target`"))

  ;; The `target-info' variable inside the `let' has the data from the
  ;; codemodel json file for TARGET-NAME. This data is an alist and
  ;; includes a `jsonFile' field, which has the name of another json
  ;; file with more data about the target. We read this json file and
  ;; save the data in the `target-data' variable. From there we can
  ;; get the executable name from its `artifacts' field.
  (let* ((target (or target ci-current-target))
         (target-name (car (split-string target ci--multi-config-separator)))
         (target-info (alist-get
                       target
                       (ci--get-targets-from-codemodel-json-file)
                       nil nil 'equal)))

    (unless (cdr target-info)
      (if (member target-name '("all" "clean" "install"))
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


(defun ci--get-working-directory (executable-filename)
  "Get the working directory to run EXECUTABLE-FILENAME."
  (pcase ci-run-working-directory
    ('root (ci--get-project-root-folder))
    ('build (ci-get-build-folder))
    ('bin (file-name-concat (ci-get-build-folder) (file-name-directory executable-filename)))
    (_ (file-name-concat (ci--get-project-root-folder) ci-run-working-directory))))


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
      (message "Copied to kill-ring: %s" full-path))
    full-path))


(defun ci--get-program-launch-buffer-name ()
  "Get the compilation buffer name for NAME-OF-MODE current target name."
  (format "*Running - %s*" cmake-integration-current-target))


(defun ci--get-run-command (executable-filename)
  "Get the directory and the run command for EXECUTABLE-FILENAME.

Note: EXECUTABLE-FILENAME must be relative to the build folder.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run."
  (let* ((run-dir (ci--get-working-directory executable-filename))
         (executable-relative-path (file-relative-name (ci-get-target-executable-full-path executable-filename) run-dir))
         (path-separator (if (eq system-type 'windows-nt) "\\" "/"))
         (run-command (format ".%s%s %s" path-separator executable-relative-path ci-run-arguments)))
    (list run-dir run-command)))


;;;###autoload (autoload 'cmake-integration-run-last-target "cmake-integration")
(defun ci-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)

  (let ((bufer-name
         (when ci-use-separated-compilation-buffer-for-each-target
           (ci--get-program-launch-buffer-name))))
    (pcase-let* ((`(,run-dir ,cmd)
                  (ci--get-run-command (ci-get-target-executable-filename))))
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
  "Run the last compiled target."
  (interactive)
  (ci--check-if-build-folder-exists-and-throws-if-not)

  (let* ((executable-filename (ci-get-target-executable-filename))
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
  (interactive "sArguments: ")
  (setq ci-run-arguments run-arguments)
  (ci-run-last-target))



(provide 'cmake-integration-launch)

;;; cmake-integration-launch.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
