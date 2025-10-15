;;; cmake-integration-launch.el --- Run a target (with or without debugging) -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'esh-mode)  ;; For eshell-send-input

(defun ci-default-program-launch-function (command &optional buffer-name)
  "Launch COMMAND in a compilation buffer with name BUFFER-NAME.

If BUFFER-NAME is nil, use the default compilation buffer name."
  (let ((compilation-buffer-name-function (if buffer-name
                                              (lambda (_) buffer-name)
                                            compilation-buffer-name-function)))
    (compile command)))



(defun ci-eshell-program-launch-function (command &optional buffer-name)
  "Launch COMMAND in an eshell buffer with name BUFFER-NAME.

If BUFFER-NAME is nil, use the default eshell buffer name is used."
  (let ((eshell-buffer-name (if buffer-name
                                buffer-name
                              "*eshell*")))
    (unless (get-buffer eshell-buffer-name)
      (eshell))
    (let ((eshell-buffer (get-buffer eshell-buffer-name)))
      (with-current-buffer eshell-buffer
        (goto-char (point-max))
        (insert command)
        (eshell-send-input))
      (pop-to-buffer eshell-buffer))))


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
  "Get the working directory for to run EXECUTABLE-FILENAME."
  (pcase ci-run-working-directory
    ('root (ci--get-project-root-folder))
    ('build (ci-get-build-folder))
    ('bin (file-name-concat (ci-get-build-folder) (file-name-directory executable-filename)))
    (_ (file-name-concat (ci--get-project-root-folder) ci-run-working-directory))))


(defun ci-get-target-executable-full-path (&optional executable-filename)
  "Get the full path of EXECUTABLE-FILENAME.

EXECUTABLE-FILENAME must be relative to the build folder.

If it is not provided the executable for the target in
`cmake-integration-current-target' is used."
  (let ((executable-filename (or executable-filename (ci-get-target-executable-filename))))
    (file-name-concat (ci-get-build-folder) executable-filename)))


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
         (executable-path (file-relative-name (ci-get-target-executable-full-path executable-filename) run-dir))
         (run-command (format "./%s %s" executable-path ci-run-arguments)))
    (list run-dir run-command)))


;;;###autoload (autoload 'cmake-integration-run-last-target "cmake-integration")
(defun ci-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  (let ((bufer-name (when ci-use-separated-compilation-buffer-for-each-target
                      (ci--get-program-launch-buffer-name))))
    (pcase-let* ((`(,run-dir ,cmd) (ci--get-run-command (ci-get-target-executable-filename))))
      (let ((default-directory run-dir))
        (funcall ci-program-launcher cmd bufer-name)))
    )
  )


(defun ci--get-debug-command (executable-filename)
  "Get the directory and the debug command for EXECUTABLE-FILENAME.

Return a list (RUN-DIR COMMAND), where RUN-DIR is the directory from
which the command must be executed, and COMMAND is the command line
string to run (`gdb' invocation)."
  (let* ((run-dir (ci--get-working-directory executable-filename))
         (executable-path (file-relative-name (ci-get-target-executable-full-path executable-filename) run-dir))
         (gdb-command (format "gdb -i=mi --args %s %s" executable-path ci-run-arguments)))
    (list run-dir gdb-command)))


(defun ci--launch-gdb-with-last-target ()
  "Launch gdb inside Emacs to debug the last target."
  (pcase-let* ((`(,run-dir ,cmd) (ci--get-debug-command (ci-get-target-executable-filename))))
    ;; TODO GDB seems to not be respecting default-directory. It's probably
    ;; necessary to pass the `--cd' argument to gdb.
    (let ((default-directory run-dir))
      (gdb cmd))))


(declare-function dap-debug "dap-mode")


(defun ci--launch-dap-debug-cpptools-last-target ()
  "Launch `dap-debug' using cpptools to debug the last target."
  (require 'dap-mode)
  (let ((executable-filename (ci-get-target-executable-filename)))

    (let ((program-path (expand-file-name (ci-get-target-executable-full-path executable-filename)))
          (cwd (expand-file-name (ci--get-working-directory executable-filename))))

      (dap-debug (list :type "cppdbg"
                       :request "launch"
                       :name "cmake-integration-target"
                       :MIMode "gdb"
                       :program program-path
                       :arguments ci-run-arguments
                       :cwd cwd)))))


;;;###autoload (autoload 'cmake-integration-debug-last-target "cmake-integration")
(defun ci-debug-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  (if ci-use-dap-for-debug
      (ci--launch-dap-debug-cpptools-last-target)
    ;; Run the target
    (ci--launch-gdb-with-last-target)))


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
