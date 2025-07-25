;;; cmake-integration-launch.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


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
    (error "Please select a target first `cmake-integration-save-and-compile' first"))

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


(defun ci-get-target-executable-full-path (executable-filename)
  "Get the full path of EXECUTABLE-FILENAME."
  (file-name-concat (ci-get-build-folder) executable-filename))


(defun ci--get-run-command-project-root-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the project root folder."
  (format "cd %s && %s %s"
          (ci--get-working-directory executable-filename)
          (ci-get-target-executable-full-path executable-filename)
          ci-run-arguments))


(defun ci--get-run-command-build-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the build folder."
  (format "cd %s && %s %s"
          (ci--get-working-directory executable-filename)
          executable-filename
          ci-run-arguments))


(defun ci--get-run-command-bin-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the binary folder.

The binary folder is the folder containing the executable."
  (format "cd %s && ./%s %s"
          (ci--get-working-directory executable-filename)
          (file-name-nondirectory executable-filename)
          ci-run-arguments))


(defun ci--get-run-command-custom-cwd (executable-filename project-subfolder)
  "Get the correct run command EXECUTABLE-FILENAME from a PROJECT-SUBFOLDER."
  (cl-assert (stringp project-subfolder))
  (format "cd %s && %s %s"
          (ci--get-working-directory executable-filename)
          (ci-get-target-executable-full-path executable-filename)
          ci-run-arguments))


(defun ci--compilation-buffer-name-function (name-of-mode)
  "Get the compilation buffer name for NAME-OF-MODE current target name."
  name-of-mode  ;; Avoid warning about unused argument
  (format "*Running - %s*" cmake-integration-current-target))


(defun ci--get-run-command (executable-filename)
  "Get the correct run command for EXECUTABLE-FILENAME.

Get the correct run command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable."
  (pcase ci-run-working-directory
    ('root (ci--get-run-command-project-root-cwd executable-filename))
    ('build (ci--get-run-command-build-folder-cwd executable-filename))
    ('bin (ci--get-run-command-bin-folder-cwd executable-filename))
    (_ (ci--get-run-command-custom-cwd executable-filename ci-run-working-directory))))


;;;###autoload (autoload 'cmake-integration-run-last-target "cmake-integration")
(defun ci-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  (let ((compilation-buffer-name-function (if ci-use-separated-compilation-buffer-for-each-target
                                              'ci--compilation-buffer-name-function
                                            compilation-buffer-name-function)))
    ;; Run the target
    (compile (ci--get-run-command (ci-get-target-executable-filename)))))


(defun ci--get-debug-command (executable-filename)
  "Get the correct debug command for EXECUTABLE-FILENAME.

Get the correct debug command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable. This should be passed to gdb command in Emacs."
  (let ((cwd (ci--get-working-directory executable-filename)))
    (format
     "gdb -i=mi --cd=%s --args %s %s"
     cwd
     (ci-get-target-executable-full-path executable-filename)
     ci-run-arguments)))


(defun ci--launch-gdb-with-last-target ()
  "Launch gdb inside Emacs to debug the last target."
  (gdb (ci--get-debug-command (ci-get-target-executable-filename))))


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
