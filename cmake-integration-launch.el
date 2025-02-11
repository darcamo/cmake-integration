;;; cmake-integration-launch.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; TODO: Add functions to:
;; - Launch terminal in build/target folder
;; - Launch dired in build/target folder
;; 

(defun cmake-integration-get-target-executable-filename (&optional target)
  "Get the executable filename for the target TARGET.

The name is relative to the build folder. This is usually
something like just <target-name>, or bin/<target-name>.

Throws an error if the target is not an executable.

If TARGET-NAME is not provided use the last target (saved in a
`cmake-integration-current-target')."

  ;; If both `target' and `cmake-integration-current-target' are nil,
  ;; throw an error asking the UE to select a target first by calling
  ;; `cmake-integration-save-and-compile'
  (unless (or target cmake-integration-current-target)
    (error "Please select a target first `cmake-integration-save-and-compile' first"))

  ;; The `target-info' variable inside the `let' has the data from the
  ;; codemodel json file for TARGET-NAME. This data is an alist and
  ;; includes a `jsonFile' field, which has the name of another json
  ;; file with more data about the target. We read this json file and
  ;; save the data in the `target-data' variable. From there we can
  ;; get the executable name from its `artifacts' field.
  (let* ((target (or target cmake-integration-current-target))
         (target-name (car (split-string target cmake-integration--multi-config-separator)))
         (target-info (alist-get
                       target
                       (cmake-integration--get-cmake-targets-from-codemodel-json-file)
                       nil nil 'equal)))

    (unless (cdr target-info)
      (if (member target-name '("all" "clean" "install"))
          (error "Target '%s' is not a valid executable target" target-name)
        (error "Unknown target: '%s'" target-name)))

    (let* ((target-json-file (file-name-concat
                              (cmake-integration--get-reply-folder)
                              (alist-get 'jsonFile target-info)))
           (target-data (json-read-file target-json-file)))

      (unless (equal (alist-get 'type target-data) "EXECUTABLE")
        (error "Target '%s' is not an executable" target-name))

      ;; Note that target-artifacts is a vector, but with a single
      ;; element in our case
      (let ((target-artifacts (alist-get 'artifacts target-data)))
        ;; We assume the vector has just one element
        (alist-get 'path (elt target-artifacts 0))))))


(defun cmake-integration--get-working-directory (executable-filename)
  "Get the working directory for to run EXECUTABLE-FILENAME."
  (pcase cmake-integration-run-working-directory
    ('root (cmake-integration--get-project-root-folder))
    ('build (cmake-integration-get-build-folder))
    ('bin (file-name-concat (cmake-integration-get-build-folder) (file-name-directory executable-filename)))
    (_ (file-name-concat (cmake-integration--get-project-root-folder) cmake-integration-run-working-directory))))


(defun cmake-integration-get-target-executable-full-path (executable-filename)
  "Get the full path of EXECUTABLE-FILENAME."
  (file-name-concat (cmake-integration-get-build-folder) executable-filename))


(defun cmake-integration--get-run-command-project-root-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the project root folder."
  (format "cd %s && %s %s"
          (cmake-integration--get-working-directory executable-filename)
          (cmake-integration-get-target-executable-full-path executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-build-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the build folder."
  (format "cd %s && %s %s"
          (cmake-integration--get-working-directory executable-filename)
          executable-filename
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-bin-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the binary folder.

The binary folder is the folder containing the executable."
  (format "cd %s && ./%s %s"
          (cmake-integration--get-working-directory executable-filename)
          (file-name-nondirectory executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-custom-cwd (executable-filename project-subfolder)
  "Get the correct run command EXECUTABLE-FILENAME from a PROJECT-SUBFOLDER."
  (cl-assert (stringp project-subfolder))
  (format "cd %s && %s %s"
          (cmake-integration--get-working-directory executable-filename)
          (cmake-integration-get-target-executable-full-path executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command (executable-filename)
  "Get the correct run command for EXECUTABLE-FILENAME.

Get the correct run command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable."
  (pcase cmake-integration-run-working-directory
    ('root (cmake-integration--get-run-command-project-root-cwd executable-filename))
    ('build (cmake-integration--get-run-command-build-folder-cwd executable-filename))
    ('bin (cmake-integration--get-run-command-bin-folder-cwd executable-filename))
    (_ (cmake-integration--get-run-command-custom-cwd executable-filename cmake-integration-run-working-directory))))


;;;###autoload
(defun cmake-integration-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  ;; Run the target
  (compile (cmake-integration--get-run-command (cmake-integration-get-target-executable-filename))))


(defun cmake-integration--get-debug-command (executable-filename)
  "Get the correct debug command for EXECUTABLE-FILENAME.

Get the correct debug command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable. This should be passed to gdb command in Emacs."
  (let ((cwd (cmake-integration--get-working-directory executable-filename)))
    (format
     "gdb -i=mi --cd=%s --args %s %s"
     cwd
     (cmake-integration-get-target-executable-full-path executable-filename)
     cmake-integration-current-target-run-arguments)))


(defun cmake-integration--launch-gdb-with-last-target ()
  "Launch gdb inside Emacs to debug the last target."
  (gdb (cmake-integration--get-debug-command (cmake-integration-get-target-executable-filename))))


(declare-function dap-debug "dap-mode")


(defun cmake-integration--launch-dap-debug-cpptools-last-target ()
  "Launch `dap-debug' using cpptools to debug the last target."
  (require 'dap-mode)
  (let ((executable-filename (cmake-integration-get-target-executable-filename)))

    (let ((program-path (expand-file-name (cmake-integration-get-target-executable-full-path executable-filename)))
          (cwd (expand-file-name (cmake-integration--get-working-directory executable-filename))))

      (dap-debug (list :type "cppdbg"
                       :request "launch"
                       :name "cmake-integration-target"
                       :MIMode "gdb"
                       :program program-path
                       :arguments cmake-integration-current-target-run-arguments
                       :cwd cwd)))))


;;;###autoload
(defun cmake-integration-debug-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  (if cmake-integration-use-dap-for-debug
      (cmake-integration--launch-dap-debug-cpptools-last-target)
    ;; Run the target
    (cmake-integration--launch-gdb-with-last-target)))


;;;###autoload
(defun cmake-integration-run-last-target-with-arguments (run-arguments)
  "Run the last compiled target passing RUN-ARGUMENTS as arguments."
  (interactive "sArguments: ")
  (setq cmake-integration-current-target-run-arguments run-arguments)
  (cmake-integration-run-last-target))



(provide 'cmake-integration-launch)

;;; cmake-integration-launch.el ends here
