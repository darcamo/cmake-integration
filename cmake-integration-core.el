;;; cmake-integration-core.el --- Core functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'f)
(require 'cl-extra)
(require 'json)
(require 'project)

(require 'cmake-integration-variables)


;; BUG: This function seems to work correctly, but when used as the
;; ":safe" predicate in the defcustom Emacs still asks for confirming
;; if the variable is safe for the symbol values
(defun cmake-integration--run-working-directory-p (val)
  "Check if VAL is safe as a local variable.

Function to verify is VAL is save as a value for the
`cmake-integration-run-working-directory' variable"

  (if (stringp val)
      ;; Return t if VAL is a valid subfolder of the project root
      (file-exists-p (file-name-concat (cmake-integration--get-project-root-folder) val))
    ;; Return t if VAL is one of the accepted symbols
    (pcase val
      ('bin t)
      ('build t)
      ('root t)
      (_ nil))))


(defun cmake-integration--get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (project-root (project-current)))


(defun cmake-integration--get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."
  (elt (f-glob "codemodel-v2*json" (cmake-integration--get-reply-folder)) 0))


(defun cmake-integration--change-to-absolute-filename (filename parent-folder)
  "If FILENAME is relative to PARENT-FOLDER, make it absolute.
Otherwise return it unchanged."
  (if (f-absolute-p filename)
      filename
    (f-join parent-folder filename)))


(defun cmake-integration--get-query-folder ()
  "Get the query folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/query/client-emacs"))


(defun cmake-integration--get-reply-folder ()
  "Get the reply folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/reply/"))


(defun cmake-integration--get-path-of-codemodel-query-file ()
  "Get the full path of the codemodel-query-file."
  (file-name-concat (cmake-integration--get-query-folder) "codemodel-v2"))


(defun cmake-integration--create-empty-codemodel-file ()
  "Create an empty codemodel query file for CMake's file API."
  ;; Only do something if the file does not exist
  (unless (file-exists-p (cmake-integration--get-path-of-codemodel-query-file))
    ;; Create the folder if it does not exists yet
    (unless (file-exists-p (cmake-integration--get-query-folder))
      (shell-command (concat "mkdir -p " (cmake-integration--get-query-folder))))
    ;; Create the codemodel file
    (shell-command (concat "touch " (cmake-integration--get-path-of-codemodel-query-file)))))


(defun cmake-integration-get-build-folder ()
  "Get the project build folder.

Returns the build folder path based on either the configure preset or
the manually specified `cmake-integration-build-dir'. Throws an error if
no valid build folder can be determined."
  (let ((project-root-folder (cmake-integration--get-project-root-folder))
        (preset cmake-integration-configure-preset)
        (build-dir cmake-integration-build-dir))
    (unless project-root-folder
      (error "Not in a project"))

    (if preset
        ;; Use the build folder from the configure preset
        (cmake-integration--get-binaryDir-with-replacements preset)

      ;; Use manually set build directory or throw an error
      (if build-dir
          (file-name-concat project-root-folder build-dir)
        (error "Build folder is not set.
Call `cmake-integration-select-configure-preset' to select a configure preset,
or set `cmake-integration-build-dir' manually")))))


(defun cmake-integration--get-build-folder-relative-to-project ()
  "Get the build folder relative to project folder."
  (let ((build-folder (cmake-integration-get-build-folder))
        (project-folder (cmake-integration--get-project-root-folder)))
    (when (and build-folder project-folder)
      (file-relative-name build-folder project-folder))))


(defun cmake-integration--get-annotation-initial-spaces (annotated-string)
  "Get a string of spaces that should be added after ANNOTATED-STRING.

The number of spaces is enough such that when added after
ANNOTATED-STRING the annotation after that starts in the column
indicated by `cmake-integration-annotation-column'."
  (make-string
   (max 1
        (- cmake-integration-annotation-column (length annotated-string)))
   ;; 32 is the space character
   32))


(provide 'cmake-integration-core)

;;; cmake-integration-core.el ends here
