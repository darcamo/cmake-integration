;;; cmake-integration-core.el --- Core functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'f)
(require 'cl-extra)
(require 'json)
(require 'project)
(require 'map)

(require 'cmake-integration-variables)


;; BUG: This function seems to work correctly, but when used as the
;; ":safe" predicate in the defcustom Emacs still asks for confirming
;; if the variable is safe for the symbol values
(defun ci--run-working-directory-p (val)
  "Check if VAL is safe as a local variable.

Function to verify is VAL is save as a value for the
`cmake-integration-run-working-directory' variable"

  (if (stringp val)
      ;; Return t if VAL is a valid subfolder of the project root
      (file-exists-p (file-name-concat (ci--get-project-root-folder) val))
    ;; Return t if VAL is one of the accepted symbols
    (pcase val
      ('bin t)
      ('build t)
      ('root t)
      (_ nil))))


(defun ci--get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (when (project-current)
    (project-root (project-current))))


(defun ci--get-index-reply-json-filename ()
  "Get the name of the index json file.
This file is created by CMake's File API."
  (elt (f-glob "index-*json" (ci--get-reply-folder)) 0))


(defun ci--get-cmake-project-info ()
  "Get information about the generated project files."
  (interactive)
  (if-let* ((index-file (ci--get-index-reply-json-filename))
            (json-content-alist (json-read-file index-file))
            (generator (map-nested-elt json-content-alist '(cmake generator name)))
            (version (map-nested-elt json-content-alist '(cmake version string)))
            (paths (map-nested-elt json-content-alist '(cmake paths))))

      (map-let (cmake cpack ctest root) paths
        (format "
CMake version: %s\nGenerator:  %s\nCMake path: %s\nCpack path: %s\nCtest path: %s\nRoot path:  %s\n"
                (if version version "unknown")
                (if generator generator "unknown")
                (if cmake cmake "unknown")
                (if cpack cpack "unknown")
                (if ctest ctest "unknown")
                (if root root "unknown")))
    (display-warning 'cmake-integration "Project has not been generated yet. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset'")))


(defun ci-display-cmake-project-info ()
  "Display information about the generated project files in the echo area."
  (interactive)
  (message "%s" (ci--get-cmake-project-info)))


(defun ci--get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."
  (elt (f-glob "codemodel-v2*json" (ci--get-reply-folder)) 0))


(defun ci--change-to-absolute-filename (filename parent-folder)
  "If FILENAME is relative to PARENT-FOLDER, make it absolute.
Otherwise return it unchanged."
  (if (f-absolute-p filename)
      filename
    (f-join parent-folder filename)))


(defun ci--get-query-folder ()
  "Get the query folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (ci-get-build-folder) ".cmake/api/v1/query/client-emacs"))


(defun ci--get-reply-folder ()
  "Get the reply folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (ci-get-build-folder) ".cmake/api/v1/reply/"))


(defun ci--get-path-of-codemodel-query-file ()
  "Get the full path of the codemodel-query-file."
  (file-name-concat (ci--get-query-folder) "codemodel-v2"))


(defun ci--create-empty-codemodel-file ()
  "Create an empty codemodel query file for CMake's file API."
  ;; Only do something if the file does not exist
  (let ((query-folder (ci--get-query-folder))
        (query-file (ci--get-path-of-codemodel-query-file)))
    (unless (file-exists-p query-file)
      ;; Create the folder if it does not exists yet
      (unless (file-exists-p query-folder)
        (make-directory query-folder t))
      ;; Create the codemodel file
      (make-empty-file query-file))))


(defun ci-get-build-folder ()
  "Get the project build folder.

Returns the build folder path based on either the configure preset or
the manually specified `cmake-integration-build-dir'. Throws an error if
no valid build folder can be determined.

Note that the returned build folder is always an absolute path. Relative
paths from `cmake-integration-build-dir' or the active preset are
resolved against the project root."
  (let ((project-root-folder (ci--get-project-root-folder))
        (preset ci-configure-preset)
        (build-dir ci-build-dir))
    (unless project-root-folder
      (error "Not in a project"))

    (if preset
        (let ((binaryDir-with-replacements (ci--get-binaryDir-with-replacements preset)))
          (expand-file-name binaryDir-with-replacements project-root-folder))

      ;; Use manually set build directory or throw an error
      (if build-dir
          (expand-file-name build-dir project-root-folder)
        (error "Build folder is not set.
Call `cmake-integration-select-configure-preset' to select a configure preset,
or set `cmake-integration-build-dir' manually")))))


(defun ci--get-build-folder-relative-to-project ()
  "Get the build folder relative to project folder."
  (let ((build-folder (ci-get-build-folder))
        (project-folder (ci--get-project-root-folder)))
    (when (and build-folder project-folder)
      (file-relative-name build-folder project-folder))))


(defun ci--get-annotation-initial-spaces (annotated-string)
  "Get a string of spaces that should be added after ANNOTATED-STRING.

The number of spaces is enough such that when added after
ANNOTATED-STRING the annotation after that starts in the column
indicated by `cmake-integration-annotation-column'."
  (make-string
   (max 1
        (- ci-annotation-column (length annotated-string)))
   ;; 32 is the space character
   32))


(provide 'cmake-integration-core)

;;; cmake-integration-core.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
