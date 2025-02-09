;;; cmake-integration-core.el --- Core functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'f)
(require 'cl-extra)
(require 'json)
(require 'project)

(require 'cmake-integration-variables)


;; (declare-function cmake-integration-get-build-folder "cmake-integration-build.el")


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
      (_ nil)
      )
    )
  )


(defun cmake-integration--get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (project-root (project-current)))






(defun cmake-integration--get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."

  (elt (f-glob "codemodel-v2*json" (cmake-integration--get-reply-folder)) 0))


(defun cmake-integration--get-parent-preset-name (preset)
  "Get the name in the `inherits' field of the preset PRESET."
  (alist-get 'inherits preset))










(defun cmake-integration--change-to-absolute-filename (filename parent-folder)
  "If FILENAME is relative to PARENT-FOLDER, make it absolute.
Otherwise return it unchanged."
  (if (f-absolute-p filename)
      filename
    (f-join parent-folder filename)))


(defun cmake-integration--get-cmake-include-filenames (json-filename)
  "Return a list of include preset filenames in the JSON-FILENAME.

A CMake presets file can include other presets files, which can include
other preset files themselves. This function will return a flat list
with the absolute paths of all of these included presets.

NOTE: JSON-FILENAME is also returned as the lasts element, such that the
output of `cmake-integration--get-cmake-include-filenames' has all the
preset file names from where we want to get presets."
  (append (when (file-exists-p json-filename)
            (let ((parent-folder (f-parent json-filename)))
              ;; This outer -mapcat is used for the recursion such
              ;; that if an included preset file has included preset
              ;; files as well, they can be obtained
              (-mapcat 'cmake-integration--get-cmake-include-filenames
                       ;; This mapcar will return a list of cmake
                       ;; preset files included by the current preset
                       ;; file
                       (mapcar
                        (lambda (filename) (cmake-integration--change-to-absolute-filename filename parent-folder))
                        (alist-get 'include (json-read-file json-filename))))))
          (list json-filename)))



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
        (preset cmake-integration-last-configure-preset)
        (build-dir cmake-integration-build-dir))
    (unless project-root-folder
      (error "Not in a project"))

    (if preset
        ;; Use the build folder from the configure preset
        (let* ((source-dir-replacement (cons "${sourceDir}/" project-root-folder))
               (preset-name-replacement (cons "${presetName}" (alist-get 'name preset)))
               (replacements (list source-dir-replacement preset-name-replacement)))
          (s-replace-all replacements (cmake-integration--get-binaryDir preset)))

      ;; Use manually set build directory or throw an error
      (if build-dir
          (file-name-concat project-root-folder build-dir)
        (error "Build folder is not set.
Call `cmake-integration-select-configure-preset' to select a configure preset,
or set `cmake-integration-build-dir' manually")))))








(defun cmake-integration--get-preset-name (preset)
  "Get the name of the preset PRESET.

PRESET is an alist obtained from reading the cmake presets file
and getting one of the configure presets in it."
  (when preset (alist-get 'name preset)))


(defun cmake-integration--is-preset-visible (preset)
  "Returns t if the preset PRESET if not hidden.

PRESET is an alist obtained from reading the cmake presets file
and getting one of the configure presets in it."
  (when preset (not (alist-get 'hidden preset))))


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
