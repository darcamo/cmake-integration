;;; cmake-integration-core-presets.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun cmake-integration--get-system-presets-file ()
  "Get the path of the system presets file."
  (file-name-concat (cmake-integration--get-project-root-folder) "CMakePresets.json"))


(defun cmake-integration--get-user-presets-file ()
  "Get the path of the user presets file."
  (file-name-concat (cmake-integration--get-project-root-folder) "CMakeUserPresets.json"))


(defun cmake-integration--get-include-presets-filenames (json-filename)
  "Return a list of include preset filenames in the JSON-FILENAME.

A CMake presets file can include other presets files, which can include
other preset files themselves. This function will return a flat list
with the absolute paths of all of these included presets.

NOTE: JSON-FILENAME is also returned as the lasts element, such that the
output of `cmake-integration--get-include-presets-filenames' has all the
preset file names from where we want to get presets."
  (append (when (file-exists-p json-filename)
            (let ((parent-folder (f-parent json-filename)))
              ;; This outer -mapcat is used for the recursion such
              ;; that if an included preset file has included preset
              ;; files as well, they can be obtained
              (-mapcat 'cmake-integration--get-include-presets-filenames
                       ;; This mapcar will return a list of cmake
                       ;; preset files included by the current preset
                       ;; file
                       (mapcar
                        (lambda (filename) (cmake-integration--change-to-absolute-filename filename parent-folder))
                        (alist-get 'include (json-read-file json-filename))))))
          (list json-filename)))


(defun cmake-integration--get-parent-preset-name (preset)
  "Get the name in the `inherits' field of the preset PRESET."
  (alist-get 'inherits preset))


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


(defun cmake-integration--get-presets-of-given-type (json-filename type)
  "Get the list of presets in JSON-FILENAME of type TYPE.

The TYPE of a preset is one of the possible presets type, such as:
`configurePresets', `configurePresets', `testPresets', `packagePresets',
or `workflowPresets'.

Return nil if the file does not exist."
  (when (file-exists-p json-filename)
    ;; Append a vector to nil transforms it into a list
    (append
     (alist-get type (json-read-file json-filename))
     nil)))


(provide 'cmake-integration-core-presets)

;;; cmake-integration-core-presets.el ends here
