;;; cmake-integration-core-presets.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun cmake-integration--get-system-presets-file ()
  "Get the path of the system presets file."
  (file-name-concat (cmake-integration--get-project-root-folder) "CMakePresets.json"))


(defun cmake-integration--get-user-presets-file ()
  "Get the path of the user presets file."
  (file-name-concat (cmake-integration--get-project-root-folder) "CMakeUserPresets.json"))


(defun cmake-integration--expand-included-presets (json-filename)
  "Return a list containing JSON-FILENAME and any included presets file.

A CMake presets file can include other preset files, which can include
other preset files themselves. This function returns a list containing
both JSON-FILENAME as well as any presets file included in it."
  (when (file-exists-p json-filename)
    (let* ((parent-folder (f-parent json-filename))
           (included-files (alist-get 'include (json-read-file json-filename)))
           (absolute-included-files (mapcar (lambda (filename)
                                              (cmake-integration--change-to-absolute-filename filename parent-folder))
                                            included-files)))
      (append (-mapcat #'cmake-integration--expand-included-presets absolute-included-files)
              (list json-filename)))))


(defun cmake-integration--get-parent-preset-name (preset)
  "Get the name in the `inherits' field of the preset PRESET."
  (alist-get 'inherits preset))


(defun cmake-integration--get-all-preset-files ()
  "Get the system and user preset files, as well as included files."
  (let ((system-presets-file (cmake-integration--get-system-presets-file))
        (user-presets-file (cmake-integration--get-user-presets-file)))
    (append (cmake-integration--expand-included-presets system-presets-file)
            (cmake-integration--expand-included-presets user-presets-file))))


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


(defun cmake-integration-get-all-presets-of-type (type)
  "Get the presets of type TYPE from all preset files.

Get the configure presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files, as well as any included file.."
  (let* ((all-preset-files (cmake-integration--get-all-preset-files))
         (all-configure-presets (-mapcat
                                 #'(lambda (presets-file) (cmake-integration--get-presets-of-given-type presets-file type))
                                 all-preset-files)))
    (seq-filter 'cmake-integration--is-preset-visible all-configure-presets)))


(defun cmake-integration--get-preset-by-name (name list-of-presets)
  "Get the preset in LIST-OF-PRESETS with name NAME."
  (seq-find
   (lambda (preset) (equal name (alist-get 'name preset)))
   list-of-presets))


(provide 'cmake-integration-core-presets)

;;; cmake-integration-core-presets.el ends here
