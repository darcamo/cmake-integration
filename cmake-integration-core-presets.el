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


(defun cmake-integration--prepare-for-completing-read (list-of-presets)
  "Transform LIST-OF-PRESETS to be used with `completing-read'.

This will create a transformed list of presets which maps each preset in
LIST-OF-PRESETS into a cons with the preset name and the preset itself.
It will also append the string \\='No Preset\\=' to this transformed
list. This makes it suitable to be used as the collection argument in
`completing-read'."
  (let ((transformed-list-of-presets (mapcar
   (lambda (preset) (cons (cmake-integration--get-preset-name preset) preset))
   list-of-presets)))
    ;; Add a "No Preset" option to all-presets to allow a user to
    ;; remove the preset
    (append transformed-list-of-presets '("No Preset"))))


;; TODO: Make it work when configurePreset is not present, and instead
;; its value should be taken from an inherited preset.
(defun cmake-integration--get-associated-configure-preset (preset)
  "Get the associated configure-preset of a PRESET."
  (alist-get 'configurePreset preset))


(defun cmake-integration--preset-has-matching-configure-preset-p (preset configure-preset)
  "Check if PRESET has a configure preset name matching CONFIGURE-PRESET."
  (let* ((configure-preset-name (cmake-integration--get-preset-name configure-preset))
         (associated-configure-preset-name (cmake-integration--get-associated-configure-preset preset)))
    (equal configure-preset-name associated-configure-preset-name)))


(defun cmake-integration-get-presets-of-type (type &optional configure-preset)
  "Get the presets of type TYPE associated with CONFIGURE-PRESET.

Get the presets in both `CMakePresets.json' and `CMakeUserPresets.json'
files, as well as in any included files, whose configure preset is
CONFIGURE-PRESET. If CONFIGURE-PRESET is not provided, then the value in
the `cmake-integration-configure-preset' variable will be used."
  (let ((configure-preset (or configure-preset cmake-integration-configure-preset))
        (all-presets (cmake-integration-get-all-presets-of-type type)))
    (seq-filter
     #'(lambda (preset) (cmake-integration--preset-has-matching-configure-preset-p preset configure-preset))
     all-presets)))


(defun cmake-integration--annotation-from-displayName-function (preset)
  "Function that returns an annotation with the displayName field in a PRESET.

This is used in `cmake-integration-select-*-preset' functions when
completing a preset name to generate an annotation for that preset. This
annotation is shown during the completions if you are using the
marginalia package, or in Emacs standard completion buffer."

  (let* ((initial-spaces (cmake-integration--get-annotation-initial-spaces preset))
         (no-preset-annotation (format "%sDon't use any preset. The build folder is '%s'" initial-spaces (cmake-integration--get-build-folder-relative-to-project)))
         ;; Note that `minibuffer-completion-table' has the list of
         ;; completions currently in use, from which we know PRESET is
         ;; one of them
         (display-name (alist-get 'displayName (alist-get preset minibuffer-completion-table nil nil 'equal)))
         (preset-annotation (concat initial-spaces display-name)))
    (if (equal preset "No Preset")
        no-preset-annotation
      preset-annotation)))


(provide 'cmake-integration-core-presets)

;;; cmake-integration-core-presets.el ends here
