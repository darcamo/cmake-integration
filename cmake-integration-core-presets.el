;;; cmake-integration-core-presets.el --- Core functions to work with presets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun ci--get-system-presets-file ()
  "Get the path of the system presets file."
  (file-name-concat (ci--get-project-root-folder) "CMakePresets.json"))


(defun ci--get-user-presets-file ()
  "Get the path of the user presets file."
  (file-name-concat (ci--get-project-root-folder) "CMakeUserPresets.json"))


(defun ci--expand-included-presets (json-filename)
  "Return a list containing JSON-FILENAME and any included presets file.

A CMake presets file can include other preset files, which can include
other preset files themselves. This function returns a list containing
both JSON-FILENAME as well as any presets file included in it."
  (when (file-exists-p json-filename)
    (let* ((parent-folder (f-parent json-filename))
           (included-files (alist-get 'include (json-read-file json-filename)))
           (absolute-included-files (mapcar (lambda (filename)
                                              (ci--change-to-absolute-filename filename parent-folder))
                                            included-files)))
      (append (-mapcat #'ci--expand-included-presets absolute-included-files)
              (list json-filename)))))


(defun ci--get-parent-preset-name (preset)
  "Get the name in the `inherits' field of the preset PRESET."
  (alist-get 'inherits preset))


(defun ci--get-all-preset-files ()
  "Get the system and user preset files, as well as included files."
  (let ((system-presets-file (ci--get-system-presets-file))
        (user-presets-file (ci--get-user-presets-file)))
    (append (ci--expand-included-presets system-presets-file)
            (ci--expand-included-presets user-presets-file))))


(defun ci--get-preset-name (preset)
  "Get the name of the preset PRESET.

PRESET is an alist obtained from reading the cmake presets file
and getting one of the configure presets in it."
  (when preset (alist-get 'name preset)))


(defun ci--is-preset-visible (preset)
  "Returns t if the preset PRESET if not hidden.

PRESET is an alist obtained from reading the cmake presets file
and getting one of the configure presets in it."
  (when preset (not (alist-get 'hidden preset))))


(defun ci--get-presets-of-given-type (json-filename type)
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


(defun ci-get-all-presets-of-type (type &optional show-hidden)
  "Get the presets of type TYPE from all preset files.

Get the configure presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files, as well as any included file. If
SHOW-HIDDEN is t, include hidden presets."
  (let* ((all-preset-files (ci--get-all-preset-files))
         (all-configure-presets (-mapcat
                                 #'(lambda (presets-file) (ci--get-presets-of-given-type presets-file type))
                                 all-preset-files)))
    (if show-hidden
        all-configure-presets
      (seq-filter 'ci--is-preset-visible all-configure-presets))))


(defun ci--get-preset-by-name (name list-of-presets)
  "Get the preset in LIST-OF-PRESETS with name NAME."
  (seq-find
   (lambda (preset) (equal name (ci--get-preset-name preset)))
   list-of-presets))


(defun ci--prepare-for-completing-read (list-of-presets)
  "Transform LIST-OF-PRESETS to be used with `completing-read'.

This will create a transformed list of presets which maps each preset in
LIST-OF-PRESETS into a cons with the preset name and the preset itself.
It will also append the string \\='No Preset\\=' to this transformed
list. This makes it suitable to be used as the collection argument in
`completing-read'."
  (let ((transformed-list-of-presets (mapcar
                                      (lambda (preset) (cons (ci--get-preset-name preset) preset))
                                      list-of-presets)))
    ;; Add a "No Preset" option to all-presets to allow a user to
    ;; remove the preset
    (append transformed-list-of-presets '("No Preset"))))


;; TODO: Make it work when configurePreset is not present, and instead
;; its value should be taken from an inherited preset.
(defun ci--get-associated-configure-preset (preset)
  "Get the associated configure-preset of a PRESET."
  (alist-get 'configurePreset preset))


(defun ci--preset-has-matching-configure-preset-p (preset configure-preset)
  "Check if PRESET has a configure preset name matching CONFIGURE-PRESET."
  (let* ((configure-preset-name (ci--get-preset-name configure-preset))
         (associated-configure-preset-name (ci--get-associated-configure-preset preset)))
    (equal configure-preset-name associated-configure-preset-name)))


(defun ci-get-presets-of-type (type &optional configure-preset)
  "Get the presets of type TYPE associated with CONFIGURE-PRESET.

Get the presets in both `CMakePresets.json' and `CMakeUserPresets.json'
files, as well as in any included files, whose configure preset is
CONFIGURE-PRESET. If CONFIGURE-PRESET is not provided, then the value in
the `cmake-integration-configure-preset' variable will be used."
  (let ((configure-preset (or configure-preset ci-configure-preset))
        (all-presets (ci-get-all-presets-of-type type)))
    (seq-filter
     #'(lambda (preset) (ci--preset-has-matching-configure-preset-p preset configure-preset))
     all-presets)))


(defun ci--annotation-from-displayName-function (preset)
  "Function that returns an annotation with the displayName field in a PRESET.

This is used in `cmake-integration-select-*-preset' functions when
completing a preset name to generate an annotation for that preset. This
annotation is shown during the completions if you are using the
marginalia package, or in Emacs standard completion buffer."

  (let* ((initial-spaces (ci--get-annotation-initial-spaces preset))
         (no-preset-annotation (format "%sDon't use any preset. Current build folder: '%s'" initial-spaces ci-build-dir))
         ;; Note that `minibuffer-completion-table' has the list of
         ;; completions currently in use, from which we know PRESET is
         ;; one of them
         (display-name (alist-get 'displayName (alist-get preset minibuffer-completion-table nil nil 'equal)))
         (preset-annotation (concat initial-spaces display-name)))
    (if (equal preset "No Preset")
        no-preset-annotation
      preset-annotation)))


(defun ci-select-preset (all-presets prompt)
  "Select a CMake preset from ALL-PRESETS using PROMPT for the message.

ALL-PRESETS is the list of all available presets. PROMPT is the prompt
string for the `completing-read' function. Returns the selected preset
or nil if \\='No Preset\\=' is selected."
  (let* ((collection (ci--prepare-for-completing-read all-presets))
         (completion-extra-properties '(:annotation-function ci--annotation-from-displayName-function))
         (choice (completing-read prompt collection nil t)))
    (if (equal choice "No Preset")
        nil
      (ci--get-preset-by-name choice all-presets))))



(provide 'cmake-integration-core-presets)

;;; cmake-integration-core-presets.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
