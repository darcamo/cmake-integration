;;; cmakepresets-mode.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun cmake-presets-imenu-create-index ()
  "Create an imenu index for CMake presets files."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (index-alist '())
        (preset-types '(("configurePresets" . "Configure")
                        ("buildPresets" . "Build")
                        ("testPresets" . "Test")
                        ("packagePresets" . "Package")
                        ("workflowPresets" . "Workflow"))))
    (save-excursion
      (goto-char (point-min))
      (let ((json-data (condition-case nil
                           (json-read)
                         (error nil))))
        (when (and json-data (listp json-data))
          (dolist (preset-type preset-types)
            (let ((type-key (car preset-type))
                  (type-name (cdr preset-type)))
              (when-let ((presets (assoc-default type-key json-data)))
                (dolist (preset presets)
                  (when-let ((name (assoc-default "name" preset)))
                    (let ((entry-name (format "%s / %s" type-name name))
                          (entry-pos (save-excursion
                                       (goto-char (point-min))
                                       (search-forward (format "\"name\": \"%s\"" name) nil t))))
                      (when entry-pos
                        (push (cons entry-name entry-pos) index-alist)))))))))))
    (nreverse index-alist)))


(defun cmake-presets-imenu-create-index-with-groups ()
  "Create an imenu index for CMake presets files with Consult groups."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (index-alist '())
        (preset-types '(("configurePresets" . "Configure")
                        ("buildPresets" . "Build")
                        ("testPresets" . "Test")
                        ("packagePresets" . "Package")
                        ("workflowPresets" . "Workflow"))))
    (save-excursion
      (goto-char (point-min))
      (let ((json-data (condition-case nil
                           (json-read)
                         (error nil))))
        (when (and json-data (listp json-data))
          (dolist (preset-type preset-types)
            (let ((type-key (car preset-type))
                  (type-name (cdr preset-type))
                  (group-alist '()))
              (when-let ((presets (assoc-default type-key json-data)))
                (dolist (preset presets)
                  (when-let ((name (assoc-default "name" preset)))
                    (let ((entry-pos (save-excursion
                                       (goto-char (point-min))
                                       (search-forward (format "\"name\": \"%s\"" name) nil t))))
                      (when entry-pos
                        (push (cons name entry-pos) group-alist))))))
              (when group-alist
                (push (cons type-name (nreverse group-alist)) index-alist)))))))
    (nreverse index-alist)))


(defun dummy-index-function ()
  '(("Entry 1" . 1)
    ("Entry 2" . 20)
    ("Entry 3" . 123))
  )


(defun grouped-dummy-index-function ()
  '(("Group 1" . (("Entry 1" . 1)
                  ("Entry 2" . 20)))
    ("Group 2" . (("Entry 3" . 123)
                  ("Entry 4" . 456)))
    ("Group 3" . (("Entry 5" . 789)
                   ("Entry 6" . 101112)))
    ))



(defun cmake-presets-setup-imenu ()
  "Set up imenu for CMake presets files."
  ;; (setq-local imenu-create-index-function #'cmake-presets-imenu-create-index)
  (setq-local imenu-create-index-function #'cmake-presets-imenu-create-index-with-groups)
  ;; (setq-local imenu-create-index-function #'dummy-index-function)
  ;; (setq-local imenu-create-index-function #'grouped-dummy-index-function)
  )





;; (add-hook 'json-mode-hook
;;           (lambda ()
;;             (when (and buffer-file-name
;;                        (string-match-p "CMakePresets\\.json\\'" buffer-file-name))
;;               (cmake-presets-setup-imenu))))


(provide 'cmakepresets-mode)
