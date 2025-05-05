;;; cmake-integration-conan.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'tablist)
(require 'cmake-integration-build)

(defun ci--get-conan-available-profiles ()
  "Get the available conan profiles."
  (let ((output (shell-command-to-string "conan profile list")))
    (cdr (split-string output "\n" t))))


(defun ci-select-conan-profile ()
  "Select one of the available conan profiles and return the chosen one."
  (interactive)
  (let* ((all-profiles (ci--get-conan-available-profiles))
         (choice (completing-read "Conan profile: " all-profiles nil t)))
    (setq ci-conan-profile choice)))


(defun ci--parse-conan-library-spec (library-spec)
  "Parse the LIBRARY-SPEC string and return a list of strings.

The retuened list has 4 elements, being the library name, the version,
the user and the channel. If the user and channel are not present, they
are set to nil."
  (pcase-let* (
               (`(,first-part ,second-part) (split-string library-spec "@"))
               (`(,name ,version) (split-string first-part "/"))
               (`(,user ,channel) (if second-part (split-string second-part "/") '("" "")))
               )
    (vector
     (propertize name 'face 'font-lock-type-face)
     (propertize version 'face 'font-lock-number-face)
     (propertize user 'face 'font-lock-variable-name-face)
     (propertize channel 'face 'font-lock-variable-name-face)
     )))


(defun ci--get-tabulated-entry (library-spec)
  "Parse the LIBRARY-SPEC string and an entry for a tabulated list."
  (list library-spec (ci--parse-conan-library-spec library-spec)))


(defun ci--get-tabulated-entries-from-conan-local-cache (library-symbols)
  "Map a list of LIBRARY-SYMBOLS into entries for a tabulated list.

The list of LIBRARY-SYMBOLS is obtained from parsing the conan list
command output in json format. This function will turn each element in
the list into a string, then break it into the different parts (name,
version, user and channel) and then build an entry suitable to be used
in `tabulated-list-entries'."
  (mapcar #'(lambda (elem)
              (let ((full-library-spec-string (symbol-name (car elem))))
                (ci--get-tabulated-entry full-library-spec-string)))
          library-symbols))


(defun ci--get-conan-list-as-tabulated-entries ()
  "Get a list of library information of the installed conan packages.

Each library information in the returned list is a list of 4 elements,
in the form (NAME VERSION USER CHANNEL), being the library name, the
library version, the user name and the channel name. If the user and
channel names are not present, they are set to nil."
  (let* ((json-string (shell-command-to-string "conan list -f json 2> /dev/null"))
         (parsed-json (json-read-from-string json-string))
         (local-cache (alist-get 'Local\ Cache parsed-json)))
    (ci--get-tabulated-entries-from-conan-local-cache local-cache)))


(defvar ci--conan-tabulated-list-columns
  [("Name" 30 t)
   ("Version" 14 t)
   ("User" 10 t)
   ("Channel" 15 t)]
  "Columns for the tabulated list.")


(defun ci-view-conan-list-as-table ()
  "Initialize the tabulated list mode."
  (interactive)
  (let ((buffer (get-buffer-create "*Conan List*")))
    (with-current-buffer buffer
      (setq tabulated-list-entries 'ci--get-conan-list-as-tabulated-entries)
      (conan-list-view-mode)
      (tabulated-list-print t))
    (switch-to-buffer buffer)
    (tablist-minor-mode 1)
    ))



(define-derived-mode conan-list-view-mode tablist-mode "Conan List"
  "Visualize the output of conan list in as a table."
  (setq tabulated-list-format ci--conan-tabulated-list-columns)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;; TODO Handle marked entries
;; TODO Handle items marked for deletion





;; (defun ci-run-conan-list ()
;;   "Run `conan list` to list installed packages.

;; The result is displayed in a `Conan List` buffer."
;;   (interactive)
;;   (let* ((output (shell-command-to-string "conan list"))
;;          (buffer (get-buffer-create "*Conan List*")))
;;     (with-current-buffer buffer
;;       (erase-buffer)
;;       (insert output))
;;     (display-buffer buffer)))


;; (defun ci-run-conan-list-2 ()
;;   "Run `conan list` to list installed packages.

;; The result is displayed in a `Conan List` buffer."
;;   (interactive)
;;   (let ((json-output (shell-command-to-string "conan list -f json 2> /dev/null"))
;;         (buffer (get-buffer-create "*Conan List*")))
;;     (with-current-buffer buffer
;;       (erase-buffer)
;;       (insert json-output))
;;     (display-buffer buffer)))


(defun ci--get-conan-run-command (&optional profile)
  "Get the command to run `conan install' using PROFILE.

The command is run from the build folder of the current cmake
configuration."
  (if profile
      (format "%s --profile %s" (ci--get-conan-run-command) profile)
    (format "cd %s && conan install %s %s"
            (ci-get-build-folder)
            (ci--get-project-root-folder)
            ci-conan-arguments)))


(defun ci-get-conan-run-command ()
  "Get the command to run `conan install'.

The command is run from the build folder of the current cmake
configuration and the profile passed to conan is taken from the
`cmake-integration-conan-profile' variable."
  (if ci-conan-profile
      ;; If cmake-integration-conan-profile is set, it can be either a string with a single profile, or an alist mapping cmake profile names to conan profile names
      (if (stringp ci-conan-profile)
          (ci--get-conan-run-command ci-conan-profile)
        ;; Map cmake profile name to conan profile name
        (let* ((cmake-profile-name (alist-get 'name ci-configure-preset))
               (conan-profile-name (alist-get cmake-profile-name ci-conan-profile nil nil 'equal)))
          ;; Note that if we have no conan profile name in the alist
          ;; for the current cmake profile nane, then
          ;; `conan-profile-name' is nil and no profile will be used
          (ci--get-conan-run-command conan-profile-name)))

    ;; If cmake-integration-conan-profile is not set we get the conan command without using a profile
    (ci--get-conan-run-command nil)))


;;;###autoload (autoload 'cmake-integration-run-conan "cmake-integration")
(defun ci-run-conan ()
  "Run conan install in the current build folder."
  (compile (ci-get-conan-run-command)))


(defun ci-prepend-conan-command (cmake-command)
  "Prepend the CMAKE-COMMAND with the conan command.

The output can be passed to compile,"
  (let* ((conan-command (format "%s && " (ci-get-conan-run-command))))
    (if ci-include-conan-toolchain-file
        (format "%s%s --toolchain conan_toolchain.cmake" conan-command cmake-command)
      (format "%s%s" conan-command cmake-command))))


(provide 'cmake-integration-conan)

;;; cmake-integration-conan.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
