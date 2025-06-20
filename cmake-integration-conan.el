;;; cmake-integration-conan.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'tablist)
(require 'cmake-integration-build)
(require 'cmake-integration-conanfile)

(defvar ci--library-location "" "Library location that will be locally set.")
(defvar ci-addition-marker-char ?i "Character used to mark items for addition.")

(defun ci--get-conan-available-profiles ()
  "Get the available conan profiles."
  (let ((output (shell-command-to-string "conan profile list")))
    (cdr (split-string output "\n" t))))


(defun ci--get-conan-remote-repositories-names ()
  "Get the list of Conan remote names."
  (interactive)
  (let* ((command-output (shell-command-to-string "conan remote list -f json"))
         (parsed-json (json-read-from-string command-output)))
    (seq-filter
     #'identity
     (mapcar (lambda (remote)
               (when (eq (alist-get 'enabled remote) t) ;; Check if 'enabled' is true (t)
                 (alist-get 'name remote))) ;; Extract name if enabled
             parsed-json))))


;;;###autoload (autoload 'cmake-integration-select-conan-profile "cmake-integration")
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
     (propertize ci--library-location 'face 'font-lock-builtin-face)
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


(defun ci--parsed-local-cache-has-error (local-cache)
  "Check if the LOCAL-CACHE has an error."
  (eq (caar local-cache) 'error))


(defun ci--get-conan-command-result-as-tabulated-entries (conan-command)
  "Run the CONAN-COMMAND and return the result as tabulated entries.

CONAN-COMMAND is a string with the conan command to be run. It should
pass the flag to conan to output the result in json format, as well as
redirect the error output

A suitable command would be something like below.

    `conan list -f json 2> /dev/null'"

  (let* ((json-string (shell-command-to-string conan-command))
         (parsed-json (json-read-from-string json-string))
         (repositories (mapcar 'car parsed-json))
         (tabulated-entries))

    (dolist (repo repositories)
      (let ((ci--library-location (symbol-name repo))
            (libraries (alist-get repo parsed-json)))

        (unless (ci--parsed-local-cache-has-error libraries)
          (setq tabulated-entries
                (append tabulated-entries
                        (ci--get-tabulated-entries-from-conan-local-cache libraries))))))
    tabulated-entries))


(defun ci--get-conan-list-as-tabulated-entries (&optional pattern)
  "Get installed conan packages that match PATTERN.

If PATTERN is nil, return all packages in the conan cache.

Each library information in the returned list is a list of 4 elements,
in the form (NAME VERSION USER CHANNEL), being the library name, the
library version, the user name and the channel name. If the user and
channel names are not present, they are set to nil."
  (let* ((pattern (or pattern "*"))
         (command (format "conan list -f json \"%s\" 2> /dev/null" pattern)))
    (ci--get-conan-command-result-as-tabulated-entries command)))


(defun ci--conan-search-as-tabulated-entries (pattern &optional remote)
  "Search for PATTERN in the REMOTE repository.

If REMOTE is nil, search in all remotes.

The result is returned as tabulated entries."
  (let* ((remote-string (if remote
                            (format "--remote %s" remote)
                          ""))
         (command (format "conan search -f json \"%s\" %s 2> /dev/null" pattern remote-string)))
    (ci--get-conan-command-result-as-tabulated-entries command)))


(defvar ci--conan-tabulated-list-columns
  [("Name" 30 t)
   ("Version" 14 t)
   ("User" 10 t)
   ("Channel" 15 t)
   ("Location" 15 t)
   ]
  "Columns for the tabulated list.")


(defun ci--show-in-tabulated-mode (buffer tabulated-list-entries-func)
  "Compute entries with TABULATED-LIST-ENTRIES-FUNC and shown them in BUFFER.

The entries are shown using `conan-list-view-mode' and
TABULATED-LIST-ENTRIES-FUNC should return entries in the format
specified by `ci--conan-tabulated-list-columns'."
  (with-current-buffer buffer
    (setq tabulated-list-entries tabulated-list-entries-func)
    (conan-list-view-mode)
    (tabulated-list-print t))
  (switch-to-buffer buffer)
  (tablist-minor-mode 1))


;;;###autoload (autoload 'cmake-integration-conan-list-packages-in-local-cache "cmake-integration")
(defun ci-conan-list-packages-in-local-cache (&optional pattern)
  "Show the list of packages in conan cache matching PATTERN.

If PATTERN is nil, show all packages."
  (interactive)

  (when current-prefix-arg
    (setq pattern (read-string "Enter a pattern to filter the conan list: ")))

  (let ((buffer (get-buffer-create "*Conan List*"))
        (func (if pattern
                  (lambda () (ci--get-conan-list-as-tabulated-entries pattern))
                (lambda () (ci--get-conan-list-as-tabulated-entries)))))
    (ci--show-in-tabulated-mode buffer func)))


(defun ci--get-conan-search-buffer-name (pattern)
  "Return the name of the buffer for searching conan packages with PATTERN."
  (format "*Conan Search: \"%s\"*" pattern))


;;;###autoload (autoload 'cmake-integration-conan-search "cmake-integration")
(defun ci-conan-search (&optional pattern remote)
  "Search for PATTERN in the REMOTE repository.

If REMOTE is nil, search in all remotes."
  (interactive)

  (unless pattern
    (setq pattern (read-string "Enter a pattern to search in conan remote repositories: ")))

  (let ((buffer (get-buffer-create (ci--get-conan-search-buffer-name pattern)))
        (func (lambda () (ci--conan-search-as-tabulated-entries pattern remote))))
    (ci--show-in-tabulated-mode buffer func)))


(defun ci--conan-delete-itens (items)
  "Delete the items in ITEMS.

ITEMS is a list of conan library specifications. The function will
delete the libraries from the conan cache.

The output of the commands is added to a `*conan remove output*' buffer."
  (mapc #'(lambda (item)
            (call-process "conan" nil "*conan remove output*" nil "remove" "--confirm" item))
        items))


(defun ci--conan-find-item (library-spec)
  "Get only the library name from the LIBRARY-SPEC."
  (let* ((library-name (car (split-string library-spec "/")))
         (buffer-name (ci--get-conan-search-buffer-name library-name))
         (buffer-exists (bufferp (get-buffer buffer-name))))
    (if buffer-exists
        (switch-to-buffer buffer-name)
      (ci-conan-search library-name))))


(defun ci--conan-extract-library-spects-from-marked-items (marked-items)
  "Convert the list of MARKED-ITEMS into a list of library specifications.

This will map what `tablist-get-marked-items' returns into a list of
library specifications."
  (mapcar 'car marked-items))


(defun ci--tablist-get-marked-items ()
  "Get the library specifiacations of the marked items."
  (let ((marked-items (tablist-get-marked-items)))
    (mapcar 'car marked-items)))


(defun ci--conan-marked-items-export-to-kill-ring ()
  "Get the current marked items and export them to the kill ring."
  (interactive)
  (let ((marked-items (ci--tablist-get-marked-items)))
    (if marked-items
        (progn
          (kill-new (mapconcat 'identity marked-items "\n"))
          (message "Marked items exported to kill ring."))
      (message "No marked items to export."))))


(defun ci--conan-flag-forward-addition ()
  "Mark the current item with a `+' mark."
  (interactive)
  ;; Temporarily change the marker character such that tablist command use our
  ;; custom mark
  (let ((tablist-marker-char ci-addition-marker-char))
    (tablist-mark-forward)))


(defun ci--get-items-flagged-for-addition ()
  "Return items marked with `+' in the tablist."
  ;; Temporarily change the marker character such that tablist command use our
  ;; custom mark
  (let ((tablist-marker-char ci-addition-marker-char))
    (tablist-get-marked-items)))


(defun ci-conan-do-addition ()
  "Perform the addition operation on the items marked with `+'.

The addition operation adds the marked libraries to the conanfile in the
project root."
  (interactive)
  (let ((items (tablist-get-marked-items)))
    (when (tablist-yes-or-no-p 'addition nil items)
      (funcall tablist-operations-function
               'addition (mapcar 'car items)))))


(defun ci--conan-do-flagged-addition ()
  "Add all items flagged for addition to the conanfile in the project root."
  (interactive)
  (let ((tablist-marker-char ci-addition-marker-char))
    (ci-conan-do-addition)))


(defun ci--conan-do-flagged-items ()
  "Delete entries flagged for deletion and add entries flagged for addition."
  (interactive)
  (tablist-do-flagged-delete)
  (ci--conan-do-flagged-addition))


(defun ci--conan-add-marked-items-to-conanfile-txt (&optional marked-items)
  "Add the MARKED-ITEMS to the conanfile.txt in the project root."
  (interactive)
  (if-let* ((conanfile (cmake-integration--find-conanfile))
            (marked-items (or marked-items (ci--tablist-get-marked-items))))
      (if marked-items
          (progn
            (dolist (item marked-items)
              (ci-add-requirement-to-project-conanfile-txt item))
            (message "Marked items added to conanfile.txt."))
        (message "No marked items to add."))
    (message "No conanfile.txt found in the project root at %s." (cmake-integration--get-project-root-folder))))


(defun ci--conan-tablist-operations-function (operation &optional args)
  "Handles OPERATION on ARGS.

This is a function for handling operations on the entries. The operation
is indicated by OPERATION.

See the variable `tablist-operations-function' for more."
  (message (format "Operation is %s" operation))
  (pcase operation
    ('supported-operations '(refresh delete find-entry))
    ('refresh (message "Refreshing the tablist... Not implemented yet"))
    ('delete (ci--conan-delete-itens args))
    ('find-entry (ci--conan-find-item args))
    ('addition (ci--conan-add-marked-items-to-conanfile-txt args))
    (_ (message "Operation %s not implemented." operation)))
  )


(define-derived-mode conan-list-view-mode tablist-mode "Conan"
  "Visualize the output of conan list in as a table."
  :interactive nil
  (setq tabulated-list-format ci--conan-tabulated-list-columns)
  (setq tabulated-list-padding 2)
  (setq tablist-operations-function 'ci--conan-tablist-operations-function)
  (tabulated-list-init-header)

  ;; Add a keymap for the mode
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tablist-mode-map)
                   (define-key map "x" 'ci--conan-do-flagged-items)
                   (define-key map "o" 'ci--conan-transient)
                   (define-key map "c" 'ci--conan-transient)
                   (define-key map "l" 'ci-conan-list-packages-in-local-cache)
                   (define-key map "S" 'ci-conan-search)
                   (define-key map "w" 'ci--conan-marked-items-export-to-kill-ring)
                   (define-key map "I" 'ci-conan-do-addition)
                   (define-key map "i" 'ci--conan-flag-forward-addition)
                   map)))


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
      ;; If cmake-integration-conan-profile is set, it can be either a string
      ;; with a single profile, or an alist mapping cmake profile names to conan
      ;; profile names
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
