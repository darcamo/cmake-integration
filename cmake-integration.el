;;; cmake-integration.el --- Easily configure cmake projects and run/debug targets -*- lexical-binding: t -*-

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") f s json)
;; Homepage: https://github.com/darcamo/cmake-integration
;; Keywords: c c++ cmake languages tools
;; URL: https://github.com/darcamo/cmake-integration/


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Provide functions to configure cmake projects, and to compile and
;; run a given target. Completions are provided with targets obtained
;; from cmake. It also support cmake presets.

;;; Code:
(require 'project)
(require 'f)
(require 's)
(require 'json)
(require 'cl-extra)



(defgroup cmake-integration nil "Easily call cmake configure and run compiled targets." :group 'tools :prefix "cmake-integration-")


(defcustom cmake-integration-build-dir "build"
  "The build folder to use when no presets are used.

If this is nil, then using presets is required." :type '(string) :group 'cmake-integration)

;; TODO: Set all possible choices of generators for a better
;; customization interface (but still allow a free string as a
;; generator)
(defcustom cmake-integration-generator nil
  "The generator to pass to cmake when no presets are used.

If this is nil, then the generator is not explicitly set." :type '(string) :group 'cmake-integration)

(defcustom cmake-integration-annotation-column 30
  "Column where annotations should start during completion." :type '(integer) :group 'cmake-integration)

(defcustom cmake-integration-include-subproject-targets-during-completion t
  "If t, include subproject targets when presenting target names for completion.

When t (default) all targets are included during completion of
target names. If nil, then only targets from the main cmake
project are included (targets with projectIndex equal to zero)."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)

(defcustom cmake-integration-hide-utility-targets-during-completion nil
  "If t, then utility targets are not included during completion."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)

(defcustom cmake-integration-hide-library-targets-during-completion nil
  "If t, then library targets are not included during completion."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)

(defcustom cmake-integration-run-working-directory 'bin
  "Working directory when running a target executable.

Possible values are the symbols `bin' (to run from the folder
containing the executable), `build' (to run from the build folder)
and `root' (to run from the project root), as well as any string.
In the case of a string, it should match an existing subfolder of
the project root." :type '(choice symbol string)
  :group 'cmake-integration
  :safe #'cmake-integration--run-working-directory-p
  :local t)


(defcustom cmake-integration-create-compile-commands-link t
  "If t, make a link of `compile_commands.json' to the project root.

This helps lsp and clangd correctly parsing the project files."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom cmake-integration-use-dap-for-debug nil
  "If t, use `dap-mode' with cpptools for debug.

If nil, use standard gdb graphical interface (see Emacs manual)."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom cmake-integration-conan-arguments "--build missing" "Extra arguments to pass to conan." :type '(string) :group 'cmake-integration)

;; TODO: Investigate if it is possible to get completions for the conan and cmake profiles in the custom interface
(defcustom cmake-integration-conan-profile nil
  "Conan profile to use, or an alist mapping cmake profiles to conan profiles."
  :type '(choice
          (const :tag "Don't use any Conan profile" nil)
          (string :tag "Use a single Conan profile")
          (alist :tag "Map a CMake profile into a Conan profile"
                 :key-type (string :tag "Cmake profile")
                 :value-type (string :tag "Conan profile")))
  :safe #'listp
  :group 'cmake-integration)


(defcustom cmake-integration-include-conan-toolchain-file nil
  "If t, pass '--toolchain conan_toolchain.cmake' to cmake.

If you are using the 'CMakeToolchain' generator, set this to true
in a directory local variable in your project."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom cmake-integration-docs-folder "${sourceDir}/docs"
  "Folder containing the Doxyfile.

If \"${sourceDir}/\" is in `cmake-integration-docs-folder' it
will be replaced by the project root." :type 'string :group 'cmake-integration)


(defvar cmake-integration-current-target nil)
(defvar cmake-integration-current-target-run-arguments "")
(defvar cmake-integration-last-configure-preset nil)

(defconst cmake-integration--multi-config-separator "/"
  "Character used to separate target name from config name.

In case of of multi-config generators, target names have the
special form <target-name><sep><config-name> (e.g. `all/Debug'
with `/' as configured separator).

Note: The selected separator shall be a character that it is not
a valid component of a CMake target name (see
https://cmake.org/cmake/help/latest/policy/CMP0037.html).")


;; BUG: This function seems to work correctly, but when used as the
;; ":safe" predicate in the defcustom Emacs still asks for confirming
;; if the variable is safe for the symbol values
(defun cmake-integration--run-working-directory-p (val)
  "Check if VAL is safe as a local variable.

Function to verify is VAL is save as a value for the
`cmake-integration-run-working-directory' variable"

  (if (stringp val)
      ;; Return t if VAL is a valid subfolder of the project root
      (file-exists-p (file-name-concat (cmake-integration-get-project-root-folder) val))
    ;; Return t if VAL is one of the accepted symbols
    (pcase val
      ('bin t)
      ('build t)
      ('root t)
      (_ nil)
      )
    )
  )


(defun cmake-integration--mktarget (target-name &optional config-name)
  "Return target constructed from from TARGET-NAME and CONFIG-NAME."
  (if config-name
      (concat target-name cmake-integration--multi-config-separator config-name)
    target-name))


(defun cmake-integration-get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (project-root (project-current)))


(defun cmake-integration-get-build-folder ()
  "Get the project build folder.

Returns the build folder path based on either the configure preset or
the manually specified `cmake-integration-build-dir'. Throws an error if
no valid build folder can be determined."
  (let ((project-root-folder (cmake-integration-get-project-root-folder))
        (preset cmake-integration-last-configure-preset)
        (build-dir cmake-integration-build-dir))
    (unless project-root-folder
      (error "Not in a project"))

    (if preset
        ;; Use the build folder from the configure preset
        (let* ((source-dir-replacement (cons "${sourceDir}/" project-root-folder))
               (preset-name-replacement (cons "${presetName}" (alist-get 'name preset)))
               (replacements (list source-dir-replacement preset-name-replacement)))
          (s-replace-all replacements (cmake-integration-get-binaryDir preset)))

      ;; Use manually set build directory or throw an error
      (if build-dir
          (file-name-concat project-root-folder build-dir)
        (error "Build folder is not set.
Call `cmake-integration-select-configure-preset' to select a configure preset,
or set `cmake-integration-build-dir' manually")))))


(defun cmake-integration-get-query-folder ()
  "Get the query folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/query/client-emacs"))


(defun cmake-integration-get-reply-folder ()
  "Get the reply folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (cmake-integration-get-build-folder) ".cmake/api/v1/reply/"))


(defun cmake-integration-get-path-of-codemodel-query-file ()
  "Get the full path of the codemodel-query-file."
  (file-name-concat (cmake-integration-get-query-folder) "codemodel-v2"))


(defun cmake-integration-get-compile-command (target)
  "Get the command to compile target TARGET."
  (pcase-let ((`(,target-name ,config-name)
               (split-string target
                             cmake-integration--multi-config-separator)))
    (format "cd %s && cmake --build %s%s --target %s"
            (cmake-integration-get-project-root-folder)
            (if cmake-integration-last-configure-preset
                (format "--preset %s" (cmake-integration-get-last-configure-preset-name))
              (file-relative-name (cmake-integration-get-build-folder)
                                  (cmake-integration-get-project-root-folder)))
            (if config-name (format " --config %s" config-name) "")
            target-name)))


(defun cmake-integration-get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."

  (elt (f-glob "codemodel-v2*json" (cmake-integration-get-reply-folder)) 0))


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


(defun cmake-integration-get-last-configure-preset-name ()
  "Get the `name' field of the last preset used for configure."
  (cmake-integration--get-preset-name cmake-integration-last-configure-preset))


(defun cmake-integration-create-empty-codemodel-file ()
  "Create an empty codemodel query file for CMake's file API."
  ;; Only do something if the file does not exist
  (unless (file-exists-p (cmake-integration-get-path-of-codemodel-query-file))
    ;; Create the folder if it does not exists yet
    (unless (file-exists-p (cmake-integration-get-query-folder))
      (shell-command (concat "mkdir -p " (cmake-integration-get-query-folder))))
    ;; Create the codemodel file
    (shell-command (concat "touch " (cmake-integration-get-path-of-codemodel-query-file)))))


;; See CMake file API documentation for what projectIndex is
;; https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html
(defun cmake-integration--target-is-in-projectIndex0-p (target)
  "Return t if the projectIndex field of TARGET is 0."
  (eq (alist-get 'projectIndex target) 0)
  )


(defun cmake-integration--target-is-not-utility-p (target)
  "Return t if TARGET type is not `UTILITY'."
  (not (equal (alist-get 'type target) "UTILITY"))
  )


(defun cmake-integration--target-is-not-library-p (target)
  "Return t if TARGET type is not `UTILITY'."
  (let ((type (alist-get 'type target)))
    (when type
      (not (equal (car (cdr (split-string type "_"))) "LIBRARY"))
      )))


(defun cmake-integration-get-cmake-targets-from-codemodel-json-file (&optional json-filename predicate)
  "Return the targets found in JSON-FILENAME that respect PREDICATE.

Return an alist of (target-name . target-info) elements for
targets found in JSON-FILENAME.

JSON-FILENAME must be a CMake API codemodel file.

If JSON-FILENAME is not provided, use the value obtained with
`cmake-integration-get-codemodel-reply-json-filename'.

In addition to the targets defined in JSON-FILENAME, the returned
alist also contains elements for the implicit targets `all' and
`clean' plus optional `install' targets. These special targets
don't have the `target-info' data."

  (let* ((json-filename (or json-filename
                            ;; If json-filename was not provided, get it from
                            ;; 'cmake-integration-get-codemodel-reply-json-filename'.
                            (cmake-integration-get-codemodel-reply-json-filename)))
         (configurations (alist-get 'configurations (json-read-file json-filename))))
    ;; The result of the nested `mapcar's below is a list of list of alists.
    ;; What we need a list of alists so remove one level and combine the next
    ;; level lists (of alists) of into a single list (of alists).
    (apply #'nconc
           ;; process configurations vector
           (mapcar (lambda (config-data)
                     ;; config-name is non-nil only when using ninja
                     ;; multi-config generator, where we have more
                     ;; than one configuration
                     (let ((config-name (and (> (length configurations) 1)
                                             (alist-get 'name config-data)))
                           (has-install-rule (cl-some (lambda (dir) (alist-get 'hasInstallRule dir))
                                                      (alist-get 'directories config-data))))
                       ;; Add implicit 'all', 'clean' and optional 'install' targets
                       (cmake-integration--add-all-clean-install-targets
                        (mapcar (lambda (target-info)
                                  (let ((target-name (alist-get 'name target-info)))
                                    (cons (cmake-integration--mktarget target-name config-name)
                                          target-info)))
                                ;; Sequence of targets from the json file. If predicate was
                                ;; provided, get only the targets that match predicate.
                                ;; Otherwise, get all targets.
                                (if predicate
                                    (seq-filter predicate (alist-get 'targets config-data))
                                  (alist-get 'targets config-data)
                                  ))
                        config-name
                        has-install-rule
                        )))
                   ;; Sequence mapped in the mapcar
                   configurations))))


(defun cmake-integration-get-cmake-targets-from-codemodel-json-file-2 (&optional json-filename predicate)
  "Return the targets found in JSON-FILENAME that respect PREDICATE.

This function is the same as
`cmake-integration-get-cmake-targets-from-codemodel-json-file',
with the exception that it adds the type of each target to a
`type' field in the target. The main use for this information is
during completion of target names, where this type information is
shown as an annotation."
  ;; Start with the list of targets returned by cmake-integration-get-cmake-targets-from-codemodel-json-file, then loop over each target to add the type information, skipping the "all", "clean" and "install" targets.
  (let ((list-of-targets (cmake-integration-get-cmake-targets-from-codemodel-json-file json-filename predicate)))
    (dolist (target list-of-targets)
      (let ((target-name (car (split-string (car target) cmake-integration--multi-config-separator))))
        (unless (or (equal target-name "all") (equal target-name "clean") (equal target-name "install"))
          (let* ((json-file (alist-get 'jsonFile (cdr target)))
                 (json-full-filename (file-name-concat (cmake-integration-get-reply-folder) json-file))
                 (target-json-data (json-read-file json-full-filename))
                 )

            ;; Set the value from the json data to `type' field
            (setf (alist-get 'type (cdr target)) (alist-get 'type target-json-data))
            ))))
    list-of-targets
    ))


(defun cmake-integration--add-all-clean-install-targets (targets config-name has-install-rule)
  "Return TARGETS with extra `all', `clean' and `install' for CONFIG-NAME.

The `install' target is only included if HAS-INSTALL-RULE is true."
  (nconc `((,(cmake-integration--mktarget "all" config-name)))
         `((,(cmake-integration--mktarget "clean" config-name)))
         (when has-install-rule
           `((,(cmake-integration--mktarget "install" config-name))))
         targets))


(defun cmake-integration--change-to-absolute-filename (filename parent-folder)
  "If FILENAME is relative to PARENT-FOLDER, make it absolute.
Otherwise return it unchanged."
  (if (f-absolute-p filename)
      filename
    (f-join parent-folder filename)))


(defun cmake-integration--get-cmake-include-filenames (json-filename)
  "Return a list of include preset filenames in the JSON-FILENAME.

A CMake presets file can include other presets files, which can
include other preset files themselves. This function will return
a flat list with the absolute paths of all of these included
presets.

NOTE: JSON-FILENAME is also returned as the lasts element, such
that the output of
`cmake-integration--get-cmake-include-filenames' has all the
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


(defun cmake-integration--get-cmake-configure-presets-from-filename (json-filename)
  "Get the configure presets from the JSON-FILENAME.

Return nil if the file does not exist."
  (when (file-exists-p json-filename)
    ;; Append a vector to nil transforms it into a list
    (append
     (alist-get 'configurePresets (json-read-file json-filename))
     nil)))


(defun cmake-integration--get-cmake-configure-presets-from-filename-2 (json-filename)
  "Get the configure presets from the JSON-FILENAME also considering included presets."
  (when (file-exists-p json-filename)
    ;; The mapcar will turn the list of presets (each preset is an
    ;; alist) into an alist where the key is the preset name. This
    ;; will allow us to use the returned alist as the COLLECTION
    ;; argument of completing-read and to also retrieve information of
    ;; the chosen preset.
    (seq-filter 'cmake-integration--is-preset-visible
                (mapcar
                 (lambda (preset) (cons (alist-get 'name preset) preset))

                 (vconcat
                  (-mapcat
                   (lambda (elem) (cmake-integration--get-cmake-configure-presets-from-filename elem) )
                   (cmake-integration--get-cmake-include-filenames json-filename)))))))


(defun cmake-integration-get-cmake-configure-presets ()
  "Get the configure presets.

Get the configure presets in both 'CMakePresets.json' and
'CMakeUserPresets.json' files."
  (let ((cmake-system-presets-filename (file-name-concat (cmake-integration-get-project-root-folder) "CMakePresets.json"))
        (cmake-user-presets-filename (file-name-concat (cmake-integration-get-project-root-folder) "CMakeUserPresets.json")))

    (append (cmake-integration--get-cmake-configure-presets-from-filename-2 cmake-system-presets-filename)
            (cmake-integration--get-cmake-configure-presets-from-filename-2 cmake-user-presets-filename))))


(defun cmake-integration--get-cmake-configure-with-preset-command (preset)
  "Get the command to configure with CMake using the preset PRESET."
  (format "cd %s && cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON --preset %s"
                                 (cmake-integration-get-project-root-folder)
                                 (cmake-integration--get-preset-name preset)))


(defun cmake-integration--get-cmake-configure-without-preset-command ()
  "Get the command to configure with CMake when presets are not used."
  (if cmake-integration-generator
      ;; case with generator set
      (format "cd %s && cmake -G \"%s\" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s"
              (cmake-integration-get-build-folder)
              cmake-integration-generator
              (cmake-integration-get-project-root-folder))

    ;; case without generator set
    (format "cd %s && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s"
            (cmake-integration-get-build-folder)
            (cmake-integration-get-project-root-folder))))


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


(defun cmake-integration--configure-annotation-function (preset)
  "Annotation function that takes a PRESET and return an annotation for it.

This is used in `cmake-integration-select-configure-preset'
when completing a preset name to generate an annotation for that
preset, which is shown during the completions if you are using
the marginalia package, or in Emacs standard completion buffer."

  (if (equal preset "No Preset")
      (concat (cmake-integration--get-annotation-initial-spaces "No Preset")
              "Don't use any preset."
              (cond
               ((and cmake-integration-build-dir cmake-integration-generator)
                (format "The build folder is '%s' and the generator is '%s'."
                        cmake-integration-build-dir
                        cmake-integration-generator))
               (t (format "The build folder is '%s'."
                          cmake-integration-build-dir))))
    ;; Note that `minibuffer-completion-table' has the list of
    ;; completions currently in use, from which we know PRESET is one
    ;; of them
    (concat (cmake-integration--get-annotation-initial-spaces preset) (alist-get 'displayName (alist-get preset minibuffer-completion-table nil nil 'equal)))))


;;;###autoload
(defun cmake-integration-select-configure-preset ()
  "Select a configure preset for CMake.

A list of preset names if obtained from 'CMakePresets.json' and
'CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion)."
  ;; TODO: Pass a predicate function to completing-read that remove any preset with "hidden" value of true
  (interactive)

  ;; Since we are changing the configure preset, the last target (if
  ;; any) might not be valid anymore. Thus, we unset the current
  ;; target.
  (setq cmake-integration-current-target nil)

  (let ((all-presets
         ;; Add a "No Preset" option to all-presets to allow a user to
         ;; remove the preset and use default build folder
         (append (cmake-integration-get-cmake-configure-presets) '("No Preset")))
        choice)

    (let ((completion-extra-properties '(:annotation-function cmake-integration--configure-annotation-function )))
      (setq choice (completing-read "Build preset: " all-presets nil t))
      )

    ;; If "No Preset" was selected, then we are not using any preset
    ;; and thus cmake-integration-last-configure-preset should be nil
    (if (equal choice "No Preset")
        (setq cmake-integration-last-configure-preset nil)
      (setq cmake-integration-last-configure-preset (alist-get choice all-presets nil nil 'equal))
      )

    ))


;;;###autoload
(defun cmake-integration-cmake-configure-with-preset ()
    "Configure CMake using one of the availeble presets.

A list of preset names if obtained from 'CMakePresets.json' and
'CMakeUserPresets.json', if they exist. Then the user is asked to
choose one of them (with completion) and CMake is configured with
the chosen preset."
  (interactive)
  (cmake-integration-select-configure-preset)
  ;; Configure the project -> This will do the right thing in both
    ;; cases when 'cmake-integration-last-configure-preset' is nil and when it
    ;; has a specific preset
    (cmake-integration-cmake-reconfigure)
  )


;;;###autoload
(defun cmake-integration-cmake-reconfigure ()
  "Call cmake again to re-configure the project.

Note: If no preset is used then
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON' is passed to cmake."
  (interactive)

  (cmake-integration-create-empty-codemodel-file)

  (let ((cmake-command (if cmake-integration-last-configure-preset
                           (cmake-integration--get-cmake-configure-with-preset-command
                            cmake-integration-last-configure-preset)
                         (cmake-integration--get-cmake-configure-without-preset-command)))
        ;; If a prefix argument was passed we will call conan before cmake
        (conan-command (if current-prefix-arg
                           (format "%s && " (cmake-integration-get-conan-run-command))
                         "")))

    (if cmake-integration-include-conan-toolchain-file
        (compile (format "%s%s --toolchain conan_toolchain.cmake" conan-command cmake-command))
      (compile (format "%s%s" conan-command cmake-command))
      )
    )

  ;; Make a link of the compile_commands.json file to the project root
  (let ((compile-commands-file (file-name-concat (cmake-integration-get-build-folder) "compile_commands.json")))
    (when (and cmake-integration-create-compile-commands-link
               (file-exists-p compile-commands-file))
      (make-symbolic-link (file-relative-name compile-commands-file (cmake-integration-get-project-root-folder))  (cmake-integration-get-project-root-folder) t))))


(defun cmake-integration-delete-build-folder ()
  "Delete the current build folder.

It's useful in case you changed the generator, since CMake would
complain in that case."
  (interactive)
  (delete-directory (cmake-integration-get-build-folder) t)
  )


(defun cmake-integration-get-target-executable-filename (&optional target)
  "Get the executable filename for the target TARGET.

The name is relative to the build folder. This is usually
something like just <target-name>, or bin/<target-name>.

Throws an error if the target is not an executable.

If TARGET-NAME is not provided use the last target (saved in a
`cmake-integration-current-target')."

  ;; If both `target' and `cmake-integration-current-target' are nil,
  ;; throw an error asking the UE to select a target first by calling
  ;; `cmake-integration-save-and-compile'
  (unless (or target cmake-integration-current-target)
    (error "Please select a target first `cmake-integration-save-and-compile' first")
    )

  ;; The `target-info' variable inside the `let' has the data from the
  ;; codemodel json file for TARGET-NAME. This data is an alist and
  ;; includes a `jsonFile' field, which has the name of another json
  ;; file with more data about the target. We read this json file and
  ;; save the data in the `target-data' variable. From there we can
  ;; get the executable name from its `artifacts' field.
  (let* ((target (or target cmake-integration-current-target))
         (target-name (car (split-string target cmake-integration--multi-config-separator)))
         (target-info (alist-get
                       target
                       (cmake-integration-get-cmake-targets-from-codemodel-json-file)
                       nil nil 'equal)))

    (unless (cdr target-info)
      (if (member target-name '("all" "clean" "install"))
          (error "Target '%s' is not a valid executable target" target-name)
        (error "Unknown target: '%s'" target-name)))

    (let* ((target-json-file (file-name-concat
                              (cmake-integration-get-reply-folder)
                              (alist-get 'jsonFile target-info)))
           (target-data (json-read-file target-json-file)))

      (unless (equal (alist-get 'type target-data) "EXECUTABLE")
        (error "Target '%s' is not an executable" target-name))

      ;; Note that target-artifacts is a vector, but with a single
      ;; element in our case
      (let ((target-artifacts (alist-get 'artifacts target-data)))
        ;; We assume the vector has just one element
        (alist-get 'path (elt target-artifacts 0))))))


(defun check-if-build-folder-exists-and-throws-if-not ()
  "Check that the build folder exists and throws an error if not."
  (unless (file-exists-p (cmake-integration-get-build-folder))
    (error "The build folder is missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset' to configure the project")))


;;;###autoload
(defun cmake-integration-save-and-compile-no-completion (target)
  "Save the buffer and compile TARGET."
  (interactive "sTarget: ")
  (save-buffer 0)

  (check-if-build-folder-exists-and-throws-if-not)

  (setq cmake-integration-current-target target)
  (let ((compile-command (cmake-integration-get-compile-command target)))
    (compile compile-command)))


(defun cmake-integration--get-target-type-from-name (target-name all-targets)
  "Get the type of the target with name TARGET-NAME from ALL-TARGETS.
ALL-TARGETS is an alist like the one returned by
`cmake-integration-get-cmake-targets-from-codemodel-json-file'."
  (let ((target (alist-get target-name all-targets nil nil 'equal)))
    ;; (cmake-integration--get-target-type target)
    (alist-get 'type target)
    ))


(defun cmake-integration--target-annotation-function (target-name)
  "Annotation function that takes a TARGET-NAME and return an annotation for it.

This is used in `cmake-integration--get-target-using-completions'
when completing a target name to generate an annotation for that
target, which is shown during the completions if you are using
the marginalia package, or in Emacs standard completion buffer."
  (pcase (car (split-string target-name cmake-integration--multi-config-separator))
    ("clean" (concat (cmake-integration--get-annotation-initial-spaces target-name) "Clean all compiled targets"))
    ("all" (concat (cmake-integration--get-annotation-initial-spaces target-name) "Compile all targets"))
    (_ (concat (cmake-integration--get-annotation-initial-spaces target-name) (cmake-integration--get-target-type-from-name target-name minibuffer-completion-table)))
    ))



(defun cmake-integration--get-target-using-completions (list-of-targets)
  "Ask the user to choose one of the targets in LIST-OF-TARGETS using completions."
  (let ((completion-extra-properties '(:annotation-function cmake-integration--target-annotation-function)))
    (completing-read "Target: " list-of-targets nil t)))


(defun cmake-integration--get-all-targets (json-filename)
  "Get all targets for completion specified in JSON-FILENAME.

Get the name of all targets for completion, respecting the value
of the `*-targets-during-completion' variables.

If a prefix argument is provided, then the value of
`cmake-integration-include-subproject-targets-during-completion'
will be ignored."
  (let* ((include-subprojects (or current-prefix-arg cmake-integration-include-subproject-targets-during-completion))
         (list-of-targets (if include-subprojects
                              (cmake-integration-get-cmake-targets-from-codemodel-json-file-2
                               json-filename)
                            (cmake-integration-get-cmake-targets-from-codemodel-json-file-2
                             json-filename
                             'cmake-integration--target-is-in-projectIndex0-p))))

    ;; Filter the list of targets
    (cond
     ;; Do not include utility and library targets
     ((and cmake-integration-hide-utility-targets-during-completion
           cmake-integration-hide-library-targets-during-completion)
      (seq-filter '(lambda (target) (and
                                (cmake-integration--target-is-not-utility-p target)
                                (cmake-integration--target-is-not-library-p target)))
                  list-of-targets))

     ;; Do not include only utility targets
     (cmake-integration-hide-utility-targets-during-completion
      ;; Do not include utility targets
      (seq-filter 'cmake-integration--target-is-not-utility-p list-of-targets))

     ; Do not include only library targets
     (cmake-integration-hide-library-targets-during-completion
      (seq-filter 'cmake-integration--target-is-not-library-p list-of-targets))

     ;; Include all targets
     (t list-of-targets))))


;;;###autoload
(defun cmake-integration-save-and-compile ()
  "Ask for a target name and compile it.

A list of target names is obtained from the project using CMake's
file API and completion is used to choose the desired target. If
that is not possible, ask for the target name without
completions."

  (interactive)
  ;; If the build folder is missing we should stop with an error
  (check-if-build-folder-exists-and-throws-if-not)

  (if-let* ((json-filename (cmake-integration-get-codemodel-reply-json-filename))
            ;; The list of targets also includes the convenience
            ;; targets 'all', 'clean' and optional 'install' target
            (list-of-targets (cmake-integration--get-all-targets json-filename))
            (chosen-target (cmake-integration--get-target-using-completions list-of-targets)))
      (cmake-integration-save-and-compile-no-completion chosen-target)

    (unless json-filename
      ;; If `json-filename' is nil that means we could not find the
      ;; CMake reply with the file API, which means the query file is
      ;; missing. All we need to do is to configure using either
      ;; `cmake-integration-cmake-reconfigure' or
      ;; `cmake-integration-cmake-configure-with-preset', which created
      ;; the query file.
      (display-warning 'cmake-integration "Could not find list of targets due to CMake file API file
missing. Please run either `cmake-integration-cmake-reconfigure' or
`cmake-integration-cmake-configure-with-preset'."))

    (command-execute 'cmake-integration-save-and-compile-no-completion)))


;;;###autoload
(defun cmake-integration-save-and-compile-last-target ()
  "Recompile the last target that was compiled (or `all')."
  (interactive)
  (cmake-integration-save-and-compile-no-completion
   (or cmake-integration-current-target "all")))


(defun cmake-integration--get-working-directory (executable-filename)
  "Get the working directory for to run EXECUTABLE-FILENAME."
  (pcase cmake-integration-run-working-directory
    ('root (cmake-integration-get-project-root-folder))
    ('build (cmake-integration-get-build-folder))
    ('bin (file-name-concat (cmake-integration-get-build-folder) (file-name-directory executable-filename)))
    (_ (file-name-concat (cmake-integration-get-project-root-folder) cmake-integration-run-working-directory)))
  )


(defun cmake-integration-get-target-executable-full-path (executable-filename)
  "Get the full path of EXECUTABLE-FILENAME."
  (file-name-concat (cmake-integration-get-build-folder) executable-filename))


(defun cmake-integration--get-run-command-project-root-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the project root folder."
  (format "cd %s && %s %s"
          (cmake-integration--get-working-directory executable-filename)
          (cmake-integration-get-target-executable-full-path executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-build-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the build folder."
  (format "cd %s && %s %s"
          (cmake-integration--get-working-directory executable-filename)
          executable-filename
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-bin-folder-cwd (executable-filename)
  "Get the run command for EXECUTABLE-FILENAME from the binary folder.

The binary folder is the folder containing the executable."
  (format "cd %s && ./%s %s"
          (cmake-integration--get-working-directory executable-filename)
          (file-name-nondirectory executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command-custom-cwd (executable-filename project-subfolder)
  "Get the correct run command EXECUTABLE-FILENAME from a PROJECT-SUBFOLDER."
  (cl-assert (stringp project-subfolder))
  (format "cd %s && %s %s"
          (cmake-integration--get-working-directory executable-filename)
          (cmake-integration-get-target-executable-full-path executable-filename)
          cmake-integration-current-target-run-arguments))


(defun cmake-integration--get-run-command (executable-filename)
  "Get the correct run command for EXECUTABLE-FILENAME.

Get the correct run command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable."
  (pcase cmake-integration-run-working-directory
    ('root (cmake-integration--get-run-command-project-root-cwd executable-filename))
    ('build (cmake-integration--get-run-command-build-folder-cwd executable-filename))
    ('bin (cmake-integration--get-run-command-bin-folder-cwd executable-filename))
    (_ (cmake-integration--get-run-command-custom-cwd executable-filename cmake-integration-run-working-directory))))


;;;###autoload
(defun cmake-integration-run-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  ;; Run the target
  (compile (cmake-integration--get-run-command (cmake-integration-get-target-executable-filename))))


(defun cmake-integration--get-debug-command (executable-filename)
  "Get the correct debug command for EXECUTABLE-FILENAME.

Get the correct debug command for EXECUTABLE-FILENAME respecting
the value of the `cmake-integration-run-working-directory'
variable. This should be passed to gdb command in Emacs."
  (let ((cwd (cmake-integration--get-working-directory executable-filename)))
    (format
     "gdb -i=mi --cd=%s --args %s %s"
     cwd
     (cmake-integration-get-target-executable-full-path executable-filename)
     cmake-integration-current-target-run-arguments)))


(defun cmake-integration--launch-gdb-with-last-target ()
  "Launch gdb inside Emacs to debug the last target."
  (gdb (cmake-integration--get-debug-command (cmake-integration-get-target-executable-filename))))


(declare-function dap-debug "dap-mode")

(defun cmake-integration--launch-dap-debug-cpptools-last-target ()
  "Launch `dap-debug' using cpptools to debug the last target."
  (require 'dap-mode)
  (let ((executable-filename (cmake-integration-get-target-executable-filename)))

    (let ((program-path (expand-file-name (cmake-integration-get-target-executable-full-path executable-filename)))
          (cwd (expand-file-name (cmake-integration--get-working-directory executable-filename))))

      (dap-debug (list :type "cppdbg"
                       :request "launch"
                       :name "cmake-integration-target"
                       :MIMode "gdb"
                       :program program-path
                       :arguments cmake-integration-current-target-run-arguments
                       :cwd cwd)))))


;;;###autoload
(defun cmake-integration-debug-last-target ()
  "Run the last compiled target."
  (interactive)
  (check-if-build-folder-exists-and-throws-if-not)

  (if cmake-integration-use-dap-for-debug
      (cmake-integration--launch-dap-debug-cpptools-last-target)
    ;; Run the target
    (cmake-integration--launch-gdb-with-last-target)))


;;;###autoload
(defun cmake-integration-run-last-target-with-arguments (run-arguments)
  "Run the last compiled target passing RUN-ARGUMENTS as arguments."
  (interactive "sArguments: ")
  (setq cmake-integration-current-target-run-arguments run-arguments)
  (cmake-integration-run-last-target))


(defun cmake-integration-get-parent-preset-name (preset)
  "Get the name in the `inherits' field of the preset PRESET."
  (alist-get 'inherits preset))


(defun cmake-integration--get-preset-by-name (preset-name)
  "Get the preset from the list of presets with name PRESET-NAME."
  (alist-get
   preset-name
   (cmake-integration-get-cmake-configure-presets)
   nil nil 'equal))


(defun cmake-integration-get-parent-preset (preset)
  "Get the parent preset of PRESET.

If the 'inherits' field in the presets file is a single string, then a
single preset is returned. If the 'inherits' is an array of names, then
a vector of presets is returned."
  (let ((parent-name (cmake-integration-get-parent-preset-name preset)))
    (if (vectorp parent-name)
        (let ((presets (cl-coerce (remq nil (mapcar 'cmake-integration--get-preset-by-name parent-name)) 'vector)))
          (if (seq-empty-p presets)
              nil
            presets))
      (cmake-integration--get-preset-by-name parent-name))))


(defun cmake-integration-get-binaryDir (preset)
  "Get the `binaryDir' field of a preset PRESET.

If not available, get the binaryDir or a parent preset."
  (when preset
    (if-let* ((binary-dir (alist-get 'binaryDir preset)))
        binary-dir
      (let ((parents (cmake-integration-get-parent-preset preset)))
        (if (vectorp parents)
            (let ((binaryDirs (mapcar 'cmake-integration-get-binaryDir parents)))
              (seq-find 'stringp binaryDirs))
          (cmake-integration-get-binaryDir parents))))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxx Functions for conan integration xxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(defun cmake-integration--get-conan-run-command (&optional profile)
  "Get the command to run `conan install' using PROFILE.

The command is run from the build folder of the current cmake
configuration."
  (if profile
      (format "%s --profile %s" (cmake-integration--get-conan-run-command) profile)
    (format "cd %s && conan install %s %s"
          (cmake-integration-get-build-folder)
          (cmake-integration-get-project-root-folder)
          cmake-integration-conan-arguments)))


(defun cmake-integration-get-conan-run-command ()
  "Get the command to run `conan install'.

The command is run from the build folder of the current cmake
configuration and the profile passed to conan is taken from the
`cmake-integration-conan-profile' variable."
  (if cmake-integration-conan-profile
      ;; If cmake-integration-conan-profile is set, it can be either a string with a single profile, or an alist mapping cmake profile names to conan profile names
      (if (stringp cmake-integration-conan-profile)
          (cmake-integration--get-conan-run-command cmake-integration-conan-profile)
        ;; Map cmake profile name to conan profile name
        (let* ((cmake-profile-name (alist-get 'name cmake-integration-last-configure-preset))
               (conan-profile-name (alist-get cmake-profile-name cmake-integration-conan-profile nil nil 'equal)))
          ;; Note that if we have no conan profile name in the alist
          ;; for the current cmake profile nane, then
          ;; `conan-profile-name' is nil and no profile will be used
          (cmake-integration--get-conan-run-command conan-profile-name)))

    ;; If cmake-integration-conan-profile is not set we get the conan command without using a profile
    (cmake-integration--get-conan-run-command nil)))


;;;###autoload
(defun cmake-integration-run-conan ()
  "Run conan install in the current build folder."
  (compile (cmake-integration-get-conan-run-command)))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxx Doxygen xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(defun cmake-integration--get-docs-folder ()
  "Get the folder with the Doxyfile."
  (s-replace "${sourceDir}/" (cmake-integration-get-project-root-folder) cmake-integration-docs-folder)
  )

(defun cmake-integration-generate-project-documentation ( )
  "Generate the documentation in a cmake based project using Doxygen.

This assume that there is a `doc' folder in the project root,
from where the doxygen command will be run."
  (interactive)
  (let ((doxygen-command (format "cd %s && doxygen" (cmake-integration--get-docs-folder))))
    (compile doxygen-command)
    )
  )


(defun cmake-integration-view-project-documentation ()
  "Open generated doxygen documentation."
  (interactive)
  (browse-url (file-name-concat (expand-file-name (cmake-integration--get-docs-folder)) "html/index.html"))
  )

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(provide 'cmake-integration)

;;; cmake-integration.el ends here
