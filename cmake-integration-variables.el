;;; cmake-integration-variables.el --- Custom variables and constants -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

;; Tell the byte compile that
;; cmake-integration--run-working-directory-p is defined in the
;; "-core" file
(declare-function cmake-integration--run-working-directory-p "cmake-integration-core.el")

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

If you are using the `CMakeToolchain' generator, set this to true
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

(provide 'cmake-integration-variables)

;;; cmake-integration-variables.el ends here
