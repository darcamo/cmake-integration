;;; cmake-integration-variables.el --- Custom variables and constants -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function ci-default-program-launch-function "cmake-integration-launch")
(declare-function ci-eshell-program-launch-function "cmake-integration-launch")

(defgroup cmake-integration nil "Easily call cmake configure and run compiled targets." :group 'tools :prefix "cmake-integration-")


(defcustom ci-build-dir "build"
  "The build folder to use when no presets are used.

If this is nil, then using presets is required." :type '(string) :group 'cmake-integration)


(defcustom ci-install-prefix nil
  "The install preffix to use when running cmake install.

If this is nil, then no install preffix is used." :type '(string) :group 'cmake-integration)


(defcustom ci-generator nil
  "The generator to pass to cmake when no presets are used.

If this is nil, then the generator is not explicitly set." :type '(string) :group 'cmake-integration)


(defcustom ci-annotation-column 30
  "Column where annotations should start during completion." :type '(integer) :group 'cmake-integration)


(defcustom ci-include-subproject-targets-during-completion t
  "If t, include subproject targets when presenting target names for completion.

When t (default) all targets are included during completion of
target names. If nil, then only targets from the main cmake
project are included (targets with projectIndex equal to zero)."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom ci-hide-utility-targets-during-completion nil
  "If t, then utility targets are not included during completion."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom ci-hide-library-targets-during-completion nil
  "If t, then library targets are not included during completion."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


;; Tell the byte compile that
;; cmake-integration--run-working-directory-p is defined in the
;; "-core" file
(declare-function ci--run-working-directory-p "cmake-integration-core.el")


(defcustom ci-run-working-directory 'bin
  "Working directory when running a target executable.

Possible values are the symbols `bin' (to run from the folder
containing the executable), `build' (to run from the build folder)
and `root' (to run from the project root), as well as any string.
In the case of a string, it should match an existing subfolder of
the project root." :type '(choice symbol string)
  :group 'cmake-integration
  :safe #'ci--run-working-directory-p
  :local t)


(defcustom ci-program-launcher #'ci-default-program-launch-function
  "Stores the function that is used to launch the current target.

It takes two arguments:

1. COMMAND: A string representing the shell command to execute the
   current target, including any command-line arguments.
2. BUFFER-NAME: The name to be used for the buffer displaying the
   running program. This could be nil, which indicates a default name
   should be used.

Built-in functions are:
- `cmake-integration-default-program-launch-function': Uses
  `compile' to run the command in a compilation buffer.
- `cmake-integration-comint-program-launch-function': Uses `compile'
  with the comint argument set to t to run the command in a comint
  buffer. Also switch to that buffer.
- `cmake-integration-eshell-program-launch-function': Uses eshell to run
  the command in a eshell buffer."
  :type 'function
  :group 'cmake-integration)


(defcustom ci-create-compile-commands-link t
  "If t, make a link of `compile_commands.json' to the project root.

This helps lsp and clangd correctly parsing the project files."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom ci-use-dap-for-debug nil
  "If t, use `dap-mode' with cpptools for debug.

If nil, use standard gdb graphical interface (see Emacs manual)."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom ci-conan-arguments "--build missing" "Extra arguments to pass to conan." :type '(string) :group 'cmake-integration)

;; TODO: Investigate if it is possible to get completions for the conan and cmake profiles in the custom interface
(defcustom ci-conan-profile nil
  "Conan profile to use, or an alist mapping cmake profiles to conan profiles."
  :type '(choice
          (const :tag "Don't use any Conan profile" nil)
          (string :tag "Use a single Conan profile")
          (alist :tag "Map a CMake profile into a Conan profile"
                 :key-type (string :tag "Cmake profile")
                 :value-type (string :tag "Conan profile")))
  :safe #'listp
  :group 'cmake-integration)


(defcustom ci-include-conan-toolchain-file nil
  "If t, pass '--toolchain conan_toolchain.cmake' to cmake.

If you are using the `CMakeToolchain' generator, set this to true
in a directory local variable in your project."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom ci-docs-folder "${sourceDir}/docs"
  "Folder containing the Doxyfile.

If \"${sourceDir}/\" is in `cmake-integration-docs-folder' it
will be replaced by the project root." :type 'string :group 'cmake-integration)


(defcustom ci-use-separated-compilation-buffer-for-each-target nil
  "If t, use a separate compilation buffer for each target."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defvar ci-current-target nil "Name of the target that will be compiled and run.")


(defvar ci-run-arguments "" "Command line arguments when running a target.")


;; This an alist like
;;   ((name . "preset-name") (displayName . "Preset name shown in annotation")
;;    (description . "...")
;;    (generator . "Ninja")
;;    (cacheVariables (CMAKE_POLICY_DEFAULT_CMP0091 . "NEW"))
;;    (toolchainFile . "generators/conan_toolchain.cmake")
;;    (binaryDir . "/some/path/to/build/folder"))
(defvar ci-configure-preset nil "Preset used for the configure step.")


(defvar ci-build-preset nil "Preset used for the build step.")


(defvar ci-test-preset nil "Preset used for the test step.")


(defvar ci-package-preset nil "Preset used for the package step.")


(defconst ci--multi-config-separator "/"
  "Character used to separate target name from config name.

In case of of multi-config generators, target names have the
special form <target-name><sep><config-name> (e.g. `all/Debug'
with `/' as configured separator).

Note: The selected separator shall be a character that it is not
a valid component of a CMake target name (see
https://cmake.org/cmake/help/latest/policy/CMP0037.html ).")


(defvar ci--ctest-label-include-regexp nil
  "Regexp to pass to ctest for label inclusion.

This is a list of strings, each one representing a label to include.")


(defvar ci--ctest-label-exclude-regexp nil
  "Regexp to pass to ctest for label exclusion.

This is a list of strings, each one representing a label to include.")


(provide 'cmake-integration-variables)

;;; cmake-integration-variables.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
