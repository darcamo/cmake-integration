;;; cmake-integration-variables.el --- Custom variables and constants -*- lexical-binding: t -*-

;; Copyright (C) 2025 Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs

;; This file is part of cmake-integration.

;; cmake-integration is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cmake-integration is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cmake-integration. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines most of the variables used in the cmake-integration
;; package.

;;; Code:

(declare-function ci-default-program-launch-function "cmake-integration-launch-functions")
(declare-function ci-eshell-program-launch-function "cmake-integration-launch-functions")
(declare-function ci-default-debug-launch-function "cmake-integration-launch-functions")
(declare-function ci-dape-debug-launch-function "cmake-integration-launch-functions")

(defgroup cmake-integration nil "Easily call cmake configure and run compiled targets." :group 'tools :prefix "cmake-integration-")

(defgroup cmake-integration-completions nil "Customize completions." :group 'cmake-integration :prefix "cmake-integration-")

(defgroup cmake-integration-faces nil "Faces defined in cmake-integration." :group 'cmake-integration :prefix "cmake-integration-")

(defgroup cmake-integration-conan nil "Conan related variables for cmake-integration." :group 'cmake-integration :prefix "cmake-integration-")

(defgroup cmake-integration-persistence nil "Persist state of cmake-integration for the project." :group 'cmake-integration :prefix "cmake-integration-")

(defgroup cmake-integration-project nil "Minor mode for CMake project." :group 'cmake-integration :prefix "cmake-integration-")


(defcustom ci-build-dir "build"
  "The build folder to use when presets are not used.

If this is nil, then using presets is required. If this is a relative
path, it is considered relative to the project root."
  :type
  '(choice
    (const
     :tag "Don't specity the build folder (requires using presets)" nil)
    (directory :tag "Specity the build folder (can be relative to project root)"))
  :group 'cmake-integration)


(defcustom ci-install-prefix nil
  "The install preffix to use when running cmake install.

If this is nil, then no install preffix is used."
  :type '(choice
          (const :tag "Don't set install prefix")
          (directory :tag "Specify the install prefix")) :group 'cmake-integration)


(defcustom ci-generator nil
  "The generator to pass to cmake when no presets are used.

If this is nil, then the generator is not explicitly set." :type '(string) :group 'cmake-integration)


(defcustom ci-annotation-column 30
  "Column where annotations should start during completion." :type '(integer) :group 'cmake-integration-completions)


(defcustom ci-include-subproject-targets-during-completion t
  "If t, include subproject targets when presenting target names for completion.

When t (default) all targets are included during completion of
target names. If nil, then only targets from the main cmake
project are included (targets with projectIndex equal to zero)."
  :type 'boolean :safe #'booleanp :group 'cmake-integration-completions)


(defcustom ci-hide-utility-targets-during-completion nil
  "If t, then utility targets are not included during completion."
  :type 'boolean :safe #'booleanp :group 'cmake-integration-completions)


(defcustom ci-hide-library-targets-during-completion nil
  "If t, then library targets are not included during completion."
  :type 'boolean :safe #'booleanp :group 'cmake-integration-completions)


;; TODO Allow the values 't', 'local-only' and 'nil'. With local-only, it should
;; not annotate in tramp connections.
(defcustom ci-annotate-targets t
  "Annotate the targets in the list of targets with the target type"
  :type 'boolean
  :safe #'booleanp
  :group 'cmake-integration-completions)


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
the project root."
  :type
  '(radio
    (const :tag "Binary folder" bin)
    (const :tag "Build folder" build)
    (const :tag "Project root folder" root)
    (directory :tag "Custom subfolder of project root"))
  :group 'cmake-integration
  :safe #'ci--run-working-directory-p
  :local t)


(defcustom ci-program-launcher-function 'compilation
  "Function or method to launch a program.

This variable can be set to one of the following values:

- `compilation': Use `compile` to run the program (does not allow input
  interaction).
- `comint': Use a comint buffer to run the program (allows input
  interaction).
- `eshell': Use Eshell to run the program.
- A function: Use a custom function to define how the program is run.
  The function should take one argument, the program command, and handle
  the invocation.

Customize this variable to control how programs are launched."
  :type
  '(radio
    (const :tag "Compilation Buffer" compilation)
    (const :tag "Comint Buffer" comint)
    (const :tag "Eshell" eshell)
    (function :tag "Custom Function"))
  :group 'cmake-integration)


(defcustom ci-debug-launcher-function 'classic-gdb
  "Function or method to launch a debugger.

This variable can be set to one of the following values:
- classic-gdb: Use the `gdb' command in Emacs to debug the target.
- dape: Use `dape' package to debug the target using gdb's Debugger
  Adapter Protocol support.
- Custom function: Use a custom function to define how the debugger is
  invoked. The function should take three parameters: the program to be
  executed in the debugger, a string with the command line arguments to
  pass to to the program, and the working directory to be used.

Customize this variable to control how the debugger is launched."
  :type
  '(choice
    (const :tag "Classic GDB" classic-gdb)
    (const :tag "DAPE (with gdb's native DAP support)" dape)
    (function :tag "Custom Function"))
  :group 'cmake-integration)


(defcustom ci-conan-arguments "--build missing" "Extra arguments to pass to conan." :type '(string) :group 'cmake-integration-conan)

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
  :group 'cmake-integration-conan)


(defcustom ci-include-conan-toolchain-file nil
  "If t, pass '--toolchain conan_toolchain.cmake' to cmake.

If you are using the `CMakeToolchain' generator, set this to true
in a directory local variable in your project."
  :type 'boolean :safe #'booleanp :group 'cmake-integration-conan)


(defcustom ci-docs-folder "${sourceDir}/docs"
  "Folder containing the Doxyfile.

If \"${sourceDir}/\" is in `cmake-integration-docs-folder' it
will be replaced by the project root." :type 'directory :group 'cmake-integration)


(defcustom ci-use-separated-compilation-buffer-for-each-target nil
  "If t, use a separate compilation buffer for each target."
  :type 'boolean :safe #'booleanp :group 'cmake-integration)


(defcustom ci-persist-location 'user-directory
  "Location where cmake-integration state is serialized."
  :type
  '(choice
    (const :tag "User Emacs Directory" user-directory)
    (const :tag "Project Directory" project-directory)
    (directory :tag "Custom location"))
  :group 'cmake-integration-persistence)


(defvar ci-current-target nil "Name of the target that will be compiled and run.")


(defvar ci--target-extra-data-cache (make-hash-table :test 'equal)
  "Cache for target types. Maps target names to their types.")


(defvar ci--build-folder-cache nil
  "Cache for the build folder path (when presets are used).")


(defvar ci-run-arguments "" "Command line arguments when running a target.")


;; This an alist like
;;   ((name . "preset-name") (displayName . "Preset name shown in annotation")
;;    (description . "...")
;;    (generator . "Ninja")
;;    (cacheVariables (CMAKE_POLICY_DEFAULT_CMP0091 . "NEW"))
;;    (toolchainFile . "generators/conan_toolchain.cmake")
;;    (binaryDir . "/some/path/to/build/folder"))
(defvar ci-configure-preset nil "Preset used for the configure step.")


(defvar ci-cache-variables nil "Cache variables to set in the command line during configure.")


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


(defface ci-phony-target-face '((t . (:inherit font-lock-constant-face)))
  "Face used to annotate the phony targets (all, clean and install)."
  :group 'cmake-integration-faces)

(defface ci-executable-target-face '((t . (:inherit font-lock-function-name-face)))
  "Face used to annotate the executable targets."
  :group 'cmake-integration-faces)

(defface ci-library-target-face '((t . (:inherit font-lock-comment-face)))
  "Face used to annotate the library targets."
  :group 'cmake-integration-faces)

(defface ci-utility-target-face '((t . (:inherit font-lock-builtin-face)))
  "Face used to annotate the utility targets."
  :group 'cmake-integration-faces)

(defface ci-unknown-target-face '((t . (:inherit font-lock-warning-face)))
  "Face used to annotate targets without type information.

See `cmake-integration-annotate-targets'."
  :group 'cmake-integration-faces)


(provide 'cmake-integration-variables)

;;; cmake-integration-variables.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
