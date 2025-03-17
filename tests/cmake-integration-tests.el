;;; tests.el --- Tests for the cmake-integration library  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darlan@darlan-notebook>
;; Keywords: keywords

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Tests for the cmake-integration package.

;;; Code:
(require 'ert)
(require 'cmake-integration)


;; TODO: Test cmake-integration-get-target-executable-filename


;; NOTE: Trailling backslash matters
(defun filepath-equal-p (filepath1 filepath2)
  "Check that two file paths FILEPATH1 and FILEPATH2 are equal.

This is similar to `file-equal-p', but does not require
file/folder to exist."
  (equal (expand-file-name filepath1)
         (expand-file-name filepath2))
  )


(defun test-fixture-setup (subfolder body)
  "Run BODY with 'default-directory' set to SUBFOLDER.

This is used in the tests definitions to make sure we run the
test code from inside a 'test project'."
  (let ((default-directory (expand-file-name subfolder))
        (cmake-integration-configure-preset nil))
    (funcall body)))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxx Define the tests xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

(ert-deftest test-cmake-integration--get-system-presets-file ()
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (let* ((project-root-folder (cmake-integration--get-project-root-folder))
            (expected-system-preset (file-name-concat project-root-folder "CMakePresets.json")))
       (should (equal (cmake-integration--get-system-presets-file) expected-system-preset))))))


(ert-deftest test-cmake-integration--get-user-presets-file ()
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (let* ((project-root-folder (cmake-integration--get-project-root-folder))
            (expected-user-preset (file-name-concat project-root-folder "CMakeUserPresets.json")))
       (should (equal (cmake-integration--get-user-presets-file) expected-user-preset))))))


(ert-deftest test-cmake-integration--expand-included-presets ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((filenames (cmake-integration--expand-included-presets "CMakePresets.json")))
       (should (equal filenames '("CMakePresets.json"))))))

  (test-fixture-setup
   "./test-project-with-presets-with-includes"
   (lambda ()
     (let ((filenames (cmake-integration--expand-included-presets "CMakePresets.json")))
       (should (equal filenames '("subfolder2/MorePresets-Extra.json""MorePresets.json" "subfolder/EvenMorePresets.json" "CMakePresets.json")))))))


(ert-deftest test-cmake-integration--get-all-preset-files ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((all-files (cmake-integration--get-all-preset-files))
           ;; There are no preset files in test-project
           (expected-preset-files '()))
       (should (equal all-files expected-preset-files)))))

  (test-fixture-setup
   "./test-project-with-presets/"
   (lambda ()
     (let ((all-files (cmake-integration--get-all-preset-files))
           ;; Theres only the system preset, and it does not include other presets
           (expected-preset-files (list (cmake-integration--get-system-presets-file))))
       (should (equal all-files expected-preset-files)))))

  (test-fixture-setup
   "./test-project-with-presets-with-includes/"
   (lambda ()
     (let* ((all-files (cmake-integration--get-all-preset-files))
            ;; Theres only the system preset, but it include other presets (which include other preset)
            (expected-preset-files (list
                                    (expand-file-name "./subfolder2/MorePresets-Extra.json")
                                    (expand-file-name "./MorePresets.json")
                                    (expand-file-name "./subfolder/EvenMorePresets.json")
                                    (expand-file-name "./CMakePresets.json"))))

       (should (equal (length all-files) (length expected-preset-files)))
       ;; Iterate over both lists (all-files and expected-preset-files) and compare each the elements in them
       (cl-mapc (lambda (obtained expected) (should (filepath-equal-p expected obtained)) ) all-files expected-preset-files)))))


(ert-deftest test-cmake-integration--create-target-fullname ()
  (should (equal (cmake-integration--create-target-fullname "target" "config") "target/config"))
  (let ((cmake-integration--multi-config-separator "|"))
    (should (equal (cmake-integration--create-target-fullname "target" "config") "target|config"))))


;; NOTE: For this test to pass a folder with a `.project' file must be recognized as a project.
;; See https://www.reddit.com/r/emacs/comments/k3og7d/extending_projectel/
(ert-deftest test-cmake-integration--get-project-root-folder ()
  "Test getting the project root when no presets are used."
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (should (file-equal-p (cmake-integration--get-project-root-folder)
                           "..")))))


(ert-deftest test-cmake-integration--get-preset-by-name ()
  (let* ((preset1 '((name . "preset1") (displayName . "The first preset")))
         (preset2 '((name . "preset2") (displayName . "The second preset")))
         (list-of-presets (list preset1 preset2))
         )
    (should (equal (cmake-integration--get-preset-by-name "preset1" list-of-presets) preset1))
    (should (equal (cmake-integration--get-preset-by-name "preset2" list-of-presets) preset2))))


(ert-deftest test-cmake-integration--get-configure-parent-preset ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     ;; If there is no parent, return nil
     (let* ((preset (cmake-integration--get-configure-preset-by-name "default"))
            (parent-preset (cmake-integration--get-configure-parent-preset preset)))
       (should-not parent-preset))

     ;; When there is a single parent, return it
     (let* ((preset (cmake-integration--get-configure-preset-by-name "ninjamulticonfig"))
            (parent-preset (cmake-integration--get-configure-parent-preset preset))
            (parent-name (cmake-integration--get-preset-name parent-preset)))
       (should (string-equal parent-name "default")))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "ninjamulticonfig2"))
            (parent-presets (cmake-integration--get-configure-parent-preset preset))
            (parent-names (mapcar 'cmake-integration--get-preset-name parent-presets))
            )
       (should (vectorp parent-presets))
       (should (equal parent-names '("default" "Dummy")))))))


(ert-deftest test-cmake-integration--perform-binaryDir-replacements ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((project-root-folder "~/some_path/my-project/")
           (preset-name "my-preset"))

       (let ((binaryDir "~/some-folder/some-subfolder"))
         (should (filepath-equal-p
                  (cmake-integration--perform-binaryDir-replacements binaryDir project-root-folder preset-name)
                  "~/some-folder/some-subfolder")))

       (let ((binaryDir "${sourceDir}/build/${presetName}/"))
         (should (filepath-equal-p
                  (cmake-integration--perform-binaryDir-replacements binaryDir project-root-folder preset-name)
                  "~/some_path/my-project/build/my-preset/")))))))


(ert-deftest test-cmake-integration--get-binaryDir ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((preset (cmake-integration--get-configure-preset-by-name "Dummy")))
       (should-not (cmake-integration--get-binaryDir preset)))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "default"))
            (binaryDir (cmake-integration--get-binaryDir preset))
            (expected-binaryDir "${sourceDir}/build/${presetName}/"))
       (should (filepath-equal-p binaryDir expected-binaryDir)))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "ninjamulticonfig"))
            (binaryDir (cmake-integration--get-binaryDir preset))
            (expected-binaryDir "${sourceDir}/build/${presetName}/"))
       (should (filepath-equal-p binaryDir expected-binaryDir)))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "ninjamulticonfig2"))
            (binaryDir (cmake-integration--get-binaryDir preset))
            (expected-binaryDir "${sourceDir}/build/${presetName}/"))
       (should (filepath-equal-p binaryDir expected-binaryDir)))
     )))


(ert-deftest test-cmake-integration--get-binaryDir-with-replacements ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((preset (cmake-integration--get-configure-preset-by-name "Dummy")))
       (should-not (cmake-integration--get-binaryDir-with-replacements preset)))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "default"))
            (binaryDir (cmake-integration--get-binaryDir-with-replacements preset))
            (expected-binaryDir "~/.emacs.d/elpaca/repos/cmake-integration/tests/test-project-with-presets/build/default/"))
       (should (filepath-equal-p binaryDir expected-binaryDir)))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "ninjamulticonfig"))
            (binaryDir (cmake-integration--get-binaryDir-with-replacements preset))
            (expected-binaryDir "~/.emacs.d/elpaca/repos/cmake-integration/tests/test-project-with-presets/build/ninjamulticonfig/"))
       (should (filepath-equal-p binaryDir expected-binaryDir)))

     (let* ((preset (cmake-integration--get-configure-preset-by-name "ninjamulticonfig2"))
            (binaryDir (cmake-integration--get-binaryDir-with-replacements preset))
            (expected-binaryDir "~/.emacs.d/elpaca/repos/cmake-integration/tests/test-project-with-presets/build/ninjamulticonfig2/"))
       (should (filepath-equal-p binaryDir expected-binaryDir)))
     )))


(ert-deftest test-cmake-integration-get-build-folder-without-presets ()
  (test-fixture-setup
   "./test-project" ;; project root is the parent "test-project" folder
   (lambda ()
     ;; Without setting `cmake-integration-build-dir' -> default value is "build"
     (should (filepath-equal-p (cmake-integration-get-build-folder) "./build"))

     ;; Set `cmake-integration-build-dir'
     (let ((cmake-integration-build-dir "some-build-folder"))
       (should (filepath-equal-p (cmake-integration-get-build-folder) "./some-build-folder"))
       ))))


(ert-deftest test-cmake-integration-get-build-folder-with-presets ()
  (test-fixture-setup
   "./test-project-with-presets" ;; project root is the parent "test-project" folder
   (lambda ()
     ;; Build folder is taken from the `binaryDir' field in
     ;; `cmake-integration-configure-preset', which is an alist
     (let ((cmake-integration-configure-preset '((binaryDir . "${sourceDir}/build-with-ninja-multiconfig"))))
       (should (filepath-equal-p (cmake-integration-get-build-folder) "./build-with-ninja-multiconfig")))

     (let ((cmake-integration-configure-preset '((binaryDir . "${sourceDir}/build-with-ninja"))))
       (should (filepath-equal-p (cmake-integration-get-build-folder) "./build-with-ninja"))))))



(ert-deftest test-cmake-integration--get-query-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration--get-query-folder) "./build/.cmake/api/v1/query/client-emacs")))))


(ert-deftest test-cmake-integration--get-reply-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration--get-reply-folder) "./build/.cmake/api/v1/reply/")))))


(ert-deftest test-cmake-integration--get-path-of-codemodel-query-file ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration--get-path-of-codemodel-query-file) "./build/.cmake/api/v1/query/client-emacs/codemodel-v2")))))


(ert-deftest test-cmake-integration--get-codemodel-reply-json-filename ()
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (should (filepath-equal-p
              (cmake-integration--get-codemodel-reply-json-filename)
              (format "%s%s" (cmake-integration--get-reply-folder)
                      "codemodel-v2-some-hash.json"))))))


(ert-deftest test-cmake-integration--get-working-directory ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((cmake-integration-current-target "bin/main")
           (cmake-integration-configure-preset '((name . "default")
                                                 (binaryDir . "${sourceDir}/build-with-ninja/"))))
       (let ((cmake-integration-run-working-directory 'root))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (format "~/%s" (file-relative-name "./" "~/"))))
         )

       (let ((cmake-integration-run-working-directory 'build))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (format "~/%s" (file-relative-name "./build-with-ninja/" "~/"))))
         )

       (let ((cmake-integration-run-working-directory 'bin))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (format "~/%s" (file-relative-name "./build-with-ninja/bin/" "~/"))))
         )

       (let ((cmake-integration-run-working-directory "some/subfolder/"))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (format "~/%s" (file-relative-name "./some/subfolder/" "~/")))))))))


(ert-deftest test-cmake-integration-get-target-executable-full-path ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((cmake-integration-current-target "bin/main"))
       (should (equal
                (cmake-integration-get-target-executable-full-path cmake-integration-current-target)
                (file-name-concat (cmake-integration-get-build-folder) cmake-integration-current-target))))

     (let ((cmake-integration-current-target "main"))
       (should (equal
                (cmake-integration-get-target-executable-full-path cmake-integration-current-target)
                (file-name-concat (cmake-integration-get-build-folder) cmake-integration-current-target)))))))



(ert-deftest test-cmake-integration--get-run-command--root-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'root))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s && ~/%s arg1 arg2 arg3"
                              (file-relative-name "./" "~/")
                              (file-relative-name "./build/bin/myexec" "~/"))))))))

(ert-deftest test-cmake-integration--get-run-command--build-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'build))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s && %s arg1 arg2 arg3"
                              (file-relative-name "./build" "~/")
                              "bin/myexec")))))))


(ert-deftest test-cmake-integration--get-run-command--bin-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'bin))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s/ && %s arg1 arg2 arg3"
                              (file-relative-name "./build/bin" "~/")
                              "./myexec")))))))


(ert-deftest test-cmake-integration--get-run-command--custom-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory "subfolder"))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s && ~/%s arg1 arg2 arg3"
                              (file-relative-name "./subfolder" "~/")
                              (file-relative-name "./build/bin/myexec" "~/"))))))))



(ert-deftest test-cmake-integration--get-debug-command--root-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory 'root))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-run-arguments)))))))


(ert-deftest test-cmake-integration--get-debug-command--build-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory 'build))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./build" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-run-arguments)))))))


(ert-deftest test-cmake-integration--get-debug-command--bin-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory 'bin))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./build/bin/" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-run-arguments
                              )))))))


(ert-deftest test-cmake-integration--get-debug-command--custom-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory "subfolder"))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./subfolder" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-run-arguments)))))))


;;; TODO: add more cases to the test (install target, ninja multi-config, etc)
(ert-deftest test-cmake-integration--get-targets-from-codemodel-json-file ()
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()

     (let ((targets (cmake-integration--get-targets-from-codemodel-json-file))
           (expected-targets '(("all")
                               ("clean")
                               ("somelib"
                                (jsonFile . "target-somelib-some-hash.json")
                                (name . "somelib")
                                (projectIndex . 0))
                               ("main"
                                (jsonFile . "target-main-some-hash.json")
                                (name . "main")
                                (projectIndex . 0)))))
       (should (equal targets expected-targets))))))


(ert-deftest test-cmake-integration--add-all-clean-install-targets ()
  ;; Test when config-name is nil
  (let* ((targets '(("main" (name . "main")) ("somelib" (name . "somelib"))))
         (expected-all-targets '(("all") ("clean") ("main" (name . "main")) ("somelib" (name . "somelib"))))
         (config-name nil)
         (install-rule? nil)
         (all-targets (cmake-integration--add-all-clean-install-targets targets config-name install-rule?)))
    (should (equal all-targets expected-all-targets)))

  ;; With a non-nil config-name
  (let* ((targets '(("main" (name . "main")) ("somelib" (name . "somelib"))))
         (expected-all-targets '(("all/someConfig") ("clean/someConfig") ("main" (name . "main")) ("somelib" (name . "somelib"))))
         (config-name "someConfig")
         (install-rule? nil)
         (all-targets (cmake-integration--add-all-clean-install-targets targets config-name install-rule?)))
    (should (equal all-targets expected-all-targets)))

  ;; With an install rule
  (let* ((targets '(("main" (name . "main")) ("somelib" (name . "somelib"))))
         (expected-all-targets '(("all") ("clean") ("install") ("main" (name . "main")) ("somelib" (name . "somelib"))))
         (config-name nil)
         (install-rule? t)
         (all-targets (cmake-integration--add-all-clean-install-targets targets config-name install-rule?)))
    (should (equal all-targets expected-all-targets)))

  ;; With non-nil config-name and an install rule
  (let* ((targets '(("main" (name . "main")) ("somelib" (name . "somelib"))))
         (expected-all-targets '(("all/someConfig") ("clean/someConfig") ("install/someConfig") ("main" (name . "main")) ("somelib" (name . "somelib"))))
         (config-name "someConfig")
         (install-rule? t)
         (all-targets (cmake-integration--add-all-clean-install-targets targets config-name install-rule?)))
    (should (equal all-targets expected-all-targets))))


(ert-deftest test-cmake-integration--change-to-absolute-filename ()
  (let ((absolute-filename "/somefolder/subfolder/filename1.txt")
        (relative-filename "subfolder/filename2.txt")
        (parent-folder "/somefolder"))
    (should (equal (cmake-integration--change-to-absolute-filename absolute-filename parent-folder)
                   absolute-filename))
    (should (equal (cmake-integration--change-to-absolute-filename relative-filename parent-folder)
                   (f-join parent-folder relative-filename)))))


(ert-deftest test-cmake-integration-get-configure-presets ()
  ;; Without any presets file
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((presets (cmake-integration-get-configure-presets)))
       (should (equal presets nil)))))

  ;; With a presets file
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((presets-names (mapcar
                           #'cmake-integration--get-preset-name
                           (cmake-integration-get-configure-presets)))
           (expected-preset-names '("default" "ninjamulticonfig" "Dummy" "ninjamulticonfig2")))
       (should (equal presets-names expected-preset-names)))))

  (test-fixture-setup
   "./test-project-with-presets-with-includes"
   (lambda ()
     (let ((presets-names (mapcar
                           #'cmake-integration--get-preset-name
                           (cmake-integration-get-configure-presets)))
           (expected-preset-names '("MorePresets-Extra-1"
                                    "MorePresets-Extra-2"
                                    "MorePresets-Extra-3"
                                    "MorePresets-1"
                                    "MorePresets-2"
                                    "MorePresets-3"
                                    "EvenMorePresets-1"
                                    "EvenMorePresets-2"
                                    "CMakePresets-1"
                                    "CMakePresets-2")))
       (should (equal presets-names expected-preset-names))))))


(ert-deftest test-cmake-integration-get-build-command ()
  (let ((cmake-integration-build-preset nil))

    (test-fixture-setup
     "./test-project"
     (lambda ()
       (let ((project-dir (format "~/%s" (file-relative-name "./" "~/"))))
         (should (equal (cmake-integration-get-build-command "the_target")
                        (format "cd %s && cmake --build %s --target the_target" project-dir cmake-integration-build-dir)))
         )))

    ;; Without setting a preset
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((project-dir (format "~/%s" (file-relative-name "./" "~/"))))
         (should (equal (cmake-integration-get-build-command "the_target")
                        (format "cd %s && cmake --build %s --target the_target" project-dir cmake-integration-build-dir))))))

    ;; Without a build preset
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((project-dir (format "~/%s" (file-relative-name "./" "~/")))
             (cmake-integration-configure-preset '("default" (name . "default") (binaryDir . "theBuildFolder"))))
         (should (equal (cmake-integration-get-build-command "the_target")
                        (format "cd %s && cmake --build theBuildFolder --target the_target" project-dir))))))

    ;; With a build preset
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((project-dir (format "~/%s" (file-relative-name "./" "~/")))
             (cmake-integration-configure-preset '("config-preset" (name . "config-preset") (binaryDir . "theBuildFolder")))
             (cmake-integration-build-preset '("build-preset" (name . "build-preset") (configurePreset . "config-preset")))
             )
         (should (equal (cmake-integration-get-build-command "the_target")
                        (format "cd %s && cmake --build --preset build-preset --target the_target" project-dir))))))

    ;; With a build preset and a target that has the configuration in the name
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((project-dir (format "~/%s" (file-relative-name "./" "~/")))
             (cmake-integration-configure-preset '("config-preset" (name . "config-preset") (binaryDir . "theBuildFolder")))
             (cmake-integration-build-preset '("build-preset" (name . "build-preset") (configurePreset . "config-preset")))
             )
         (should (equal (cmake-integration-get-build-command "the_target/Debug")
                        (format "cd %s && cmake --build --preset build-preset --config Debug --target the_target" project-dir))))))

    ;; Passing extra args
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((project-dir (format "~/%s" (file-relative-name "./" "~/")))
             (cmake-integration-configure-preset '("config-preset" (name . "config-preset") (binaryDir . "theBuildFolder")))
             (cmake-integration-build-preset '("build-preset" (name . "build-preset") (configurePreset . "config-preset")))
             )
         (should (equal (cmake-integration-get-build-command "the_target" '("--lala lele" "--lili lolo"))
                        (format "cd %s && cmake --build --preset build-preset --lala lele --lili lolo --target the_target" project-dir))))))
    ))


(ert-deftest test-cmake-integration-get-conan-run-command ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     ;; Test conan command without using a conan profile
     (let ((cmake-integration-conan-profile nil))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing"
                              (cmake-integration-get-build-folder)
                              (cmake-integration--get-project-root-folder)))))
     ;; Test conan command when using a single fixed conan profile
     (let ((cmake-integration-conan-profile "some-conan-profile"))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing --profile some-conan-profile"
                              (cmake-integration-get-build-folder)
                              (cmake-integration--get-project-root-folder)))))
     ;; Test conan command when cmake profile names are mapped to conan profile names
     (let ((cmake-integration-conan-profile '(("cmake-profile-1" . "conan-profile-1")
                                              ("cmake-profile-2" . "conan-profile-2")))
           (cmake-integration-configure-preset '((name . "cmake-profile-1") (binaryDir . "build")))
           )
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing --profile conan-profile-1"
                              (cmake-integration-get-build-folder)
                              (cmake-integration--get-project-root-folder))
                      ))
       ;; If we change cmake-integration-configure-preset the conan profile will be affected
       (setq cmake-integration-configure-preset '((name . "cmake-profile-2") (binaryDir . "build")))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing --profile conan-profile-2"
                              (cmake-integration-get-build-folder)
                              (cmake-integration--get-project-root-folder))
                      ))
       ;; If the cmake profile has no conan profile mapped to it, then no profile will be used
       (setq cmake-integration-configure-preset '((name . "cmake-profile-3") (binaryDir . "build")))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing"
                              (cmake-integration-get-build-folder)
                              (cmake-integration--get-project-root-folder))
                      ))))))


(ert-deftest test-cmake-integration--get-associated-configure-preset ()
  (let ((build-preset1 '((name . "presetName1") (configurePreset . "configurePresetName1")))
        (build-preset2 '((name . "presetName2") (configurePreset . "configurePresetName2"))))
    (should (equal (cmake-integration--get-associated-configure-preset build-preset1) "configurePresetName1"))
    (should (equal (cmake-integration--get-associated-configure-preset build-preset2) "configurePresetName2"))))


(ert-deftest test-cmake-integration--preset-has-matching-configure-preset-p ()
  (let ((preset1 '((name . "preset1") (configurePreset . "configurePreset1")))
        (preset2 '((name . "preset2") (configurePreset . "configurePreset2")))
        (configurePreset1 '((name . "configurePreset1")))
        (configurePreset2 '((name . "configurePreset2"))))
    (should (cmake-integration--preset-has-matching-configure-preset-p preset1 configurePreset1))
    (should (cmake-integration--preset-has-matching-configure-preset-p preset2 configurePreset2))
    (should-not (cmake-integration--preset-has-matching-configure-preset-p preset1 configurePreset2))
    (should-not (cmake-integration--preset-has-matching-configure-preset-p preset2 configurePreset1))))



(ert-deftest test-cmake-integration-get-build-presets ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let* ((all-build-presets (cmake-integration-get-all-presets-of-type 'buildPresets))
            (configure-preset '((name . "ninjamulticonfig")))
            (build-presets (cmake-integration-get-build-presets configure-preset))
            (expected-build-presets
             '(((name . "ninjamulticonfig") (displayName . "Build preset using ninja multi-config") (configurePreset . "ninjamulticonfig") (configuration . "Release")))))
       (should (equal (length all-build-presets) 2))
       (should (equal build-presets expected-build-presets)))))

  ;; Test when not passing a configure preset to
  ;; cmake-integration-get-build-presets -> The value in the
  ;; `cmake-integration-configure-preset' is used.
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let* ((all-build-presets (cmake-integration-get-all-presets-of-type 'buildPresets))
            (cmake-integration-configure-preset '((name . "ninjamulticonfig")))
            (build-presets (cmake-integration-get-build-presets))
            (expected-build-presets
             '(((name . "ninjamulticonfig") (displayName . "Build preset using ninja multi-config") (configurePreset . "ninjamulticonfig") (configuration . "Release")))))
       (should (equal (length all-build-presets) 2))
       (should (equal build-presets expected-build-presets)))))

  ;; Test the case where there are multiple build presets with the same configure preset
  (test-fixture-setup
   "./test-project-with-presets-and-many-targets"
   (lambda ()
     (let* ((all-build-presets (cmake-integration-get-all-presets-of-type 'buildPresets))
            (configure-preset '((name . "default")))
            (build-presets (cmake-integration-get-build-presets configure-preset))
            (expected-build-presets
             '(((name . "default") (displayName . "Default build preset") (configurePreset . "default"))
               ((name . "another") (displayName . "Another build preset with default as configure preset") (configurePreset . "default")))))

       (should (equal (length all-build-presets) 3))
       (should (equal build-presets expected-build-presets))))))



(ert-deftest test-name ()
    (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (cmake-integration-get-build-presets)
     ))
  )




;; Assert macros:
;; - should
;; - should-not
;; - should-error


(provide 'cmake-integration-tests)
;;; cmake-integration-tests.el ends here
