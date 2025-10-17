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


;; NOTE: Trailling backslash matters
(defun filepath-equal-p (filepath1 filepath2)
  "Check that two file paths FILEPATH1 and FILEPATH2 are equal.

This is similar to `file-equal-p', but does not require
file/folder to exist."
  (equal (expand-file-name filepath1)
         (expand-file-name filepath2)))


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

(ert-deftest test-ci--get-preset-name ()
  (let ((preset '((name . "my-preset") (displayName . "My Preset")))
        (expected-name "my-preset"))
    (should (equal (ci--get-preset-name preset) expected-name))))


(ert-deftest test-ci-get-last-build-preset-name ()
  (let ((ci-build-preset))
    (should (null (ci-get-last-build-preset-name))))

  (let ((ci-build-preset '((name . "my-preset") (displayName . "My Preset"))))
    (should (equal (ci-get-last-build-preset-name) "my-preset"))))



(ert-deftest test-ci--get-system-presets-file ()
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (let* ((project-root-folder (cmake-integration--get-project-root-folder))
            (expected-system-preset (file-name-concat project-root-folder "CMakePresets.json")))
       (should (equal (cmake-integration--get-system-presets-file) expected-system-preset))))))


(ert-deftest test-ci--get-user-presets-file ()
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (let* ((project-root-folder (cmake-integration--get-project-root-folder))
            (expected-user-preset (file-name-concat project-root-folder "CMakeUserPresets.json")))
       (should (equal (cmake-integration--get-user-presets-file) expected-user-preset))))))


(ert-deftest test-ci--expand-included-presets ()
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


(ert-deftest test-ci--get-all-preset-files ()
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


(ert-deftest test-ci--create-target ()
  ;; ci--create-target returns a list with a sigle string
  (should (equal (ci--create-target "target-name" "config-name") '("target-name/config-name")))

  (let ((cmake-integration--multi-config-separator "|"))
    (should (equal (ci--create-target "all" "Debug") '("all|Debug")))))


(ert-deftest test-ci--create-target-fullname ()
  (should (equal (cmake-integration--create-target-fullname "target" "config") "target/config"))
  (let ((cmake-integration--multi-config-separator "|"))
    (should (equal (cmake-integration--create-target-fullname "target" "config") "target|config"))))


;; NOTE: For this test to pass a folder with a `.project' file must be recognized as a project.
;; See https://www.reddit.com/r/emacs/comments/k3og7d/extending_projectel/
(ert-deftest test-ci--get-project-root-folder ()
  "Test getting the project root when no presets are used."
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (should (file-equal-p (cmake-integration--get-project-root-folder)
                           "..")))))


(ert-deftest test-ci--get-preset-by-name ()
  (let* ((preset1 '((name . "preset1") (displayName . "The first preset")))
         (preset2 '((name . "preset2") (displayName . "The second preset")))
         (list-of-presets (list preset1 preset2))
         )
    (should (equal (cmake-integration--get-preset-by-name "preset1" list-of-presets) preset1))
    (should (equal (cmake-integration--get-preset-by-name "preset2" list-of-presets) preset2))))


(ert-deftest test-ci--get-configure-parent-preset ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     ;; If there is no parent, return nil
     (let* ((preset (cmake-integration--get-configure-preset-by-name "Ninja"))
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


(ert-deftest test-ci--perform-binaryDir-replacements ()
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


(ert-deftest test-ci--get-binaryDir ()
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


(ert-deftest test-ci--get-binaryDir-with-replacements ()
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


(ert-deftest test-ci-get-build-folder-without-presets ()
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     ;; Without setting `cmake-integration-build-dir' -> default value is "build"
     (let* ((project-root (cmake-integration--get-project-root-folder))
            (expected-build-folder (expand-file-name "./build" project-root)))
       (should (filepath-equal-p (cmake-integration-get-build-folder) expected-build-folder)))

     ;; Set `cmake-integration-build-dir'
     (let* ((project-root (cmake-integration--get-project-root-folder))
            (cmake-integration-build-dir "some-build-folder")
            (expected-build-folder (expand-file-name cmake-integration-build-dir project-root)))
       (should (filepath-equal-p (cmake-integration-get-build-folder) expected-build-folder))))))


(ert-deftest test-ci-get-build-folder-with-presets ()
  (test-fixture-setup
   ;; Note that in this test we set default-directory to a subfolder in the
   ;; project root
   "./test-project-with-presets/subfolder"
   (lambda ()
     ;; Build folder is taken from the `binaryDir' field in
     ;; `cmake-integration-configure-preset', which is an alist.
     ;; Here we test with a binaryDir value with some replacements
     (let* ((cmake-integration-configure-preset '((binaryDir . "${sourceDir}/build/${presetName}")
                                                  (name . "ninjamulticonfig")))
            (project-root (cmake-integration--get-project-root-folder))
            (expected-build-folder (expand-file-name "./build/ninjamulticonfig" project-root)))
       (should (filepath-equal-p (cmake-integration-get-build-folder) expected-build-folder)))

     ;; Now we test with a binaryDir that has a relative path
     (let* ((cmake-integration-configure-preset '((binaryDir . "build/${presetName}")
                                                  (name . "ninjamulticonfig")))
            (project-root (cmake-integration--get-project-root-folder))
            (expected-build-folder (expand-file-name "./build/ninjamulticonfig" project-root)))
       (should (filepath-equal-p (cmake-integration-get-build-folder) expected-build-folder)))

     ;; Test with a different preset
     (let* ((cmake-integration-configure-preset '((binaryDir . "./build/${presetName}")
                                                  (name . "Ninja")))
            (project-root (cmake-integration--get-project-root-folder))
            (expected-build-folder (expand-file-name "./build/Ninja" project-root)))
       (should (filepath-equal-p (cmake-integration-get-build-folder) expected-build-folder))))))


(ert-deftest test-ci--get-query-folder ()
  (test-fixture-setup
   "./test-project/subfolder"
   (lambda ()
     (let* ((build-folder (cmake-integration-get-build-folder))
            (query-relative-path ".cmake/api/v1/query/client-emacs")
            (expected-query-folder (expand-file-name query-relative-path build-folder)))
       (should (filepath-equal-p (cmake-integration--get-query-folder) expected-query-folder))))))


(ert-deftest test-ci--get-reply-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration--get-reply-folder) "./build/.cmake/api/v1/reply/")))))


(ert-deftest test-ci--get-path-of-codemodel-query-file ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration--get-path-of-codemodel-query-file) "./build/.cmake/api/v1/query/client-emacs/codemodel-v2")))))


(ert-deftest test-ci--get-codemodel-reply-json-filename ()
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (should (filepath-equal-p
              (cmake-integration--get-codemodel-reply-json-filename)
              (format "%s%s" (cmake-integration--get-reply-folder)
                      "codemodel-v2-some-hash.json"))))))


(ert-deftest test-ci--get-working-directory ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let ((cmake-integration-current-target "bin/main")
           (cmake-integration-configure-preset '((name . "default")
                                                 (binaryDir . "${sourceDir}/build-with-ninja/"))))
       (let ((cmake-integration-run-working-directory 'root))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (cmake-integration--get-project-root-folder))))

       (let ((cmake-integration-run-working-directory 'build))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (cmake-integration-get-build-folder))))

       (let ((cmake-integration-run-working-directory 'bin))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (file-name-concat (cmake-integration-get-build-folder) "bin/"))))

       (let ((cmake-integration-run-working-directory "some/subfolder/"))
         (should (equal (cmake-integration--get-working-directory cmake-integration-current-target)
                        (file-name-concat (cmake-integration--get-project-root-folder) "some/subfolder/"))))))))


(ert-deftest test-ci-get-target-executable-full-path ()
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



(ert-deftest test-ci--get-run-command--root-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'root))
       (let* ((expected-run-dir (cmake-integration--get-project-root-folder))
              (expected-cmd (format "./%s %s"
                                    (file-relative-name (cmake-integration-get-target-executable-full-path "bin/myexec") expected-run-dir)
                                    cmake-integration-run-arguments)))
         (pcase-let* ((`(,dir ,command) (cmake-integration--get-run-command "bin/myexec")))
           (should (filepath-equal-p dir expected-run-dir))
           (should (equal command expected-cmd))))))))

(ert-deftest test-ci--get-run-command--build-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'build))
       (let* ((expected-run-dir (cmake-integration-get-build-folder))
              (expected-cmd (format "./%s %s" "bin/myexec" cmake-integration-run-arguments)))

         (pcase-let* ((`(,dir ,command) (cmake-integration--get-run-command "bin/myexec")))
           (should (filepath-equal-p dir expected-run-dir))
           (should (equal command expected-cmd))))))))

(ert-deftest test-ci--get-run-command--bin-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'bin))
       (let* ((expected-run-dir (file-name-concat (cmake-integration-get-build-folder) "bin/"))
              (expected-cmd (format "./%s %s" "myexec" cmake-integration-run-arguments)))

         (pcase-let* ((`(,dir ,command) (cmake-integration--get-run-command "bin/myexec")))
           (should (filepath-equal-p dir expected-run-dir))
           (should (equal command expected-cmd))))))))

;; (ert-deftest test-ci--get-run-command--custom-folder ()
;;   (test-fixture-setup
;;    "./test-project"
;;    (lambda ()
;;      (let* ((cmake-integration-run-working-directory "subfolder")
;;             (cmake-integration-run-arguments "arg1 arg2")
;;             (executable-relative-path "../build/bin/myexec")
;;             (expected-run-dir (file-name-concat (cmake-integration--get-project-root-folder) "subfolder"))
;;             (expected-cmd (format "gdb -i=mi --args %s %s"
;;                                   executable-relative-path
;;                                   cmake-integration-run-arguments)))
;;        (pcase-let* ((`(,dir ,command) (cmake-integration--get-debug-command "bin/myexec")))
;;          (should (filepath-equal-p dir expected-run-dir))
;;          (should (equal command expected-cmd)))))))


;; (ert-deftest test-ci--get-debug-command--root-folder ()
;;   (test-fixture-setup
;;    "./test-project"
;;    (lambda ()
;;      (let* ((cmake-integration-run-working-directory 'root)
;;             (cmake-integration-run-arguments "arg1 arg2")
;;             (executable-relative-path "build/bin/myexec")
;;             (expected-run-dir (cmake-integration--get-project-root-folder))
;;             (expected-cmd (format "gdb -i=mi --args %s %s"
;;                                   executable-relative-path
;;                                   cmake-integration-run-arguments)))
;;        (pcase-let* ((`(,dir ,command) (cmake-integration--get-debug-command "bin/myexec")))
;;          (should (filepath-equal-p dir expected-run-dir))
;;          (should (equal command expected-cmd)))))))


;; (ert-deftest test-ci--get-debug-command--build-folder ()
;;   (test-fixture-setup
;;    "./test-project"
;;    (lambda ()
;;      (let* ((cmake-integration-run-working-directory 'build)
;;             (cmake-integration-run-arguments "arg1 arg2")
;;             (executable-relative-path "bin/myexec")
;;             (expected-run-dir (cmake-integration-get-build-folder))
;;             (expected-cmd (format "gdb -i=mi --args %s %s"
;;                                   executable-relative-path
;;                                   cmake-integration-run-arguments)))
;;        (pcase-let* ((`(,dir ,command) (cmake-integration--get-debug-command "bin/myexec")))
;;          (should (filepath-equal-p dir expected-run-dir))
;;          (should (equal command expected-cmd)))))))

;; (ert-deftest test-ci--get-debug-command--bin-folder ()
;;   (test-fixture-setup
;;    "./test-project"
;;    (lambda ()
;;      (let* ((cmake-integration-run-working-directory 'bin)
;;             (cmake-integration-run-arguments "arg1 arg2")
;;             (executable-relative-path "myexec")
;;             (expected-run-dir (file-name-concat (cmake-integration-get-build-folder) "bin/"))
;;             (expected-cmd (format "gdb -i=mi --args %s %s"
;;                                   executable-relative-path
;;                                   cmake-integration-run-arguments)))
;;        (pcase-let* ((`(,dir ,command) (cmake-integration--get-debug-command "bin/myexec")))
;;          (should (filepath-equal-p dir expected-run-dir))
;;          (should (equal command expected-cmd)))))))


;; (ert-deftest test-ci--get-debug-command--custom-folder ()
;;   (test-fixture-setup
;;    "./test-project"
;;    (lambda ()
;;      (let* ((cmake-integration-run-working-directory "subfolder")
;;             (cmake-integration-run-arguments "arg1 arg2")
;;             (executable-relative-path "../build/bin/myexec")
;;             (expected-run-dir (file-name-concat (cmake-integration--get-project-root-folder) "subfolder"))
;;             (expected-cmd (format "gdb -i=mi --args %s %s"
;;                                   executable-relative-path
;;                                   cmake-integration-run-arguments)))
;;        (pcase-let* ((`(,dir ,command) (cmake-integration--get-debug-command "bin/myexec")))
;;          (should (filepath-equal-p dir expected-run-dir))
;;          (should (equal command expected-cmd)))))))


;;; TODO: add more cases to the test (install target, ninja multi-config, etc)
(ert-deftest test-ci--get-targets-from-codemodel-json-file ()
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


(ert-deftest test-ci--add-all-clean-install-targets ()
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


(ert-deftest test-ci--change-to-absolute-filename ()
  (let ((absolute-filename "/somefolder/subfolder/filename1.txt")
        (relative-filename "subfolder/filename2.txt")
        (parent-folder "/somefolder"))
    (should (equal (cmake-integration--change-to-absolute-filename absolute-filename parent-folder)
                   absolute-filename))
    (should (equal (cmake-integration--change-to-absolute-filename relative-filename parent-folder)
                   (f-join parent-folder relative-filename)))))


(ert-deftest test-ci-get-configure-presets ()
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


(ert-deftest test-ci-get-build-command ()
  (let ((cmake-integration-build-preset nil))

    (test-fixture-setup
     "./test-project"
     (lambda ()
       (let ((expected-run-dir (cmake-integration--get-project-root-folder)))
         (should (equal (cmake-integration-get-build-command "the_target")
                        (list expected-run-dir
                              (format "cmake --build %s --target the_target" cmake-integration-build-dir)))))))

    ;; Without setting a preset
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((expected-run-dir (cmake-integration--get-project-root-folder)))
         (should (equal (cmake-integration-get-build-command "the_target")
                        (list expected-run-dir
                              (format "cmake --build %s --target the_target" cmake-integration-build-dir)))))))

    ;; Without a build preset
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((expected-run-dir (cmake-integration--get-project-root-folder))
             (cmake-integration-configure-preset '("default" (name . "default") (binaryDir . "theBuildFolder"))))
         (should (equal (cmake-integration-get-build-command "the_target")
                        (list expected-run-dir
                              "cmake --build theBuildFolder --target the_target"))))))

    ;; With a build preset
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((expected-run-dir (cmake-integration--get-project-root-folder))
             (cmake-integration-configure-preset '("config-preset" (name . "config-preset") (binaryDir . "theBuildFolder")))
             (cmake-integration-build-preset '("build-preset" (name . "build-preset") (configurePreset . "config-preset")))
             )
         (should (equal (cmake-integration-get-build-command "the_target")
                        (list expected-run-dir
                              "cmake --build --preset build-preset --target the_target"))))))

    ;; With a build preset and a target that has the configuration in the name
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((expected-run-dir (cmake-integration--get-project-root-folder))
             (cmake-integration-configure-preset '("config-preset" (name . "config-preset") (binaryDir . "theBuildFolder")))
             (cmake-integration-build-preset '("build-preset" (name . "build-preset") (configurePreset . "config-preset")))
             )
         (should (equal (cmake-integration-get-build-command "the_target/Debug")
                        (list expected-run-dir
                              "cmake --build --preset build-preset --target the_target --config Debug"))))))

    ;; Passing extra args
    (test-fixture-setup
     "./test-project-with-presets"
     (lambda ()
       (let ((expected-run-dir (cmake-integration--get-project-root-folder))
             (cmake-integration-configure-preset '("config-preset" (name . "config-preset") (binaryDir . "theBuildFolder")))
             (cmake-integration-build-preset '("build-preset" (name . "build-preset") (configurePreset . "config-preset")))
             )
         (should (equal (cmake-integration-get-build-command "the_target" '("--lala lele" "--lili lolo"))
                        (list expected-run-dir
                              "cmake --build --preset build-preset --target the_target --lala lele --lili lolo"))))))))


(ert-deftest test-ci-get-conan-run-command ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let* ((project-root-folder (cmake-integration--get-project-root-folder))
            (build-folder (cmake-integration-get-build-folder))
            (conanfile-relative-path (file-relative-name project-root-folder build-folder)))
       ;; Test conan command without using a conan profile
       (let ((cmake-integration-conan-profile nil))
         (should (equal (cmake-integration-get-conan-run-command)
                        (format "conan install %s --build missing"
                                conanfile-relative-path))))

       ;; Test conan command when using a single fixed conan profile
       (let ((cmake-integration-conan-profile "some-conan-profile"))
         (should (equal (cmake-integration-get-conan-run-command)
                        (format "conan install %s --build missing --profile some-conan-profile"
                                conanfile-relative-path))))

       ;; Test conan command when cmake profile names are mapped to conan profile names
       (let ((cmake-integration-conan-profile '(("cmake-profile-1" . "conan-profile-1")
                                                ("cmake-profile-2" . "conan-profile-2")))
             ;; Declare a local version of the
             ;; cmake-integration-configure-preset variable such that the tests
             ;; here don't affect the original variable
             (cmake-integration-configure-preset))

         (setq cmake-integration-configure-preset '((name . "cmake-profile-1") (binaryDir . "build")))
         (should (equal (cmake-integration-get-conan-run-command)
                        (format "conan install %s --build missing --profile conan-profile-1"
                                conanfile-relative-path)))

         ;; If we change cmake-integration-configure-preset the conan profile will be affected
         (setq cmake-integration-configure-preset '((name . "cmake-profile-2") (binaryDir . "build")))
         (should (equal (cmake-integration-get-conan-run-command)
                        (format "conan install %s --build missing --profile conan-profile-2"
                                conanfile-relative-path)))

         ;; If the cmake profile has no conan profile mapped to it, then no profile will be used
         (setq cmake-integration-configure-preset '((name . "cmake-profile-3") (binaryDir . "build")))
         (should (equal (cmake-integration-get-conan-run-command)
                        (format "conan install %s --build missing"
                                conanfile-relative-path))))))))


(ert-deftest test-ci--get-associated-configure-preset ()
  (let ((build-preset1 '((name . "presetName1") (configurePreset . "configurePresetName1")))
        (build-preset2 '((name . "presetName2") (configurePreset . "configurePresetName2"))))
    (should (equal (cmake-integration--get-associated-configure-preset build-preset1) "configurePresetName1"))
    (should (equal (cmake-integration--get-associated-configure-preset build-preset2) "configurePresetName2"))))


(ert-deftest test-ci--preset-has-matching-configure-preset-p ()
  (let ((preset1 '((name . "preset1") (configurePreset . "configurePreset1")))
        (preset2 '((name . "preset2") (configurePreset . "configurePreset2")))
        (configurePreset1 '((name . "configurePreset1")))
        (configurePreset2 '((name . "configurePreset2"))))
    (should (cmake-integration--preset-has-matching-configure-preset-p preset1 configurePreset1))
    (should (cmake-integration--preset-has-matching-configure-preset-p preset2 configurePreset2))
    (should-not (cmake-integration--preset-has-matching-configure-preset-p preset1 configurePreset2))
    (should-not (cmake-integration--preset-has-matching-configure-preset-p preset2 configurePreset1))))


(ert-deftest test-ci-get-all-presets-of-type ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     ;; Include hidden presets
     (let ((all-presets (ci-get-all-presets-of-type 'configurePresets t))
           (expected-preset-names '("Ninja" "Debug" "default" "ninjamulticonfig" "Dummy" "ninjamulticonfig2")))
       (should (equal (length all-presets) 6))
       (should (equal (mapcar #'ci--get-preset-name all-presets) expected-preset-names)))

     ;; No Hidden presets
     (let ((all-presets (ci-get-all-presets-of-type 'configurePresets))
           (expected-preset-names '("default" "ninjamulticonfig" "Dummy" "ninjamulticonfig2")))
       (should (equal (length all-presets) 4))
       (should (equal (mapcar #'ci--get-preset-name all-presets) expected-preset-names))))))


(ert-deftest test-ci-get-presets-of-type ()
  (test-fixture-setup
   "./test-project-with-presets"
   (lambda ()
     (let* ((all-configure-presets (ci-get-presets-of-type 'configurePresets))
            (configure-preset-1 (elt all-configure-presets 0)) ;; default
            (configure-preset-2 (elt all-configure-presets 1)) ;; ninjamulticonfig
            (configure-preset-3 (elt all-configure-presets 2)) ;; Dummy
            )
       ;; Case when passing a configure preset
       (let* ((build-presets-1 (ci-get-presets-of-type 'buildPresets configure-preset-1))
              (build-presets-2 (ci-get-presets-of-type 'buildPresets configure-preset-2))
              (build-preset-nil (ci-get-presets-of-type 'buildPresets configure-preset-3))
              )
         (should (equal (length build-presets-1) 1))
         (should (equal (ci--get-preset-name (elt build-presets-1 0))
                        (ci--get-preset-name configure-preset-1)))

         (should (equal (length build-presets-2) 1))
         (should (equal (ci--get-preset-name (elt build-presets-2 0))
                        (ci--get-preset-name configure-preset-2)))

         ;; There are no build presets with configure preset "dummy"
         (should (equal (length build-preset-nil) 0)))

       ;; Case when not passing a configure preset
       (let* ((ci-configure-preset (elt all-configure-presets 1))
              ;; (expected-build-preset (ci-get-presets-of-type 'buildPresets configure-preset-1))
              (all-build-presets (ci-get-all-presets-of-type 'buildPresets))
              (build-preset-1 (elt all-build-presets 0))
              (build-preset-2 (elt all-build-presets 1))
              (build-preset-3 (elt all-build-presets 2))

              (obtained-build-preset (ci-get-presets-of-type 'buildPresets))
              )

         (should (equal (length all-build-presets) 2))
         ;; There is only one build preset with the configure preset "ninjamulticonfig"
         (should (equal (length obtained-build-preset) 1))

         (should (equal (ci--get-preset-name ci-configure-preset) "ninjamulticonfig"))

         (should (equal (elt obtained-build-preset 0) build-preset-2)))))))



(ert-deftest test-ci-get-build-presets ()
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


(ert-deftest test-ci--get-all-targets ()
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (let* ((codemodel-file (cmake-integration--get-codemodel-reply-json-filename))
            (all-targets (ci--get-all-targets codemodel-file))
            (target-1 (elt all-targets 0))
            (target-2 (elt all-targets 1))
            (target-3 (elt all-targets 2))
            (target-4 (elt all-targets 3)))

       ;; Two targets defined in the codemodel + all + clean
       (should (equal (length all-targets) 4))

       (should (equal target-1 '("all")))
       (should (equal target-2 '("clean")))
       (should (equal (alist-get 'name target-3) "somelib"))
       (should (equal (alist-get 'type target-3) "STATIC_LIBRARY"))

       (should (equal (alist-get 'name target-4) "main"))
       (should (equal (alist-get 'type target-4) "EXECUTABLE"))))))



(ert-deftest test-ci--get-target-type-from-name ()
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (let* ((codemodel-file (cmake-integration--get-codemodel-reply-json-filename))
            (all-targets (ci--get-all-targets codemodel-file))
            (target-type-main (ci--get-target-type-from-name "main" all-targets))
            (target-type-somelib (ci--get-target-type-from-name "somelib" all-targets))
            )
       (should (equal target-type-main "EXECUTABLE"))
       (should (equal target-type-somelib "STATIC_LIBRARY"))))))


(ert-deftest test-ci--get-target-name ()
  (let* (
         (target-name "target-name")
         (config-name "config-name")
         (target-info `((name . ,target-name)))
         (target-name-with-config-name (format "%s/%s" target-name config-name)))
    ;; If config-name is non-nil, the returned value is in the form "target-name/config-name"
    (should (equal (ci--get-target-name target-info config-name) target-name-with-config-name))

    ;; If config is nil, it returns just the target name
    (should (equal (ci--get-target-name target-info nil) target-name))))


(ert-deftest test-ci--add-name-to-target ()
  (let* ((target-name "the-target-name")
         (config-name "the-config-name")
         (target-info `((name . ,target-name) (somethingElse . "somethingElse") ))
         (target-name-with-config "the-target-name/the-config-name")
         (expected (cons target-name-with-config target-info))
         (expected2 (cons target-name target-info)))

    ;; Case where we pass the config name
    (should (equal (ci--add-name-to-target target-info config-name) expected))

    ;; Case where we do not pass the config name (it's nil)
    (should (equal (ci--add-name-to-target target-info) expected2))
    (should (equal (ci--add-name-to-target target-info nil) expected2))))



(ert-deftest test-ci--get-prepared-targets-from-configuration ()
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (let* ((codemodel-file (ci--get-codemodel-reply-json-filename))
            (all-config (alist-get 'configurations (json-read-file codemodel-file)))
            (first-config (elt all-config 0))
            (first-config-name (alist-get 'name first-config))
            (targets-no-config-name (ci--get-prepared-targets-from-configuration first-config nil))
            (targets-with-config-name (ci--get-prepared-targets-from-configuration first-config t)))

       (should (equal (alist-get 'name first-config) "the-config-name"))

       (should (equal (length targets-no-config-name) 4))
       (should (equal (elt targets-no-config-name 0) '("all")))
       (should (equal (elt targets-no-config-name 1) '("clean")))
       (should (equal (car (elt targets-no-config-name 2)) "somelib"))
       (should (equal (car (elt targets-no-config-name 3)) "main"))

       (should (equal (length targets-with-config-name) 4))
       (should (equal (elt targets-with-config-name 0) '("all/the-config-name")))
       (should (equal (elt targets-with-config-name 1) '("clean/the-config-name")))
       (should (equal (car (elt targets-with-config-name 2)) "somelib/the-config-name"))
       (should (equal (car (elt targets-with-config-name 3)) "main/the-config-name"))))))


(ert-deftest test-ci--add-type-field-to-target ()

  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (let* ((codemodel-file (ci--get-codemodel-reply-json-filename))
            (all-targets (ci--get-targets-from-codemodel-json-file codemodel-file))
            (somelib-target (elt all-targets 2))
            (main-target (elt all-targets 3)))
       ;; There is no type field in original target obtained with
       ;; ci--get-targets-from-codemodel-json-file
       (should (null (alist-get 'type somelib-target)))
       (should (null (alist-get 'type main-target)))

       (ci--add-type-field-to-target somelib-target)
       (ci--add-type-field-to-target main-target)

       ;; The input to ci--add-type-field-to-target is modified
       (should (not (null (alist-get 'type somelib-target))))
       (should (not (null (alist-get 'type main-target))))

       (should (equal (alist-get 'type somelib-target) "STATIC_LIBRARY"))
       (should (equal (alist-get 'type main-target) "EXECUTABLE"))))))


(ert-deftest test-ci--get-targets-from-codemodel-json-file-2 ()
  ;; ci--get-targets-from-codemodel-json-file-2 is the same as
  ;; ci--get-targets-from-codemodel-json-file, but it calls
  ;; ci--add-type-field-to-target to add a type field in each target
  (test-fixture-setup
   "./test-project-with-codemodel-reply"
   (lambda ()
     (let* ((codemodel-file (ci--get-codemodel-reply-json-filename))
            (all-targets-without-type (ci--get-targets-from-codemodel-json-file codemodel-file))
            (all-targets-with-type (ci--get-targets-from-codemodel-json-file-2 codemodel-file))
            )

       ;; Both have the same number of elements
       (should (equal (length all-targets-without-type) (length all-targets-with-type)))

       ;; First two targets are the "all" and "clean" targets
       (should (equal (elt all-targets-without-type 0) (elt all-targets-with-type 0)))
       (should (equal (elt all-targets-without-type 1) (elt all-targets-with-type 1)))

       ;; Other two elements are the "main" and "somelib" targets
       (should (not (equal (elt all-targets-without-type 2) (elt all-targets-with-type 2))))
       (should (not (equal (elt all-targets-without-type 3) (elt all-targets-with-type 3))))

       ;; If we call ci--add-type-field-to-target to add the type field to the
       ;; elements in all-targets-without-type, then they will have a type field
       ;; and they should now be equal to what we have in all-targets-with-type
       (ci--add-type-field-to-target (elt all-targets-without-type 2))
       (ci--add-type-field-to-target (elt all-targets-without-type 3))
       (should (equal (elt all-targets-without-type 2) (elt all-targets-with-type 2)))
       (should (equal (elt all-targets-without-type 3) (elt all-targets-with-type 3)))))))


;; Assert macros:
;; - should
;; - should-not
;; - should-error


(provide 'cmake-integration-tests)
;;; cmake-integration-tests.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
