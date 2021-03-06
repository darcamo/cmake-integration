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
         (expand-file-name filepath2))
  )


(defun test-fixture-setup (subfolder body)
  "Run BODY with 'default-directory' set to SUBFOLDER.

This is used in the tests definitions to make sure we run the
test code from inside a 'test project'."
  (unwind-protect
      (let ((default-directory (expand-file-name subfolder)))
        (funcall body))))


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxx Define the tests xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

;; NOTE: For this test to pass a folder with a `.project' file must be recognized as a project.
;; See https://www.reddit.com/r/emacs/comments/k3og7d/extending_projectel/
(ert-deftest test-getting-project-root-no-presets ()
  "Test getting the project root when no presets are used."
  (test-fixture-setup
   "./test-project/subfolder" ;; project root is the parent "test-project" folder
   (lambda ()
     (should (file-equal-p (cmake-integration-get-project-root-folder)
                           "..")))))


(ert-deftest test-getting-build-folder-without-presets ()
  (test-fixture-setup
   "./test-project" ;; project root is the parent "test-project" folder
   (lambda ()
     ;; Without setting `cmake-integration-build-dir' -> default value is "build"
     (should (filepath-equal-p (cmake-integration-get-build-folder) "./build"))

     ;; Set `cmake-integration-build-dir'
     (let ((cmake-integration-build-dir "some-build-folder"))
       (should (filepath-equal-p (cmake-integration-get-build-folder) "./some-build-folder"))
       ))))


(ert-deftest test-getting-build-folder-with-presets ()
  (test-fixture-setup
   "./test-project-with-presets" ;; project root is the parent "test-project" folder
   (lambda ()
     ;; Build folder is taken from the `binaryDir' field in
     ;; `cmake-integration-last-configure-preset', which is an alist
     (let ((cmake-integration-last-configure-preset '((binaryDir . "${sourceDir}/build-with-ninja-multiconfig"))))
       (should (filepath-equal-p (cmake-integration-get-build-folder) "./build-with-ninja-multiconfig")))

     (let ((cmake-integration-last-configure-preset '((binaryDir . "${sourceDir}/build-with-ninja"))))
       (should (filepath-equal-p (cmake-integration-get-build-folder) "./build-with-ninja"))))))



(ert-deftest test-cmake-integration-get-query-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration-get-query-folder) "./build/.cmake/api/v1/query/client-emacs")))))


(ert-deftest test-cmake-integration-get-reply-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration-get-reply-folder) "./build/.cmake/api/v1/reply/")))))


(ert-deftest test-cmake-integration-get-path-of-codemodel-query-file ()
  (test-fixture-setup
   "./test-project"
   (lambda () (should (filepath-equal-p (cmake-integration-get-path-of-codemodel-query-file) "./build/.cmake/api/v1/query/client-emacs/codemodel-v2")))))


(ert-deftest test-cmake-integration--get-run-command--root-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'root))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s && ~/%s arg1 arg2 arg3"
                              (file-relative-name "./" "~/")
                              (file-relative-name "./build/bin/myexec" "~/"))))))))

(ert-deftest test-cmake-integration--get-run-command--build-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'build))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s && %s arg1 arg2 arg3"
                              (file-relative-name "./build" "~/")
                              "bin/myexec")))))))


(ert-deftest test-cmake-integration--get-run-command--bin-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory 'bin))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s/ && %s arg1 arg2 arg3"
                              (file-relative-name "./build/bin" "~/")
                              "./myexec")))))))


(ert-deftest test-cmake-integration--get-run-command--custom-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2 arg3")
           (cmake-integration-run-working-directory "subfolder"))
       (should (equal (cmake-integration--get-run-command "bin/myexec")
                      (format "cd ~/%s && ~/%s arg1 arg2 arg3"
                              (file-relative-name "./subfolder" "~/")
                              (file-relative-name "./build/bin/myexec" "~/"))))))))



(ert-deftest test-cmake-integration--get-debug-command--root-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory 'root))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-current-target-run-arguments)))))))


(ert-deftest test-cmake-integration--get-debug-command--build-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory 'build))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./build" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-current-target-run-arguments)))))))


(ert-deftest test-cmake-integration--get-debug-command--bin-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory 'bin))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./build/bin/" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-current-target-run-arguments
                              )))))))


(ert-deftest test-cmake-integration--get-debug-command--custom-folder ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     (let ((cmake-integration-current-target-run-arguments "arg1 arg2")
           (cmake-integration-run-working-directory "subfolder"))
       (should (equal (cmake-integration--get-debug-command "bin/myexec")
                      (format "gdb -i=mi --cd=~/%s --args ~/%s %s"
                              (file-relative-name "./subfolder" "~/")
                              (file-relative-name "./build/bin/myexec" "~/")
                              cmake-integration-current-target-run-arguments)))))))




;;; TODO
(ert-deftest test-cmake-integration-get-cmake-targets-from-codemodel-json-file ()
  ;; (test-fixture-setup
  ;;  "./test-project-with-presets"
  ;;  (lambda ()
  ;;    (let ((targets (cmake-integration-get-cmake-targets-from-codemodel-json-file)))

  ;;      )

  ;;    )
  ;;  )
  )




;; TODO: Test with and without using presets
(ert-deftest test-cmake-integration-get-compile-command ()
  )



(ert-deftest test-cmake-integration-get-conan-run-command ()
  (test-fixture-setup
   "./test-project"
   (lambda ()
     ;; Test conan command without using a conan profile
     (let ((cmake-integration-conan-profile nil))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing"
                              (cmake-integration-get-build-folder)
                              (cmake-integration-get-project-root-folder)))))
     ;; Test conan command when using a single fixed conan profile
     (let ((cmake-integration-conan-profile "some-conan-profile"))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing --profile some-conan-profile"
                              (cmake-integration-get-build-folder)
                              (cmake-integration-get-project-root-folder)))))
     ;; Test conan command when cmake profile names are mapped to conan profile names
     (let ((cmake-integration-conan-profile '(("cmake-profile-1" . "conan-profile-1")
                                              ("cmake-profile-2" . "conan-profile-2")))
           (cmake-integration-last-configure-preset '((name . "cmake-profile-1") (binaryDir . "build")))
           )
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing --profile conan-profile-1"
                              (cmake-integration-get-build-folder)
                              (cmake-integration-get-project-root-folder))
                      ))
       ;; If we change cmake-integration-last-configure-preset the conan profile will be affected
       (setq cmake-integration-last-configure-preset '((name . "cmake-profile-2") (binaryDir . "build")))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing --profile conan-profile-2"
                              (cmake-integration-get-build-folder)
                              (cmake-integration-get-project-root-folder))
                      ))
       ;; If the cmake profile has no conan profile mapped to it, then no profile will be used
       (setq cmake-integration-last-configure-preset '((name . "cmake-profile-3") (binaryDir . "build")))
       (should (equal (cmake-integration-get-conan-run-command)
                      (format "cd %s && conan install %s --build missing"
                              (cmake-integration-get-build-folder)
                              (cmake-integration-get-project-root-folder))
                      ))))))



;; ((name . "default")
;;  (displayName . "Default build using Ninja generator")
;;  (generator . "Ninja")
;;  (binaryDir . "build")
;;  (toolchainFile . "${sourceDir}/dpc++-toolchain.cmake")
;;  (cacheVariables
;;   (CMAKE_EXPORT_COMPILE_COMMANDS . t)
;;   (CMAKE_BUILD_TYPE . "Debug")))


;; (setq cmake-integration-last-configure-preset '((name . "cmake-profile-1") (binaryDir . "build")))
;; (setq cmake-integration-conan-profile '(("cmake-profile-1" . "conan-profile-1")
;;                                               ("cmake-profile-2" . "conan-profile-2")))
;; (cmake-integration-get-conan-run-command)




;; (filepath-equal-p
;;  "~/git_files/cmake-integration/tests/test-project/build"
;;  "./test-project/build")


;; ;; Test getting the build folder
;; ;; - no presets
;; ;;   - different values of cmake-integration-build-dir
;; ;; - with different presets
;; (ert-deftest test-getting-build-folder ()
;;     (let ((default-directory (expand-file-name "./test-project/subfolder"))
;;           (cmake-integration-build-dir "some-build-folder")
;;           expected-build-folder)
;;       (setq expected-build-folder (file-name-concat (cmake-integration-get-project-root-folder) cmake-integration-build-dir))
;;     (should (file-equal-p (cmake-integration-get-build-folder) expected-build-folder))
;;       ))




;; Assert macros:
;; - should
;; - should-not
;; - should-error




(provide 'cmake-integration-tests)
;;; cmake-integration-tests.el ends here
