;;; run-tests.el --- Run all the tests  -*- lexical-binding: t; -*-

;; (setq ert-batch-backtrace-right-margin 120)
;; (setq ert-batch-print-level 50)

;; (setq test-folder default-directory)
(setq test-folder (file-name-directory load-file-name))
(setq cmake-integration-package-folder (expand-file-name "../" test-folder))
;; Parent folder of the cmake-integration-package-folder. We assume others
;; packages are in folders located in this same parent folder
(setq packages-cache-folder (expand-file-name "../" cmake-integration-package-folder))


;; Add some dependency folders to load-path (we assume all of them are in packages-cache-folder)
(add-to-list 'load-path (file-name-concat packages-cache-folder "f/"))
(add-to-list 'load-path (file-name-concat packages-cache-folder "s/"))
(add-to-list 'load-path (file-name-concat packages-cache-folder "dash/"))
(add-to-list 'load-path (file-name-concat packages-cache-folder "tablist/"))
(add-to-list 'load-path cmake-integration-package-folder)


(message "test-folder: %s" test-folder)
(message "cmake-integration-package-folder: %s" cmake-integration-package-folder)


;; Load the cmake-integration package and the tests
(load-file (file-name-concat cmake-integration-package-folder "cmake-integration.el"))
(load-file (file-name-concat cmake-integration-package-folder "cmake-integration-variables.el"))
(load-file (file-name-concat test-folder "cmake-integration-tests.el"))


;; Make Emacs recognize folders cointaining a ".project" file as a project
(add-to-list 'project-vc-extra-root-markers ".project")


;; Run the tests If no argument was provided to the run-tests script in the
;; command line, run all tests. If arguments were provided, only use the first
;; one and assumes it's the name of a single test that should run
(if command-line-args-left
    (ert-run-tests-batch-and-exit (car command-line-args-left))
  (ert-run-tests-batch-and-exit t))

;; Note: You can run this script in batch mode with the command below
;;     emacs -Q --batch -l run-tests.el
;; or
;;     emacs -Q --batch -l run-tests.el some-test-name-or-pattern
