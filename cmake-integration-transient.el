;;; cmake-integration-transient.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'transient)
(require 'cmake-integration-conan)
(require 'cmake-integration-cpack)
(require 'cmake-integration-ctest)


(defun cmake-integration--display-configure-preset ()
  "Return a string with the configure preset name."
  (interactive)
  (format "Configure preset: %s" (cmake-integration--get-preset-name cmake-integration-configure-preset)))


(defun cmake-integration--display-build-preset ()
  "Return a string with the build preset name."
  (interactive)
  (format "Build preset: %s" (cmake-integration--get-preset-name cmake-integration-build-preset)))


(defun cmake-integration--display-test-preset ()
  "Return a string with the test preset name."
  (interactive)
  (format "Test preset: %s" (cmake-integration--get-preset-name cmake-integration-test-preset)))


(defun cmake-integration--display-package-preset ()
  "Return a string with the package preset name."
  (interactive)
  (format "Package preset: %s" (cmake-integration--get-preset-name cmake-integration-package-preset)))


(defun cmake-integration--get-command-line-arg-with-face (option is-set)
  "Return OPTION propertied with a face the depends on IS-SET.

If IS-SET is t, then OPTION is returned using the transient-value face.
Otherwise it is returned with the transient-inactive-value face."
  (let ((face (if is-set 'transient-value 'transient-inactive-value)))
    (propertize option 'face face)))


(defun cmake-integration--describe-preset-command-line (preset)
  "Describe command line argument to set the PRESET.

If PRESET is nil, this just return the string `--preset=' using the
`transient-inactive-value' face. If it's non-nil, then the preset name
will be obtained from PRESET and this returns the string
`--preset=<preset-name>' using the `transient-value' face."
  (let* ((preset-name (if preset (cmake-integration--get-preset-name preset) ""))
         (command-line (format "--preset=%s" preset-name)))
    (cmake-integration--get-command-line-arg-with-face command-line preset)))


(defun cmake-integration--describe-configure-preset-command-line ( )
  "Describe command line argument to set the configure preset."
  (format "Set the configure preset (%s)" (cmake-integration--describe-preset-command-line cmake-integration-configure-preset)))


(defun cmake-integration--describe-build-preset-command-line ( )
  "Describe command line argument to set the build preset."
  (format "Set the build preset (%s)"
          (cmake-integration--describe-preset-command-line cmake-integration-build-preset)))


(defun cmake-integration--describe-test-preset-command-line ( )
  "Describe command line argument to set the test preset."
  (format "Set the test preset (%s)"
          (cmake-integration--describe-preset-command-line cmake-integration-test-preset)))


(defun cmake-integration--describe-package-preset-command-line ( )
  "Describe command line argument to set the package preset."
  (format "Set the package preset (%s)"
          (cmake-integration--describe-preset-command-line cmake-integration-package-preset)))


(defun cmake-integration--describe-build-target ()
  "Describe the command line argument to set the build target."
  (let* ((target-name (if cmake-integration-current-target cmake-integration-current-target ""))
         (command-line (format "--target=%s" target-name)))
    (format "Select the build target (%s)" (cmake-integration--get-command-line-arg-with-face command-line cmake-integration-current-target))))


(defun cmake-integration--describe-conan-profile ()
  "Describe the current conan profile."
  (let* ((profile-is-string (stringp cmake-integration-conan-profile))
         (profile-name (if profile-is-string cmake-integration-conan-profile ""))
         (command-line (format "--profile=%s" profile-name)))
    (format "Select conan profile (%s)" (cmake-integration--get-command-line-arg-with-face command-line profile-is-string))))



(transient-define-suffix cmake-integration--set-configure-preset-sufix ()
  "Set configure preset."
  :transient 'transient--do-call
  :description #'cmake-integration--describe-configure-preset-command-line
  (interactive)
  (if cmake-integration-configure-preset
      (setq cmake-integration-configure-preset nil)
    (cmake-integration-select-configure-preset)))


(transient-define-suffix cmake-integration--set-build-preset-sufix ()
  "Set build preset."
  :transient 'transient--do-call
  :description #'cmake-integration--describe-build-preset-command-line
  (interactive)
  (if cmake-integration-build-preset
      (setq cmake-integration-build-preset nil)
    (cmake-integration-select-build-preset)))


(transient-define-suffix cmake-integration--set-test-preset-sufix ()
  "Set test preset."
  :transient 'transient--do-call
  :description #'cmake-integration--describe-test-preset-command-line
  (interactive)
  (if cmake-integration-test-preset
      (setq cmake-integration-test-preset nil)
    (cmake-integration-select-test-preset)))


(transient-define-suffix cmake-integration--set-package-preset-sufix ()
  "Set package preset."
  :transient 'transient--do-call
  :description #'cmake-integration--describe-package-preset-command-line
  (interactive)
  (if cmake-integration-package-preset
      (setq cmake-integration-package-preset nil)
    (cmake-integration-select-package-preset)))


(transient-define-suffix cmake-integration--set-build-target-sufix ()
  :transient 'transient--do-call
  :description 'cmake-integration--describe-build-target
  (interactive)
  (if cmake-integration-current-target
      (setq cmake-integration-current-target nil)
    (cmake-integration--select-build-target)))


(transient-define-suffix cmake-integration--set-conan-profile-sufix ()
  :transient 'transient--do-call
  :description 'cmake-integration--describe-conan-profile
  (interactive)
  (if cmake-integration-conan-profile
      (setq cmake-integration-conan-profile nil)
    (cmake-integration-select-conan-profile)))


(transient-define-prefix cmake-integration--conan-transient ()
  "Perform actions related to the conan package manager."
  ["Conan"
   ("p" cmake-integration--set-conan-profile-sufix)
   ("l" "List installed packages" cmake-integration-run-conan-list)
   ("i" "Install" (lambda () (interactive) (cmake-integration-run-conan)))
   ]
  )


(transient-define-prefix cmake-integration--configure-transient ()
  "Perform actions related to configuring the project."
  ["Configure"
   :pad-keys t
   ("f" "Removing existing cache" "--fresh" :transient t)
   ("p" cmake-integration--set-configure-preset-sufix)
   ;; ("b" "Select the build folder" (lambda ()
   ;;             (interactive)
   ;;             (message "Build folder %s selected"
   ;;                      (read-directory-name "Select build folder")))
   ;;  :transient t)
   ;; ("G" "Select the generator" (lambda () (interactive)(message "Set Generator")) :transient t)
   ""
   ("c" "Configure" (lambda () (interactive)
                      (let ((extra-args (transient-args (oref transient-current-prefix command))))
                        (cmake-integration-cmake-reconfigure extra-args)))
    :transient nil)
   ]
  )


(transient-define-prefix cmake-integration--build-transient ()
  "Performe actions related to building a target."
  ["Build"
   ("p" cmake-integration--set-build-preset-sufix)
   ("t" cmake-integration--set-build-target-sufix)
   ("j" "Number of concurrent processes to use" "--jobs=" :transient t)
   ;; ("c" "Select the configuration (only for ninja multi-config)" (lambda () (interactive) (message "Implement-me")) :transient nil)
   ("C" "Clean first" "--clean-first" :transient t)
   ("b" "Build" (lambda () (interactive)
                  (let ((extra-args (transient-args (oref transient-current-prefix command))))
                    (cmake-integration-save-and-compile-last-target extra-args))) :transient nil)
   ]
  )


(transient-define-prefix cmake-integration--test-transient ()
  "Perform actions related to running ctest."
  ["Test"
   ("p" cmake-integration--set-test-preset-sufix)
   ;; ("d" "Select test directory" cmake-integration-select-configure-preset :transient nil)
   ("f" "Run only previously failed tests" "--rerun-failed" :transient t)
   ("o" "Output anything if the test should fail." "--output-on-failure" :transient t)
   ;; ("j" "Number of test in parallel" cmake-integration-select-configure-preset :transient nil)
   ("P" "Show progress" "--progress")
   ("q" "Quiet" "--quiet" :transient t)
   ("t" "Run tests" (lambda () (interactive)
                      (let ((extra-args (transient-args (oref transient-current-prefix command))))
                        (cmake-integration-run-ctest extra-args)))
    :transient nil)
   ]
  )


(transient-define-prefix cmake-integration--install-transient ()
  "Perform actions related to installing wih cmake."
  ["Install"
   ;; ("c" "Select the configuration (only for ninja multi-config)" (lambda () (interactive) (message "Implement-me")) :transient nil)
   ("p" "Override the installation prefix" "--prefix=")
   ("s" "Strip before installing" "--strip")
   ("C" "Component" "--component=")
   ("i" "Install" (lambda () (interactive)
                    (let ((extra-args (transient-args (oref transient-current-prefix command))))
                      (cmake-integration-run-cmake-install extra-args)))
    :transient nil)
   ]
  )

;; ("-G" "Configure Generator" "--generator=" :choices ("primeira" "segunda" "terceira"))

(transient-define-prefix cmake-integration--package-transient ()
  "Perform actions related to running cpack."
  ["Package"
   ("p" cmake-integration--set-package-preset-sufix)
   ;; ("G" "Select the generator" (lambda () (interactive)(message "Set Generator")) :transient t)
   ("c" "Create package" cmake-integration-run-cpack :transient t)
   ]
  )


(transient-define-prefix cmake-integration--launch-transient ()
  "Perform actions related to running or debugging an executable target."
  ["Run and Debug"
   ("t" cmake-integration--set-build-target-sufix)
   ("r" "Run last target" cmake-integration-run-last-target)
   ("d" "Debug with gdb" cmake-integration-debug-last-target)
   ]
  )


(transient-define-prefix cmake-integration--workflow-transient ()
  "Perform actions related to running a workflow."
  ["Workflow"
   ;; choose preset -> Implement-me
   ("w" "Run workflow" (lambda () (interactive) (message "Implement-me")) :transient nil)]
  )


(transient-define-prefix cmake-integration-transient ()
  "Main transient menu, from where all other transient menus can be reached."
  [
   ["Presets"
    (:info #'cmake-integration--display-configure-preset :format " %d")
    (:info #'cmake-integration--display-build-preset :format " %d")
    (:info #'cmake-integration--display-test-preset :format " %d")
    (:info #'cmake-integration--display-package-preset :format " %d")
    ]
   ["Util"
    ("ud" "Open dired in build folder" cmake-integration-open-dired-in-build-folder)
    ("us" "Open shell in build folder" cmake-integration-open-shell-in-build-folder)]
   ]
  ["Operations"
   [("C" "Conan" cmake-integration--conan-transient)]
   [("c" "Configure" cmake-integration--configure-transient)]
   [("b" "Build" cmake-integration--build-transient)]
   [("l" "Run and Debug" cmake-integration--launch-transient)]
   [("t" "Test" cmake-integration--test-transient)]
   [("i" "Install" cmake-integration--install-transient)]
   [("p" "Package" cmake-integration--package-transient)]
   ;; [("w" "Workflow" cmake-integration--workflow-transient)]
   ]
  )

(provide 'cmake-integration-transient)

;;; cmake-integration-transient.el ends here
