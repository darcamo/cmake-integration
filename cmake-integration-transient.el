;;; cmake-integration-transient.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'transient)
(require 'cmake-integration-conan)
(require 'cmake-integration-cpack)
(require 'cmake-integration-ctest)

;; TODO: Change function names with "sufix" to suffix

;; TODO: Add a transient for doxygen commands


;; (defun ci--display-configure-preset ()
;;   "Return a string with the configure preset name."
;;   (interactive)
;;   (format "Configure preset: %s" (ci--get-preset-name ci-configure-preset)))


;; (defun ci--display-build-preset ()
;;   "Return a string with the build preset name."
;;   (interactive)
;;   (format "Build preset: %s" (ci--get-preset-name ci-build-preset)))


;; (defun ci--display-test-preset ()
;;   "Return a string with the test preset name."
;;   (interactive)
;;   (format "Test preset: %s" (ci--get-preset-name ci-test-preset)))


;; (defun ci--display-package-preset ()
;;   "Return a string with the package preset name."
;;   (interactive)
;;   (format "Package preset: %s" (ci--get-preset-name ci-package-preset)))


(defun ci--get-command-line-arg-with-face (option is-set)
  "Return OPTION propertied with a face the depends on IS-SET.

If IS-SET is t, then OPTION is returned using the transient-value face.
Otherwise it is returned with the transient-inactive-value face."
  (let ((face (if is-set 'transient-value 'transient-inactive-value)))
    (propertize option 'face face)))


(defun ci--describe-preset-command-line (preset)
  "Describe command line argument to set the PRESET.

If PRESET is nil, this just return the string `--preset=' using the
`transient-inactive-value' face. If it's non-nil, then the preset name
will be obtained from PRESET and this returns the string
`--preset=<preset-name>' using the `transient-value' face."
  (let* ((preset-name (if preset (ci--get-preset-name preset) ""))
         (command-line (format "--preset=%s" preset-name)))
    (ci--get-command-line-arg-with-face command-line preset)))


(defun ci--describe-configure-preset-command-line ( )
  "Describe command line argument to set the configure preset."
  (format "Configure preset (%s)" (ci--describe-preset-command-line ci-configure-preset)))


(defun ci--describe-build-preset-command-line ( )
  "Describe command line argument to set the build preset."
  (format "Build preset (%s)"
          (ci--describe-preset-command-line ci-build-preset)))


(defun ci--describe-test-preset-command-line ( )
  "Describe command line argument to set the test preset."
  (format "Test preset (%s)"
          (ci--describe-preset-command-line ci-test-preset)))


(defun ci--describe-package-preset-command-line ( )
  "Describe command line argument to set the package preset."
  (format "Package preset (%s)"
          (ci--describe-preset-command-line ci-package-preset)))


(defun ci--describe-build-target ()
  "Describe the command line argument to set the build target."
  (let* ((target-name (if ci-current-target ci-current-target ""))
         (command-line (format "--target=%s" target-name)))
    (format "Build target (%s)" (ci--get-command-line-arg-with-face command-line ci-current-target))))


(defun ci--describe-conan-profile ()
  "Describe the current conan profile."
  (let* ((profile-is-string (stringp ci-conan-profile))
         (profile-name (if profile-is-string ci-conan-profile ""))
         (command-line (format "--profile=%s" profile-name)))
    (format "Select conan profile (%s)" (ci--get-command-line-arg-with-face command-line profile-is-string))))


(defun ci--describe-install-prefix ()
  "Describe the current cmake install prefix."
  (let* ((has-value (stringp ci-install-prefix))
         (install-prefix (if has-value ci-install-prefix ""))
         (command-line (format "--prefix=%s" install-prefix)))
    (format "Select install prefix (%s)"
            (ci--get-command-line-arg-with-face command-line has-value))))


(transient-define-suffix ci--set-configure-preset-sufix ()
  "Set configure preset."
  :transient 'transient--do-call
  :description #'ci--describe-configure-preset-command-line
  (interactive)
  (if ci-configure-preset
      (setq ci-configure-preset nil)
    (ci-select-configure-preset)))


(transient-define-suffix ci--set-build-preset-sufix ()
  "Set build preset."
  :transient 'transient--do-call
  :description #'ci--describe-build-preset-command-line
  (interactive)
  (if ci-build-preset
      (setq ci-build-preset nil)
    (ci-select-build-preset)))


(transient-define-suffix ci--set-test-preset-sufix ()
  "Set test preset."
  :transient 'transient--do-call
  :description #'ci--describe-test-preset-command-line
  (interactive)
  (if ci-test-preset
      (setq ci-test-preset nil)
    (ci-select-test-preset)))


(transient-define-suffix ci--set-package-preset-sufix ()
  "Set package preset."
  :transient 'transient--do-call
  :description #'ci--describe-package-preset-command-line
  (interactive)
  (if ci-package-preset
      (setq ci-package-preset nil)
    (ci-select-package-preset)))


(transient-define-suffix ci--set-build-target-sufix ()
  :transient 'transient--do-call
  :description 'ci--describe-build-target
  (interactive)
  (if ci-current-target
      (setq ci-current-target nil)
    (ci--select-build-target)))


(transient-define-suffix ci--set-conan-profile-sufix ()
  :transient 'transient--do-call
  :description 'ci--describe-conan-profile
  (interactive)
  (if ci-conan-profile
      (setq ci-conan-profile nil)
    (ci-select-conan-profile)))


(transient-define-suffix ci--set-install-prefix-sufix ()
  :transient 'transient--do-call
  :description 'ci--describe-install-prefix
  (interactive)
  (if ci-install-prefix
      (setq ci-install-prefix nil)
    (ci-set-install-prefix)))


(transient-define-prefix ci--all-presetts-transient ()
  "Easily set any o the possible cmake presets."
  ["Set Presets"
   ("sc" ci--set-configure-preset-sufix)
   ("sb" ci--set-build-preset-sufix)
   ("st" ci--set-test-preset-sufix)
   ("sp" ci--set-package-preset-sufix)
   ("q" "Quit" (lambda () (interactive) (transient-quit-seq)))
   ]
  )


(transient-define-prefix ci--conan-transient ()
  "Perform actions related to the conan package manager."
  ["Conan"
   ("p" ci--set-conan-profile-sufix)
   ;; ("l" "List installed packages" ci-run-conan-list)
   ("l" "List installed packages" ci-view-conan-list-as-table)
   ("i" "Install" (lambda () (interactive) (ci-run-conan)))
   ]
  )


(transient-define-prefix ci--configure-transient ()
  "Perform actions related to configuring the project."
  ["Configure"
   :pad-keys t
   ("f" "Removing existing cache" "--fresh" :transient t)
   ("p" ci--set-configure-preset-sufix)
   ;; ("b" "Select the build folder" (lambda ()
   ;;             (interactive)
   ;;             (message "Build folder %s selected"
   ;;                      (read-directory-name "Select build folder")))
   ;;  :transient t)
   ;; ("G" "Select the generator" (lambda () (interactive)(message "Set Generator")) :transient t)
   ""
   ("c" "Configure" (lambda () (interactive)
                      (let ((extra-args (transient-args (oref transient-current-prefix command))))
                        (ci-cmake-reconfigure extra-args)))
    :transient nil)
   ]
  )


(transient-define-prefix ci--build-transient ()
  "Performe actions related to building a target."
  ["Build"
   ("p" ci--set-build-preset-sufix)
   ("t" ci--set-build-target-sufix)
   ("j" "Number of concurrent processes to use" "--jobs=" :transient t)
   ;; ("c" "Select the configuration (only for ninja multi-config)" (lambda () (interactive) (message "Implement-me")) :transient nil)
   ("C" "Clean first" "--clean-first" :transient t)
   ("b" "Build" (lambda () (interactive)
                  (let ((extra-args (transient-args (oref transient-current-prefix command))))
                    (ci-save-and-compile-last-target extra-args))) :transient nil)
   ]
  )


(transient-define-prefix ci--test-transient ()
  "Perform actions related to running ctest."
  ["Test"
   ("p" ci--set-test-preset-sufix)
   ;; ("d" "Select test directory" ci-select-configure-preset :transient nil)
   ("f" "Run only previously failed tests" "--rerun-failed" :transient t)
   ("o" "Output anything if the test should fail." "--output-on-failure" :transient t)
   ;; ("j" "Number of test in parallel" ci-select-configure-preset :transient nil)
   ("P" "Show progress" "--progress")
   ("q" "Quiet" "--quiet" :transient t)
   ("t" "Run tests" (lambda () (interactive)
                      (let ((extra-args (transient-args (oref transient-current-prefix command))))
                        (ci-run-ctest extra-args)))
    :transient nil)
   ]
  )


(transient-define-prefix ci--install-transient ()
  "Perform actions related to installing wih cmake."
  ["Install"
   ;; ("c" "Select the configuration (only for ninja multi-config)" (lambda () (interactive) (message "Implement-me")) :transient nil)
   ("p" ci--set-install-prefix-sufix)
   ("s" "Strip before installing" "--strip")
   ("C" "Component" "--component=")
   ("i" "Install" (lambda () (interactive)
                    (let ((extra-args (transient-args (oref transient-current-prefix command))))
                      (ci-run-cmake-install extra-args)))
    :transient nil)
   ]
  )

;; ("-G" "Configure Generator" "--generator=" :choices ("primeira" "segunda" "terceira"))

(transient-define-prefix ci--package-transient ()
  "Perform actions related to running cpack."
  ["Package"
   ("p" ci--set-package-preset-sufix)
   ;; ("G" "Select the generator" (lambda () (interactive)(message "Set Generator")) :transient t)
   ("c" "Create package" ci-run-cpack :transient t)
   ]
  )


(transient-define-prefix ci--launch-transient ()
  "Perform actions related to running or debugging an executable target."
  ["Run and Debug"
   ("a" "Set runtime arguments" ci--set-runtime-arguments :transient t)
   ("t" ci--set-build-target-sufix)
   ("r" "Run last target" ci-run-last-target)
   ("d" "Debug with gdb" ci-debug-last-target)
   ]
  )


(transient-define-prefix ci--workflow-transient ()
  "Perform actions related to running a workflow."
  ["Workflow"
   ;; choose preset -> Implement-me
   ("w" "Run workflow" (lambda () (interactive) (message "Implement-me")) :transient nil)]
  )


(transient-define-prefix ci-transient ()
  "Main transient menu, from where all other transient menus can be reached."
  [
   ["Presets"
    ;; (:info #'ci--display-configure-preset :format " %d")
    ;; (:info #'ci--display-build-preset :format " %d")
    ;; (:info #'ci--display-test-preset :format " %d")
    ;; (:info #'ci--display-package-preset :format " %d")
    ("sc" ci--set-configure-preset-sufix)
    ("sb" ci--set-build-preset-sufix)
    ("st" ci--set-test-preset-sufix)
    ("sp" ci--set-package-preset-sufix)
    ;; ("s" "Set any of the presets" ci--all-presetts-transient)
    ]
   ["Util"
    ("ud" "Dired in build folder" ci-open-dired-in-build-folder)
    ("us" "Shell in build folder" ci-open-shell-in-build-folder)
    ("ur" "Remove the build folder" ci-delete-build-folder :transient transient--do-call)]
   ["Target"
    ("t" ci--set-build-target-sufix)
    ]
   ]
  ["Operations"
   [("oo" "Conan" ci--conan-transient)]
   [("oc" "Configure" ci--configure-transient)]
   [("ob" "Build" ci--build-transient)]
   [("ol" "Run and Debug" ci--launch-transient)]
   [("ot" "Test" ci--test-transient)]
   [("oi" "Install" ci--install-transient)]
   [("op" "Package" ci--package-transient)]
   ;; [("w" "Workflow" ci--workflow-transient)]
   ]
  )

(provide 'cmake-integration-transient)

;;; cmake-integration-transient.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
