;;; cmake-integration-transient.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:



;; (transient-define-argument tsc--exclusive-switches ()
;;     "This is a specialized infix for only selecting one of several values."
;;     :class 'transient-switches
;;     :argument-format "--%s-snowcone"
;;     :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
;;     :choices '("grape" "orange" "cherry" "lime"))


;; Variáveis de interesse
;; - transient-current-prefix
;; - transient-current-command
;; - transient-current-suffixes



;; Veja o link abaixo para um exemplo de como ler de uma variável
;; [[file:~/git_files/transient-showcase/transient-showcase.org::*Lisp Variables][Lisp Variables]]


;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxx Preset display functions xxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


;; (defun cmake-integration--get-preset-names-in-list (list-of-presets)
;;   "Get all preset names for the presets in LIST-OF-PRESETS."
;;   (mapcar 'cmake-integration--get-preset-name list-of-presets))


;; (defun cmake-integration--get-configure-preset-names ()
;;   "Get all configure preset names."
;;   (cmake-integration--get-preset-names-in-list (cmake-integration-get-configure-presets)))


;; (defun cmake-integration--get-build-preset-names ()
;;   "Get all build preset names."
;;   (cmake-integration--get-preset-names-in-list (cmake-integration-get-build-presets)))

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
  (cmake-integration-select-configure-preset)
  )


(transient-define-suffix cmake-integration--set-build-preset-sufix ()
  "Set build preset."
  :transient 'transient--do-call
  :description #'cmake-integration--describe-build-preset-command-line
  (interactive)
  (cmake-integration-select-build-preset)
  )


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


;; (transient-define-suffix darlan-sufix (the-prefix-arg)
;;     "Report the PREFIX-ARG, prefix's scope, and infix values."
;;     :transient 'transient--do-call
;;     :description (lambda () (format "Eita hiha %s" (cmake-integration--get-preset-name cmake-integration-configure-preset)))
;;     (interactive "P")
;;     (let ((args (transient-args (oref transient-current-prefix command)))
;;           (scope (transient-scope)))
;;       (message "prefix-arg: %S - prefix's scope value: %S - transient-args: %S"
;;                the-prefix-arg scope args)
;;       ))


;; (transient-define-prefix darlan-prefix ()
;;   ["The prefix title"
;;    ("-f" "Removing existing cache" "--fresh" :transient t)
;;    ("-G" "Configure Generator" "--generator=" :choices ("primeira" "segunda" "terceira"))
;;    ("pc" cmake-integration--set-configure-preset-sufix)
;;    ("pb" cmake-integration--set-build-preset-sufix)
;;    ("c" darlan-sufix)
;;    ]
;;   )


(transient-define-prefix cmake-integration--conan-transient ()
  ["Conan"
   ("p" cmake-integration--set-conan-profile-sufix)
   ("l" "List installed packages" cmake-integration-run-conan-list)
   ("i" "Install" (lambda () (interactive) (cmake-integration-run-conan)))
   ]
  )


(transient-define-prefix cmake-integration--configure-transient ()
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
                        (message (format "DARLAN extra-args: %s" extra-args))
                        (cmake-integration-cmake-reconfigure extra-args)
                        )

                     ;; (let ((scope (transient-scope)))
                     ;;   (message "scope: %s" scope))
                     ) :transient nil)
   ]
  )


(transient-define-prefix cmake-integration--build-transient ()
  ["Build"
    ("p" cmake-integration--set-build-preset-sufix)
    ("t" cmake-integration--set-build-target-sufix)
    ;; ("j" "Number of jobs" cmake-integration-select-configure-preset :transient nil)
    ;; ("c" "Select the configuration (only for ninja multi-config)" cmake-integration-select-configure-preset :transient nil)
    ("C" "Clean first" "--clean-first" :transient t)
    ("b" "Build" (lambda () (interactive)
                   (let ((extra-args (transient-args (oref transient-current-prefix command))))
                     (cmake-integration-save-and-compile-last-target extra-args)
                     )

                     ;; (let ((scope (transient-scope)))
                     ;;   (message "scope: %s" scope))
                     ) :transient nil)
  ]
  )


(transient-define-prefix cmake-integration--test-transient ()
  ["Test"
    ("p" "Select the test preset" cmake-integration-select-configure-preset :transient nil)
    ("d" "Select test directory" cmake-integration-select-configure-preset :transient nil)
    ("f" "Re-run failed" cmake-integration-select-configure-preset :transient nil)
    ("j" "Number of test in parallel" cmake-integration-select-configure-preset :transient nil)
    ("P" "Show progress" cmake-integration-select-configure-preset :transient nil)
    ("q" "Quiet" cmake-integration-select-configure-preset :transient nil)
    ("t" "Run tests" cmake-integration-select-configure-preset :transient nil)
    ]
  )


(transient-define-prefix cmake-integration--install-transient ()
  ["Install"
   ("c" "Select the configuration (only for ninja multi-config)" cmake-integration-select-configure-preset :transient nil)
   ("p" "Install prefix" cmake-integration-select-configure-preset :transient nil)
   ("s" "Strip" cmake-integration-select-configure-preset :transient nil)
   ("C" "Component" cmake-integration-select-configure-preset :transient nil)

   ]
  )


(transient-define-prefix cmake-integration--package-transient ()
  ["Package"
   ("p" "Select the test preset" cmake-integration-select-configure-preset :transient nil)
   ("G" "Select the generator" (lambda () (interactive)(message "Set Generator")) :transient t)
   ("c" "Create package" (lambda () (interactive)(message "Set Generator")) :transient t)
   ]
  )


(transient-define-prefix cmake-integration--workflow-transient ()
  ["Workflow"
   ("wi" "lele" cmake-integration-select-configure-preset :transient nil)]
  )


(transient-define-prefix cmake-integration-transient ()
  [
   ["Presets"
    (:info #'cmake-integration--display-configure-preset :format " %d")
    (:info #'cmake-integration--display-build-preset :format " %d")
    (:info #'cmake-integration--display-test-preset :format " %d")
    (:info #'cmake-integration--display-package-preset :format " %d")
    ]
   ["Util"
    ("ed" "Open dired in build folder" cmake-integration-open-dired-in-build-folder)
    ("es" "Open shell in build folder" cmake-integration-open-shell-in-build-folder)]
   ]
  ["Operations"
   [("C" "Conan" cmake-integration--conan-transient)]
   [("c" "Configure" cmake-integration--configure-transient)]
   [("b" "Build" cmake-integration--build-transient)]
   [("t" "Test" cmake-integration--test-transient)]
   [("i" "Install" cmake-integration--install-transient)]
   [("p" "Package" cmake-integration--package-transient)]
   [("w" "Workflow" cmake-integration--workflow-transient)]
   ]
  )


;; (cmake-integration-transient)

(provide 'cmake-integration-transient)

;;; cmake-integration-transient.el ends here
