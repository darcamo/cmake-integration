;;; cmake-integration-cpack.el --- CPack functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'tramp)

;; TODO: Add option to specify components -> use completing-read-multiple

;; TODO: Allow specifying other arguments to "cmake --install", such as
;; "--config", "--component" and "--strip".

(defun ci-get-last-package-preset-name ()
  "Get the `name' field of the last preset used for package."
  (ci--get-preset-name ci-package-preset))


(defun ci--get-install-command (&optional extra-args)
  "Get the command to run cmake install passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (let ((build-folder (ci-get-build-folder))
        (extra-args-string (string-join extra-args " ")))
    (format "cmake --install %s %s" (tramp-file-local-name build-folder) extra-args-string)))


(defun ci-set-install-prefix ()
  "Ask the user for a install prefix."
  (interactive)
  (let ((dir (read-directory-name "Enter install prefix directory: ")))
    ;; If we are in a TRAMP buffer, convert the directory to a local path, since
    ;; that's what we will need to pass to cmake.
    (setq ci-install-prefix (tramp-file-local-name dir))))


;;;###autoload (autoload 'cmake-integration-run-cmake-install "cmake-integration")
(defun ci-run-cmake-install (&optional extra-args)
  "Run `cmake --install' in the current build folder passing EXTRA-ARGS.

EXTRA-ARGS must be a list of strings. These strings will be concatenated
with a space as separator and the result string will be appended to the
cmake command."
  (interactive)

  (when ci-install-prefix
    (push (format "--prefix %s" ci-install-prefix) extra-args))

  (compile (ci--get-install-command extra-args)))


(defun ci--get-cpack-command (&optional extra-args)
  "Return a list (RUN-DIR CMD) to run cpack with EXTRA-ARGS.

RUN-DIR is the directory where the command should be executed. CMD is
the cpack command string to be executed in RUN-DIR."
  (let* ((preset-name (ci-get-last-package-preset-name))
         (project-folder (ci--get-project-root-folder))
         (build-folder (ci-get-build-folder))
         (run-dir (if preset-name project-folder build-folder))
         (extra-args-string (string-join extra-args " "))
         (cmd (if preset-name
                  (format "cpack --preset %s %s" preset-name extra-args-string)
                (format "cpack . %s" extra-args-string))))
    (list run-dir cmd)))


;;;###autoload (autoload 'cmake-integration-run-cpack "cmake-integration")
(defun ci-run-cpack (&optional extra-args)
  "Run cpack passing EXTRA-ARGS."
  (interactive)
  (pcase-let ((`(,run-dir ,cmd) (ci--get-cpack-command extra-args)))
    (let ((default-directory run-dir))
      (compile cmd))))


;;;###autoload (autoload 'cmake-integration-get-package-presets "cmake-integration")
(defun ci-get-package-presets (&optional configure-preset)
  "Get the package presets associated with CONFIGURE-PRESET.

Get the package presets in both `CMakePresets.json' and
`CMakeUserPresets.json' files as well as in any included files whose
configure preset is CONFIGURE-PRESET. If CONFIGURE-PRESET is not
provided, then the value in the `cmake-integration-configure-preset'
variable will be used."
  (ci-get-presets-of-type 'packagePresets configure-preset))


;;;###autoload (autoload 'cmake-integration-select-package-preset "cmake-integration")
(defun ci-select-package-preset ()
  "Select a package preset for CMake."
  (interactive)
  (when (not ci-configure-preset)
    (error "Please, select a configure preset first"))

  (let ((all-presets (ci-get-package-presets)))
    (setq ci-package-preset
          (ci-select-preset all-presets "Package preset: "))))


(defun ci--adjust-package-preset ()
  "Adjust the package preset when changing the configure preset.

This function is added to `cmake-integration-after-set-configure-preset-hook'."
  (if ci-configure-preset
      (let ((presets (ci-get-package-presets)))
        ;; Only change the package preset if there is excactly one
        ;; package preset for the conffigure preset
        (if (= (length presets) 1)
            (setq ci-package-preset (car presets))
          (setq ci-package-preset nil)))
    (setq ci-package-preset nil)))


(add-hook 'ci-after-set-configure-preset-hook 'ci--adjust-package-preset)



(provide 'cmake-integration-cpack)

;;; cmake-integration-cpack.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
