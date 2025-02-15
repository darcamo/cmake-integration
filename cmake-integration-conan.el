;;; cmake-integration-conan.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cmake-integration-build)


(defun cmake-integration--get-conan-run-command (&optional profile)
  "Get the command to run `conan install' using PROFILE.

The command is run from the build folder of the current cmake
configuration."
  (if profile
      (format "%s --profile %s" (cmake-integration--get-conan-run-command) profile)
    (format "cd %s && conan install %s %s"
            (cmake-integration-get-build-folder)
            (cmake-integration--get-project-root-folder)
            cmake-integration-conan-arguments)))


(defun cmake-integration-get-conan-run-command ()
  "Get the command to run `conan install'.

The command is run from the build folder of the current cmake
configuration and the profile passed to conan is taken from the
`cmake-integration-conan-profile' variable."
  (if cmake-integration-conan-profile
      ;; If cmake-integration-conan-profile is set, it can be either a string with a single profile, or an alist mapping cmake profile names to conan profile names
      (if (stringp cmake-integration-conan-profile)
          (cmake-integration--get-conan-run-command cmake-integration-conan-profile)
        ;; Map cmake profile name to conan profile name
        (let* ((cmake-profile-name (alist-get 'name cmake-integration-configure-preset))
               (conan-profile-name (alist-get cmake-profile-name cmake-integration-conan-profile nil nil 'equal)))
          ;; Note that if we have no conan profile name in the alist
          ;; for the current cmake profile nane, then
          ;; `conan-profile-name' is nil and no profile will be used
          (cmake-integration--get-conan-run-command conan-profile-name)))

    ;; If cmake-integration-conan-profile is not set we get the conan command without using a profile
    (cmake-integration--get-conan-run-command nil)))


;;;###autoload
(defun cmake-integration-run-conan ()
  "Run conan install in the current build folder."
  (compile (cmake-integration-get-conan-run-command)))



(provide 'cmake-integration-conan)

;;; cmake-integration-conan.el ends here
