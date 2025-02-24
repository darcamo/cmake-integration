





(let ((all-build-presets (cmake-integration-get-all-presets-of-type 'buildPresets)))
  ;; (seq-filter #'cmake-integration--preset-has-matching-configure-preset-p all-build-presets)

  all-build-presets
  )

(let ((cmake-integration-configure-preset '((name "lalala"))))
  (cmake-integration-get-last-configure-preset-name)
  )




(((name . "default") (displayName . "Default build preset") (configurePreset . "default")) ((name . "ninjamulticonfig") (displayName . "Build preset using ninja multi-config") (configurePreset . "ninjamulticonfig") (configuration . "Release")))


