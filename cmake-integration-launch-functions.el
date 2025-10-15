;;; cmake-integration-launch-functions.el --- Run a target (with or without debugging) -*- lexical-binding: t -*-

;;; Commentary:

;; Define different "launch functions" to run a target in different ways. These
;; functions can be assigned to `cmake-integration-program-launcher'.

;;; Code:

(require 'esh-mode)  ;; For eshell-send-input

(defun ci-default-program-launch-function (command &optional buffer-name)
  "Launch COMMAND in a compilation buffer with name BUFFER-NAME.

If BUFFER-NAME is nil, use the default compilation buffer name."
  (let ((compilation-buffer-name-function (if buffer-name
                                              (lambda (_) buffer-name)
                                            compilation-buffer-name-function)))
    (compile command)))


(defun ci-comint-program-launch-function (command &optional buffer-name)
  "Launch COMMAND in a comint buffer with name BUFFER-NAME.

If BUFFER-NAME is nil, use the default compilation buffer name."
  (let* ((buffer-name (or buffer-name "*compilation*"))
         (compilation-buffer-name-function (lambda (_) buffer-name)))
    (compile command t)
    (pop-to-buffer buffer-name)))


(defun ci-eshell-program-launch-function (command &optional buffer-name)
  "Launch COMMAND in an eshell buffer with name BUFFER-NAME.

If BUFFER-NAME is nil, use the default eshell buffer name is used."
  (let ((eshell-buffer-name (if buffer-name
                                buffer-name
                              "*eshell*")))
    (unless (get-buffer eshell-buffer-name)
      (eshell))
    (let ((eshell-buffer (get-buffer eshell-buffer-name)))
      (with-current-buffer eshell-buffer
        (goto-char (point-max))
        (insert command)
        (eshell-send-input))
      (pop-to-buffer eshell-buffer))))


(provide 'cmake-integration-launch-functions)

;;; cmake-integration-launch-functions.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
