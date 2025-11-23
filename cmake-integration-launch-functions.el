;;; cmake-integration-launch-functions.el --- Run a target (with or without debugging) -*- lexical-binding: t -*-

;; Copyright (C) 2025 Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs

;; This file is part of cmake-integration.

;; cmake-integration is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cmake-integration is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cmake-integration. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define different "launch functions" to run or debug a target in different
;; ways.
;;
;; "Launcher functions" can be assigned to
;; `cmake-integration-program-launcher-function' and affect how the program is
;; run.
;;
;; "Debug launch functions" can be assigned to
;; `cmake-integration-debug-launcher-function' and affect how the program is run
;; in the debugger

;;; Code:

(require 'eshell)
(require 'esh-mode)  ;; For eshell-send-input

(declare-function dape "dape")


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
  (let ((eshell-buffer-name (or buffer-name "*eshell*")))
    (unless (get-buffer eshell-buffer-name)
      (eshell))
    (let ((eshell-buffer (get-buffer eshell-buffer-name)))
      (with-current-buffer eshell-buffer
        (goto-char (point-max))
        (insert command)
        (eshell-send-input))
      (pop-to-buffer eshell-buffer))))


(defun ci-default-debug-launch-function (executable-path &optional args run-dir)
  "Debug EXECUTABLE-PATH passign ARGS and in directory RUN-DIR using `gdb'.

Start debugging the executable in EXECUTABLE-PATH with gdb and pass it
the command line arguments in ARGS. The RUN-DIR is passed to gdb with
the \"--cd\" option."
  (let* ((default-directory (or run-dir default-directory))
         (gdb-command (format "gdb -i=mi --cd=%s --args %s %s"
                              default-directory
                              executable-path
                              (or args ""))))
    (gdb gdb-command)))


(defun ci-dape-debug-launch-function (executable-path &optional args run-dir)
  "Debug EXECUTABLE-PATH with dape, passign ARGS and using RUN-DIR as cwd.

Note: This is EXPERIMENTAL and has not been tested much. It may also
break in the future in case dap changes, since there is no official
documentation on how to call it from Lisp."

  (let ((default-directory (or run-dir default-directory))
        (args-vector (if args
                         (vconcat (split-string args " " t))
                       [])))
    ;; Dape documentaton does not tell us how to call it from lisp. Hence, this
    ;; could break in the future. The current approach was taken from the dape's
    ;; author information in this github issue:
    ;; https://github.com/svaante/dape/issues/193
    (dape `( command "gdb"
             command-args ("--interpreter=dap")
             command-cwd ,default-directory
             :request "launch"
             :type "debug"
             :cwd ,default-directory
             :program ,executable-path
             :args ,args-vector))))


(provide 'cmake-integration-launch-functions)

;;; cmake-integration-launch-functions.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
