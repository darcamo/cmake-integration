;;; cmake-integration-dape.el --- Add configuration use dape  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Darlan Cavalcante Moreira

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

;;; Code:
(defvar dape-configs)

(defun ci--get-run-arguments-as-a-vector (&optional args)
  "Get arguments to pass to the binary as a vector of strings.

If ARGS is not provided, use `cmake-integration-run-arguments'."
  (let ((args (or args cmake-integration-run-arguments)))
    (if args
        (vconcat (split-string args " " t))
      [])))


;;;###autoload (autoload 'cmake-integration-setup-dape "cmake-integration")
(defun ci-setup-dape ()
  "Setup dape configurations for Go projects."
  (interactive)
  (require 'dape)
  (add-to-list
   'dape-configs
   '(ci-debug
     modes
     (c++-mode c++-ts-mode c-mode c-ts-mode)
     command "gdb"
     command-args ("--interpreter=dap")
     command-cwd dape-command-cwd
     ;; port :autoport
     :type "debug"
     :request "launch"
     :mode "debug"
     :name "Debug current target with gdb"
     :cwd ci--get-working-directory
     :program ci-get-target-executable-full-path
     :args ci--get-run-arguments-as-a-vector))
  )


(provide 'cmake-integration-dape)
;;; cmake-integration-dape.el ends here


;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
