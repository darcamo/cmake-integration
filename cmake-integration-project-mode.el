;;; cmake-integration-project-mode.el --- Automatically detect when in a CMake project  -*- lexical-binding: t; -*-

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

;; This module provides a minor mode that has no functionality beyond having a
;; key-map for CMake-related commands and running hooks. This gives the user
;; more flexibility when setting keybindings for CMake projects. It also
;; provides functionality to automatically detect if the current project is a
;; CMake project by looking for CMakeLists.txt files in the project root and
;; activate the minor-mode accordingly.

;;; Code:

(require 'cmake-integration-variables)


(defcustom ci-project-ignored-major-modes nil
  "List of major modes for which not to enable project mode.

When `global-cmake-integration-project-mode' is enabled and the current
project is detected as a CMake project, the
`cmake-integration-project-mode' minor mode is enabled. However, if the
current major mode is in this list, the minor mode will not be enabled."
  :type '(repeat symbol)
  :group 'cmake-integration-project)


;;;###autoload (autoload 'cmake-integration-is-cmake-project-p "cmake-integration-project-mode" nil t)
(defun ci-is-cmake-project-p ()
  "Check if the current project is a CMake project."
  (interactive)
  (if-let* ((project (project-current))
            (project-root (project-root project))
            (cmakelist-path (expand-file-name "CMakeLists.txt" project-root)))
    (file-exists-p cmakelist-path)))


(defvar ci-project-mode-map (make-sparse-keymap)
  "Keymap for `cmake-integration-project-mode'.")


;;;###autoload (autoload 'cmake-integration-project-mode "cmake-integration-project-mode" nil t)
(define-minor-mode ci-project-mode
  "A minor-mode for CMake projects.

This minor-mode does not add any functionality, but it can be used to
add keybindings to compile C++ code using cmake-integration package and
also running anything in its hook."
  :keymap
  ci-project-mode-map
  (require 'cmake-integration))


(defun ci--turn-on-project-mode-func ()
  "Turn on `cmake-integration-project-mode' in CMake projects."
  (when (and (ci-is-cmake-project-p)
             (not (member major-mode ci-project-ignored-major-modes)))
    (ci-project-mode 1)))


(define-globalized-minor-mode global-cmake-integration-project-mode
  ci-project-mode
  ci--turn-on-project-mode-func
  :group 'cmake-integration-project)


(provide 'cmake-integration-project-mode)

;;; cmake-integration-project-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
