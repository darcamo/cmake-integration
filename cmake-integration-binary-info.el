;;; cmake-integration-binary-info.el --- Get information about target binaries -*- lexical-binding: t -*-

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

;;; Code:

(require 'cmake-integration-launch)

;; TODO Get information from a target binary
;; - RPATH or RUNPATH
;; - SONAME for libraries
;; - Dependencies (other libraries)
;; - Symbols (exported, imported, undefined)
;; - Build ID
;; - Debug info (if any)
;; - Whether it is stripped or not
;; - Whether it is a PIE or not
;; - Whether it is statically or dynamically linked
;; - Whether it is a shared library or not
;; - Whether it is an executable or not
;; - Whether it is a position-independent executable or not
;; - Whether it is a relocatable or not


(defun ci-get-binary-runpath (binary-path)
  "Get the RUNPATH of the binary in BINARY-PATH."
  (let ((binary-path (expand-file-name binary-path)))
    (if (not (file-exists-p binary-path))
        (message "Binary file does not exist: %s" binary-path)
      (with-temp-buffer
        (let ((exit-code (call-process "readelf" nil t nil "-d" binary-path)))
          (if (/= exit-code 0)
              (message "Failed to run readelf on %s" binary-path)
            (goto-char (point-min))
            (let ((result))
              (if (re-search-forward "RUNPATH.*\\[\\(.*?\\)\\]" nil t)
                  (setq result (match-string 1))
                (if (re-search-forward "RPATH.*\\[\\(.*?\\)\\]" nil t)
                    (setq result (match-string 1))
                  nil))
              (when result
                (split-string result ":")))))))))


(defun ci-get-current-target-runpath ()
  "Get the RUNPATH of the binary for the current target."
  (interactive)
  (ci-get-binary-runpath (ci-get-target-executable-full-path)))


(defun ci-get-library-soname (library-path)
  "Get the SONAME of the shared library in LIBRARY-PATH."
  (interactive)
  (let ((library-path (expand-file-name library-path)))
    (if (not (file-exists-p library-path))
        (message "Library file does not exist: %s" library-path)
      (with-temp-buffer
        (let ((exit-code (call-process "readelf" nil t nil "-d" library-path)))
          (if (/= exit-code 0)
              (message "Failed to run readelf on %s" library-path)
            (goto-char (point-min))
            (if (re-search-forward "SONAME.*\\[\\(.*?\\)\\]" nil t)
                (match-string 1))))))))



;;; cmake-integration-binary-info.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
