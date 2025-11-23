;;; cmake-integration-core.el --- Core functionality -*- lexical-binding: t -*-

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
(require 'f)
(require 'cl-extra)
(require 'json)
(require 'project)

(require 'cmake-integration-variables)


;; BUG: This function seems to work correctly, but when used as the
;; ":safe" predicate in the defcustom Emacs still asks for confirming
;; if the variable is safe for the symbol values
(defun ci--run-working-directory-p (val)
  "Check if VAL is safe as a local variable.

Function to verify is VAL is save as a value for the
`cmake-integration-run-working-directory' variable"

  (if (stringp val)
      ;; Return t if VAL is a valid subfolder of the project root
      (file-exists-p (file-name-concat (ci--get-project-root-folder) val))
    ;; Return t if VAL is one of the accepted symbols
    (pcase val
      ('bin t)
      ('build t)
      ('root t)
      (_ nil))))


(defun ci--get-project-root-folder ()
  "Get the current project root using Emacs built-in project."
  (when (project-current)
    (project-root (project-current))))


(defun ci--get-codemodel-reply-json-filename ()
  "Get the name of the json file with the targets.

This file is created by CMake's File API."
  (elt (f-glob "codemodel-v2*json" (ci--get-reply-folder)) 0))


(defun ci--change-to-absolute-filename (filename parent-folder)
  "If FILENAME is relative to PARENT-FOLDER, make it absolute.
Otherwise return it unchanged."
  (if (f-absolute-p filename)
      filename
    (f-join parent-folder filename)))


(defun ci--get-query-folder ()
  "Get the query folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (ci-get-build-folder) ".cmake/api/v1/query/client-emacs"))


(defun ci--get-reply-folder ()
  "Get the reply folder for our codemodel-v2 file with CMake's file API."
  (file-name-concat (ci-get-build-folder) ".cmake/api/v1/reply/"))


(defun ci--get-path-of-codemodel-query-file ()
  "Get the full path of the codemodel-query-file."
  (file-name-concat (ci--get-query-folder) "codemodel-v2"))


(defun ci--create-empty-codemodel-file ()
  "Create an empty codemodel query file for CMake's file API."
  ;; Only do something if the file does not exist
  (let ((query-folder (ci--get-query-folder))
        (query-file (ci--get-path-of-codemodel-query-file)))
    (unless (file-exists-p query-file)
      ;; Create the folder if it does not exists yet
      (unless (file-exists-p query-folder)
        (make-directory query-folder t))
      ;; Create the codemodel file
      (make-empty-file query-file))))


(defun ci--get-build-dir-if-set (&optional warn-if-nil)
  "Get the manually set build directory.

If it is nil and WARN-IF-NIL is set, a warning is displayed.

Note: If it not in a project, an error is always thrown."
  (let ((project-root-folder (ci--get-project-root-folder))
        (build-dir ci-build-dir))
    (if project-root-folder

        (if build-dir
            (expand-file-name build-dir project-root-folder)

          (when warn-if-nil
            (warn
             "Build folder is not set.
Call `cmake-integration-select-configure-preset' to select a configure preset,
or set `cmake-integration-build-dir' manually")))

      (error "Not in a project"))))


(defun ci-get-build-folder ()
  "Get the project build folder.

Returns the build folder path based on either the configure preset or
the manually specified `cmake-integration-build-dir'. Throws an error if
no valid build folder can be determined.

Note that the returned build folder is always an absolute path. Relative
paths from `cmake-integration-build-dir' or the active preset are
resolved against the project root."
  (let ((project-root-folder (ci--get-project-root-folder))
        (preset ci-configure-preset))
    (unless project-root-folder
      (error "Not in a project"))

    (if preset
        (if-let* ((binaryDir-with-replacements
                   (ci--get-binaryDir-with-replacements preset)))
          (expand-file-name binaryDir-with-replacements project-root-folder)
          ;; Maybe the preset or any parent preset has no binaryDir set, or
          ;; maybe a parent preset is missing. We need to warn the user and than
          ;; we try using the manually set build dir instead
          (warn
           "Could not determine build folder from preset '%s'.\n   - Maybe the preset and none of its parent presets has the 'binaryDir' field, or a parent preset is missing."
           (ci--get-preset-name preset))
          (if-let* ((build-dir (ci--get-build-dir-if-set)))
            (progn
              (warn "Using manually set build folder: '%s'" build-dir)
              build-dir)
            (error
             "Build folder could not be determined from preset '%s' and no manual build folder is set"
             (ci--get-preset-name preset))))

      ;; Use manually set build directory or throw an error
      (ci--get-build-dir-if-set t))))


(defun ci--get-build-folder-relative-to-project ()
  "Get the build folder relative to project folder."
  (let ((build-folder (ci-get-build-folder))
        (project-folder (ci--get-project-root-folder)))
    (when (and build-folder project-folder)
      (file-relative-name build-folder project-folder))))


(defun ci--get-annotation-initial-spaces (annotated-string)
  "Get a string of spaces that should be added after ANNOTATED-STRING.

The number of spaces is enough such that when added after
ANNOTATED-STRING the annotation after that starts in the column
indicated by `cmake-integration-annotation-column'."
  (make-string
   (max 1
        (- ci-annotation-column (length annotated-string)))
   ;; 32 is the space character
   32))


(provide 'cmake-integration-core)

;;; cmake-integration-core.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ci-" . "cmake-integration-"))
;; End:
