;;; cmake-integration.el --- Easily configure cmake projects and run/debug targets -*- lexical-binding: t -*-

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (f "0.20.0") (s "1.12.0") (json "1.5") (dash "2.19.1"))
;; Homepage: https://github.com/darcamo/cmake-integration
;; Keywords: c c++ cmake languages tools
;; URL: https://github.com/darcamo/cmake-integration/


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Provide functions to configure cmake projects, and to compile and
;; run a given target. Completions are provided with targets obtained
;; from cmake. It also support cmake presets.

;;; Code:
(require 'project)
(require 'f)
(require 's)
(require 'json)
(require 'dash)
(require 'cl-extra)

(require 'cmake-integration-variables)
(require 'cmake-integration-core)
(require 'cmake-integration-core-presets)
(require 'cmake-integration-conan)
(require 'cmake-integration-configure)
(require 'cmake-integration-build)
(require 'cmake-integration-launch)
(require 'cmake-integration-doxygen)
(require 'cmake-integration-ctest)
(require 'cmake-integration-cpack)
(require 'cmake-integration-extra)
(require 'cmake-integration-transient)


(provide 'cmake-integration)

;;; cmake-integration.el ends here
