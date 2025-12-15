;;; cmake-integration.el --- Easily configure cmake projects and run/debug targets -*- lexical-binding: t -*-

;; Copyright (C) 2025 Darlan Cavalcante Moreira

;; Author: Darlan Cavalcante Moreira <darcamo@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (f "0.20.0") (s "1.12.0") (json "1.5") (dash "2.19.1") (tablist "1.1"))
;; Homepage: https://github.com/darcamo/cmake-integration
;; Keywords: c c++ cmake languages tools

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

;; cmake-integration provides functions to configure cmake projects, and to
;; compile and run a given target. Completions are provided with targets
;; obtained from cmake using cmake file API
;; <https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html>. It also
;; supports cmake presets.

;;; Code:

(require 'cmake-integration-variables)
(require 'cmake-integration-core)
(require 'cmake-integration-core-presets)
(require 'cmake-integration-conan)
(require 'cmake-integration-configure)
(require 'cmake-integration-build)
(require 'cmake-integration-launch)
(require 'cmake-integration-launch-functions)
(require 'cmake-integration-doxygen)
(require 'cmake-integration-ctest)
(require 'cmake-integration-cpack)
(require 'cmake-integration-extra)
(require 'cmake-integration-transient)
(require 'cmake-integration-persistence)
(require 'cmake-integration-project-mode)
(require 'cmake-integration-language-servers)

(provide 'cmake-integration)

;;; cmake-integration.el ends here
