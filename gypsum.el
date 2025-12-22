;;; gypsum.el --- Generate Alabaster-style Emacs themes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Gypsum Contributors
;; URL: https://github.com/davidhmartin/gypsum
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes, tools

;; This file is not part of GNU Emacs.

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

;; Gypsum is a generator for Emacs themes following Nikita Tonsky's
;; Alabaster/minimal highlighting philosophy.
;;
;; The core principle: "If everything is highlighted, nothing is highlighted."
;;
;; Gypsum themes only highlight FOUR semantic categories:
;; 1. Strings - including regexps and symbols
;; 2. Constants - numbers, booleans, language constants
;; 3. Comments - prominently, NOT dimmed (if someone wrote it, read it!)
;; 4. Definitions - function/class/variable definitions
;;
;; Everything else (keywords, operators, variable uses, function calls)
;; uses the default foreground color.  This is intentional.
;;
;; Usage:
;;
;;   ;; Interactive - guided theme creation
;;   M-x gypsum-generate-theme
;;
;;   ;; Programmatic - minimal (seed only)
;;   (gypsum-generate "my-theme" :seed "#3498db" :variant 'dark)
;;
;;   ;; Programmatic - with overrides
;;   (gypsum-generate "my-theme"
;;     :seed "#3498db"
;;     :variant 'dark
;;     :comment "#FFCC00")
;;
;;   ;; Programmatic - fully explicit
;;   (gypsum-generate "my-theme"
;;     :background "#0E1415"
;;     :string "#99C592"
;;     :constant "#9999FF"
;;     :comment "#DFDF8E"
;;     :definition "#71ADE7")
;;
;;   ;; Preview without saving
;;   (gypsum-preview :seed "#3498db" :variant 'dark)
;;
;;   ;; Show palette preview
;;   M-x gypsum-show-palette
;;
;; For more information on the philosophy behind this approach, see:
;; https://tonsky.me/blog/syntax-highlighting/

;;; Code:

(require 'gypsum-color)
(require 'gypsum-palette)
(require 'gypsum-faces)
(require 'gypsum-generate)
(require 'gypsum-ui)

;;;###autoload
(defgroup gypsum nil
  "Generate Alabaster-style Emacs themes."
  :group 'faces
  :prefix "gypsum-"
  :link '(url-link :tag "GitHub" "https://github.com/davidhmartin/gypsum")
  :link '(url-link :tag "Alabaster Philosophy" "https://tonsky.me/blog/syntax-highlighting/"))

;;; --- Public API Summary ---

;; Theme Generation:
;;   `gypsum'                    - Main interactive command (guided)
;;   `gypsum-generate'           - Generate a theme file (programmatic)
;;   `gypsum-generate-all'       - Generate all 4 variants (programmatic)
;;
;; Preview:
;;   `gypsum-preview-dismiss'    - Dismiss current preview
;;   `gypsum-show-palette'       - Show palette colors for a seed
;;
;; Palette Creation:
;;   `gypsum-palette-create'     - Create a palette programmatically
;;
;; Color Manipulation:
;;   `gypsum-color-rotate'       - Rotate hue
;;   `gypsum-color-lighten'      - Lighten a color
;;   `gypsum-color-darken'       - Darken a color
;;   `gypsum-color-saturate'     - Increase saturation
;;   `gypsum-color-desaturate'   - Decrease saturation
;;   `gypsum-color-blend'        - Blend two colors
;;   `gypsum-color-contrast'     - Calculate contrast ratio
;;   `gypsum-color-luminance'    - Calculate luminance

(provide 'gypsum)

;;; gypsum.el ends here
