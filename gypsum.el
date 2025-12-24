;;; gypsum.el --- Generate Alabaster-style Emacs themes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David
;; URL: https://github.com/davidhmartin/gypsum
;; Version: 2.0.0
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
;; Three Workflows:
;;
;; 1. Use a curated preset directly:
;;
;;   (gypsum-generate-from-palette
;;     "my-alabaster"
;;     (gypsum-preset-get 'alabaster-light))
;;
;; 2. Transform a preset (tint or derive variant):
;;
;;   (gypsum-generate-from-palette
;;     "tinted"
;;     (gypsum-palette-tint
;;       (gypsum-preset-get 'alabaster-light)
;;       :mode 'hue-shift :degrees 30))
;;
;;   (gypsum-generate-from-palette
;;     "my-dark"
;;     (gypsum-palette-derive-dark
;;       (gypsum-preset-get 'alabaster-light)))
;;
;; 3. Generate from a seed color:
;;
;;   (gypsum-generate-from-palette
;;     "seeded"
;;     (gypsum-palette-generate "#3498DB" 'dark))
;;
;; Interactive:
;;
;;   M-x gypsum          - Guided theme creation (all 3 workflows)
;;   M-x gypsum-from-preset - Quick preset-based generation
;;   M-x gypsum-show-palette - Preview palette colors
;;
;; For more information on the philosophy behind this approach, see:
;; https://tonsky.me/blog/syntax-highlighting/

;;; Code:

(require 'gypsum-color)
(require 'gypsum-presets)
(require 'gypsum-palette)
(require 'gypsum-faces)
(require 'gypsum-generate)
(require 'gypsum-discover)
(require 'gypsum-sources)
(require 'gypsum-ui)

;;;###autoload
(defgroup gypsum nil
  "Generate Alabaster-style Emacs themes."
  :group 'faces
  :prefix "gypsum-"
  :link '(url-link :tag "GitHub" "https://github.com/davidhmartin/gypsum")
  :link '(url-link :tag "Alabaster Philosophy" "https://tonsky.me/blog/syntax-highlighting/"))

;;; --- Public API Summary ---

;; Interactive Commands:
;;   `gypsum'                    - Main interactive command (guided)
;;   `gypsum-from-source'        - Generate from preset or discovered theme
;;   `gypsum-from-preset'        - Alias for gypsum-from-source
;;   `gypsum-show-palette'       - Show palette colors
;;   `gypsum-preview-dismiss'    - Dismiss current preview
;;   `gypsum-discover-refresh'   - Refresh discovered themes cache
;;
;; Theme Sources (presets + discovered themes):
;;   `gypsum-sources-list'       - List all sources (presets + discovered)
;;   `gypsum-sources-get-palette' - Get palette from any source
;;   `gypsum-sources-preset-p'   - Check if source is a preset
;;   `gypsum-sources-discovered-p' - Check if source is discovered
;;
;; Discovery:
;;   `gypsum-discover-themes'    - Discover gypsum themes in load path
;;   `gypsum-discover-get-palette' - Get palette from discovered theme
;;   `gypsum-discover-list'      - List discovered theme names
;;
;; Presets (built-in):
;;   `gypsum-preset-get'         - Get a curated preset palette by name
;;   `gypsum-preset-list'        - List available preset names
;;
;; Palette Operations:
;;   `gypsum-palette-tint'       - Tint a palette (hue-shift, set-color, blend)
;;   `gypsum-palette-derive-dark'- Derive dark variant from light palette
;;   `gypsum-palette-derive-light' - Derive light variant from dark palette
;;   `gypsum-palette-generate'   - Generate palette from seed color
;;   `gypsum-palette-validate'   - Validate a palette has all keys
;;
;; Theme Generation:
;;   `gypsum-generate-from-palette' - Generate theme file from palette
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
