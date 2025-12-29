;;; gypsum-presets.el --- Curated color palettes for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David Martin
;; Keywords: faces, theme

;; This file is part of Gypsum.

;;; Commentary:

;; This file contains curated color palettes that follow Nikita Tonsky's
;; Alabaster design principles.  Each palette provides hand-picked colors
;; for the four semantic categories:
;;
;; 1. Strings (green-ish) - string literals, regexps, symbols
;; 2. Constants (magenta-ish) - numbers, booleans, language constants
;; 3. Comments (red-ish) - comments are PROMINENT, not dimmed
;; 4. Definitions (blue-ish) - function/class/variable definitions
;;
;; Every palette has both light and dark variants.
;;
;; Users can:
;; - Use these palettes directly
;; - Tint them with hue shifts, blending, or custom definition colors
;; - Use them as a basis for seed-based generation

;;; Code:

(require 'gypsum-color)

;;; Palette structure
;;
;; Each palette is a plist with the following keys:
;;
;; Required:
;;   :name        - Symbol identifying the palette
;;   :variant     - 'light or 'dark
;;
;; Semantic colors (the 4 Alabaster categories):
;;   :string      - Strings, regexps, symbols
;;   :constant    - Numbers, booleans, builtins
;;   :comment     - Comments (prominent!)
;;   :definition  - Function/class/variable definitions
;;
;; Base colors:
;;   :background  - Main background
;;   :foreground  - Main text color
;;   :fg-dim      - Dimmed/secondary text (punctuation, line numbers)
;;   :bg-alt      - Alternate background (hl-line, code blocks)
;;
;; UI colors:
;;   :selection   - Selection/region background
;;   :highlight   - Cursor, current match
;;   :find-hl     - Search match highlight
;;
;; Status colors:
;;   :error       - Errors
;;   :warning     - Warnings
;;   :success     - Success/added
;;
;; Diff colors:
;;   :diff-add-bg - Added lines background
;;   :diff-del-bg - Deleted lines background
;;   :diff-chg-bg - Changed lines background
;;
;; Terminal colors (ANSI 16-color palette):
;;   :term-black, :term-red, :term-green, :term-yellow
;;   :term-blue, :term-magenta, :term-cyan, :term-white
;;   :term-bright-black, :term-bright-red, :term-bright-green
;;   :term-bright-yellow, :term-bright-blue, :term-bright-magenta
;;   :term-bright-cyan, :term-bright-white

;;; Alabaster palette
;;
;; The original Alabaster palette by Nikita Tonsky.
;; Light variant uses exact colors from the reference implementation.
;; Dark variant is algorithmically derived.

(defconst gypsum-preset-alabaster-light
  '(:name alabaster
    :variant light
    ;; The 4 semantic colors
    :string "#448C27"
    :constant "#7A3E9D"
    :comment "#AA3731"
    :definition "#325CC0"
    ;; Base colors
    :background "#F7F7F7"
    :foreground "#000000"
    :fg-dim "#777777"
    :bg-alt "#EEEEEE"
    ;; UI colors
    :selection "#BFDBFE"
    :highlight "#007ACC"
    :find-hl "#FFBC5D"
    ;; Status colors
    :error "#E51400"
    :warning "#F09000"
    :success "#448C27"
    ;; Diff colors
    :diff-add-bg "#D4EDDA"
    :diff-del-bg "#F8D7DA"
    :diff-chg-bg "#FFF3CD"
    ;; Terminal colors (ANSI)
    :term-black "#000000"
    :term-red "#AA3731"
    :term-green "#448C27"
    :term-yellow "#CB9000"
    :term-blue "#325CC0"
    :term-magenta "#7A3E9D"
    :term-cyan "#0083B2"
    :term-white "#F7F7F7"
    :term-bright-black "#777777"
    :term-bright-red "#E51400"
    :term-bright-green "#5DA713"
    :term-bright-yellow "#F09000"
    :term-bright-blue "#007ACC"
    :term-bright-magenta "#9B4DCA"
    :term-bright-cyan "#00A8C6"
    :term-bright-white "#FFFFFF")
  "Alabaster light palette - the original Tonsky colors.")

(defconst gypsum-preset-alabaster-dark
  '(:name alabaster-dark
    :variant dark
    ;; The 4 semantic colors (adjusted for dark background)
    :string "#98C379"
    :constant "#C678DD"
    :comment "#E5786D"
    :definition "#61AFEF"
    ;; Base colors
    :background "#1E1E1E"
    :foreground "#D4D4D4"
    :fg-dim "#808080"
    :bg-alt "#252526"
    ;; UI colors
    :selection "#264F78"
    :highlight "#007ACC"
    :find-hl "#623315"
    ;; Status colors
    :error "#F44747"
    :warning "#FF8C00"
    :success "#98C379"
    ;; Diff colors
    :diff-add-bg "#2D4A2D"
    :diff-del-bg "#4A2D2D"
    :diff-chg-bg "#4A4A2D"
    ;; Terminal colors (ANSI)
    :term-black "#1E1E1E"
    :term-red "#E5786D"
    :term-green "#98C379"
    :term-yellow "#E5C07B"
    :term-blue "#61AFEF"
    :term-magenta "#C678DD"
    :term-cyan "#56B6C2"
    :term-white "#D4D4D4"
    :term-bright-black "#808080"
    :term-bright-red "#F44747"
    :term-bright-green "#B5E890"
    :term-bright-yellow "#FFD700"
    :term-bright-blue "#82CFFF"
    :term-bright-magenta "#DA8EE7"
    :term-bright-cyan "#7FDBF0"
    :term-bright-white "#FFFFFF")
  "Alabaster dark palette - derived from light variant.")

;;; Preset registry

(defvar gypsum-presets
  (list
   (cons 'alabaster-light gypsum-preset-alabaster-light)
   (cons 'alabaster-dark gypsum-preset-alabaster-dark))
  "Alist of available presets.
Keys are symbols like 'alabaster-light, values are palette plists.")

;;; API functions

(defun gypsum-preset-get (name)
  "Get a preset palette by NAME.
NAME is a symbol like \\='alabaster-light or \\='alabaster-dark.
Returns a palette plist or nil if not found."
  (cdr (assq name gypsum-presets)))

(defun gypsum-preset-list ()
  "Return a list of available preset names.
Returns a list of preset name symbols."
  (mapcar #'car gypsum-presets))

;;; Palette validation

(defconst gypsum-palette-required-keys
  '(:name :variant
    :string :constant :comment :definition
    :background :foreground :fg-dim :bg-alt
    :selection :highlight :find-hl
    :error :warning :success
    :diff-add-bg :diff-del-bg :diff-chg-bg
    :term-black :term-red :term-green :term-yellow
    :term-blue :term-magenta :term-cyan :term-white
    :term-bright-black :term-bright-red :term-bright-green
    :term-bright-yellow :term-bright-blue :term-bright-magenta
    :term-bright-cyan :term-bright-white)
  "List of required keys in a palette plist.")

(defun gypsum-palette-validate (palette)
  "Validate that PALETTE has all required keys.
Returns t if valid, signals an error otherwise."
  (dolist (key gypsum-palette-required-keys)
    (unless (plist-get palette key)
      (error "Palette missing required key: %s" key)))
  t)

(provide 'gypsum-presets)
;;; gypsum-presets.el ends here
