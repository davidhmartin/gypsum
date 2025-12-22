;;; gypsum-palette.el --- Palette generation for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Gypsum Contributors
;; Keywords: faces, themes, colors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Generates complete color palettes from a seed color following
;; Alabaster/minimal highlighting principles.  The seed becomes the
;; definition color, and other semantic colors are derived via
;; color wheel relationships.

;;; Code:

(require 'gypsum-color)

;;; --- Configuration ---

(defcustom gypsum-string-hue-offset -120
  "Hue rotation from seed to derive string color."
  :type 'number
  :group 'gypsum)

(defcustom gypsum-constant-hue-offset 60
  "Hue rotation from seed to derive constant color."
  :type 'number
  :group 'gypsum)

(defcustom gypsum-comment-hue-offset-light 150
  "Hue rotation from seed to derive comment color for light themes."
  :type 'number
  :group 'gypsum)

(defcustom gypsum-comment-hue-offset-dark 165
  "Hue rotation from seed to derive comment color for dark themes.
Shifted toward yellow for better readability on dark backgrounds."
  :type 'number
  :group 'gypsum)

;;; --- Background Generation ---

(defun gypsum-palette--default-background (variant seed)
  "Generate default background color for VARIANT using SEED for subtle tinting."
  (let* ((seed-hsl (gypsum-color-hex-to-hsl seed))
         (seed-hue (nth 0 seed-hsl)))
    (pcase variant
      ('light
       ;; Light background: very light, slightly tinted by seed
       (gypsum-color-hsl-to-hex seed-hue 5.0 97.0))
      ('dark
       ;; Dark background: very dark, slightly tinted by seed
       (gypsum-color-hsl-to-hex seed-hue 10.0 6.0))
      (_
       (error "Unknown variant: %s" variant)))))

;;; --- Semantic Color Derivation ---

(defun gypsum-palette--derive-string (seed variant)
  "Derive string color from SEED for VARIANT."
  (let* ((rotated (gypsum-color-rotate seed gypsum-string-hue-offset))
         (hsl (gypsum-color-hex-to-hsl rotated)))
    (pcase variant
      ('light
       ;; Light theme: higher saturation, medium lightness
       (gypsum-color-hsl-to-hex (nth 0 hsl) 60.0 35.0))
      ('dark
       ;; Dark theme: lower saturation, higher lightness (muted pastel)
       (gypsum-color-hsl-to-hex (nth 0 hsl) 40.0 70.0)))))

(defun gypsum-palette--derive-constant (seed variant)
  "Derive constant color from SEED for VARIANT."
  (let* ((rotated (gypsum-color-rotate seed gypsum-constant-hue-offset))
         (hsl (gypsum-color-hex-to-hsl rotated)))
    (pcase variant
      ('light
       ;; Light theme: rich saturation, medium lightness
       (gypsum-color-hsl-to-hex (nth 0 hsl) 55.0 40.0))
      ('dark
       ;; Dark theme: muted, lighter
       (gypsum-color-hsl-to-hex (nth 0 hsl) 45.0 75.0)))))

(defun gypsum-palette--derive-comment (seed variant)
  "Derive comment color from SEED for VARIANT."
  (let* ((offset (pcase variant
                   ('light gypsum-comment-hue-offset-light)
                   ('dark gypsum-comment-hue-offset-dark)))
         (rotated (gypsum-color-rotate seed offset))
         (hsl (gypsum-color-hex-to-hsl rotated)))
    (pcase variant
      ('light
       ;; Light theme: saturated, visible red-ish
       (gypsum-color-hsl-to-hex (nth 0 hsl) 60.0 40.0))
      ('dark
       ;; Dark theme: desaturated yellow for readability
       (gypsum-color-hsl-to-hex (nth 0 hsl) 50.0 75.0)))))

(defun gypsum-palette--adjust-definition (seed variant)
  "Adjust SEED (definition color) for optimal display in VARIANT."
  (let ((hsl (gypsum-color-hex-to-hsl seed)))
    (pcase variant
      ('light
       ;; Light theme: ensure sufficient saturation and darker
       (gypsum-color-hsl-to-hex
        (nth 0 hsl)
        (max 50.0 (nth 1 hsl))
        (min 45.0 (max 30.0 (nth 2 hsl)))))
      ('dark
       ;; Dark theme: ensure visible, possibly lighter
       (gypsum-color-hsl-to-hex
        (nth 0 hsl)
        (max 40.0 (min 70.0 (nth 1 hsl)))
        (max 60.0 (min 80.0 (nth 2 hsl))))))))

;;; --- Supporting Color Derivation ---

(defun gypsum-palette--derive-foreground (background)
  "Derive foreground color for BACKGROUND."
  (if (gypsum-color-light-p background)
      "#000000"
    "#CECECE"))

(defun gypsum-palette--derive-fg-dim (background foreground)
  "Derive dimmed foreground for BACKGROUND given FOREGROUND."
  (gypsum-color-blend foreground background 0.5))

(defun gypsum-palette--derive-bg-alt (background variant)
  "Derive alternate background for BACKGROUND in VARIANT."
  (pcase variant
    ('light (gypsum-color-darken background 4))
    ('dark (gypsum-color-lighten background 5))))

(defun gypsum-palette--derive-selection (seed variant)
  "Derive selection color from SEED for VARIANT."
  (let ((hsl (gypsum-color-hex-to-hsl seed)))
    (pcase variant
      ('light
       ;; Light selection: desaturated, very light version of seed
       (gypsum-color-hsl-to-hex (nth 0 hsl) 60.0 85.0))
      ('dark
       ;; Dark selection: desaturated, dark version of seed
       (gypsum-color-hsl-to-hex (nth 0 hsl) 50.0 30.0)))))

(defun gypsum-palette--derive-highlight (seed variant)
  "Derive highlight/cursor color from SEED for VARIANT."
  (let ((hsl (gypsum-color-hex-to-hsl seed)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex (nth 0 hsl) 80.0 45.0))
      ('dark
       (gypsum-color-hsl-to-hex (nth 0 hsl) 70.0 55.0)))))

(defun gypsum-palette--derive-find-hl (seed variant)
  "Derive find/search highlight from SEED for VARIANT."
  ;; Use a warm color (orange-ish) shifted from seed
  (let* ((rotated (gypsum-color-rotate seed 40))
         (hsl (gypsum-color-hex-to-hsl rotated)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex (nth 0 hsl) 80.0 70.0))
      ('dark
       (gypsum-color-hsl-to-hex (nth 0 hsl) 70.0 35.0)))))

;;; --- Status Colors (Harmonized) ---

(defun gypsum-palette--derive-error (seed variant)
  "Derive error color harmonized with SEED for VARIANT.
Shifts toward red while maintaining some harmony with seed."
  (let* ((seed-hsl (gypsum-color-hex-to-hsl seed))
         (seed-hue (nth 0 seed-hsl))
         ;; Target red (~0-10 degrees), blend with seed influence
         (target-hue (mod (+ 0 (* (mod (- seed-hue 0) 180) 0.1)) 360)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex target-hue 75.0 45.0))
      ('dark
       (gypsum-color-hsl-to-hex target-hue 70.0 60.0)))))

(defun gypsum-palette--derive-warning (seed variant)
  "Derive warning color harmonized with SEED for VARIANT.
Shifts toward yellow/orange while maintaining some harmony."
  (let* ((seed-hsl (gypsum-color-hex-to-hsl seed))
         (seed-hue (nth 0 seed-hsl))
         ;; Target yellow/orange (~40-50 degrees)
         (target-hue (mod (+ 45 (* (mod (- seed-hue 45) 180) 0.1)) 360)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex target-hue 80.0 40.0))
      ('dark
       (gypsum-color-hsl-to-hex target-hue 70.0 55.0)))))

(defun gypsum-palette--derive-success (string-color variant)
  "Derive success color from STRING-COLOR for VARIANT.
Success typically aligns with green, and string is usually green-ish."
  (let ((hsl (gypsum-color-hex-to-hsl string-color)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex (nth 0 hsl) 65.0 35.0))
      ('dark
       (gypsum-color-hsl-to-hex (nth 0 hsl) 50.0 65.0)))))

;;; --- Diff Colors ---

(defun gypsum-palette--derive-diff-add-bg (success-color variant)
  "Derive diff added background from SUCCESS-COLOR for VARIANT."
  (let ((hsl (gypsum-color-hex-to-hsl success-color)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex (nth 0 hsl) 40.0 90.0))
      ('dark
       (gypsum-color-hsl-to-hex (nth 0 hsl) 35.0 15.0)))))

(defun gypsum-palette--derive-diff-del-bg (error-color variant)
  "Derive diff deleted background from ERROR-COLOR for VARIANT."
  (let ((hsl (gypsum-color-hex-to-hsl error-color)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex (nth 0 hsl) 40.0 92.0))
      ('dark
       (gypsum-color-hsl-to-hex (nth 0 hsl) 35.0 15.0)))))

(defun gypsum-palette--derive-diff-chg-bg (warning-color variant)
  "Derive diff changed background from WARNING-COLOR for VARIANT."
  (let ((hsl (gypsum-color-hex-to-hsl warning-color)))
    (pcase variant
      ('light
       (gypsum-color-hsl-to-hex (nth 0 hsl) 50.0 92.0))
      ('dark
       (gypsum-color-hsl-to-hex (nth 0 hsl) 40.0 15.0)))))

;;; --- Low Contrast Adjustments ---

(defun gypsum-palette--apply-low-contrast (palette)
  "Apply low contrast adjustments to PALETTE."
  (let* ((variant (plist-get palette :variant))
         (fg (plist-get palette :foreground)))
    ;; Reduce contrast by moving fg closer to bg
    (setq palette
          (plist-put palette :foreground
                     (pcase variant
                       ('light (gypsum-color-lighten fg 20))
                       ('dark (gypsum-color-darken fg 15)))))
    ;; Also adjust semantic colors to be less vibrant
    (dolist (key '(:string :constant :comment :definition))
      (let ((color (plist-get palette key)))
        (setq palette
              (plist-put palette key
                         (gypsum-color-desaturate color 15)))))
    palette))

;;; --- Main Palette Creation ---

(cl-defun gypsum-palette-create (&key seed variant contrast
                                      background string constant
                                      comment definition)
  "Create a complete color palette.

Required arguments (one of):
  :seed HEXCOLOR      - Base color (becomes definition), derive others
  :background + :definition - Explicit bg and definition

Optional arguments:
  :variant SYMBOL     - \\='light or \\='dark (inferred from bg if not specified)
  :contrast SYMBOL    - \\='normal (default) or \\='low
  :string HEXCOLOR    - Override string color
  :constant HEXCOLOR  - Override constant color
  :comment HEXCOLOR   - Override comment color

Returns a plist with all palette colors."
  (let (palette)
    ;; Validate inputs
    (unless (or seed (and background definition))
      (error "Must provide :seed or both :background and :definition"))

    ;; Use seed as definition if not explicitly provided
    (unless definition
      (setq definition seed))

    ;; Determine variant
    (unless variant
      (if background
          (setq variant (if (gypsum-color-light-p background) 'light 'dark))
        (error "Must specify :variant when using :seed alone")))

    ;; Generate or use background
    (unless background
      (setq background (gypsum-palette--default-background variant seed)))

    ;; Adjust definition for variant
    (setq definition (gypsum-palette--adjust-definition definition variant))

    ;; Derive semantic colors (use overrides if provided)
    (setq string (or string (gypsum-palette--derive-string definition variant)))
    (setq constant (or constant (gypsum-palette--derive-constant definition variant)))
    (setq comment (or comment (gypsum-palette--derive-comment definition variant)))

    ;; Derive supporting colors
    (let* ((foreground (gypsum-palette--derive-foreground background))
           (fg-dim (gypsum-palette--derive-fg-dim background foreground))
           (bg-alt (gypsum-palette--derive-bg-alt background variant))
           (selection (gypsum-palette--derive-selection definition variant))
           (highlight (gypsum-palette--derive-highlight definition variant))
           (find-hl (gypsum-palette--derive-find-hl definition variant))
           (error-color (gypsum-palette--derive-error definition variant))
           (warning-color (gypsum-palette--derive-warning definition variant))
           (success-color (gypsum-palette--derive-success string variant))
           (diff-add-bg (gypsum-palette--derive-diff-add-bg success-color variant))
           (diff-del-bg (gypsum-palette--derive-diff-del-bg error-color variant))
           (diff-chg-bg (gypsum-palette--derive-diff-chg-bg warning-color variant)))

      ;; Build palette plist
      (setq palette
            (list :variant variant
                  :contrast (or contrast 'normal)
                  ;; Core
                  :background background
                  :foreground foreground
                  :fg-dim fg-dim
                  :bg-alt bg-alt
                  ;; Semantic 4
                  :string string
                  :constant constant
                  :comment comment
                  :definition definition
                  ;; UI
                  :selection selection
                  :highlight highlight
                  :find-hl find-hl
                  ;; Status
                  :error error-color
                  :warning warning-color
                  :success success-color
                  ;; Diff
                  :diff-add-bg diff-add-bg
                  :diff-del-bg diff-del-bg
                  :diff-chg-bg diff-chg-bg)))

    ;; Apply low contrast if requested
    (when (eq contrast 'low)
      (setq palette (gypsum-palette--apply-low-contrast palette)))

    palette))

(defun gypsum-palette-to-alist (palette)
  "Convert PALETTE plist to an alist for easier use in theme generation."
  (let (alist)
    (cl-loop for (key val) on palette by #'cddr
             do (push (cons key val) alist))
    (nreverse alist)))

(provide 'gypsum-palette)

;;; gypsum-palette.el ends here
