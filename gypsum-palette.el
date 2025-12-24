;;; gypsum-palette.el --- Palette operations for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David
;; Keywords: faces, themes, colors

;; This file is part of Gypsum.

;;; Commentary:

;; This file provides operations on color palettes:
;; - Tinting palettes with various modes
;; - Deriving dark variants from light palettes
;; - Seed-based palette generation (stub)
;;
;; Palettes are plists with keys defined in gypsum-presets.el.

;;; Code:

(require 'gypsum-color)
(require 'gypsum-presets)

;;; Tinting modes
;;
;; Three ways to tint a palette:
;; 1. hue-shift - rotate all semantic colors by N degrees
;; 2. definition-only - replace definition color, keep others
;; 3. blend - blend all colors toward an accent color

(defun gypsum-palette-tint (palette &rest args)
  "Tint PALETTE using the specified mode.

ARGS is a plist with:
  :mode MODE - Tinting mode: \\='hue-shift, \\='definition-only, or \\='blend

For hue-shift mode:
  :degrees N - Number of degrees to rotate (default 0)

For definition-only mode:
  :definition COLOR - The new definition color

For blend mode:
  :color COLOR - Color to blend toward
  :amount N - Blend percentage 0-100 (default 20)

Returns a new palette with tinted colors."
  (let ((mode (plist-get args :mode)))
    (pcase mode
      ('hue-shift
       (gypsum-palette--tint-hue-shift palette (or (plist-get args :degrees) 0)))
      ('definition-only
       (gypsum-palette--tint-definition-only palette (plist-get args :definition)))
      ('blend
       (gypsum-palette--tint-blend palette
                                    (plist-get args :color)
                                    (or (plist-get args :amount) 20)))
      (_
       (error "Unknown tinting mode: %s" mode)))))

(defun gypsum-palette--tint-hue-shift (palette degrees)
  "Rotate all semantic colors in PALETTE by DEGREES."
  (let ((result (copy-sequence palette)))
    ;; Rotate the 4 semantic colors
    (dolist (key '(:string :constant :comment :definition))
      (let ((color (plist-get palette key)))
        (setq result (plist-put result key (gypsum-color-rotate color degrees)))))
    ;; Also rotate UI colors that should match
    (dolist (key '(:selection :highlight :find-hl))
      (let ((color (plist-get palette key)))
        (when color
          (setq result (plist-put result key (gypsum-color-rotate color degrees))))))
    ;; Rotate status/diff colors
    (dolist (key '(:error :warning :success :diff-add-bg :diff-del-bg :diff-chg-bg))
      (let ((color (plist-get palette key)))
        (when color
          (setq result (plist-put result key (gypsum-color-rotate color degrees))))))
    result))

(defun gypsum-palette--tint-definition-only (palette definition)
  "Replace only the definition color in PALETTE with DEFINITION."
  (unless definition
    (error "Must provide :definition color"))
  (let ((result (copy-sequence palette)))
    (plist-put result :definition definition)))

(defun gypsum-palette--tint-blend (palette color amount)
  "Blend all colors in PALETTE toward COLOR by AMOUNT percent."
  (unless color
    (error "Must provide :color to blend toward"))
  (let ((result (copy-sequence palette))
        (blend-ratio (/ amount 100.0)))
    ;; Blend semantic colors
    (dolist (key '(:string :constant :comment :definition))
      (let ((orig (plist-get palette key)))
        (setq result (plist-put result key (gypsum-color-blend orig color blend-ratio)))))
    ;; Blend UI colors (less intensely)
    (let ((ui-ratio (* blend-ratio 0.5)))
      (dolist (key '(:selection :highlight :find-hl))
        (let ((orig (plist-get palette key)))
          (when orig
            (setq result (plist-put result key (gypsum-color-blend orig color ui-ratio)))))))
    result))

;;; Deriving dark from light

(defun gypsum-palette-derive-dark (light-palette)
  "Derive a dark palette from LIGHT-PALETTE.

Adjusts lightness and saturation of colors for dark background display."
  (unless (eq (plist-get light-palette :variant) 'light)
    (error "Can only derive dark from a light palette"))
  (let ((result (copy-sequence light-palette)))
    ;; Set variant
    (setq result (plist-put result :variant 'dark))
    ;; Invert base colors
    (setq result (plist-put result :background
                            (gypsum-palette--dark-background light-palette)))
    (setq result (plist-put result :foreground "#D4D4D4"))
    (setq result (plist-put result :fg-dim "#808080"))
    (setq result (plist-put result :bg-alt
                            (gypsum-color-lighten (plist-get result :background) 5)))
    ;; Adjust semantic colors for dark readability
    (dolist (key '(:string :constant :comment :definition))
      (let ((color (plist-get light-palette key)))
        (setq result (plist-put result key
                                (gypsum-palette--adjust-for-dark color)))))
    ;; Adjust UI colors
    (setq result (plist-put result :selection
                            (gypsum-palette--dark-selection
                             (plist-get light-palette :definition))))
    (setq result (plist-put result :find-hl
                            (gypsum-palette--dark-find-hl
                             (plist-get light-palette :find-hl))))
    ;; Adjust status colors
    (dolist (key '(:error :warning :success))
      (let ((color (plist-get light-palette key)))
        (setq result (plist-put result key
                                (gypsum-palette--adjust-for-dark color)))))
    ;; Adjust diff colors
    (dolist (key '(:diff-add-bg :diff-del-bg :diff-chg-bg))
      (let ((color (plist-get light-palette key)))
        (setq result (plist-put result key
                                (gypsum-palette--dark-diff-bg color)))))
    result))

(defun gypsum-palette--dark-background (light-palette)
  "Generate dark background from LIGHT-PALETTE."
  (let* ((light-bg (plist-get light-palette :background))
         (hsl (gypsum-color-hex-to-hsl light-bg)))
    ;; Very dark with subtle hue from light bg
    (gypsum-color-hsl-to-hex (nth 0 hsl) 8.0 12.0)))

(defun gypsum-palette--adjust-for-dark (color)
  "Adjust COLOR for readability on dark background."
  (let ((hsl (gypsum-color-hex-to-hsl color)))
    ;; Increase lightness, slightly reduce saturation
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (max 30.0 (min 70.0 (nth 1 hsl)))
     (max 55.0 (min 80.0 (+ (nth 2 hsl) 25.0))))))

(defun gypsum-palette--dark-selection (definition)
  "Generate dark selection color from DEFINITION."
  (let ((hsl (gypsum-color-hex-to-hsl definition)))
    (gypsum-color-hsl-to-hex (nth 0 hsl) 50.0 25.0)))

(defun gypsum-palette--dark-find-hl (light-find-hl)
  "Generate dark find highlight from LIGHT-FIND-HL."
  (let ((hsl (gypsum-color-hex-to-hsl light-find-hl)))
    (gypsum-color-hsl-to-hex (nth 0 hsl) 70.0 25.0)))

(defun gypsum-palette--dark-diff-bg (light-diff-bg)
  "Generate dark diff background from LIGHT-DIFF-BG."
  (let ((hsl (gypsum-color-hex-to-hsl light-diff-bg)))
    (gypsum-color-hsl-to-hex (nth 0 hsl) 35.0 18.0)))

;;; Helper functions for background changes

(defun gypsum-palette--derive-foreground (background variant)
  "Derive foreground color for BACKGROUND and VARIANT with high contrast."
  (let ((base-fg (if (eq variant 'dark) "#D4D4D4" "#000000")))
    (gypsum-color-ensure-contrast base-fg background 7.0)))

(defun gypsum-palette--derive-fg-dim (background variant)
  "Derive dimmed foreground color for BACKGROUND and VARIANT."
  (let ((base-dim (if (eq variant 'dark) "#808080" "#777777")))
    (gypsum-color-ensure-contrast base-dim background 4.5)))

(defun gypsum-palette--derive-diff-bg (type variant)
  "Derive diff background for TYPE (add/del/chg) and VARIANT."
  (let* ((hue (pcase type
                ('add 120.0)   ; Green
                ('del 0.0)     ; Red
                ('chg 45.0)))  ; Yellow
         (sat (if (eq variant 'dark) 35.0 50.0))
         (light (if (eq variant 'dark) 18.0 90.0)))
    (gypsum-color-hsl-to-hex hue sat light)))

;;; Deriving light from dark

(defun gypsum-palette-derive-light (dark-palette)
  "Derive a light palette from DARK-PALETTE.

Adjusts lightness and saturation of colors for light background display."
  (unless (eq (plist-get dark-palette :variant) 'dark)
    (error "Can only derive light from a dark palette"))
  (let ((result (copy-sequence dark-palette)))
    ;; Set variant
    (setq result (plist-put result :variant 'light))
    ;; Invert base colors
    (setq result (plist-put result :background
                            (gypsum-palette--light-background dark-palette)))
    (setq result (plist-put result :foreground "#000000"))
    (setq result (plist-put result :fg-dim "#777777"))
    (setq result (plist-put result :bg-alt
                            (gypsum-color-darken (plist-get result :background) 5)))
    ;; Adjust semantic colors for light readability
    (dolist (key '(:string :constant :comment :definition))
      (let ((color (plist-get dark-palette key)))
        (setq result (plist-put result key
                                (gypsum-palette--adjust-for-light color)))))
    ;; Adjust UI colors
    (setq result (plist-put result :selection
                            (gypsum-palette--light-selection
                             (plist-get dark-palette :definition))))
    (setq result (plist-put result :find-hl
                            (gypsum-palette--light-find-hl
                             (plist-get dark-palette :find-hl))))
    ;; Adjust status colors
    (dolist (key '(:error :warning :success))
      (let ((color (plist-get dark-palette key)))
        (setq result (plist-put result key
                                (gypsum-palette--adjust-for-light color)))))
    ;; Adjust diff colors
    (dolist (key '(:diff-add-bg :diff-del-bg :diff-chg-bg))
      (let ((color (plist-get dark-palette key)))
        (setq result (plist-put result key
                                (gypsum-palette--light-diff-bg color)))))
    result))

(defun gypsum-palette--light-background (dark-palette)
  "Generate light background from DARK-PALETTE."
  (let* ((dark-bg (plist-get dark-palette :background))
         (hsl (gypsum-color-hex-to-hsl dark-bg)))
    ;; Very light with subtle hue from dark bg
    (gypsum-color-hsl-to-hex (nth 0 hsl) 5.0 97.0)))

(defun gypsum-palette--adjust-for-light (color)
  "Adjust COLOR for readability on light background."
  (let ((hsl (gypsum-color-hex-to-hsl color)))
    ;; Decrease lightness, adjust saturation for vibrancy
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (max 40.0 (min 80.0 (nth 1 hsl)))
     (max 30.0 (min 50.0 (- (nth 2 hsl) 20.0))))))

(defun gypsum-palette--light-selection (definition)
  "Generate light selection color from DEFINITION."
  (let ((hsl (gypsum-color-hex-to-hsl definition)))
    (gypsum-color-hsl-to-hex (nth 0 hsl) 60.0 85.0)))

(defun gypsum-palette--light-find-hl (dark-find-hl)
  "Generate light find highlight from DARK-FIND-HL."
  (let ((hsl (gypsum-color-hex-to-hsl dark-find-hl)))
    (gypsum-color-hsl-to-hex (nth 0 hsl) 80.0 75.0)))

(defun gypsum-palette--light-diff-bg (dark-diff-bg)
  "Generate light diff background from DARK-DIFF-BG."
  (let ((hsl (gypsum-color-hex-to-hsl dark-diff-bg)))
    (gypsum-color-hsl-to-hex (nth 0 hsl) 50.0 90.0)))

;;; Changing background color

(defun gypsum-palette-change-background (palette new-background)
  "Create a new palette from PALETTE with NEW-BACKGROUND.

Automatically adjusts:
- :variant (based on background luminance)
- :bg-alt (derived from new background)
- :foreground and :fg-dim (ensuring contrast)
- :selection, :highlight, :find-hl (UI colors)
- :diff-add-bg, :diff-del-bg, :diff-chg-bg
- Semantic and status colors (if contrast insufficient)

Returns a new palette plist."
  (let* ((result (copy-sequence palette))
         (is-dark (gypsum-color-dark-p new-background))
         (new-variant (if is-dark 'dark 'light)))
    ;; 1. Set background and variant
    (setq result (plist-put result :background new-background))
    (setq result (plist-put result :variant new-variant))
    ;; 2. Calculate bg-alt
    (setq result (plist-put result :bg-alt
                            (if is-dark
                                (gypsum-color-lighten new-background 5)
                              (gypsum-color-darken new-background 5))))
    ;; 3. Set foreground colors with contrast
    (setq result (plist-put result :foreground
                            (gypsum-palette--derive-foreground new-background new-variant)))
    (setq result (plist-put result :fg-dim
                            (gypsum-palette--derive-fg-dim new-background new-variant)))
    ;; 4. Calculate UI colors
    (let ((definition (plist-get result :definition)))
      (setq result (plist-put result :selection
                              (if is-dark
                                  (gypsum-palette--dark-selection definition)
                                (gypsum-palette--light-selection definition))))
      (setq result (plist-put result :find-hl
                              (if is-dark "#623315" "#FFBC5D"))))
    ;; 5. Calculate diff colors
    (setq result (plist-put result :diff-add-bg
                            (gypsum-palette--derive-diff-bg 'add new-variant)))
    (setq result (plist-put result :diff-del-bg
                            (gypsum-palette--derive-diff-bg 'del new-variant)))
    (setq result (plist-put result :diff-chg-bg
                            (gypsum-palette--derive-diff-bg 'chg new-variant)))
    ;; 6. Ensure semantic colors have sufficient contrast
    (dolist (key '(:string :constant :comment :definition))
      (let ((color (plist-get result key)))
        (setq result (plist-put result key
                                (gypsum-color-ensure-contrast color new-background 4.5)))))
    ;; 7. Ensure status colors have sufficient contrast
    (dolist (key '(:error :warning :success))
      (let ((color (plist-get result key)))
        (setq result (plist-put result key
                                (gypsum-color-ensure-contrast color new-background 4.5)))))
    result))

;;; Seed-based generation (stub)

(defun gypsum-palette-generate (seed variant)
  "Generate a palette from SEED color for VARIANT.

SEED is a hex color string (the definition color).
VARIANT is \\='light or \\='dark.

This is currently a stub that uses the closest preset.
The algorithm may be improved in the future."
  ;; For now, start with alabaster and tint toward the seed
  (let* ((base-name (if (eq variant 'dark) 'alabaster-dark 'alabaster-light))
         (base (gypsum-preset-get base-name))
         (base-def (plist-get base :definition))
         (hue-diff (gypsum-palette--hue-difference seed base-def)))
    (gypsum-palette-tint base :mode 'hue-shift :degrees hue-diff)))

(defun gypsum-palette--hue-difference (color1 color2)
  "Calculate the hue difference between COLOR1 and COLOR2 in degrees."
  (let ((hsl1 (gypsum-color-hex-to-hsl color1))
        (hsl2 (gypsum-color-hex-to-hsl color2)))
    (- (nth 0 hsl1) (nth 0 hsl2))))

;;; Utility functions

(defun gypsum-palette-get (palette key)
  "Get KEY from PALETTE, with validation."
  (or (plist-get palette key)
      (error "Palette missing key: %s" key)))

(defun gypsum-palette-set (palette key value)
  "Set KEY to VALUE in PALETTE, returning a new palette."
  (let ((result (copy-sequence palette)))
    (plist-put result key value)))

(defun gypsum-palette-name (palette)
  "Get the display name for PALETTE."
  (format "%s-%s"
          (plist-get palette :name)
          (plist-get palette :variant)))

(defun gypsum-palette-to-alist (palette)
  "Convert PALETTE plist to an alist."
  (let (alist)
    (cl-loop for (key val) on palette by #'cddr
             do (push (cons key val) alist))
    (nreverse alist)))

(provide 'gypsum-palette)
;;; gypsum-palette.el ends here
