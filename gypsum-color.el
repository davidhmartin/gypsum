;;; gypsum-color.el --- Color manipulation for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Gypsum Contributors
;; Keywords: faces, themes, colors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Color conversion and manipulation functions for Gypsum theme generator.
;; Provides RGB/HSL conversion, color manipulation (rotate, lighten, darken,
;; saturate, desaturate), and color analysis (luminance, contrast).

;;; Code:

(require 'cl-lib)
(require 'color)

;;; --- Conversion Functions ---

(defun gypsum-color-hex-to-rgb (hex)
  "Convert HEX color string to RGB values (0-255).
HEX should be a string like \"#RRGGBB\" or \"#RGB\"."
  (let ((hex (if (string-prefix-p "#" hex) (substring hex 1) hex)))
    ;; Handle short form #RGB
    (when (= (length hex) 3)
      (setq hex (concat (substring hex 0 1) (substring hex 0 1)
                        (substring hex 1 2) (substring hex 1 2)
                        (substring hex 2 3) (substring hex 2 3))))
    (list (string-to-number (substring hex 0 2) 16)
          (string-to-number (substring hex 2 4) 16)
          (string-to-number (substring hex 4 6) 16))))

(defun gypsum-color-rgb-to-hex (r g b)
  "Convert RGB values (0-255) to hex color string."
  (format "#%02X%02X%02X"
          (max 0 (min 255 (round r)))
          (max 0 (min 255 (round g)))
          (max 0 (min 255 (round b)))))

(defun gypsum-color-rgb-to-hsl (r g b)
  "Convert RGB values (0-255) to HSL values.
Returns (H S L) where H is 0-360, S and L are 0-100."
  (let* ((r (/ r 255.0))
         (g (/ g 255.0))
         (b (/ b 255.0))
         (max-c (max r g b))
         (min-c (min r g b))
         (delta (- max-c min-c))
         (l (/ (+ max-c min-c) 2.0))
         h s)
    ;; Calculate saturation
    (setq s (if (zerop delta)
                0.0
              (/ delta (- 1.0 (abs (- (* 2 l) 1.0))))))
    ;; Calculate hue
    (setq h (cond
             ((zerop delta) 0.0)
             ((= max-c r) (* 60.0 (mod (/ (- g b) delta) 6)))
             ((= max-c g) (* 60.0 (+ (/ (- b r) delta) 2)))
             ((= max-c b) (* 60.0 (+ (/ (- r g) delta) 4)))))
    (when (< h 0) (setq h (+ h 360.0)))
    (list h (* s 100.0) (* l 100.0))))

(defun gypsum-color-hsl-to-rgb (h s l)
  "Convert HSL values to RGB values (0-255).
H is 0-360, S and L are 0-100."
  (let* ((h (mod h 360.0))
         (s (/ s 100.0))
         (l (/ l 100.0))
         (c (* (- 1.0 (abs (- (* 2 l) 1.0))) s))
         (x (* c (- 1.0 (abs (- (mod (/ h 60.0) 2) 1.0)))))
         (m (- l (/ c 2.0)))
         r g b)
    (cond
     ((< h 60)  (setq r c g x b 0))
     ((< h 120) (setq r x g c b 0))
     ((< h 180) (setq r 0 g c b x))
     ((< h 240) (setq r 0 g x b c))
     ((< h 300) (setq r x g 0 b c))
     (t         (setq r c g 0 b x)))
    (list (* (+ r m) 255.0)
          (* (+ g m) 255.0)
          (* (+ b m) 255.0))))

(defun gypsum-color-hex-to-hsl (hex)
  "Convert HEX color to HSL values."
  (apply #'gypsum-color-rgb-to-hsl (gypsum-color-hex-to-rgb hex)))

(defun gypsum-color-hsl-to-hex (h s l)
  "Convert HSL values to hex color string."
  (apply #'gypsum-color-rgb-to-hex (gypsum-color-hsl-to-rgb h s l)))

;;; --- Color Manipulation ---

(defun gypsum-color-rotate (hex degrees)
  "Rotate the hue of HEX color by DEGREES."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (mod (+ (nth 0 hsl) degrees) 360.0)
     (nth 1 hsl)
     (nth 2 hsl))))

(defun gypsum-color-lighten (hex percent)
  "Lighten HEX color by PERCENT (0-100)."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (nth 1 hsl)
     (min 100.0 (+ (nth 2 hsl) percent)))))

(defun gypsum-color-darken (hex percent)
  "Darken HEX color by PERCENT (0-100)."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (nth 1 hsl)
     (max 0.0 (- (nth 2 hsl) percent)))))

(defun gypsum-color-saturate (hex percent)
  "Increase saturation of HEX color by PERCENT (0-100)."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (min 100.0 (+ (nth 1 hsl) percent))
     (nth 2 hsl))))

(defun gypsum-color-desaturate (hex percent)
  "Decrease saturation of HEX color by PERCENT (0-100)."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (max 0.0 (- (nth 1 hsl) percent))
     (nth 2 hsl))))

(defun gypsum-color-set-saturation (hex saturation)
  "Set the saturation of HEX color to SATURATION (0-100)."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (max 0.0 (min 100.0 saturation))
     (nth 2 hsl))))

(defun gypsum-color-set-lightness (hex lightness)
  "Set the lightness of HEX color to LIGHTNESS (0-100)."
  (let ((hsl (gypsum-color-hex-to-hsl hex)))
    (gypsum-color-hsl-to-hex
     (nth 0 hsl)
     (nth 1 hsl)
     (max 0.0 (min 100.0 lightness)))))

;;; --- Color Analysis ---

(defun gypsum-color-luminance (hex)
  "Calculate relative luminance of HEX color (0.0-1.0).
Uses the formula from WCAG 2.0."
  (let* ((rgb (gypsum-color-hex-to-rgb hex))
         (r (/ (nth 0 rgb) 255.0))
         (g (/ (nth 1 rgb) 255.0))
         (b (/ (nth 2 rgb) 255.0)))
    ;; Apply gamma correction
    (setq r (if (<= r 0.03928) (/ r 12.92) (expt (/ (+ r 0.055) 1.055) 2.4)))
    (setq g (if (<= g 0.03928) (/ g 12.92) (expt (/ (+ g 0.055) 1.055) 2.4)))
    (setq b (if (<= b 0.03928) (/ b 12.92) (expt (/ (+ b 0.055) 1.055) 2.4)))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun gypsum-color-contrast (hex1 hex2)
  "Calculate contrast ratio between HEX1 and HEX2.
Returns a value from 1 to 21 (higher is more contrast)."
  (let ((l1 (gypsum-color-luminance hex1))
        (l2 (gypsum-color-luminance hex2)))
    (when (< l1 l2)
      (cl-rotatef l1 l2))
    (/ (+ l1 0.05) (+ l2 0.05))))

(defun gypsum-color-light-p (hex)
  "Return non-nil if HEX is a light color.
Uses luminance threshold of 0.5."
  (> (gypsum-color-luminance hex) 0.5))

(defun gypsum-color-dark-p (hex)
  "Return non-nil if HEX is a dark color."
  (not (gypsum-color-light-p hex)))

;;; --- Color Blending ---

(defun gypsum-color-blend (hex1 hex2 &optional ratio)
  "Blend HEX1 and HEX2 colors.
RATIO is the amount of HEX2 to mix in (0.0-1.0, default 0.5)."
  (let* ((ratio (or ratio 0.5))
         (rgb1 (gypsum-color-hex-to-rgb hex1))
         (rgb2 (gypsum-color-hex-to-rgb hex2)))
    (gypsum-color-rgb-to-hex
     (+ (* (nth 0 rgb1) (- 1 ratio)) (* (nth 0 rgb2) ratio))
     (+ (* (nth 1 rgb1) (- 1 ratio)) (* (nth 1 rgb2) ratio))
     (+ (* (nth 2 rgb1) (- 1 ratio)) (* (nth 2 rgb2) ratio)))))

(defun gypsum-color-tint (hex &optional amount)
  "Tint HEX color (mix with white).
AMOUNT is 0.0-1.0 (default 0.5)."
  (gypsum-color-blend hex "#FFFFFF" (or amount 0.5)))

(defun gypsum-color-shade (hex &optional amount)
  "Shade HEX color (mix with black).
AMOUNT is 0.0-1.0 (default 0.5)."
  (gypsum-color-blend hex "#000000" (or amount 0.5)))

;;; --- Harmony Functions ---

(defun gypsum-color-complementary (hex)
  "Return the complementary color of HEX (180 degree rotation)."
  (gypsum-color-rotate hex 180))

(defun gypsum-color-triadic (hex)
  "Return triadic colors for HEX (120 degree rotations).
Returns list of two colors."
  (list (gypsum-color-rotate hex 120)
        (gypsum-color-rotate hex 240)))

(defun gypsum-color-tetradic (hex)
  "Return tetradic/square colors for HEX (90 degree rotations).
Returns list of three colors."
  (list (gypsum-color-rotate hex 90)
        (gypsum-color-rotate hex 180)
        (gypsum-color-rotate hex 270)))

(defun gypsum-color-split-complementary (hex)
  "Return split-complementary colors for HEX.
Returns list of two colors (150 and 210 degree rotations)."
  (list (gypsum-color-rotate hex 150)
        (gypsum-color-rotate hex 210)))

(defun gypsum-color-analogous (hex &optional angle)
  "Return analogous colors for HEX.
ANGLE is the rotation amount (default 30).
Returns list of two colors."
  (let ((angle (or angle 30)))
    (list (gypsum-color-rotate hex angle)
          (gypsum-color-rotate hex (- angle)))))

;;; --- Utility Functions ---

(defun gypsum-color-ensure-contrast (fg bg &optional min-ratio)
  "Adjust FG color to ensure minimum contrast with BG.
MIN-RATIO is the minimum contrast ratio (default 4.5 for WCAG AA)."
  (let* ((min-ratio (or min-ratio 4.5))
         (current-ratio (gypsum-color-contrast fg bg))
         (light-bg (gypsum-color-light-p bg))
         (hsl (gypsum-color-hex-to-hsl fg))
         (step 5))
    (while (and (< current-ratio min-ratio)
                (if light-bg
                    (> (nth 2 hsl) 0)
                  (< (nth 2 hsl) 100)))
      (setq hsl (list (nth 0 hsl)
                      (nth 1 hsl)
                      (if light-bg
                          (max 0 (- (nth 2 hsl) step))
                        (min 100 (+ (nth 2 hsl) step)))))
      (setq fg (apply #'gypsum-color-hsl-to-hex hsl))
      (setq current-ratio (gypsum-color-contrast fg bg)))
    fg))

(defun gypsum-color-readable-on (bg)
  "Return a readable foreground color for background BG.
Returns black or white depending on background luminance."
  (if (gypsum-color-light-p bg) "#000000" "#FFFFFF"))

(provide 'gypsum-color)

;;; gypsum-color.el ends here
