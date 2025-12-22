;;; gypsum-test.el --- Tests for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for Gypsum theme generator.

;;; Code:

(require 'ert)
(require 'gypsum-color)
(require 'gypsum-palette)
(require 'gypsum-faces)
(require 'gypsum-generate)
(require 'gypsum-ui)

;;; --- Color Conversion Tests ---

(ert-deftest gypsum-test-hex-to-rgb ()
  "Test hex to RGB conversion."
  (should (equal (gypsum-color-hex-to-rgb "#FF0000") '(255 0 0)))
  (should (equal (gypsum-color-hex-to-rgb "#00FF00") '(0 255 0)))
  (should (equal (gypsum-color-hex-to-rgb "#0000FF") '(0 0 255)))
  (should (equal (gypsum-color-hex-to-rgb "#FFFFFF") '(255 255 255)))
  (should (equal (gypsum-color-hex-to-rgb "#000000") '(0 0 0)))
  ;; Short form
  (should (equal (gypsum-color-hex-to-rgb "#F00") '(255 0 0)))
  ;; Without hash
  (should (equal (gypsum-color-hex-to-rgb "FF0000") '(255 0 0))))

(ert-deftest gypsum-test-rgb-to-hex ()
  "Test RGB to hex conversion."
  (should (equal (gypsum-color-rgb-to-hex 255 0 0) "#FF0000"))
  (should (equal (gypsum-color-rgb-to-hex 0 255 0) "#00FF00"))
  (should (equal (gypsum-color-rgb-to-hex 0 0 255) "#0000FF"))
  ;; Clamping
  (should (equal (gypsum-color-rgb-to-hex 300 -10 128) "#FF0080")))

(ert-deftest gypsum-test-rgb-hsl-roundtrip ()
  "Test RGB -> HSL -> RGB roundtrip."
  (let ((colors '("#FF0000" "#00FF00" "#0000FF" "#FFFF00" "#FF00FF" "#00FFFF"
                  "#808080" "#C0C0C0" "#800000" "#008000")))
    (dolist (color colors)
      (let* ((rgb (gypsum-color-hex-to-rgb color))
             (hsl (apply #'gypsum-color-rgb-to-hsl rgb))
             (rgb2 (apply #'gypsum-color-hsl-to-rgb hsl))
             (hex2 (apply #'gypsum-color-rgb-to-hex rgb2)))
        ;; Allow small rounding differences
        (should (< (abs (- (nth 0 rgb) (nth 0 rgb2))) 2))
        (should (< (abs (- (nth 1 rgb) (nth 1 rgb2))) 2))
        (should (< (abs (- (nth 2 rgb) (nth 2 rgb2))) 2))))))

(ert-deftest gypsum-test-hex-hsl-roundtrip ()
  "Test hex -> HSL -> hex roundtrip."
  (let ((colors '("#3498DB" "#E74C3C" "#2ECC71" "#F1C40F" "#9B59B6")))
    (dolist (color colors)
      (let* ((hsl (gypsum-color-hex-to-hsl color))
             (hex2 (apply #'gypsum-color-hsl-to-hex hsl)))
        ;; Should be very close
        (should (equal (upcase color) (upcase hex2)))))))

;;; --- Color Manipulation Tests ---

(ert-deftest gypsum-test-color-rotate ()
  "Test hue rotation."
  ;; 180 degree rotation should give complementary
  (let* ((blue "#0000FF")
         (rotated (gypsum-color-rotate blue 180)))
    ;; Blue (240) + 180 = 60 (yellow)
    (let ((hsl (gypsum-color-hex-to-hsl rotated)))
      (should (< (abs (- (nth 0 hsl) 60)) 5))))
  ;; 360 degree rotation should be same color
  (let* ((color "#3498DB")
         (rotated (gypsum-color-rotate color 360)))
    (should (equal (upcase color) (upcase rotated)))))

(ert-deftest gypsum-test-color-lighten-darken ()
  "Test lightening and darkening."
  (let* ((color "#808080")
         (lightened (gypsum-color-lighten color 20))
         (darkened (gypsum-color-darken color 20)))
    ;; Lightened should have higher luminance
    (should (> (gypsum-color-luminance lightened)
               (gypsum-color-luminance color)))
    ;; Darkened should have lower luminance
    (should (< (gypsum-color-luminance darkened)
               (gypsum-color-luminance color)))))

(ert-deftest gypsum-test-color-saturate-desaturate ()
  "Test saturation changes."
  (let* ((color "#7799BB")
         (saturated (gypsum-color-saturate color 30))
         (desaturated (gypsum-color-desaturate color 30)))
    ;; Saturated should have higher S value
    (let ((orig-hsl (gypsum-color-hex-to-hsl color))
          (sat-hsl (gypsum-color-hex-to-hsl saturated)))
      (should (> (nth 1 sat-hsl) (nth 1 orig-hsl))))
    ;; Desaturated should have lower S value
    (let ((orig-hsl (gypsum-color-hex-to-hsl color))
          (desat-hsl (gypsum-color-hex-to-hsl desaturated)))
      (should (< (nth 1 desat-hsl) (nth 1 orig-hsl))))))

;;; --- Color Analysis Tests ---

(ert-deftest gypsum-test-color-luminance ()
  "Test luminance calculation."
  ;; White should have luminance ~1.0
  (should (> (gypsum-color-luminance "#FFFFFF") 0.99))
  ;; Black should have luminance ~0.0
  (should (< (gypsum-color-luminance "#000000") 0.01))
  ;; Gray should be in middle
  (let ((gray-lum (gypsum-color-luminance "#808080")))
    (should (and (> gray-lum 0.1) (< gray-lum 0.5)))))

(ert-deftest gypsum-test-color-contrast ()
  "Test contrast ratio calculation."
  ;; Black on white should have max contrast (~21)
  (let ((contrast (gypsum-color-contrast "#FFFFFF" "#000000")))
    (should (> contrast 20)))
  ;; Same color should have min contrast (1)
  (let ((contrast (gypsum-color-contrast "#808080" "#808080")))
    (should (< contrast 1.1))))

(ert-deftest gypsum-test-color-light-dark-p ()
  "Test light/dark detection."
  (should (gypsum-color-light-p "#FFFFFF"))
  (should (gypsum-color-light-p "#F7F7F7"))
  (should (gypsum-color-dark-p "#000000"))
  (should (gypsum-color-dark-p "#0E1415")))

;;; --- Color Blending Tests ---

(ert-deftest gypsum-test-color-blend ()
  "Test color blending."
  ;; 50% blend of black and white should be gray
  (let ((blend (gypsum-color-blend "#000000" "#FFFFFF" 0.5)))
    (let ((rgb (gypsum-color-hex-to-rgb blend)))
      (should (and (> (nth 0 rgb) 120) (< (nth 0 rgb) 136)))))
  ;; 0% blend should be first color
  (should (equal (gypsum-color-blend "#FF0000" "#00FF00" 0.0) "#FF0000"))
  ;; 100% blend should be second color
  (should (equal (gypsum-color-blend "#FF0000" "#00FF00" 1.0) "#00FF00")))

;;; --- Palette Tests ---

(ert-deftest gypsum-test-palette-create-dark ()
  "Test dark palette creation."
  (let ((palette (gypsum-palette-create :seed "#3498DB" :variant 'dark)))
    ;; Should have all required keys
    (should (plist-get palette :background))
    (should (plist-get palette :foreground))
    (should (plist-get palette :string))
    (should (plist-get palette :constant))
    (should (plist-get palette :comment))
    (should (plist-get palette :definition))
    ;; Background should be dark
    (should (gypsum-color-dark-p (plist-get palette :background)))
    ;; Foreground should be light
    (should (gypsum-color-light-p (plist-get palette :foreground)))
    ;; Variant should be stored
    (should (eq (plist-get palette :variant) 'dark))))

(ert-deftest gypsum-test-palette-create-light ()
  "Test light palette creation."
  (let ((palette (gypsum-palette-create :seed "#3498DB" :variant 'light)))
    ;; Background should be light
    (should (gypsum-color-light-p (plist-get palette :background)))
    ;; Foreground should be dark
    (should (gypsum-color-dark-p (plist-get palette :foreground)))
    ;; Variant should be stored
    (should (eq (plist-get palette :variant) 'light))))

(ert-deftest gypsum-test-palette-override ()
  "Test palette with color overrides."
  (let ((custom-comment "#FF00FF")
        (palette (gypsum-palette-create
                  :seed "#3498DB"
                  :variant 'dark
                  :comment "#FF00FF")))
    ;; Comment should be our override
    (should (equal (upcase (plist-get palette :comment))
                   (upcase custom-comment)))))

(ert-deftest gypsum-test-palette-infer-variant ()
  "Test variant inference from background."
  (let ((dark-palette (gypsum-palette-create
                       :background "#0E1415"
                       :definition "#71ADE7"))
        (light-palette (gypsum-palette-create
                        :background "#F7F7F7"
                        :definition "#3498DB")))
    (should (eq (plist-get dark-palette :variant) 'dark))
    (should (eq (plist-get light-palette :variant) 'light))))

(ert-deftest gypsum-test-palette-low-contrast-differs ()
  "Test that low contrast palette differs from normal contrast."
  (let* ((normal (gypsum-palette-create :seed "#3498DB" :variant 'dark :contrast 'normal))
         (low (gypsum-palette-create :seed "#3498DB" :variant 'dark :contrast 'low)))
    ;; Foreground should be darker in low contrast dark theme
    (should-not (equal (plist-get normal :foreground)
                       (plist-get low :foreground)))
    ;; Semantic colors should be less saturated
    (should-not (equal (plist-get normal :string)
                       (plist-get low :string)))
    (should-not (equal (plist-get normal :constant)
                       (plist-get low :constant)))
    (should-not (equal (plist-get normal :comment)
                       (plist-get low :comment)))
    (should-not (equal (plist-get normal :definition)
                       (plist-get low :definition)))
    ;; Contrast should be stored correctly
    (should (eq (plist-get normal :contrast) 'normal))
    (should (eq (plist-get low :contrast) 'low))))

;;; --- Face Generation Tests ---

(ert-deftest gypsum-test-faces-generate ()
  "Test face spec generation."
  (let* ((palette (gypsum-palette-create :seed "#3498DB" :variant 'dark))
         (faces (gypsum-faces-generate palette)))
    ;; Should return a list
    (should (listp faces))
    ;; Should have many faces
    (should (> (length faces) 50))
    ;; First face should be 'default
    (should (eq (caar faces) 'default))
    ;; Each face should be well-formed
    (dolist (face-spec faces)
      (should (symbolp (car face-spec)))
      (should (listp (cadr face-spec))))))

(ert-deftest gypsum-test-faces-resolve-colors ()
  "Test that face colors are properly resolved."
  (let* ((palette '(:foreground "#FFFFFF" :background "#000000" :string "#00FF00"))
         (face-spec '(font-lock-string-face :fg string)))
    ;; Build face spec
    (let ((result (gypsum-faces--build-face-spec face-spec palette)))
      ;; Should have the face name
      (should (eq (car result) 'font-lock-string-face))
      ;; Result structure: (FACE-NAME ((CLASS ATTRS)))
      ;; Get the attrs from the nested structure
      (let* ((display-spec (cadr result))        ; ((CLASS ATTRS))
             (class-attrs (car display-spec))    ; (CLASS ATTRS)
             (attrs (cadr class-attrs)))         ; ATTRS = (:foreground "#00FF00")
        (should (member :foreground attrs))
        (should (member "#00FF00" attrs))))))

;;; --- Theme Generation Tests ---

(ert-deftest gypsum-test-generate-file-content ()
  "Test that generated theme content is valid."
  (let* ((palette (gypsum-palette-create :seed "#3498DB" :variant 'dark))
         (content (gypsum-generate--build-file-content "test-theme" palette)))
    ;; Should be a string
    (should (stringp content))
    ;; Should contain deftheme
    (should (string-match-p "deftheme test-theme" content))
    ;; Should contain custom-theme-set-faces
    (should (string-match-p "custom-theme-set-faces" content))
    ;; Should contain provide-theme
    (should (string-match-p "provide-theme 'test-theme" content))
    ;; Should contain key face definitions
    (should (string-match-p "font-lock-string-face" content))
    (should (string-match-p "font-lock-comment-face" content))
    (should (string-match-p ":foreground ,definition" content))
    ;; The workflow tests verify actual loadability
    ))

(ert-deftest gypsum-test-generate-creates-file ()
  "Test that gypsum-generate creates a file."
  (let* ((temp-dir (make-temp-file "gypsum-test" t))
         (output-path (expand-file-name "test-theme.el" temp-dir)))
    (unwind-protect
        (progn
          (gypsum-generate "test"
                           :seed "#3498DB"
                           :variant 'dark
                           :output output-path
                           :load nil)
          ;; File should exist
          (should (file-exists-p output-path))
          ;; File should have content
          (should (> (file-attribute-size (file-attributes output-path)) 0))
          ;; File should be loadable
          (should (ignore-errors
                    (load output-path nil t)
                    t)))
      ;; Cleanup
      (when (file-exists-p output-path)
        (delete-file output-path))
      (delete-directory temp-dir))))

;;; --- Harmony Function Tests ---

(ert-deftest gypsum-test-complementary ()
  "Test complementary color generation."
  (let* ((color "#0000FF")  ; Blue at 240
         (comp (gypsum-color-complementary color))
         (comp-hsl (gypsum-color-hex-to-hsl comp)))
    ;; Complementary of blue (240) should be yellow (60)
    (should (< (abs (- (nth 0 comp-hsl) 60)) 5))))

(ert-deftest gypsum-test-triadic ()
  "Test triadic color generation."
  (let* ((color "#FF0000")  ; Red at 0
         (triadic (gypsum-color-triadic color)))
    ;; Should return 2 colors
    (should (= (length triadic) 2))
    ;; Colors should be 120 degrees apart from original
    (let ((hsl1 (gypsum-color-hex-to-hsl (nth 0 triadic)))
          (hsl2 (gypsum-color-hex-to-hsl (nth 1 triadic))))
      (should (< (abs (- (nth 0 hsl1) 120)) 5))
      (should (< (abs (- (nth 0 hsl2) 240)) 5)))))

;;; --- Integration Tests ---

(ert-deftest gypsum-test-full-workflow-dark ()
  "Test complete workflow for dark theme."
  (let* ((temp-dir (make-temp-file "gypsum-test" t))
         (output-path (expand-file-name "my-dark-theme.el" temp-dir)))
    (unwind-protect
        (progn
          ;; Generate theme
          (gypsum-generate "my-dark"
                           :seed "#71ADE7"
                           :variant 'dark
                           :contrast 'normal
                           :output output-path
                           :load nil)
          ;; Verify file
          (should (file-exists-p output-path))
          ;; Load and verify theme
          (load output-path nil t)
          (should (memq 'my-dark (custom-available-themes))))
      ;; Cleanup
      (when (file-exists-p output-path)
        (delete-file output-path))
      (delete-directory temp-dir))))

(ert-deftest gypsum-test-full-workflow-light ()
  "Test complete workflow for light theme."
  (let* ((temp-dir (make-temp-file "gypsum-test" t))
         (output-path (expand-file-name "my-light-theme.el" temp-dir)))
    (unwind-protect
        (progn
          ;; Generate theme
          (gypsum-generate "my-light"
                           :seed "#325CC0"
                           :variant 'light
                           :contrast 'normal
                           :output output-path
                           :load nil)
          ;; Verify file
          (should (file-exists-p output-path))
          ;; Load and verify theme
          (load output-path nil t)
          (should (memq 'my-light (custom-available-themes))))
      ;; Cleanup
      (when (file-exists-p output-path)
        (delete-file output-path))
      (delete-directory temp-dir))))

(ert-deftest gypsum-test-generate-all ()
  "Test generating all 4 theme variants."
  (let* ((temp-dir (make-temp-file "gypsum-test" t)))
    (unwind-protect
        (let ((results (gypsum-generate-all "test"
                                            :seed "#5E81AC"
                                            :output-dir temp-dir)))
          ;; Should return 4 paths
          (should (= (length results) 4))
          ;; All files should exist
          (should (file-exists-p (expand-file-name "test-dark-theme.el" temp-dir)))
          (should (file-exists-p (expand-file-name "test-dark-lc-theme.el" temp-dir)))
          (should (file-exists-p (expand-file-name "test-light-theme.el" temp-dir)))
          (should (file-exists-p (expand-file-name "test-light-lc-theme.el" temp-dir)))
          ;; All files should be loadable
          (dolist (path results)
            (should (ignore-errors (load path nil t) t))))
      ;; Cleanup
      (dolist (file (directory-files temp-dir t "\\.el$"))
        (delete-file file))
      (delete-directory temp-dir))))

(ert-deftest gypsum-test-preview-create-temp-theme ()
  "Test that preview creates a valid temporary theme."
  (let* ((palette (gypsum-palette-create :seed "#3498DB" :variant 'dark))
         (result (gypsum-preview--create-temp-theme palette))
         (theme-name (car result))
         (face-list (cdr result)))
    (unwind-protect
        (progn
          ;; Theme should be a symbol
          (should (symbolp theme-name))
          ;; Theme should be declared
          (should (custom-theme-p theme-name))
          ;; Theme should have face settings
          (should (get theme-name 'theme-settings))
          ;; Face list should be non-empty
          (should (listp face-list))
          (should (> (length face-list) 0)))
      ;; Cleanup
      (disable-theme theme-name))))

(provide 'gypsum-test)

;;; gypsum-test.el ends here
