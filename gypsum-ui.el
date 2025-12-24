;;; gypsum-ui.el --- Interactive UI for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David
;; Keywords: faces, themes

;; This file is part of Gypsum.

;;; Commentary:

;; Interactive commands for Gypsum theme generator:
;; - `gypsum' - main entry point for theme generation
;; - `gypsum-from-preset' - generate from a curated preset
;; - `gypsum-preview-dismiss' - dismiss active preview
;; - `gypsum-show-palette' - display generated palette colors

;;; Code:

(require 'gypsum-color)
(require 'gypsum-presets)
(require 'gypsum-palette)
(require 'gypsum-faces)
(require 'gypsum-generate)

;;; --- Color Picker ---

(defvar gypsum-ui--color-picker-callback nil
  "Callback function for color picker.")

(defvar gypsum-ui--color-picker-current nil
  "Currently selected color in picker.")

(defvar gypsum-ui--color-picker-colors nil
  "Current color list being displayed in picker.")

(defvar gypsum-ui--color-picker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gypsum-ui--color-picker-select)
    (define-key map (kbd "q") #'gypsum-ui--color-picker-quit)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "h") #'gypsum-ui--color-picker-enter-hex)
    (define-key map (kbd "s") #'gypsum-ui--color-picker-select-by-name)
    map)
  "Keymap for color picker buffer.")

(define-derived-mode gypsum-ui--color-picker-mode special-mode "Gypsum-Color"
  "Major mode for Gypsum color picker."
  :keymap gypsum-ui--color-picker-mode-map
  (setq cursor-type 'box)
  (setq buffer-read-only t))

(defvar gypsum-ui--curated-colors
  '(;; Blues
    ("#1E3A5F" "Deep Navy")
    ("#2563EB" "Royal Blue")
    ("#325CC0" "Alabaster Blue")
    ("#3B82F6" "Blue")
    ("#60A5FA" "Light Blue")
    ("#71ADE7" "Sky Blue")
    ;; Greens
    ("#14532D" "Forest Green")
    ("#448C27" "Alabaster Green")
    ("#16A34A" "Green")
    ("#22C55E" "Emerald")
    ("#4ADE80" "Light Green")
    ("#99C592" "Sage")
    ;; Purples
    ("#581C87" "Deep Purple")
    ("#7A3E9D" "Alabaster Magenta")
    ("#7C3AED" "Violet")
    ("#8B5CF6" "Purple")
    ("#A78BFA" "Lavender")
    ("#C4B5FD" "Light Lavender")
    ;; Reds
    ("#7F1D1D" "Maroon")
    ("#AA3731" "Alabaster Red")
    ("#DC2626" "Red")
    ("#EF4444" "Coral")
    ("#DB2777" "Pink")
    ("#F472B6" "Light Pink")
    ;; Oranges/Yellows
    ("#9A3412" "Rust")
    ("#EA580C" "Orange")
    ("#F97316" "Bright Orange")
    ("#FBBF24" "Amber")
    ("#FCD34D" "Yellow")
    ("#FDE68A" "Light Yellow")
    ;; Teals/Cyans
    ("#134E4A" "Teal Dark")
    ("#0D9488" "Teal")
    ("#14B8A6" "Aqua")
    ("#2DD4BF" "Turquoise")
    ("#5EEAD4" "Light Teal")
    ("#99F6E4" "Pale Teal"))
  "Curated color palette with names for easy selection.")

(defun gypsum-ui--color-picker-format-line (color name)
  "Format a line for COLOR with NAME in the picker buffer."
  (let* ((swatch (propertize "    " 'face `(:background ,color)))
         (text (format " %s  %s" color name)))
    (propertize (concat swatch text "\n")
                'gypsum-color color)))

(defun gypsum-ui--color-picker-insert-colors ()
  "Insert color swatches into the picker buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Gypsum Color Picker\n" 'face 'bold))
    (insert "-------------------------------------------\n\n")
    (insert "RET: select, s: search by name, h: enter hex, q: quit\n\n")
    (insert (propertize "Blues\n" 'face 'bold))
    (dolist (c (seq-take gypsum-ui--curated-colors 6))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Greens\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 6 12))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Purples\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 12 18))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Reds\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 18 24))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Oranges/Yellows\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 24 30))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Teals/Cyans\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 30))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (goto-char (point-min))
    (forward-line 5)))

(defun gypsum-ui--color-picker-select ()
  "Select the color on the current line."
  (interactive)
  (let ((color (get-text-property (point) 'gypsum-color)))
    (if color
        (progn
          (setq gypsum-ui--color-picker-current color)
          (quit-window)
          (if gypsum-ui--color-picker-callback
              (funcall gypsum-ui--color-picker-callback color)
            (exit-recursive-edit)))
      (message "No color on this line"))))

(defun gypsum-ui--color-picker-quit ()
  "Quit the color picker without selecting."
  (interactive)
  (setq gypsum-ui--color-picker-current nil)
  (quit-window)
  (unless gypsum-ui--color-picker-callback
    (exit-recursive-edit)))

(defun gypsum-ui--color-picker-enter-hex ()
  "Enter a hex color code manually."
  (interactive)
  (let ((color (read-string "Hex color (e.g., #3498db): ")))
    (when (string-match-p "^#[0-9A-Fa-f]\\{6\\}$" color)
      (setq gypsum-ui--color-picker-current color)
      (quit-window)
      (if gypsum-ui--color-picker-callback
          (funcall gypsum-ui--color-picker-callback color)
        (exit-recursive-edit)))))

(defun gypsum-ui--color-picker-select-by-name ()
  "Select a color by searching for its name."
  (interactive)
  (let* ((colors (or gypsum-ui--color-picker-colors gypsum-ui--curated-colors))
         (names (mapcar #'cadr colors))
         (selected-name (completing-read "Color name: " names nil t))
         (color-entry (seq-find (lambda (c) (string= (cadr c) selected-name))
                               colors)))
    (when color-entry
      (setq gypsum-ui--color-picker-current (car color-entry))
      (quit-window)
      (if gypsum-ui--color-picker-callback
          (funcall gypsum-ui--color-picker-callback (car color-entry))
        (exit-recursive-edit)))))

(defun gypsum-ui--pick-color (prompt &optional callback)
  "Display color picker with PROMPT.
If CALLBACK is provided, call it with the selected color.
Otherwise, return the selected color synchronously."
  (let ((buf (get-buffer-create "*Gypsum Colors*")))
    (setq gypsum-ui--color-picker-callback callback)
    (setq gypsum-ui--color-picker-current nil)
    (setq gypsum-ui--color-picker-colors gypsum-ui--curated-colors)
    (with-current-buffer buf
      (gypsum-ui--color-picker-mode)
      (gypsum-ui--color-picker-insert-colors))
    (pop-to-buffer buf)
    (message "%s" prompt)
    (unless callback
      (recursive-edit)
      gypsum-ui--color-picker-current)))

;;; --- Preset Picker ---

(defun gypsum-ui--select-preset ()
  "Interactively select a preset."
  (let* ((presets (gypsum-preset-list))
         (preset-names (mapcar #'symbol-name presets)))
    (intern (completing-read "Preset: " preset-names nil t))))

;;; --- Live Preview ---

(defvar gypsum-ui--preview-active-theme nil
  "Currently active preview theme.")

(defvar gypsum-ui--preview-original-theme nil
  "Theme that was active before preview.")

(defvar gypsum-ui--preview-current-palette nil
  "Palette used to create the current preview.")

(defun gypsum-ui--preview-create-temp-theme (palette)
  "Create a temporary theme from PALETTE for preview."
  (let* ((theme-name (intern (format "gypsum-preview-%s" (random 10000))))
         (face-specs (gypsum-faces-generate palette))
         (face-list nil))
    (custom-declare-theme theme-name nil)
    (put theme-name 'theme-settings nil)
    (let ((theme-specs
           (mapcar (lambda (spec)
                     (let ((face (car spec))
                           (attrs (cadr (caadr spec))))
                       (push face face-list)
                       `(,face ((t ,attrs)))))
                   face-specs)))
      (apply #'custom-theme-set-faces theme-name theme-specs))
    (cons theme-name face-list)))

(defun gypsum-ui--preview-palette (palette)
  "Preview a theme from PALETTE.
Use `gypsum-preview-dismiss' to remove the preview."
  (setq gypsum-ui--preview-original-theme
        (car custom-enabled-themes))
  (when gypsum-ui--preview-active-theme
    (disable-theme gypsum-ui--preview-active-theme))
  (setq gypsum-ui--preview-current-palette palette)
  (let* ((result (gypsum-ui--preview-create-temp-theme palette))
         (theme-name (car result))
         (face-list (cdr result)))
    (setq gypsum-ui--preview-active-theme theme-name)
    (enable-theme theme-name)
    (dolist (face face-list)
      (when (facep face)
        (face-spec-recalc face nil)))
    (redraw-display)
    (message "Preview active. Use M-x gypsum-preview-dismiss to remove.")))

;;;###autoload
(defun gypsum-preview-dismiss ()
  "Dismiss the current theme preview."
  (interactive)
  (when gypsum-ui--preview-active-theme
    (disable-theme gypsum-ui--preview-active-theme)
    (setq gypsum-ui--preview-active-theme nil))
  (when gypsum-ui--preview-original-theme
    (enable-theme gypsum-ui--preview-original-theme)
    (setq gypsum-ui--preview-original-theme nil))
  (setq gypsum-ui--preview-current-palette nil)
  (message "Preview dismissed"))

;;; --- Transformation Selection ---

(defun gypsum-ui--select-transform ()
  "Interactively select a transformation to apply."
  (let ((choice (completing-read
                 "Transform: "
                 '("none" "hue-shift" "definition-only" "blend" "derive-dark" "derive-light")
                 nil t nil nil "none")))
    (if (string= choice "none")
        nil
      (intern choice))))

(defun gypsum-ui--apply-transform (palette transform)
  "Apply TRANSFORM to PALETTE, prompting for parameters as needed."
  (pcase transform
    ('hue-shift
     (let ((degrees (read-number "Hue shift degrees (-180 to 180): " 0)))
       (gypsum-palette-tint palette :mode 'hue-shift :degrees degrees)))
    ('definition-only
     (let ((color (gypsum-ui--pick-color "Select definition color:")))
       (gypsum-palette-tint palette :mode 'definition-only :definition color)))
    ('blend
     (let ((color (gypsum-ui--pick-color "Select color to blend toward:"))
           (amount (read-number "Blend amount (0-100): " 20)))
       (gypsum-palette-tint palette :mode 'blend :color color :amount amount)))
    ('derive-dark
     (if (eq (plist-get palette :variant) 'light)
         (gypsum-palette-derive-dark palette)
       (error "Can only derive dark from a light palette")))
    ('derive-light
     (if (eq (plist-get palette :variant) 'dark)
         (gypsum-palette-derive-light palette)
       (error "Can only derive light from a dark palette")))
    (_ palette)))

;;; --- Interactive Theme Generator ---

;;;###autoload
(defun gypsum ()
  "Interactively generate Alabaster-style Emacs themes.

Workflow:
1. Choose a preset or generate from seed color
2. Optionally apply a transformation (tint or derive variant)
3. Preview and generate the theme file"
  (interactive)
  (let* ((source (completing-read
                  "Source: "
                  '("Use preset" "Generate from seed")
                  nil t))
         palette)
    ;; Step 1: Get base palette
    (cond
     ((string= source "Use preset")
      (let ((preset (gypsum-ui--select-preset)))
        (setq palette (gypsum-preset-get preset))
        (unless palette
          (error "Preset not found: %s" preset))))
     ((string= source "Generate from seed")
      (let* ((seed (gypsum-ui--pick-color "Select seed color (becomes definition):"))
             (variant (intern (completing-read "Variant: " '("light" "dark") nil t))))
        (unless seed
          (error "Seed color is required"))
        (setq palette (gypsum-palette-generate seed variant)))))
    ;; Step 2: Optionally transform
    (when (y-or-n-p "Apply a transformation? ")
      (let ((transform (gypsum-ui--select-transform)))
        (when transform
          (setq palette (gypsum-ui--apply-transform palette transform)))))
    ;; Step 3: Preview
    (when (y-or-n-p "Preview the theme? ")
      (gypsum-ui--preview-palette palette)
      (unless (y-or-n-p "Continue with generation? ")
        (gypsum-preview-dismiss)
        (user-error "Generation cancelled")))
    ;; Step 4: Generate
    (let* ((name (read-string "Theme name: "))
           (output-dir (read-directory-name "Output directory: " gypsum-output-directory))
           output-path)
      (when (string-empty-p name)
        (user-error "Theme name cannot be empty"))
      (when (string-suffix-p "-theme" name)
        (setq name (substring name 0 -6)))
      (setq output-path (expand-file-name (format "%s-theme.el" name) output-dir))
      (gypsum-generate-from-palette name palette output-path nil)
      ;; Dismiss preview
      (when gypsum-ui--preview-active-theme
        (gypsum-preview-dismiss))
      ;; Offer to load
      (when (y-or-n-p "Load the generated theme? ")
        (load-file output-path)
        (load-theme (intern name) t))
      (message "Generated: %s" output-path))))

;;;###autoload
(defun gypsum-from-preset ()
  "Generate a theme directly from a curated preset."
  (interactive)
  (let* ((preset (gypsum-ui--select-preset))
         (palette (gypsum-preset-get preset))
         (name (read-string (format "Theme name [%s]: " preset)
                            nil nil (symbol-name preset)))
         (output-dir (read-directory-name "Output directory: " gypsum-output-directory)))
    (unless palette
      (error "Preset not found: %s" preset))
    (gypsum-generate-from-palette
     name palette
     (expand-file-name (format "%s-theme.el" name) output-dir)
     t)))

;;; --- Palette Preview ---

;;;###autoload
(defun gypsum-show-palette (&optional preset)
  "Display a palette for PRESET.
If called interactively, prompts for preset."
  (interactive
   (list (gypsum-ui--select-preset)))
  (let* ((palette (gypsum-preset-get preset))
         (buf (get-buffer-create "*Gypsum Palette*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Gypsum Palette Preview\n" 'face 'bold))
        (insert "-------------------------------------------\n\n")
        (insert (format "Preset: %s\nVariant: %s\n\n" preset (plist-get palette :variant)))
        (insert (propertize "Semantic Colors (The 4 Categories)\n" 'face 'bold))
        (dolist (key '(:string :constant :comment :definition))
          (let ((value (plist-get palette key)))
            (insert (propertize "    " 'face `(:background ,value))
                    (format " %-15s %s\n"
                            (substring (symbol-name key) 1)
                            value))))
        (insert (propertize "\nBase Colors\n" 'face 'bold))
        (dolist (key '(:background :foreground :fg-dim :bg-alt))
          (let ((value (plist-get palette key)))
            (insert (propertize "    " 'face `(:background ,value))
                    (format " %-15s %s\n"
                            (substring (symbol-name key) 1)
                            value))))
        (insert (propertize "\nUI Colors\n" 'face 'bold))
        (dolist (key '(:selection :highlight :find-hl))
          (let ((value (plist-get palette key)))
            (insert (propertize "    " 'face `(:background ,value))
                    (format " %-15s %s\n"
                            (substring (symbol-name key) 1)
                            value))))
        (insert (propertize "\nStatus Colors\n" 'face 'bold))
        (dolist (key '(:error :warning :success))
          (let ((value (plist-get palette key)))
            (insert (propertize "    " 'face `(:background ,value))
                    (format " %-15s %s\n"
                            (substring (symbol-name key) 1)
                            value))))
        (insert "\n")
        (special-mode)))
    (pop-to-buffer buf)))

(provide 'gypsum-ui)

;;; gypsum-ui.el ends here
