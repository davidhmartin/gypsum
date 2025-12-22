;;; gypsum-ui.el --- Interactive UI for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Gypsum Contributors
;; Keywords: faces, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interactive commands for Gypsum theme generator:
;; - `gypsum-generate-theme' - guided theme creation
;; - `gypsum-preview' - live preview without writing file
;; - `gypsum-pick-color' - visual color picker

;;; Code:

(require 'gypsum-color)
(require 'gypsum-palette)
(require 'gypsum-faces)
(require 'gypsum-generate)

;;; --- Color Picker ---

(defvar gypsum-color-picker-callback nil
  "Callback function for color picker.")

(defvar gypsum-color-picker-current nil
  "Currently selected color in picker.")

(defvar gypsum-color-picker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gypsum-color-picker-select)
    (define-key map (kbd "q") #'gypsum-color-picker-quit)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "h") #'gypsum-color-picker-enter-hex)
    map)
  "Keymap for color picker buffer.")

(define-derived-mode gypsum-color-picker-mode special-mode "Gypsum-Color"
  "Major mode for Gypsum color picker."
  (setq cursor-type 'box)
  (setq buffer-read-only t))

(defvar gypsum-curated-colors
  '(;; Blues
    ("#1E3A5F" "Deep Navy")
    ("#2563EB" "Royal Blue")
    ("#3B82F6" "Blue")
    ("#60A5FA" "Light Blue")
    ("#71ADE7" "Sky Blue")
    ("#93C5FD" "Pale Blue")
    ;; Greens
    ("#14532D" "Forest Green")
    ("#16A34A" "Green")
    ("#22C55E" "Emerald")
    ("#4ADE80" "Light Green")
    ("#86EFAC" "Mint")
    ("#99C592" "Sage")
    ;; Purples
    ("#581C87" "Deep Purple")
    ("#7C3AED" "Violet")
    ("#8B5CF6" "Purple")
    ("#A78BFA" "Lavender")
    ("#9999FF" "Periwinkle")
    ("#C4B5FD" "Light Lavender")
    ;; Reds/Pinks
    ("#7F1D1D" "Maroon")
    ("#DC2626" "Red")
    ("#EF4444" "Coral")
    ("#F87171" "Salmon")
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
    ("#99F6E4" "Pale Teal")
    ;; Neutrals (for backgrounds)
    ("#0E1415" "Dark BG")
    ("#1A2122" "Dark Alt")
    ("#F7F7F7" "Light BG")
    ("#EEEEEE" "Light Alt"))
  "Curated color palette with names for easy selection.")

(defun gypsum-color-picker--format-color-line (color name)
  "Format a line for COLOR with NAME in the picker buffer."
  (let* ((swatch (propertize "    " 'face `(:background ,color)))
         (text (format " %s  %s" color name)))
    (propertize (concat swatch text "\n")
                'gypsum-color color)))

(defun gypsum-color-picker--insert-colors ()
  "Insert color swatches into the picker buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Gypsum Color Picker\n" 'face 'bold))
    (insert "═══════════════════════════════════════\n\n")
    (insert "Use arrow keys to navigate, RET to select, h for hex input, q to quit\n\n")
    (insert (propertize "Blues\n" 'face 'bold))
    (dolist (c (seq-take gypsum-curated-colors 6))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (insert "\n" (propertize "Greens\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-curated-colors 6 12))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (insert "\n" (propertize "Purples\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-curated-colors 12 18))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (insert "\n" (propertize "Reds/Pinks\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-curated-colors 18 24))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (insert "\n" (propertize "Oranges/Yellows\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-curated-colors 24 30))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (insert "\n" (propertize "Teals/Cyans\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-curated-colors 30 36))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (insert "\n" (propertize "Backgrounds\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-curated-colors 36))
      (insert (gypsum-color-picker--format-color-line (car c) (cadr c))))
    (goto-char (point-min))
    (forward-line 5)))

(defun gypsum-color-picker-select ()
  "Select the color on the current line."
  (interactive)
  (let ((color (get-text-property (point) 'gypsum-color)))
    (if color
        (progn
          (setq gypsum-color-picker-current color)
          (quit-window)
          (if gypsum-color-picker-callback
              (funcall gypsum-color-picker-callback color)
            ;; Exit recursive-edit for synchronous mode
            (exit-recursive-edit)))
      (message "No color on this line"))))

(defun gypsum-color-picker-quit ()
  "Quit the color picker without selecting."
  (interactive)
  (setq gypsum-color-picker-current nil)
  (quit-window)
  ;; Exit recursive-edit for synchronous mode
  (unless gypsum-color-picker-callback
    (exit-recursive-edit)))

(defun gypsum-color-picker-enter-hex ()
  "Enter a hex color code manually."
  (interactive)
  (let ((color (read-string "Hex color (e.g., #3498db): ")))
    (when (string-match-p "^#[0-9A-Fa-f]\\{6\\}$" color)
      (setq gypsum-color-picker-current color)
      (quit-window)
      (if gypsum-color-picker-callback
          (funcall gypsum-color-picker-callback color)
        ;; Exit recursive-edit for synchronous mode
        (exit-recursive-edit)))))

(defun gypsum-pick-color (prompt &optional callback)
  "Display color picker with PROMPT.
If CALLBACK is provided, call it with the selected color.
Otherwise, return the selected color synchronously."
  (let ((buf (get-buffer-create "*Gypsum Colors*")))
    (setq gypsum-color-picker-callback callback)
    (setq gypsum-color-picker-current nil)
    (with-current-buffer buf
      (gypsum-color-picker-mode)
      (gypsum-color-picker--insert-colors))
    (pop-to-buffer buf)
    (message "%s" prompt)
    (unless callback
      ;; Synchronous mode - wait for selection
      (recursive-edit)
      gypsum-color-picker-current)))

;;;###autoload
(defun gypsum-pick-color-interactive ()
  "Interactively pick a color and insert or display it."
  (interactive)
  (gypsum-pick-color "Select a color:"
                     (lambda (color)
                       (message "Selected: %s" color)
                       (when (yes-or-no-p "Insert at point? ")
                         (insert color)))))

;;; --- Live Preview ---

(defvar gypsum-preview--active-theme nil
  "Currently active preview theme.")

(defvar gypsum-preview--original-theme nil
  "Theme that was active before preview.")

(defvar gypsum-preview--current-args nil
  "Arguments used to create the current preview, for saving.")

(defun gypsum-preview--create-temp-theme (palette)
  "Create a temporary theme from PALETTE for preview."
  (let* ((theme-name (intern (format "gypsum-preview-%s" (random 10000))))
         (face-specs (gypsum-faces-generate palette))
         (face-list nil))
    ;; Properly declare the theme so custom-theme-set-faces recognizes it
    (custom-declare-theme theme-name nil)
    (put theme-name 'theme-settings nil)
    ;; Build the face specs and track which faces we're setting
    (let ((theme-specs
           (mapcar (lambda (spec)
                     ;; spec structure: (FACE-NAME ((CLASS-DISPLAY ATTRS)))
                     (let ((face (car spec))
                           (attrs (cadr (caadr spec))))
                       (push face face-list)
                       `(,face ((t ,attrs)))))
                   face-specs)))
      (apply #'custom-theme-set-faces theme-name theme-specs))
    ;; Return both theme name and face list for recalculation
    (cons theme-name face-list)))

;;;###autoload
(cl-defun gypsum-preview (&rest args &key seed variant contrast
                                background string constant comment definition)
  "Preview a theme without writing a file.

Takes the same arguments as `gypsum-generate'.
Use `gypsum-preview-dismiss' to remove the preview."
  (interactive
   (list :seed (gypsum-pick-color "Select seed color:")
         :variant (intern (completing-read "Variant: " '("dark" "light")))))
  ;; Store current theme
  (setq gypsum-preview--original-theme
        (car custom-enabled-themes))
  ;; Disable current preview if any
  (when gypsum-preview--active-theme
    (disable-theme gypsum-preview--active-theme))
  ;; Store args for potential save
  (setq gypsum-preview--current-args args)
  ;; Create palette and temp theme
  (let* ((palette (apply #'gypsum-palette-create args))
         (result (gypsum-preview--create-temp-theme palette))
         (theme-name (car result))
         (face-list (cdr result)))
    (setq gypsum-preview--active-theme theme-name)
    (enable-theme theme-name)
    ;; Force face recalculation for all affected faces
    (dolist (face face-list)
      (when (facep face)
        (face-spec-recalc face nil)))
    ;; Redisplay to ensure changes are visible
    (redraw-display)
    (message "Preview active. Use M-x gypsum-preview-dismiss to remove, or M-x gypsum-preview-save to save.")))

;;;###autoload
(defun gypsum-preview-dismiss ()
  "Dismiss the current theme preview."
  (interactive)
  (when gypsum-preview--active-theme
    (disable-theme gypsum-preview--active-theme)
    (setq gypsum-preview--active-theme nil))
  (when gypsum-preview--original-theme
    (enable-theme gypsum-preview--original-theme)
    (setq gypsum-preview--original-theme nil))
  (setq gypsum-preview--current-args nil)
  (message "Preview dismissed"))

;;;###autoload
(defun gypsum-preview-save (name)
  "Save the current preview as a theme file with NAME."
  (interactive "sTheme name: ")
  (unless gypsum-preview--active-theme
    (error "No preview is currently active"))
  (unless gypsum-preview--current-args
    (error "No preview settings available to save"))
  (let* ((output-dir (read-directory-name
                      "Output directory: "
                      gypsum-output-directory))
         (output-path (expand-file-name
                       (format "%s-theme.el" name)
                       output-dir)))
    (apply #'gypsum-generate name
           :output output-path
           gypsum-preview--current-args)
    ;; Dismiss the preview and load the saved theme
    (gypsum-preview-dismiss)
    (load-theme (intern name) t)
    (message "Theme '%s' saved and loaded!" name)))

;;; --- Interactive Theme Generator ---

(defun gypsum--read-variant ()
  "Read theme variant from user."
  (intern (completing-read "Theme variant: " '("dark" "light") nil t)))

(defun gypsum--read-contrast ()
  "Read contrast level from user."
  (intern (completing-read "Contrast level: " '("normal" "low") nil t "normal")))

(defun gypsum--read-color (prompt &optional default)
  "Read a color from user with PROMPT.
DEFAULT is used if provided and user enters empty string."
  (let ((input (completing-read
                (format "%s%s: "
                        prompt
                        (if default (format " [%s]" default) ""))
                (mapcar #'car gypsum-curated-colors)
                nil nil nil nil default)))
    (if (string-empty-p input)
        default
      input)))

(defun gypsum--ask-override-p ()
  "Ask if user wants to override individual colors."
  (yes-or-no-p "Do you want to customize individual colors? "))

;;;###autoload
(defun gypsum-generate-theme ()
  "Interactively generate an Alabaster-style theme.
Guides you through selecting colors and options, with preview."
  (interactive)
  (let* ((name (read-string "Theme name: "))
         (variant (gypsum--read-variant))
         (contrast (gypsum--read-contrast))
         (seed nil)
         (background nil)
         (string-override nil)
         (constant-override nil)
         (comment-override nil)
         args)
    ;; Validate name
    (when (string-empty-p name)
      (error "Theme name cannot be empty"))
    (when (string-suffix-p "-theme" name)
      (setq name (substring name 0 -6)))
    ;; Get seed color
    (setq seed (gypsum-pick-color "Select seed color (this sets the theme's character):"))
    (unless seed
      (error "Seed color is required"))
    ;; Optional: custom background
    (when (yes-or-no-p "Customize background color? (default is auto-generated) ")
      (setq background (gypsum-pick-color "Select background color:")))
    ;; Optional: override individual semantic colors
    (when (gypsum--ask-override-p)
      (when (yes-or-no-p "Customize string color? ")
        (setq string-override (gypsum-pick-color "Select string color:")))
      (when (yes-or-no-p "Customize constant color? ")
        (setq constant-override (gypsum-pick-color "Select constant color:")))
      (when (yes-or-no-p "Customize comment color? ")
        (setq comment-override (gypsum-pick-color "Select comment color:"))))
    ;; Build args
    (setq args (list :seed seed :variant variant :contrast contrast))
    (when background (setq args (plist-put args :background background)))
    (when string-override (setq args (plist-put args :string string-override)))
    (when constant-override (setq args (plist-put args :constant constant-override)))
    (when comment-override (setq args (plist-put args :comment comment-override)))
    ;; Preview
    (when (yes-or-no-p "Preview the theme before saving? ")
      (apply #'gypsum-preview args)
      (unless (yes-or-no-p "Save this theme? ")
        (gypsum-preview-dismiss)
        (error "Theme generation cancelled")))
    ;; Generate
    (let ((output-dir (read-directory-name
                       "Output directory: "
                       gypsum-output-directory)))
      (setq args (plist-put args :output
                            (expand-file-name
                             (format "%s-theme.el" name)
                             output-dir)))
      (apply #'gypsum-generate name args))
    ;; Dismiss preview if still active
    (when gypsum-preview--active-theme
      (gypsum-preview-dismiss))
    ;; Offer to load the theme
    (when (yes-or-no-p "Load the generated theme now? ")
      (load-theme (intern name) t)
      (message "Theme '%s' loaded!" name))))

;;; --- Palette Preview ---

;;;###autoload
(defun gypsum-show-palette (seed variant)
  "Display the palette that would be generated from SEED for VARIANT."
  (interactive
   (list (gypsum-pick-color "Select seed color:")
         (gypsum--read-variant)))
  (let* ((palette (gypsum-palette-create :seed seed :variant variant))
         (buf (get-buffer-create "*Gypsum Palette*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Gypsum Palette Preview\n" 'face 'bold))
        (insert "══════════════════════════════════════\n\n")
        (insert (format "Seed: %s\nVariant: %s\n\n" seed variant))
        ;; Show each color
        (cl-loop for (key value) on palette by #'cddr
                 when (stringp value)
                 do (insert (propertize "    " 'face `(:background ,value))
                            (format " %-15s %s\n"
                                    (substring (symbol-name key) 1)
                                    value)))
        (insert "\n")
        (special-mode)))
    (pop-to-buffer buf)))

(provide 'gypsum-ui)

;;; gypsum-ui.el ends here
