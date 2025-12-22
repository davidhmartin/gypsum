;;; gypsum-ui.el --- Interactive UI for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Gypsum Contributors
;; Keywords: faces, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interactive commands for Gypsum theme generator:
;; - `gypsum' - main entry point for theme generation
;; - `gypsum-preview-dismiss' - dismiss active preview
;; - `gypsum-show-palette' - display generated palette colors

;;; Code:

(require 'gypsum-color)
(require 'gypsum-palette)
(require 'gypsum-faces)
(require 'gypsum-generate)

;;; --- Color Picker ---

(defvar gypsum-ui--color-picker-callback nil
  "Callback function for color picker.")

(defvar gypsum-ui--color-picker-current nil
  "Currently selected color in picker.")

(defvar gypsum-ui--color-picker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gypsum-ui--color-picker-select)
    (define-key map (kbd "q") #'gypsum-ui--color-picker-quit)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "h") #'gypsum-ui--color-picker-enter-hex)
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
    (insert "═══════════════════════════════════════\n\n")
    (insert "Use arrow keys to navigate, RET to select, h for hex input, q to quit\n\n")
    (insert (propertize "Blues\n" 'face 'bold))
    (dolist (c (seq-take gypsum-ui--curated-colors 6))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Greens\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 6 12))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Purples\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 12 18))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Reds/Pinks\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 18 24))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Oranges/Yellows\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 24 30))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Teals/Cyans\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 30 36))
      (insert (gypsum-ui--color-picker-format-line (car c) (cadr c))))
    (insert "\n" (propertize "Backgrounds\n" 'face 'bold))
    (dolist (c (seq-subseq gypsum-ui--curated-colors 36))
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
            ;; Exit recursive-edit for synchronous mode
            (exit-recursive-edit)))
      (message "No color on this line"))))

(defun gypsum-ui--color-picker-quit ()
  "Quit the color picker without selecting."
  (interactive)
  (setq gypsum-ui--color-picker-current nil)
  (quit-window)
  ;; Exit recursive-edit for synchronous mode
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
        ;; Exit recursive-edit for synchronous mode
        (exit-recursive-edit)))))

(defun gypsum-ui--pick-color (prompt &optional callback)
  "Display color picker with PROMPT.
If CALLBACK is provided, call it with the selected color.
Otherwise, return the selected color synchronously."
  (let ((buf (get-buffer-create "*Gypsum Colors*")))
    (setq gypsum-ui--color-picker-callback callback)
    (setq gypsum-ui--color-picker-current nil)
    (with-current-buffer buf
      (gypsum-ui--color-picker-mode)
      (gypsum-ui--color-picker-insert-colors))
    (pop-to-buffer buf)
    (message "%s" prompt)
    (unless callback
      ;; Synchronous mode - wait for selection
      (recursive-edit)
      gypsum-ui--color-picker-current)))

;;; --- Live Preview ---

(defvar gypsum-ui--preview-active-theme nil
  "Currently active preview theme.")

(defvar gypsum-ui--preview-original-theme nil
  "Theme that was active before preview.")

(defvar gypsum-ui--preview-current-args nil
  "Arguments used to create the current preview, for saving.")

(defun gypsum-ui--preview-create-temp-theme (palette)
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

(defun gypsum-ui--preview (args)
  "Preview a theme with ARGS (plist).
Use `gypsum-preview-dismiss' to remove the preview."
  ;; Store current theme
  (setq gypsum-ui--preview-original-theme
        (car custom-enabled-themes))
  ;; Disable current preview if any
  (when gypsum-ui--preview-active-theme
    (disable-theme gypsum-ui--preview-active-theme))
  ;; Store args for potential save
  (setq gypsum-ui--preview-current-args args)
  ;; Create palette and temp theme
  (let* ((palette (apply #'gypsum-palette-create args))
         (result (gypsum-ui--preview-create-temp-theme palette))
         (theme-name (car result))
         (face-list (cdr result)))
    (setq gypsum-ui--preview-active-theme theme-name)
    (enable-theme theme-name)
    ;; Force face recalculation for all affected faces
    (dolist (face face-list)
      (when (facep face)
        (face-spec-recalc face nil)))
    ;; Redisplay to ensure changes are visible
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
  (setq gypsum-ui--preview-current-args nil)
  (message "Preview dismissed"))

;;; --- Variant Selection ---

(defvar gypsum-ui--variant-options
  '(("dark" . (dark . normal))
    ("dark-lc" . (dark . low))
    ("light" . (light . normal))
    ("light-lc" . (light . low)))
  "Mapping of variant names to (variant . contrast) pairs.")

(defun gypsum-ui--select-variants ()
  "Prompt user to select which theme variants to generate.
Returns a list of selected variant keys."
  (let* ((options '("dark" "dark-lc" "light" "light-lc"))
         (prompt "Select variants (comma-separated, or 'all'): ")
         (input (completing-read prompt
                                 (cons "all" options)
                                 nil nil nil nil "all")))
    (if (string= input "all")
        options
      (mapcar #'string-trim (split-string input ",")))))

;;; --- Interactive Theme Generator ---

(defun gypsum-ui--read-color (prompt &optional default)
  "Read a color from user with PROMPT.
DEFAULT is used if provided and user enters empty string."
  (let ((input (completing-read
                (format "%s%s: "
                        prompt
                        (if default (format " [%s]" default) ""))
                (mapcar #'car gypsum-ui--curated-colors)
                nil nil nil nil default)))
    (if (string-empty-p input)
        default
      input)))

;;;###autoload
(defun gypsum ()
  "Interactively generate Alabaster-style Emacs themes.
Guides you through selecting colors and which variants to generate."
  (interactive)
  (let* ((name (read-string "Theme name: "))
         (seed nil)
         (background nil)
         (string-override nil)
         (constant-override nil)
         (comment-override nil)
         (variants nil)
         (output-dir nil)
         (base-args nil)
         (generated-files nil))
    ;; Validate name
    (when (string-empty-p name)
      (error "Theme name cannot be empty"))
    (when (string-suffix-p "-theme" name)
      (setq name (substring name 0 -6)))
    ;; Get seed color
    (setq seed (gypsum-ui--pick-color "Select seed color (this sets the theme's character):"))
    (unless seed
      (error "Seed color is required"))
    ;; Select variants to generate
    (setq variants (gypsum-ui--select-variants))
    (unless variants
      (error "At least one variant must be selected"))
    ;; Optional: custom background
    (when (yes-or-no-p "Customize background color? (default is auto-generated) ")
      (setq background (gypsum-ui--pick-color "Select background color:")))
    ;; Optional: override individual semantic colors
    (when (yes-or-no-p "Customize individual colors? ")
      (when (yes-or-no-p "Customize string color? ")
        (setq string-override (gypsum-ui--pick-color "Select string color:")))
      (when (yes-or-no-p "Customize constant color? ")
        (setq constant-override (gypsum-ui--pick-color "Select constant color:")))
      (when (yes-or-no-p "Customize comment color? ")
        (setq comment-override (gypsum-ui--pick-color "Select comment color:"))))
    ;; Build base args (without variant/contrast)
    (setq base-args (list :seed seed))
    (when background (setq base-args (plist-put base-args :background background)))
    (when string-override (setq base-args (plist-put base-args :string string-override)))
    (when constant-override (setq base-args (plist-put base-args :constant constant-override)))
    (when comment-override (setq base-args (plist-put base-args :comment comment-override)))
    ;; Optional preview (for first variant)
    (when (yes-or-no-p "Preview the theme before saving? ")
      (let* ((first-variant (car variants))
             (variant-spec (cdr (assoc first-variant gypsum-ui--variant-options)))
             (preview-args (copy-sequence base-args)))
        (setq preview-args (plist-put preview-args :variant (car variant-spec)))
        (setq preview-args (plist-put preview-args :contrast (cdr variant-spec)))
        (gypsum-ui--preview preview-args)
        (unless (yes-or-no-p "Continue with theme generation? ")
          (gypsum-preview-dismiss)
          (error "Theme generation cancelled"))))
    ;; Get output directory
    (setq output-dir (read-directory-name
                      "Output directory: "
                      gypsum-output-directory))
    ;; Generate each selected variant
    (dolist (variant-key variants)
      (let* ((variant-spec (cdr (assoc variant-key gypsum-ui--variant-options)))
             (variant (car variant-spec))
             (contrast (cdr variant-spec))
             (theme-name (if (eq contrast 'low)
                             (format "%s-%s-lc" name variant)
                           (format "%s-%s" name variant)))
             (output-path (expand-file-name
                           (format "%s-theme.el" theme-name)
                           output-dir))
             (args (copy-sequence base-args)))
        (setq args (plist-put args :variant variant))
        (setq args (plist-put args :contrast contrast))
        (setq args (plist-put args :output output-path))
        (apply #'gypsum-generate theme-name args)
        (push theme-name generated-files)))
    ;; Dismiss preview if still active
    (when gypsum-ui--preview-active-theme
      (gypsum-preview-dismiss))
    ;; Report what was generated
    (message "Generated %d theme(s): %s"
             (length generated-files)
             (mapconcat #'identity (nreverse generated-files) ", "))
    ;; Offer to load a theme
    (when (yes-or-no-p "Load one of the generated themes now? ")
      (let ((to-load (completing-read "Load theme: "
                                      (nreverse generated-files)
                                      nil t)))
        (load-theme (intern to-load) t)
        (message "Theme '%s' loaded!" to-load)))))

;;; --- Palette Preview ---

;;;###autoload
(defun gypsum-show-palette (seed variant)
  "Display the palette that would be generated from SEED for VARIANT."
  (interactive
   (list (gypsum-ui--pick-color "Select seed color:")
         (intern (completing-read "Variant: " '("dark" "light") nil t))))
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
