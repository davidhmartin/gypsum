;;; gypsum-faces.el --- Face definitions for Gypsum -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Gypsum Contributors
;; Keywords: faces, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Face specifications for Gypsum theme generator.  Defines how each
;; Emacs face maps to palette colors following Alabaster principles:
;; - Only strings, constants, comments, and definitions get semantic color
;; - Keywords and other syntax elements use default foreground
;; - Comments are prominent, not dimmed

;;; Code:

(require 'cl-lib)

;;; --- Face Specification Format ---

;; Each face spec is: (FACE-NAME . PROPERTIES)
;; Properties use palette keys that get resolved during generation:
;;   :fg KEY          - foreground color
;;   :bg KEY          - background color
;;   :inherit FACE    - inherit from another face
;;   :weight WEIGHT   - bold, normal, etc.
;;   :slant SLANT     - italic, normal, etc.
;;   :underline SPEC  - t, nil, or (:style wave :color KEY)
;;   :box SPEC        - box specification
;;   :height HEIGHT   - relative height

(defvar gypsum-face-specs
  '(
    ;;; === Basic Faces ===
    (default :fg foreground :bg background)
    (cursor :bg foreground)
    (region :bg selection)
    (highlight :bg selection)
    (hl-line :bg bg-alt)
    (fringe :bg background)
    (vertical-border :fg fg-dim)
    (border :fg fg-dim)
    (shadow :fg fg-dim)
    (secondary-selection :bg bg-alt)
    (trailing-whitespace :bg diff-del-bg)

    ;;; === Font-Lock - THE CORE OF ALABASTER ===
    ;; Strings (green-ish)
    (font-lock-string-face :fg string)
    (font-lock-regexp-grouping-construct :fg string)
    (font-lock-regexp-grouping-backslash :fg string)

    ;; Constants (magenta/lavender)
    (font-lock-constant-face :fg constant)
    (font-lock-number-face :fg constant)
    (font-lock-builtin-face :fg constant)

    ;; Comments (prominent, NOT dimmed!)
    (font-lock-comment-face :fg comment)
    (font-lock-comment-delimiter-face :fg comment)
    (font-lock-doc-face :fg comment)

    ;; Definitions (blue-ish)
    (font-lock-function-name-face :fg definition)
    (font-lock-variable-name-face :fg definition)
    (font-lock-type-face :fg definition)

    ;; NOT highlighted - use default foreground (THIS IS INTENTIONAL)
    (font-lock-keyword-face :fg foreground)
    (font-lock-operator-face :fg foreground)
    (font-lock-property-name-face :fg foreground)
    (font-lock-property-use-face :fg foreground)
    (font-lock-function-call-face :fg foreground)
    (font-lock-variable-use-face :fg foreground)
    (font-lock-misc-punctuation-face :fg foreground)
    (font-lock-escape-face :fg constant)
    (font-lock-preprocessor-face :fg foreground)
    (font-lock-negation-char-face :fg foreground)
    (font-lock-warning-face :fg warning)

    ;; Punctuation (dimmed)
    (font-lock-punctuation-face :fg fg-dim)
    (font-lock-delimiter-face :fg fg-dim)
    (font-lock-bracket-face :fg fg-dim)

    ;;; === Mode Line ===
    (mode-line :fg foreground :bg bg-alt :box (:line-width 1 :color fg-dim))
    (mode-line-inactive :fg fg-dim :bg background :box (:line-width 1 :color bg-alt))
    (mode-line-buffer-id :fg foreground)
    (mode-line-emphasis :fg foreground)
    (mode-line-highlight :bg selection)

    ;;; === Minibuffer ===
    (minibuffer-prompt :fg definition)

    ;;; === Search ===
    (isearch :fg foreground :bg find-hl)
    (isearch-fail :fg foreground :bg diff-del-bg)
    (lazy-highlight :bg selection)
    (match :bg find-hl)

    ;;; === Links ===
    (link :fg definition :underline t)
    (link-visited :fg constant :underline t)

    ;;; === Status ===
    (error :fg error)
    (warning :fg warning)
    (success :fg success)

    ;;; === Line Numbers ===
    (line-number :fg fg-dim :bg background)
    (line-number-current-line :fg foreground :bg bg-alt)

    ;;; === Parens ===
    (show-paren-match :fg foreground :bg find-hl)
    (show-paren-mismatch :fg foreground :bg error)

    ;;; === Diff ===
    (diff-added :bg diff-add-bg)
    (diff-removed :bg diff-del-bg)
    (diff-changed :bg diff-chg-bg)
    (diff-header :fg foreground :bg bg-alt)
    (diff-file-header :fg definition :bg bg-alt)
    (diff-hunk-header :fg fg-dim :bg bg-alt)
    (diff-indicator-added :fg success :bg diff-add-bg)
    (diff-indicator-removed :fg error :bg diff-del-bg)
    (diff-indicator-changed :fg warning :bg diff-chg-bg)
    (diff-refine-added :fg foreground :bg diff-add-bg)
    (diff-refine-removed :fg foreground :bg diff-del-bg)
    (diff-refine-changed :fg foreground :bg diff-chg-bg)

    ;;; === Completions ===
    (completions-common-part :fg definition)
    (completions-first-difference :fg foreground)
    (completions-annotations :fg fg-dim)

    ;;; === Buttons ===
    (button :fg definition :underline t)
    (custom-button :fg foreground :bg bg-alt :box (:line-width 1 :color fg-dim))

    ;;; === Org Mode ===
    (org-level-1 :fg foreground)
    (org-level-2 :fg foreground)
    (org-level-3 :fg foreground)
    (org-level-4 :fg foreground)
    (org-level-5 :fg foreground)
    (org-level-6 :fg foreground)
    (org-level-7 :fg foreground)
    (org-level-8 :fg foreground)
    (org-document-title :fg definition)
    (org-document-info :fg fg-dim)
    (org-document-info-keyword :fg fg-dim)
    (org-meta-line :fg fg-dim)
    (org-block :bg bg-alt)
    (org-block-begin-line :fg fg-dim :bg bg-alt)
    (org-block-end-line :fg fg-dim :bg bg-alt)
    (org-code :fg string)
    (org-verbatim :fg string)
    (org-link :fg definition :underline t)
    (org-date :fg constant)
    (org-todo :fg error)
    (org-done :fg success)
    (org-headline-done :fg fg-dim)
    (org-special-keyword :fg fg-dim)
    (org-table :fg foreground)
    (org-formula :fg constant)
    (org-tag :fg fg-dim)

    ;;; === Markdown Mode ===
    (markdown-header-face :fg foreground)
    (markdown-header-face-1 :fg foreground)
    (markdown-header-face-2 :fg foreground)
    (markdown-header-face-3 :fg foreground)
    (markdown-header-face-4 :fg foreground)
    (markdown-header-face-5 :fg foreground)
    (markdown-header-face-6 :fg foreground)
    (markdown-code-face :fg string)
    (markdown-inline-code-face :fg string)
    (markdown-pre-face :fg string)
    (markdown-link-face :fg definition)
    (markdown-url-face :fg definition :underline t)

    ;;; === Flycheck / Flymake ===
    (flycheck-error :underline (:style wave :color error))
    (flycheck-warning :underline (:style wave :color warning))
    (flycheck-info :underline (:style wave :color definition))
    (flymake-error :underline (:style wave :color error))
    (flymake-warning :underline (:style wave :color warning))
    (flymake-note :underline (:style wave :color definition))

    ;;; === Company Mode ===
    (company-tooltip :fg foreground :bg bg-alt)
    (company-tooltip-selection :fg foreground :bg selection)
    (company-tooltip-common :fg definition)
    (company-tooltip-common-selection :fg definition)
    (company-tooltip-annotation :fg fg-dim)
    (company-scrollbar-bg :bg bg-alt)
    (company-scrollbar-fg :bg fg-dim)
    (company-preview :fg fg-dim :bg background)
    (company-preview-common :fg definition)

    ;;; === Corfu ===
    (corfu-default :fg foreground :bg bg-alt)
    (corfu-current :fg foreground :bg selection)
    (corfu-bar :bg fg-dim)
    (corfu-border :bg fg-dim)
    (corfu-annotations :fg fg-dim)

    ;;; === Vertico ===
    (vertico-current :bg selection)
    (vertico-group-title :fg fg-dim)
    (vertico-group-separator :fg fg-dim)

    ;;; === Marginalia ===
    (marginalia-documentation :fg fg-dim)
    (marginalia-file-name :fg foreground)
    (marginalia-key :fg definition)

    ;;; === Orderless ===
    (orderless-match-face-0 :fg definition)
    (orderless-match-face-1 :fg string)
    (orderless-match-face-2 :fg constant)
    (orderless-match-face-3 :fg comment)

    ;;; === Consult ===
    (consult-file :fg foreground)
    (consult-bookmark :fg definition)
    (consult-buffer :fg foreground)
    (consult-line-number :fg fg-dim)
    (consult-preview-match :bg selection)

    ;;; === Magit ===
    (magit-section-heading :fg definition)
    (magit-section-highlight :bg bg-alt)
    (magit-branch-local :fg definition)
    (magit-branch-remote :fg string)
    (magit-branch-current :fg definition :box t)
    (magit-diff-added :fg success :bg diff-add-bg)
    (magit-diff-added-highlight :fg success :bg diff-add-bg)
    (magit-diff-removed :fg error :bg diff-del-bg)
    (magit-diff-removed-highlight :fg error :bg diff-del-bg)
    (magit-diff-context :fg fg-dim)
    (magit-diff-context-highlight :fg fg-dim :bg bg-alt)
    (magit-diff-hunk-heading :fg foreground :bg bg-alt)
    (magit-diff-hunk-heading-highlight :fg foreground :bg selection)
    (magit-diff-file-heading :fg foreground)
    (magit-diff-file-heading-highlight :fg foreground :bg bg-alt)
    (magit-hash :fg fg-dim)
    (magit-log-author :fg definition)
    (magit-log-date :fg fg-dim)

    ;;; === Git Gutter ===
    (git-gutter:added :fg success :bg success)
    (git-gutter:deleted :fg error :bg error)
    (git-gutter:modified :fg warning :bg warning)
    (git-gutter-fr:added :fg success :bg success)
    (git-gutter-fr:deleted :fg error :bg error)
    (git-gutter-fr:modified :fg warning :bg warning)

    ;;; === Diff-hl ===
    (diff-hl-insert :fg success :bg success)
    (diff-hl-delete :fg error :bg error)
    (diff-hl-change :fg warning :bg warning)

    ;;; === Whitespace ===
    (whitespace-space :fg bg-alt)
    (whitespace-tab :fg bg-alt)
    (whitespace-newline :fg bg-alt)
    (whitespace-trailing :bg diff-del-bg)
    (whitespace-line :bg diff-chg-bg)

    ;;; === Rainbow Delimiters ===
    (rainbow-delimiters-depth-1-face :fg foreground)
    (rainbow-delimiters-depth-2-face :fg fg-dim)
    (rainbow-delimiters-depth-3-face :fg definition)
    (rainbow-delimiters-depth-4-face :fg string)
    (rainbow-delimiters-depth-5-face :fg constant)
    (rainbow-delimiters-depth-6-face :fg foreground)
    (rainbow-delimiters-depth-7-face :fg fg-dim)
    (rainbow-delimiters-depth-8-face :fg definition)
    (rainbow-delimiters-depth-9-face :fg string)
    (rainbow-delimiters-unmatched-face :fg error)

    ;;; === Which-Key ===
    (which-key-key-face :fg definition)
    (which-key-separator-face :fg fg-dim)
    (which-key-command-description-face :fg foreground)
    (which-key-group-description-face :fg constant)

    ;;; === Ivy ===
    (ivy-current-match :bg selection)
    (ivy-minibuffer-match-face-1 :fg definition)
    (ivy-minibuffer-match-face-2 :fg string)
    (ivy-minibuffer-match-face-3 :fg constant)
    (ivy-minibuffer-match-face-4 :fg comment)

    ;;; === Helm ===
    (helm-selection :bg selection)
    (helm-match :fg definition)
    (helm-source-header :fg foreground :bg bg-alt)
    (helm-candidate-number :fg constant)

    ;;; === Treemacs ===
    (treemacs-directory-face :fg foreground)
    (treemacs-file-face :fg foreground)
    (treemacs-git-added-face :fg success)
    (treemacs-git-modified-face :fg warning)
    (treemacs-git-untracked-face :fg fg-dim)

    ;;; === Dired ===
    (dired-directory :fg definition)
    (dired-symlink :fg constant)
    (dired-marked :fg string :bg selection)
    (dired-flagged :fg error)
    (dired-header :fg definition)
    (dired-perm-write :fg foreground)

    ;;; === Eshell ===
    (eshell-prompt :fg definition)
    (eshell-ls-directory :fg definition)
    (eshell-ls-symlink :fg constant)
    (eshell-ls-executable :fg string)
    (eshell-ls-archive :fg constant)

    ;;; === Term / Ansi - these need special handling ===
    ;; Term colors will use actual color values, handled separately

    ;;; === Avy ===
    (avy-lead-face :fg foreground :bg find-hl)
    (avy-lead-face-0 :fg foreground :bg selection)
    (avy-lead-face-1 :fg foreground :bg bg-alt)
    (avy-lead-face-2 :fg foreground :bg diff-add-bg)

    ;;; === Ace Window ===
    (aw-leading-char-face :fg error :height 1.5)
    (aw-background-face :fg fg-dim)

    ;;; === Tab Bar / Tab Line ===
    (tab-bar :fg foreground :bg bg-alt)
    (tab-bar-tab :fg foreground :bg background :box (:line-width 1 :color fg-dim))
    (tab-bar-tab-inactive :fg fg-dim :bg bg-alt)
    (tab-line :fg foreground :bg bg-alt)
    (tab-line-tab :fg foreground :bg background)
    (tab-line-tab-current :fg foreground :bg background :box (:line-width 1 :color fg-dim))
    (tab-line-tab-inactive :fg fg-dim :bg bg-alt)

    ;;; === Header Line ===
    (header-line :fg foreground :bg bg-alt)

    ;;; === Info ===
    (info-title-1 :fg foreground)
    (info-title-2 :fg foreground)
    (info-title-3 :fg foreground)
    (info-title-4 :fg foreground)
    (info-menu-header :fg foreground)
    (info-node :fg definition)
    (info-xref :fg definition)
    (info-xref-visited :fg constant)

    ;;; === Elfeed ===
    (elfeed-search-feed-face :fg definition)
    (elfeed-search-tag-face :fg string)
    (elfeed-search-title-face :fg foreground)
    (elfeed-search-unread-title-face :fg foreground)
    (elfeed-search-date-face :fg fg-dim)

    ;;; === Eglot / LSP ===
    (eglot-highlight-symbol-face :bg selection)

    ;;; === Tree-sitter ===
    (tree-sitter-hl-face:keyword :fg foreground)
    (tree-sitter-hl-face:operator :fg foreground)
    (tree-sitter-hl-face:punctuation :fg fg-dim)
    (tree-sitter-hl-face:punctuation.bracket :fg fg-dim)
    (tree-sitter-hl-face:punctuation.delimiter :fg fg-dim)
    (tree-sitter-hl-face:string :fg string)
    (tree-sitter-hl-face:string.special :fg string)
    (tree-sitter-hl-face:escape :fg constant)
    (tree-sitter-hl-face:number :fg constant)
    (tree-sitter-hl-face:constant :fg constant)
    (tree-sitter-hl-face:constant.builtin :fg constant)
    (tree-sitter-hl-face:comment :fg comment)
    (tree-sitter-hl-face:doc :fg comment)
    (tree-sitter-hl-face:function :fg definition)
    (tree-sitter-hl-face:function.call :fg foreground)
    (tree-sitter-hl-face:method :fg definition)
    (tree-sitter-hl-face:method.call :fg foreground)
    (tree-sitter-hl-face:variable :fg foreground)
    (tree-sitter-hl-face:variable.builtin :fg constant)
    (tree-sitter-hl-face:variable.parameter :fg foreground)
    (tree-sitter-hl-face:property :fg foreground)
    (tree-sitter-hl-face:type :fg definition)
    (tree-sitter-hl-face:type.builtin :fg definition)
    (tree-sitter-hl-face:constructor :fg definition)
    (tree-sitter-hl-face:label :fg foreground)

    ;;; === Web-mode ===
    (web-mode-html-tag-face :fg foreground)
    (web-mode-html-tag-bracket-face :fg fg-dim)
    (web-mode-html-attr-name-face :fg foreground)
    (web-mode-html-attr-value-face :fg string)
    (web-mode-doctype-face :fg fg-dim)
    (web-mode-comment-face :fg comment)
    (web-mode-css-selector-face :fg definition)
    (web-mode-css-property-name-face :fg foreground)
    (web-mode-css-string-face :fg string)

    ;;; === JS2-mode ===
    (js2-function-param :fg foreground)
    (js2-external-variable :fg foreground)
    (js2-jsdoc-tag :fg fg-dim)
    (js2-jsdoc-type :fg definition)
    (js2-jsdoc-value :fg foreground)
    (js2-error :underline (:style wave :color error))
    (js2-warning :underline (:style wave :color warning))

    ;;; === TypeScript-mode ===
    (typescript-jsdoc-tag :fg fg-dim)
    (typescript-jsdoc-type :fg definition)
    (typescript-jsdoc-value :fg foreground)

    ;;; === Clojure ===
    (clojure-keyword-face :fg constant)
    (clojure-character-face :fg string)

    ;;; === Rust ===
    (rust-question-mark :fg foreground)

    ;;; === Go ===
    (go-dot-mod-module-name :fg definition)
    (go-dot-mod-module-version :fg constant)

    ;;; === Python ===
    (py-variable-name-face :fg foreground)
    (py-def-face :fg definition)
    (py-class-name-face :fg definition)

    ;;; === Sh-mode ===
    (sh-heredoc :fg string)
    (sh-quoted-exec :fg constant)

    ;;; === CSS ===
    (css-selector :fg definition)
    (css-property :fg foreground)

    ;;; === YAML ===
    (yaml-tab-face :bg diff-del-bg)

    ;;; === Bookmarks ===
    (bookmark-face :fg definition)

    ;;; === Ediff ===
    (ediff-current-diff-A :bg diff-del-bg)
    (ediff-current-diff-B :bg diff-add-bg)
    (ediff-current-diff-C :bg diff-chg-bg)
    (ediff-fine-diff-A :bg diff-del-bg)
    (ediff-fine-diff-B :bg diff-add-bg)
    (ediff-fine-diff-C :bg diff-chg-bg)
    (ediff-odd-diff-A :bg bg-alt)
    (ediff-odd-diff-B :bg bg-alt)
    (ediff-odd-diff-C :bg bg-alt)
    (ediff-even-diff-A :bg bg-alt)
    (ediff-even-diff-B :bg bg-alt)
    (ediff-even-diff-C :bg bg-alt)

    ;;; === Smerge ===
    (smerge-upper :bg diff-del-bg)
    (smerge-lower :bg diff-add-bg)
    (smerge-base :bg diff-chg-bg)
    (smerge-markers :bg bg-alt)

    ;;; === Outline ===
    (outline-1 :fg foreground)
    (outline-2 :fg foreground)
    (outline-3 :fg foreground)
    (outline-4 :fg foreground)
    (outline-5 :fg foreground)
    (outline-6 :fg foreground)
    (outline-7 :fg foreground)
    (outline-8 :fg foreground)

    ;;; === ERC ===
    (erc-default-face :fg foreground)
    (erc-nick-default-face :fg definition)
    (erc-my-nick-face :fg string)
    (erc-current-nick-face :fg constant)
    (erc-notice-face :fg fg-dim)
    (erc-input-face :fg foreground)
    (erc-timestamp-face :fg fg-dim)
    (erc-prompt-face :fg definition)

    ;;; === Message / Mail ===
    (message-header-name :fg fg-dim)
    (message-header-subject :fg foreground)
    (message-header-to :fg definition)
    (message-header-other :fg foreground)
    (message-cited-text-1 :fg comment)
    (message-cited-text-2 :fg fg-dim)

    ;;; === Notmuch ===
    (notmuch-search-date :fg fg-dim)
    (notmuch-search-count :fg fg-dim)
    (notmuch-search-subject :fg foreground)
    (notmuch-search-matching-authors :fg definition)
    (notmuch-tag-face :fg string)
    )
  "Face specifications for Gypsum themes.
Each entry is (FACE-NAME PROPERTY VALUE ...).
Properties use palette key symbols that get resolved during generation.")

;;; --- Face Generation ---

(defun gypsum-faces--resolve-color (key palette)
  "Resolve color KEY from PALETTE.
If KEY is a symbol, look it up in palette.
If KEY is a string, return it as-is (literal color)."
  (cond
   ((stringp key) key)
   ((symbolp key)
    (or (plist-get palette (intern (concat ":" (symbol-name key))))
        (error "Unknown palette key: %s" key)))
   (t (error "Invalid color key type: %s" (type-of key)))))

(defun gypsum-faces--process-underline (spec palette)
  "Process underline SPEC, resolving colors from PALETTE."
  (cond
   ((eq spec t) t)
   ((eq spec nil) nil)
   ((and (listp spec) (plist-get spec :color))
    (list :style (or (plist-get spec :style) 'line)
          :color (gypsum-faces--resolve-color (plist-get spec :color) palette)))
   (t spec)))

(defun gypsum-faces--process-box (spec palette)
  "Process box SPEC, resolving colors from PALETTE."
  (cond
   ((eq spec t) t)
   ((eq spec nil) nil)
   ((and (listp spec) (plist-get spec :color))
    (let ((result (copy-sequence spec)))
      (plist-put result :color
                 (gypsum-faces--resolve-color (plist-get spec :color) palette))
      result))
   (t spec)))

(defun gypsum-faces--build-face-spec (face-def palette)
  "Build a face specification from FACE-DEF using PALETTE.
Returns a form suitable for `custom-theme-set-faces'."
  (let* ((face-name (car face-def))
         (props (cdr face-def))
         (class '((class color) (min-colors 89)))
         attributes)
    ;; Process properties - push key first, then value (nreverse will fix order)
    (cl-loop for (key value) on props by #'cddr do
             (pcase key
               (:fg
                (push :foreground attributes)
                (push (gypsum-faces--resolve-color value palette) attributes))
               (:bg
                (push :background attributes)
                (push (gypsum-faces--resolve-color value palette) attributes))
               (:underline
                (push :underline attributes)
                (push (gypsum-faces--process-underline value palette) attributes))
               (:box
                (push :box attributes)
                (push (gypsum-faces--process-box value palette) attributes))
               (_
                (push key attributes)
                (push value attributes))))
    ;; Build final spec
    `(,face-name ((,class ,(nreverse attributes))))))

(defun gypsum-faces-generate (palette)
  "Generate all face specifications for PALETTE.
Returns a list suitable for `custom-theme-set-faces'."
  (mapcar (lambda (face-def)
            (gypsum-faces--build-face-spec face-def palette))
          gypsum-face-specs))

(provide 'gypsum-faces)

;;; gypsum-faces.el ends here
