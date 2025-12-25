;;; gypsum-sources.el --- Unified theme source API -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David Martin
;; Keywords: faces, themes

;; This file is part of Gypsum.

;;; Commentary:

;; This module provides a unified API for accessing theme palettes from
;; multiple sources: built-in presets and discovered gypsum themes.
;;
;; Users can select from all available sources without needing to know
;; whether a theme is a built-in preset or a previously generated theme.

;;; Code:

(require 'cl-lib)
(require 'gypsum-presets)
(require 'gypsum-discover)

;;; Source Types

(defun gypsum-sources--list-presets ()
  "Convert presets to source format.
Each source is a plist with :name, :type, :palette, :file."
  (mapcar (lambda (preset)
            (list :name (car preset)
                  :type 'preset
                  :palette (cdr preset)
                  :file nil))
          gypsum-presets))

(defun gypsum-sources--list-discovered ()
  "Convert discovered themes to source format."
  (mapcar (lambda (entry)
            (let ((name (car entry))
                  (info (cdr entry)))
              (list :name name
                    :type 'discovered
                    :palette (plist-get info :palette)
                    :file (plist-get info :file))))
          (gypsum-discover-themes)))

;;; Public API

(defun gypsum-sources-list ()
  "Return list of all available theme sources.
Each source is a plist with:
  :name     - Symbol identifying the source
  :type     - `preset' or `discovered'
  :palette  - Palette plist (may be nil for discovered until loaded)
  :file     - File path (for discovered themes only)

Presets are listed first, followed by discovered themes."
  (append
   (gypsum-sources--list-presets)
   (gypsum-sources--list-discovered)))

(defun gypsum-sources-names ()
  "Return list of all source names as symbols."
  (mapcar (lambda (s) (plist-get s :name)) (gypsum-sources-list)))

(defun gypsum-sources-get (name)
  "Get the source entry for NAME.
NAME is a symbol. Returns the source plist or nil."
  (cl-find name (gypsum-sources-list)
           :key (lambda (s) (plist-get s :name))))

(defun gypsum-sources-get-palette (name)
  "Get palette for source NAME, loading if necessary.
NAME is a symbol. Returns palette plist or nil.

For discovered themes, this loads the theme file to extract the palette
if it hasn't been loaded yet."
  (let ((source (gypsum-sources-get name)))
    (when source
      (pcase (plist-get source :type)
        ('preset
         (plist-get source :palette))
        ('discovered
         (gypsum-discover-get-palette name))))))

(defun gypsum-sources-preset-p (name)
  "Return non-nil if NAME is a built-in preset."
  (let ((source (gypsum-sources-get name)))
    (and source (eq (plist-get source :type) 'preset))))

(defun gypsum-sources-discovered-p (name)
  "Return non-nil if NAME is a discovered theme."
  (let ((source (gypsum-sources-get name)))
    (and source (eq (plist-get source :type) 'discovered))))

(provide 'gypsum-sources)
;;; gypsum-sources.el ends here
