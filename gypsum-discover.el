;;; gypsum-discover.el --- Discover gypsum-generated themes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David Martin
;; Keywords: faces, themes

;; This file is part of Gypsum.

;;; Commentary:

;; This module provides functionality to discover gypsum-generated themes
;; by scanning `custom-theme-load-path' for themes containing the
;; gypsum-generated marker.
;;
;; Discovered themes can be used as starting points for generating new
;; themes, just like built-in presets.

;;; Code:

(require 'cl-lib)

(defvar gypsum-output-directory)  ; Forward declaration

;;; Configuration

(defvar gypsum-discover--cache nil
  "Cache of discovered gypsum themes.
Alist of (THEME-NAME . (:file FILE :palette PALETTE-OR-NIL)).")

(defvar gypsum-discover--cache-time nil
  "Time when cache was last updated.")

(defcustom gypsum-discover-cache-ttl 60
  "Time-to-live for discovery cache in seconds.
Set to 0 to disable caching."
  :type 'integer
  :group 'gypsum)

;;; Internal Functions

(defun gypsum-discover--cache-valid-p ()
  "Return non-nil if the discovery cache is still valid."
  (and gypsum-discover--cache
       gypsum-discover--cache-time
       (> gypsum-discover-cache-ttl 0)
       (< (float-time (time-subtract nil gypsum-discover--cache-time))
          gypsum-discover-cache-ttl)))

(defun gypsum-discover--file-to-theme-name (file)
  "Extract theme name from FILE path.
E.g., '/path/to/my-theme.el' -> \"my\"."
  (let ((basename (file-name-base file)))
    (if (string-suffix-p "-theme" basename)
        (substring basename 0 -6)
      basename)))

(defun gypsum-discover--has-marker-p (file)
  "Check if FILE contains the gypsum-generated marker.
Uses fast text search without fully loading the file."
  (condition-case nil
      (with-temp-buffer
        ;; Read first 4KB - marker should be near top of file
        (insert-file-contents file nil 0 4000)
        (goto-char (point-min))
        (re-search-forward "-gypsum-generated\\s-+t" nil t))
    (error nil)))

(defun gypsum-discover--scan-directory (dir)
  "Scan DIR for gypsum theme files.
Returns list of (THEME-NAME . FILE) for files with gypsum marker."
  (when (and dir (stringp dir) (file-directory-p dir))
    (let ((theme-files (directory-files dir t "-theme\\.el\\'")))
      (cl-loop for file in theme-files
               when (gypsum-discover--has-marker-p file)
               collect (cons (gypsum-discover--file-to-theme-name file) file)))))

(defun gypsum-discover--extract-palette (file)
  "Extract the gypsum palette from theme FILE.
Loads the file to access the defconst symbol value."
  (let* ((theme-name (gypsum-discover--file-to-theme-name file))
         (palette-sym (intern (format "%s-gypsum-palette" theme-name))))
    ;; Load the file if the symbol isn't already bound
    (unless (boundp palette-sym)
      (condition-case nil
          (load file t t)
        (error nil)))
    (when (boundp palette-sym)
      (symbol-value palette-sym))))

;;; Public API

(defun gypsum-discover-themes (&optional force-refresh)
  "Scan custom-theme-load-path for gypsum-generated themes.
Returns an alist of (THEME-NAME . (:file FILE :palette PALETTE)).

THEME-NAME is a symbol.
The :palette value is initially nil and loaded lazily.

With FORCE-REFRESH non-nil, bypass the cache."
  (if (and (not force-refresh) (gypsum-discover--cache-valid-p))
      gypsum-discover--cache
    ;; Build fresh cache
    (let ((discovered '())
          (dirs (cons (when (boundp 'gypsum-output-directory)
                        gypsum-output-directory)
                      custom-theme-load-path)))
      (dolist (dir dirs)
        (dolist (entry (gypsum-discover--scan-directory dir))
          (let ((name (intern (car entry)))
                (file (cdr entry)))
            ;; Avoid duplicates (first found wins)
            (unless (assq name discovered)
              (push (cons name (list :file file :palette nil)) discovered)))))
      (setq gypsum-discover--cache (nreverse discovered)
            gypsum-discover--cache-time (current-time))
      gypsum-discover--cache)))

(defun gypsum-discover-get-palette (theme-name)
  "Get the palette for discovered theme THEME-NAME.
THEME-NAME is a symbol. Returns palette plist or nil."
  (let ((entry (assq theme-name (gypsum-discover-themes))))
    (when entry
      (let ((info (cdr entry)))
        ;; Lazy-load palette if not yet extracted
        (unless (plist-get info :palette)
          (let ((palette (gypsum-discover--extract-palette
                          (plist-get info :file))))
            (when palette
              (plist-put info :palette palette))))
        (plist-get info :palette)))))

;;;###autoload
(defun gypsum-discover-refresh ()
  "Force refresh of discovered themes cache."
  (interactive)
  (setq gypsum-discover--cache nil
        gypsum-discover--cache-time nil)
  (let ((themes (gypsum-discover-themes t)))
    (message "Discovered %d gypsum theme(s)" (length themes))
    themes))

(defun gypsum-discover-list ()
  "Return list of discovered theme names as symbols."
  (mapcar #'car (gypsum-discover-themes)))

(provide 'gypsum-discover)
;;; gypsum-discover.el ends here
