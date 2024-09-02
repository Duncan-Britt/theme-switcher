;;; theme-switcher.el --- Emacs Theme Switcher                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Duncan Britt

;; Author: Duncan Britt <dbru997@gmail.com>
;; Homepage: TODO
;; Keywords: Graphics,images,themes

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29"))

;; The software is provided “as is”, without warranty of any kind, express or implied,
;; including but not limited to the warranties of merchantability, fitness for a particular
;; purpose and noninfringement. in no event shall the authors or copyright holders be liable
;; for any claim, damages or other liability, whether in an action of contract, tort or
;; otherwise, arising from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:

;;;

;;; Code:
(require 'cl-lib)
(require 'org)

;; Utility
(defun lookup (key assoc-list)
  "Utility function to loook up value by key in assoc list."
  (cdr (assoc key assoc-list)))

(defun insert-dark-before-extension (filepath)
  "Insert '-dark' before the file extension in the given FILEPATH."
  (let* ((file (file-name-nondirectory filepath)) ; Extract the file name
         (dir  (file-name-directory filepath))    ; Extract the directory path
         (name (file-name-base file))             ; Extract the base name without extension
         (ext  (file-name-extension file)))       ; Extract the extension
    (concat dir name "-dark." ext)))              ; Concatenate to form the new path

;; Customizations
(defcustom *themes-light* '("ef-day" "ef-light" "ef-kassio" "ef-frost" "ef-arbutus" "ef-melissa-light" "ef-maris-light" "ef-elea-light" "ef-summer" "ef-cyprus" "ef-reverie")
  "List of preferred light mode themes.")

(defcustom *themes-dark* '("ef-trio-dark" "ef-rosa" "ef-winter" "ef-cherie" "ef-tritanopia-dark" "ef-elea-dark" "ef-dream" "ef-melissa-dark" "ef-owl")
  "List of preferred dark mode themes.")

(defvar *themes-category* 'Dark
  "Category of current theme: Either 'Light or 'Dark")

;; Choose theme among themes I like
(defun choose-theme ()
  "Open a selection menu in the minibuffer."
  (interactive)
  (let* ((all-themes `(("Light" . ,*themes-light*)
                       ("Dark" . ,*themes-dark*)))
         (options '("Light" "Dark"))
         (brightness-selection (completing-read "Choose category: " options))         
         (themes (lookup brightness-selection all-themes))
         (theme-chosen (completing-read "Choose a theme:" themes)))
    (setq *themes-category* (intern brightness-selection))    
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme-chosen) t)
    (ts-refresh-inline-images)
    (message "Loaded %s" theme-chosen)))

(global-set-key (kbd "C-t") 'choose-theme)

(defun ts-init ()
  (define-key org-mode-map (kbd "C-c C-x C-v") nil)  
  (define-key org-mode-map (kbd "C-c C-x C-v") 'ts-toggle-inline-images))

(defun ts-refresh-inline-images ()
  "If displaying inline images, stop and restart display."
  (when (org--inline-image-overlays)
    (org-remove-inline-images)
    (message "hello")
    (ts-display-inline-images)))

(defun ts-toggle-inline-images (&optional include-linked beg end)
  "Toggle the display of inline images,
but use dark versions for dark mode."
  (interactive "P")  
  (if (org--inline-image-overlays beg end)
      (progn
        (org-remove-inline-images beg end)
        (when (called-interactively-p 'interactive)
	  (message "Inline image display turned off")))
    (ts-display-inline-images include-linked nil beg end)
    (when (called-interactively-p 'interactive)
      (let ((new (org--inline-image-overlays beg end)))
        (message (if new
		     (format "%d images displayed inline"
			     (length new))
		   "No images to display inline"))))))

(defun ts-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
	(let* ((case-fold-search t)
	       (file-extension-re (image-file-name-regexp))
	       (link-abbrevs (mapcar #'car
				     (append org-link-abbrev-alist-local
					     org-link-abbrev-alist)))
	       ;; Check absolute, relative file names and explicit
	       ;; "file:" links.  Also check link abbreviations since
	       ;; some might expand to "file" links.
	       (file-types-re
		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
			(if (not link-abbrevs) ""
			  (concat "\\|" (regexp-opt link-abbrevs))))))
	  (while (re-search-forward file-types-re end t)
	    (let* ((link (org-element-lineage
			  (save-match-data (org-element-context))
			  '(link) t))
                   (linktype (org-element-property :type link))
		   (inner-start (match-beginning 1))
		   (path
		    (cond
		     ;; No link at point; no inline image.
		     ((not link) nil)
		     ;; File link without a description.  Also handle
		     ;; INCLUDE-LINKED here since it should have
		     ;; precedence over the next case.  I.e., if link
		     ;; contains filenames in both the path and the
		     ;; description, prioritize the path only when
		     ;; INCLUDE-LINKED is non-nil.
		     ((or (not (org-element-property :contents-begin link))
			  include-linked)
		      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
			   (org-element-property :path link)))
		     ;; Link with a description.  Check if description
		     ;; is a filename.  Even if Org doesn't have syntax
		     ;; for those -- clickable image -- constructs, fake
		     ;; them, as in `org-export-insert-image-links'.
		     ((not inner-start) nil)
		     (t
		      (org-with-point-at inner-start
			(and (looking-at
			      (if (char-equal ?< (char-after inner-start))
				  org-link-angle-re
				org-link-plain-re))
			     ;; File name must fill the whole
			     ;; description.
			     (= (org-element-property :contents-end link)
				(match-end 0))
			     (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
              (when (and path (eq *themes-category* 'Dark)) ;; <-- Modifications to original: check if in dark mode and update path
                (let ((new-path (insert-dark-before-extension path)))
                  (when (file-exists-p new-path)
                    (setq path new-path)))) ;; <-- end modifications
	      (when (and path (string-match-p file-extension-re path))
		(let ((file (if (equal "attachment" linktype)
				(progn
                                  (require 'org-attach)
				  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
		  (when (and file (file-exists-p file))
		    (let ((width (org-display-inline-image--width link))
			  (old (get-char-property-and-overlay
				(org-element-property :begin link)
				'org-image-overlay)))
		      (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
			(let ((image (org--create-inline-image file width)))
			  (when image
			    (let ((ov (make-overlay
				       (org-element-property :begin link)
				       (progn
					 (goto-char
					  (org-element-property :end link))
					 (skip-chars-backward " \t")
					 (point)))))
                              ;; FIXME: See bug#59902.  We cannot rely
                              ;; on Emacs to update image if the file
                              ;; has changed.
                              (image-flush image)
			      (overlay-put ov 'display image)
			      (overlay-put ov 'face 'default)
			      (overlay-put ov 'org-image-overlay t)
			      (overlay-put
			       ov 'modification-hooks
			       (list 'org-display-inline-remove-overlay))
			      (when (boundp 'image-map)
				(overlay-put ov 'keymap image-map))
			      (push ov org-inline-image-overlays))))))))))))))))

(provide 'theme-switcher)
;;; theme-switcher.el ends here
