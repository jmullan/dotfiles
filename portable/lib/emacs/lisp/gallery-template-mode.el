;;; gallery-template-mode-el -- Major mode for editing Gallery Template files

;; Copyright (C) 2003 Bharat Mediratta <bharat@menalto.com>
;; Portions Copyright (C) 2000 Scott Andrew Borton <scott@pp.htv.fi>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This mode is based on an excellent tutorial written:
;; 
;;     Author: Scott Andrew Borton <scott@pp.htv.fi>
;;     Created: 25 Sep 2000
;;     Keywords: WPDL major-mode
;;
;; The tutorial itself can be found here:
;; 
;;     http://two-wugs.net/emacs/mode-tutorial.html
;;
;; To use this mode, put this:
;;
;;     (load "gallery-template-mode" t t)
;;
;; In your .xemacs/init.el

;;; Code:
(defvar gallery-template-mode-hook nil)
(defvar gallery-template-mode-map nil
  "Keymap for Gallery-Template major mode.")

(if gallery-template-mode-map nil
  (setq gallery-template-mode-map (make-keymap)))

(setq auto-mode-alist (append '(("\\.tpl\\'" . gallery-template-mode)) auto-mode-alist))

(defconst smarty-keywords
  (eval-when-compile 
    (regexp-opt
     ;; Built-in Functions
     '(
       "capture" "config_load" "foreach" "foreachelse" "include" "include_php"
       "if" "elseif" "else" "ldelim" "rdelim" "literal" "php" "section" "sectionelse"
       "strip"
       
       ;; Custom functions
       "assign" "counter" "cycle" "debug" "eval" "fetch"
       "html_checkboxes" "html_image" "html_options" "html_radios" "html_select_date" "html_select_time"
       "html_table" "math" "mailto" "popup_init" "popup" "textformat") t))
  "Smarty keywords."
  )

(defconst gallery-keywords
  (eval-when-compile
    (regexp-opt
     '(
       ;; Builtin tags
       "g->text"
       "g->url"
       "g->date"
       "g->elementName"

       ;; Theme tags 
       "g->style"
       "g->input"
       "g->form"
       "g->select"
       "g->textarea"
       "g->link"
       "g->success"
       "g->warning"
       "g->error"
       "g->image"
       "g->listing"
       "g->breadcrumb"
       "g->linkset"
       "g->actionset"
       "g->infoset"
       "g->tabset"
       "g->table"
       "g->row"
       "g->column"
       "g->element"
       "g->title"
       "g->subtitle"
       "g->description"
       "g->footer"
       "g->media"
       "g->value"
       "g->main"
       "g->sidebar"
       "g->itemthumbnail"
       "g->itemview"
       "g->tabbedbox"
       "g->box"
       "g->pagebox"
       "g->banner"
       "g->item"
       "g->actionitem") t))
  "Gallery keywords"
)

(defconst gallery-template-font-lock-keywords-1
  (list
   (cons smarty-keywords 'smarty-keyword-face)
   (cons gallery-keywords 'gallery-keyword-face)
   )
  "Minimal keywords highlighting for Gallery Template mode"
  )

(defvar gallery-template-font-lock-keywords gallery-template-font-lock-keywords-1
  "Default highlighting expressions for Gallery-Template mode.")

(defun gallery-template-indent-line ()
  "Indent current line as gallery-template code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)		; First line is always non-indented
    (let ((not-indented t) cur-indent)
      (save-excursion
	(forward-to-indentation 0)
	(if (looking-at (regexp-opt '(                 ; if the line is a ...
				      "{/"             ; end of a block
				      "{else}"	       ; special statement
				      "{foreachelse}"
				      )		       ; then decrease the indentation
				    t))
	    (progn
	      (save-excursion
		(forward-line -1)
		(setq cur-indent (- (current-indentation) indent-width)))
	      (if (< cur-indent 0)	; We can't indent past the left margin
		  (setq cur-indent 0)))
	  (save-excursion
	    (while not-indented		; Iterate backwards until we find an indentation hint
	      (forward-to-indentation -1)

;  Example of printing debug statements
;	      (print "some string" (get-buffer "*scratch*"))
;	      (message "some string")

	      (if (looking-at (regexp-opt '("{/"
					    "{*"
					    "{counter "
					    "{assign "
					    "{include "
					    "{g->image}"
					    "{g->style}"
					    "{g->text "
					    "{$"
					    "{\"")
					  t))
					; Match closing statements or non-block methods
		  (progn
		    (setq cur-indent (current-indentation)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (and (looking-at "\{[^\/]") ; We need to indent another level on a {...}
			 (not (looking-at ".*\{\/"))) ; Unless the line has a matching {/...}
		    (progn
		      (setq cur-indent (+ (current-indentation) indent-width)) ; Do the actual indenting
		      (setq not-indented nil))
		  (if (bobp)
		      (setq not-indented nil))))))))
	(if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))		; If we didn't see an indentation hint, then allow no indentation

(defvar gallery-template-mode-syntax-table nil
  "Syntax table for gallery-template-mode.")

(defun gallery-template-create-syntax-table ()
  (if gallery-template-mode-syntax-table
      ()
    (setq gallery-template-mode-syntax-table (make-syntax-table))
    
					; Comments are of the form {* .. *}
    (modify-syntax-entry ?{ ". 1" gallery-template-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" gallery-template-mode-syntax-table)
    (modify-syntax-entry ?} "> 4" gallery-template-mode-syntax-table))
  (set-syntax-table gallery-template-mode-syntax-table))

(defun gallery-template-mode ()
  "Major mode for editing Gallery Template (Smarty based) files."
  (interactive)
  (kill-all-local-variables)
  (gallery-template-create-syntax-table)
  
  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gallery-template-font-lock-keywords))
  (setq font-lock-maximum-decoration t)
  
  ;; Register our indentation function
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'gallery-template-indent-line)

  (make-local-variable 'indent-width)
  (setq indent-width 2)
  
  (setq major-mode 'gallery-template-mode)
  (setq mode-name "gallery-template")
  (run-hooks 'gallery-template-mode-hook))

;; Create "default" symbol for GNU Emacs so that both Xemacs and GNU
;; emacs can refer to the default face by a variable named "default".
(unless (boundp 'default) (defvar default 'default))

;; Create faces for XEmacs
(unless (boundp 'gallery-keyword-face) (copy-face 'default 'gallery-keyword-face))
(unless (boundp 'smarty-keyword-face) (copy-face 'default 'smarty-keyword-face))

(provide 'gallery-template-mode)

;;; gallery-template-mode.el ends here

