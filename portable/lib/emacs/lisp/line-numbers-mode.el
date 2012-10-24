;;; line-numbers-mode.el --- a minor mode for on-screen/window line numbering.

;; Copyright (C) 1999 Jerry G. Chen

;; Author:     Jerry Chen <gchen275@yahoo.com>
;; Maintainer: gchen275@yahoo.com
;; Created:    Jan-29-1999
;; Version:    0.20
;; Keywords:   minor-mode, display, line numbering

;; This file may be part of XEmacs.

;; The same license/disclaimer for XEmacs also applies to this package.

;;; Commentary:

;; This package implements a minor mode for XEmacs which, when enabled,
;; will display the line number on each line on current window. Note that
;; this is different from another similarly-named minor-mode line-number-mode
;; which only displays the current line number at the point on modeline.
;; The numbers are displayed as annotations, they are not part of actual text.

;; To use it,
;; 1. byte-compile the elisp file
;; 2. add this line to your .emacs file:  (require 'line-numbers-mode)
;; 3. use the minor-mode-menu on modeline or M-x line-numbers-mode to toggle
;;    3a. if you like to turn this minor mode on for every file you open, then
;;        add this line to your .emacs file:
;;        (add-hook 'find-file-hook 'line-numbers-mode)
;;    3b. if you like to turn this minor mode on for certain major-mode files,
;;        say cperl-mode, then add a mode hook:
;;        (add-hook 'cperl-mode-hook 'line-numbers-mode)

;;; Caveats/Bugs:

;; This minor-mode was developed in XEmacs v21.0b60, and tested on XEmacs 20.4, 19.16 and 19.14
;; Since XEmacs-specific extents are used, it will not work for any version of FSF GNU Emacs.
;; Some line numbers may not show immediately after each command.

;;; Change Log:

;; 0.10 01/29/1999
;;      initial release on comp.emacs.xemacs
;; 0.20 02/02/1999
;;      - fixed regarding use `get-buffer-window-list' to obtain a list of windows
;;        for a buffer: 1) all args are optional in 21.0, but the 1st one must not
;;        in 20.4, so replaced nil w/ (current-buffer); 2) In 19.16 on solaris, it isn't
;;        defined anywhere; but in 19.14, it's defined <prefix>/lib/xemacs-19.14/lib/
;;        eos/sun-eos-debugger-extra.el with only one arg BUFFER -- in case of 19.xx,
;;        (re)define this defun with code stolen from subr.el in 21.0
;;      - sped up using a cache to keep the list of extents used, instead of `map-extents'
;;      - set alias `setnu' for Vi users
;;      - added `line-numbers-face' to hilite line numbers

;;; Code:
(require 'cl)

(defvar line-numbers-mode nil
  "*Non-nil means to display line numbers on each line in current window.")
(make-variable-buffer-local 'line-numbers-mode)

(defvar line-numbers-cache nil
  "Cache used to store the extents for the line numbers")
(make-variable-buffer-local 'line-numbers-cache)

(defvar line-numbers-rows nil)
(make-variable-buffer-local 'line-numbers-rows)

(defvar line-numbers-face (make-face 'line-numbers-face)
  "Face used to display line numbers")
(set-face-background line-numbers-face "gray90")


(fset 'setnu 'line-numbers-mode)

(defun line-numbers-mode (&optional arg) 
  "Toggle line numbers mode.
With arg, turn this mode on iff arg is positive."
  (interactive "P")
  (setq line-numbers-mode
	(if (null arg) (not line-numbers-mode)
	  (> (prefix-numeric-value arg) 0)))

  (if line-numbers-mode
      nil
    (line-numbers-hide)
    (setq line-numbers-rows nil))

  (redraw-modeline)
)


;; compatibility for 19.16, 19.14
(if (string-match "^19" emacs-version)
    ;;extracted from subr.el in 21.0b60
    (defun get-buffer-window-list (&optional buffer minibuf frame)
      "Return windows currently displaying BUFFER, or nil if none.
BUFFER defaults to the current buffer.
See `walk-windows' for the meaning of MINIBUF and FRAME."
      (cond ((null buffer)
	     (setq buffer (current-buffer)))
	    ((not (bufferp buffer))
	     (setq buffer (get-buffer buffer))))
      (let (windows)
	(walk-windows (lambda (window)
			(if (eq (window-buffer window) buffer)
			    (push window windows)))
		      minibuf frame)
	windows)))


(defun line-numbers-hide ()
  "Hide line numbers in current-buffer shown by `line-numbers-show'."
  (let (x)
    (while (setq x (pop line-numbers-cache))
      (and (extent-live-p x)
	   (delete-extent x))))
  (setq line-numbers-cache nil)
)


(defun line-numbers-dirty-p (rows)
  "Return t if any of the windows displaying the current buffer has either
the start or end position changed. ROWS is a list of (WINDOW START END)."
  (let (all r w x ok)
    (setq all rows)
    (while (setq r (pop all))
      (setq w (car r))
      (if (and (setq x (assq w line-numbers-rows))
	       (eq (nth 1 r) (nth 1 x))
	       (eq (nth 2 r) (nth 2 x)))
	  nil
	(setq all nil
	      ok t)))
    (if ok
	(setq line-numbers-rows rows))
    
    ok)
)

(defun line-numbers-show ()
  "Show line numbers on each visible line in current-window."
    
  (if (line-numbers-dirty-p (mapcar '(lambda(w)
				       (list w (window-start w) (window-end w)))
				    (get-buffer-window-list (current-buffer) 0 t)))
      (let (r b e ok p x rows g)
	(line-numbers-hide)
    
	(save-excursion
	  (setq rows line-numbers-rows)
	  (while (setq x (pop rows))
	    (setq b (nth 1 x)
		  e (nth 2 x)
		  r (1+ (count-lines 1 b))
		  ok t)

	    (goto-char b)
	    (while ok
	      (if (extent-at (setq p (point)) nil 'lnm nil 'at)
		  nil
		;;else
		(setq x (make-extent p p)
		      g (make-glyph (format "%7d " r)))
		
		(set-glyph-face g line-numbers-face)

		(set-extent-property x 'start-open t)
		(set-extent-property x 'end-open t)
		(set-extent-property x 'begin-glyph g)
		(set-extent-property x 'lnm r)
		
		(push x line-numbers-cache)
		);if
	      
	      (setq r (1+ r))
	      (if (search-forward "\n" nil t)
		  (setq ok (<= (point) e))
		(setq ok nil))
	      
	      )))))
)


(defun line-numbers-post-command-cb ()
  "Callback attached to `post-command-hook' to make `line-numbers-mode' working."
  (if line-numbers-mode
      (condition-case nil
	  (line-numbers-show)
	(error nil))
    )
)

;; attach it to the XEmacs system
(add-to-list 'minor-mode-alist 
	     (list 'line-numbers-mode (cons modeline-mousable-minor-mode-extent " L#")))

(add-hook 'post-command-hook 'line-numbers-post-command-cb)

(provide 'line-numbers-mode)

;;; File line-numbers-mode.el ends here.