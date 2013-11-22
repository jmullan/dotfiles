;;; php-lint-mode.el --- Minor mode to require B::Lint clean php.

;;; To use this, add the following code to your .emacs file and copy
;;; (add-to-list 'load-path "~/.site-lisp/")
;;; (autoload 'php-lint "php-lint-mode" nil t)
;;; (autoload 'php-lint-mode "php-lint-mode" nil t)

;;; Automatically enable php-lint-mode for php-mode
;;; (eval-after-load "php-mode" '(add-hook 'php-mode-hook 'php-lint-mode))

;;; B::Lint is a module in core php.

;;; Copyright 2006 Joshua ben Jore

;;; Author: Joshua ben Jore <jjore@cpan.org>
;;; Version: 0.02
;;; CVS Version: $Id$
;;; Keywords: php B::Lint
;;; X-URL: http://search.cpan.org/~jjore/php-lint-mode/

;;; This program is free software; you can redistribute it and/or
;;; modify it under the same terms as Php itself.

;;; To use this, add the following code to your .emacs file and copy
;;; php-lint-mode.el to your ~/.site-lisp/ directory.
;;;
;;;   (add-to-list 'load-path "~/.site-lisp/")
;;;   (autoload 'php-lint "php-lint-mode" nil t)
;;;   (autoload 'php-lint-mode "php-lint-mode" nil t)
;;;
;;; To use this automatically when php-mode is enabled, also add the
;;; following code to your .emacs file.
;;;
;;;   ; Automatically enable php-lint-mode for php-mode
;;;   (eval-after-load "php-mode"
;;;     '(add-hook 'php-mode-hook 'php-lint-mode))


(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                           Lint checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar php-lint-bin
  "/usr/bin/php"
  "The php binary used to check for lint.")


(defvar php-lint-checks () "...")
(defvar clean_output (concat " $s = stream_get_contents(STDIN); "
			     " $s = trim(str_ireplace(array('No syntax errors detected in -', 'Errors parsing -'), array('',''), $s)); "
			     " fwrite(STDOUT, $s); "
			     )
  )
(defvar get_line (concat " $s = stream_get_contents(STDIN);"
			 " $matches = array(); "
			 " if (preg_match('/on line ([0-9]+)/', $s, $matches)) { "
			 "     $s = intval($matches[1]); "
			 "     fwrite(STDOUT, $s); "
			 " } "
			 )
  )
(defvar error_line -1)
(defvar linted-ok nil)

(defun php-lint () "Returns a either nil or t depending on whether the current buffer
passes php's lint check. If there's a failure, the failures are
displayed in an adjacent buffer."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (let ((input-buf (current-buffer))
            (lint-buf (get-buffer-create "*Lint check*"))
            (linenum-buf (get-buffer-create "*Line number*"))
            (interpreter (progn (goto-char (point-min))
				(if (looking-at auto-mode-interpreter-regexp)
				    (match-string 2)
				  php-lint-bin))))
					; Clear the lint buffer if it needs it.
	(setq error_line -1)
        (if (zerop (buffer-size lint-buf))
            nil
          (set-buffer lint-buf)
          (erase-buffer))
					; Run B::Lint on the current buffer using whatever checks
					; the user would like.
        (set-buffer input-buf)
        (let ((rc (call-process-region
                   (point-min)          ; start
		   (point-max)          ; end
                   interpreter          ; progam
		   nil                  ; delete
                   (list lint-buf t)    ; destination
                   nil                  ; display
					;args
                   (reduce (lambda (l r) (concat l " " r)) (cons "-l" php-lint-checks))
		   )
		  )
	      )

					; Check that the B::Lint succeeded or clean up the error
					; messages it posted.
          (set-buffer lint-buf)
          (goto-char (point-min))
          (if (numberp rc)
              (if (not (zerop rc))
                  (let
		      ((foo nil))
		    (call-process-region
		     (point-min)            ; start
		     (point-max)            ; end
		     php-lint-bin           ; program
		     t                      ; delete
		     (list lint-buf t)      ; destination
		     t                      ; display
		     "-r"                   ; arg
		     clean_output)
		    (call-process-region
		     (point-min)            ; start
		     (point-max)            ; end
		     php-lint-bin           ; program
		     nil                      ; delete
		     (list linenum-buf t)   ; destination
		     t                      ; display
		     "-r"                   ; arg
		     get_line)
		    (set-buffer linenum-buf)
		    (setq error_line (string-to-number (buffer-string)))
		    (set-buffer lint-buf)
                                        ;(insert "\n")
                                        ;(insert (number-to-string error_line))
                                        ;(insert "\n")

		    )

                (call-process-region
                 (point-min)            ; start
		 (point-max)            ; end
                 php-lint-bin           ; program
		 t                      ; delete
                 (list lint-buf t)      ; destination
                 t                      ; display
		 "-r"                  ; arg
		 clean_output
                 ))
	    ;; Sometimes non-numeric results come back. I'm just bailing and inserting
	    ;; them for the user to deal with.
	    (insert rc "\n"))
          (let
	      ((lint-ok (and (numberp rc) (zerop rc) (zerop (buffer-size)))))
	    (setq linted-ok lint-ok)
	    (kill-buffer linenum-buf)
	    (if	lint-ok (kill-buffer lint-buf) (display-buffer lint-buf))
					; Oh yeah. Return a boolean too.

            lint-ok))))
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lint mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar in-php-lint-mode nil "Check php lint before saving.")
(make-variable-buffer-local 'in-php-lint-mode)

(defun php-lint-write-hook ()
  "Check php lint during `write-file-hooks' for `php-lint-mode'"
  (if in-php-lint-mode
      (save-restriction
        (widen)
        (save-excursion (mark-whole-buffer) (not (php-lint))))
    nil)
  (when (and in-php-lint-mode (< -1 error_line)) (goto-line error_line))
  (and in-php-lint-mode (not linted-ok))
  )

(defun php-lint-mode (&optional arg)
  "Php lint checking minor mode."
  (interactive "P")
  (setq in-php-lint-mode
        (if (null arg)
            (not in-php-lint-mode)
          (> (prefix-numeric-value arg) 0)))
  (if in-php-lint-mode
      (add-hook 'write-file-hooks 'php-lint-write-hook)
      (remove-hook 'write-file-hooks 'php-lint-write-hook))
  )

(if (not (assq 'php-lint-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(php-lint-mode " Lint") minor-mode-alist)))


(provide 'php-lint-mode)
