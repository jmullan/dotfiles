; =============================================================================
;
; Gallery - a web based photo album viewer and editor 
; Copyright (C) 2000-2001 Bharat Mediratta 
;  
; This program is free software; you can redistribute it and/or modify 
; it under the terms of the GNU General Public License as published by 
; the Free Software Foundation; either version 2 of the License, or (at 
; your option) any later version. 
;  
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of 
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
; General Public License for more details. 
;  
; You should have received a copy of the GNU General Public License 
; along with this program; if not, write to the Free Software 
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
;
; =============================================================================
;
; To use this file, you must first install php-mode.el from
;     http://sourceforge.net/projects/php-mode
;
;
; This file sets the indentation style for PHP code to match
; the coding standard for the Gallery project, which can be 
; found here:
;
;    http://sourceforge.net/docman/display_doc.php?docid=7664&group_id=7130
;
;

; Bind file suffixes to php-mode
;
;(setq auto-mode-alist (append '(("\\.php$" . php-mode)) auto-mode-alist))

; Autoload any necessary major modes
;
;(autoload 'php-mode "php-mode" "PHP editing mode." t)

;;
;
; Set up my PHP code convention compliant style.  This is very
; similar to java-style that comes with GNUEmacs 20.4
;
(defconst php-gallery-style
  '(
    (c-basic-offset . 4)
    (my-c-continuation-offset . 8)
    (c-comment-only-line-offset . (0 . 0))
    (c-offsets-alist . ((inline-open . 0)
			(topmost-intro-cont    . 0)
			(statement-block-intro . +)
			(knr-argdecl-intro     . 5)
			(substatement-open     . +)
			(label                 . 0)
			(arglist-intro         . c-lineup-arglist-intro-after-paren)
			(arglist-cont          . c-lineup-arglist-intro-after-paren)
			(arglist-close         . c-lineup-arglist)
			(statement-case-open   . +)
			(statement-cont        . ++)
			(access-label          . 0)
			))
    ; (c-echo-syntactic-information-p . t)     ; turn this on to get debug info
    )
  "PHP Style for the Gallery Project")

;;
;
; Create a hook that adds the style to the list and 
; sets a few useful variables.
;
(defun my-c-mode-common-hook ()
  (c-add-style "php-gallery" php-gallery-style t)
  (turn-on-auto-fill)                 ; turn on auto-fill by default
  (setq fill-column 79)
  (setq comment-column 50)            ; set default comment column
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  )

;;
;
; Add my hook to the c-mode hook list
;
; These colors are designed to work with an ANSI terminal emulator
; that only supports 16 colors.  The color names below are simply
; indexes; the terminal remaps them whatever color the end user wants.
; 
; You'll probably want to change them to suit your taste.
;
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;(add-hook 'php-mode-hook 'my-php-mode-hook)
;(defun my-php-mode-hook ()
;  (require 'font-lock)
;  (turn-on-font-lock)
;  (set-face-highlight-p 'font-lock-type-face nil)
;  (set-face-foreground 'font-lock-type-face "green")
;
;  (set-face-highlight-p 'font-lock-function-name-face nil)
;  (set-face-underline-p 'font-lock-function-name-face nil)
;  (set-face-foreground 'font-lock-function-name-face "blue")
;  (set-face-highlight-p 'font-lock-keyword-face nil)
;  (set-face-foreground 'font-lock-keyword-face "magenta")

;  (set-face-highlight-p 'font-lock-comment-face nil)
;  (set-face-foreground 'font-lock-comment-face "brightwhite")

;  (set-face-underline-p 'font-lock-variable-name-face nil)
;  (set-face-foreground 'font-lock-variable-name-face "yellow")

;  (set-face-foreground 'font-lock-reference-face "cyan")
;  (set-face-foreground 'font-lock-doc-string-face "x")

;  (set-face-underline-p 'font-lock-preprocessor-face nil)
;  (set-face-foreground 'font-lock-preprocessor-face "cyan")

;  (set-face-highlight-p 'font-lock-string-face nil)
;  (set-face-foreground 'font-lock-string-face "red")

;  (set-face-foreground 'font-lock-warning-face "x"))
;(setq font-lock-maximum-decoration t)
