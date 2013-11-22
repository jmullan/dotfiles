;;; XEmacs backwards compatibility file
(setq-default indent-tabs-mode nil)
(setq user-init-file (expand-file-name "init.el"   (expand-file-name ".xemacs" "~")))
(setq custom-file    (expand-file-name "custom.el" (expand-file-name ".xemacs" "~")))
(setq split-height-threshold nil)
(setq split-width-threshold 150)

(defvar font-lock-preprocessor-face 'font-lock-keyword-face  "Don't even think of using this.")

(load-file user-init-file)
(load-file custom-file)
(defun my-docbook-settings()
    (interactive)
    (setq sgml-indent-step 1)
    (setq sgml-indent-data t)
    (setq indent-tabs-mode nil)
    (setq sgml-default-dtd-file nil)
)

(load "~/lib/emacs/lisp/syntax")


(add-hook
 'sql-mode-hook
 (function
  (lambda ()
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" sql-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" sql-mode-syntax-table)
    ;; double-dash starts comments
    (modify-syntax-entry ?- ". 12b" sql-mode-syntax-table)
    (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" sql-mode-syntax-table)
    ;; single quotes (`) delimit strings
    (modify-syntax-entry ?` "\"" sql-mode-syntax-table)
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" sql-mode-syntax-table)
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "." sql-mode-syntax-table)
    ;; backslash is no escape character
    (modify-syntax-entry ?\\ "." sql-mode-syntax-table)
    (setq indent-tabs-mode nil)
    )
  )
 )

(defconst yahoo-zend-style
  '((c-basic-offset . 4)

    (my-c-continuation-offset . 8)

    (c-comment-only-line-offset . (0 . 0))
    (setq indent-tabs-mode nil)
    (c-offsets-alist . ((inline-open . 0)
                        (topmost-intro-cont    . 0)
                        (statement-block-intro . +)
                        (knr-argdecl-intro     . 5)
                        (substatement-open     . +)
                        (label                 . 0)
                        (arglist-intro         . +)
                        (arglist-cont          . 0)
                        (arglist-close         . 0)
			(case-label            . +)
                        (statement-case-open   . +)
                        (statement-cont        . +)
                        (access-label          . 0)
			)
		     )
    (c-echo-syntactic-information-p . t)     ; turn this on to get debug info
    )
  "Zend PHP Style for Yahoo")

(defconst yahoo-zendjs-style
  '((c-basic-offset . 4)
    (setq indent-tabs-mode nil)
    (my-c-continuation-offset . 8)
    (c-comment-only-line-offset . (0 . 0))
    (c-offsets-alist . (
			(inline-open . 0)
                        (topmost-intro-cont    . +)
                        (statement-block-intro . +)
                        (knr-argdecl-intro     . 5)
                        (substatement-open     . +)
                        (label                 . +)
                        (arglist-intro         . +)
                        (arglist-cont          . 0)
                        (arglist-close         . 0)
			(case-label            . +)
                        (statement-case-open   . +)
                        (statement-cont        . +)
                        (access-label          . 0)
			(block-close           . 2)
			(defun-close           . 2)
			)
		     )
    (c-echo-syntactic-information-p . t)     ; turn this on to get debug info
    )
  "Zend Javascript Style for Yahoo")

(defconst psrtwo-style
  '((c-basic-offset . 4)
    (my-c-continuation-offset . 8)
    (c-comment-only-line-offset . (0 . 0))
    (setq indent-tabs-mode nil)
    (c-offsets-alist . ((inline-open . 0)
                        (topmost-intro-cont    . 0)
                        (statement-block-intro . +)
                        (knr-argdecl-intro     . 5)
                        (substatement-open     . 0)
                        (label                 . 0)
                        (arglist-intro         . +)
                        (arglist-cont          . 0)
                        (arglist-close         . 0)
			(case-label            . +)
                        (statement-case-open   . +)
                        (statement-cont        . +)
                        (access-label          . 0)
			)
		     )
    (c-echo-syntactic-information-p . t)     ; turn this on to get debug info
    )
  "PSR2 Mode")


(add-hook
 'php-mode-hook
 (function
  (lambda ()
    (set (make-local-variable 'compile-command) (format "phpcs --report=emacs --standard=PSR2 %s" (buffer-file-name)))
    (modify-syntax-entry ?/ ". 124b" php-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" php-mode-syntax-table)
    (modify-syntax-entry ?# "< b" php-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" php-mode-syntax-table)
    (setq indent-tabs-mode nil)
    (setq c-electric-flag t)
    (c-add-style "PSR2" psrtwo-style t)
    )
  )
)

(add-hook
 'espresso-mode-hook
 (function
  (lambda ()
    (setq indent-tabs-mode nil)
    (c-add-style "Zend Yahoo js" yahoo-zendjs-style t)
    (c-echo-syntactic-information-p . t)
;    (modify-syntax-entry ?/ "\"" espresso-mode-syntax-table)
    )
  )
)


;'("\\<\\<\\<\\" . font-lock-string-face)
;'("\\<\\<\\<\\'([a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*)\n.*\1" . font-lock-keyword-face)

;; Note whether we're in XEmacs
(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version) "Non nil if using XEmacs.")

(defconst php-font-lock-syntactic-keywords
  '(
    ("\\(<\\?php\\)" 1 "(")
    ("\\(\\?>\\)" 1 ")")
  )
)
(defvar c-echo-syntactic-information-p t)

(add-to-list 'load-path "")

(autoload 'php-mode "php-mode" "PHP editing mode" t)
(autoload 'php-lint-mode "php-lint-mode" "PHP lint mode" t)
;(autoload 'php-electric-mode "php-electric" "PHP Electric Mode" t)
;(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)

(autoload 'js2-mode "js2-mode" "Starting js2-mode" t)
(autoload 'espresso-mode "espresso" "Starting espresso-mode" t)

(autoload 'css-mode "css-mode")
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

(load "~/lib/emacs/lisp/php-cleanup")
;(load "~/lib/emacs/lisp/js-cleanup")
(load "~/lib/emacs/lisp/css-cleanup")
;(load "~/lib/emacs/lisp/less-css-mode")

(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode nil)
(setq indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

(defun indent-buffer ()
    (interactive)
    (save-excursion (indent-region (point-min) (point-max) nil))
)
(global-set-key [f2] 'indent-buffer)


(setq auto-mode-alist (append '(("\\.php$" . php-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.class$" . php-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.module$" . php-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.inc$" . php-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.js$" . js2-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.json$" . js2-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("Jakefile$" . javascript-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.phtml$" . php-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.\\(htm\\|html\\)$" . nxml-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.text" . markdown-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.md" . markdown-mode)) auto-mode-alist))

;; STUFF FROM DAVID
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil
			'(lambda(ovl) (delete-overlay ovl)) ovl)
	)
      )
    )
)

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars) (js2-highlight-vars-mode))
  (message "My JS2 hook")
)

(add-hook
 'js2-mode-hook
 '(lambda ()
    (add-hook
     'before-save-hook
     (lambda ()
       (untabify (point-min) (point-max))
       (delete-trailing-whitespace)
       ))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(define-key function-key-map [delete] nil)
(global-set-key [delete] 'delete-char)

;;(require 'line-numbers-mode)
(global-linum-mode 1)

(normal-erase-is-backspace-mode 0)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(menu-bar-mode nil)

;;; To use this, add the following code to your .emacs file and copy

(add-hook 'php-mode-hook '(lambda () (php-lint-mode 1)))
;(eval-after-load "php-mode" '(add-hook 'php-mode-hook 'php-electric-mode))

(require 'setnu+)


; From:
; http://snarfed.org/space/emacs+page+up+page+down
;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(eval-after-load "sql" '(load-library "sql-indent"))


(column-number-mode)
(set-variable 'indent-tabs-mode nil)
(setq indent-tabs-mode nil)

(when (load "flymake" t)
     (defun flymake-pyflakes-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
      (local-file (file-relative-name
               temp-file
               (file-name-directory buffer-file-name))))
         (list "pyflakes" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)

(require 'flyphpcs)
(setq c-default-style "k&r"
      c-basic-offset 4)
