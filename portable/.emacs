(defun add-search-dir (path)
  (setq load-path (cons (expand-file-name path) load-path)))
(add-search-dir "~/lib/emacs/lisp")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

(setq package-list
    '(
         color-theme-solarized
         column-marker
         dash
         editorconfig
         editorconfig-core
         editorconfig-fnmatch
         epl
         flycheck
         flycheck-color-mode-line
         flycheck-pyflakes
         flymake-css
         flymake-easy
         flymake-jshint
         flymake-json
         flymake-less
         flymake-php
         flymake-phpcs
         flymake-python-pyflakes
         js3-mode
         json-mode
         json-reformat
         less-css-mode
         let-alist
         lua-mode
         markdown-mode
         php-mode
         pkg-info
         scala-mode2
         seq
         sql-indent
         )
    )

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 package-list)

(editorconfig-mode 1)

(defun my-docbook-settings()
    (interactive)
    (setq sgml-indent-step 1)
    (setq sgml-indent-data t)
    (setq indent-tabs-mode nil)
    (setq sgml-default-dtd-file nil)
)

(defun indent-buffer ()
    (interactive)
    (save-excursion (indent-region (point-min) (point-max) nil))
)

;; Everything utf-8
(define-coding-system-alias 'UTF-8 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; default editing settings
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode nil)
(setq py-start-run-py-shell nil)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq-default indent-tabs-mode nil)
(set-variable 'indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; display settings
(column-number-mode)
(global-linum-mode 1)
(menu-bar-mode nil)
(setq split-height-threshold 400)
(setq split-width-threshold 121)
(setq-default show-trailing-whitespace t)

;; key settings
(define-key esc-map "[3~" 'delete-char)
(define-key global-map [kp-0] "0")
(define-key global-map [kp-1] "1")
(define-key global-map [kp-2] "2")
(define-key global-map [kp-3] "3")
(define-key global-map [kp-4] "4")
(define-key global-map [kp-5] "5")
(define-key global-map [kp-6] "6")
(define-key global-map [kp-7] "7")
(define-key global-map [kp-8] "8")
(define-key global-map [kp-9] "9")
(define-key global-map [kp-separator] "+")
(define-key global-map [kp-enter] "
")
(define-key global-map [select] 'end-of-line)
(define-key global-map (kbd "ESC <up>") 'beginning-of-buffer)
(define-key global-map (kbd "ESC <down>") 'end-of-buffer)

(normal-erase-is-backspace-mode 0)
(define-key function-key-map [delete] nil)
(global-set-key [delete] 'delete-char)
(global-set-key [f2] 'indent-buffer)

(fset 'indent-by-four "\C-u4\C-x\C-i")
(fset 'dedent-by-four "\C-u-4\C-x\C-i")
(fset 'indent-by-two "\C-u2\C-x\C-i")
(fset 'dedent-by-two "\C-u-2\C-x\C-i")

(define-key input-decode-map "\e[1;6H" [S-home]) ; control-shift-home
(define-key input-decode-map "\e[1;6F" [S-end]) ; control-shift-end
(define-key input-decode-map "\e[1;2H" [S-up]) ; shift-home
(define-key input-decode-map "\e[1;2F" [S-down]) ; shift-end
(define-key input-decode-map "\e[1;2A" [S-up])
;(global-set-key (kbd "C-c TAB") 'indent-by-four)
;(global-set-key (kbd "C-c q") 'dedent-by-four)
(global-set-key (kbd "C-c TAB") 'indent-by-two)
(global-set-key (kbd "C-c q") 'dedent-by-two)

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


;; python flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let*
        ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
      (list "lint-python" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(when (require 'flymake)
  (set-variable 'flymake-log-level 9)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flymake-no-changes-timeout 5))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; editorconfig hooks
(add-hook 'editorconfig-custom-hooks
  '(lambda (props)
       (let ((max_line_length (gethash 'max_line_length props)))
           (column-marker-1
               (string-to-number (if max_line_length max_line_length "80")))
           )
       )
    )

(add-hook 'editorconfig-custom-hooks
  '(lambda (props)
       (let ((indent_size (gethash 'indent_size props)))
           (setq-default c-basic-offset
               (string-to-number (if indent_size indent_size "4")))
           (setq-default python-indent
               (string-to-number (if indent_size indent_size "4")))
           (setq-default python-indent-offset
               (string-to-number (if indent_size indent_size "4")))
           )
       )
    )

(add-hook 'editorconfig-custom-hooks
  '(lambda (props)
       (let ((tab_width (gethash 'tab_width props)))
           (setq-default tab-width
               (string-to-number (if tab_width tab_width "4")))
           )
       )
    )

(add-hook 'editorconfig-custom-hooks
  '(lambda (props)
       (let ((indent_style (gethash 'indent_style props)))
           (setq-default indent-tabs-mode (eq indent_style "tab")
             )
          )
       )
    )


;(load "php-style" t t)
;(load "gallery-template-mode" t t)
;(setq auto-mode-alist (cons '("\\.r$" . c-mode) auto-mode-alist))

; block saving of php files unless they are syntactically valid
(autoload 'php-lint-mode "php-lint-mode" "PHP lint mode" t)
(add-hook 'php-mode-hook '(lambda () (php-lint-mode 1)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(solarized-contrast (quote high)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq auto-mode-alist (append '(("\\.arcconfig" . json-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.arclint" . json-mode)) auto-mode-alist))
