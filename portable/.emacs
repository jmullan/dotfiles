(defun add-search-dir (path)
  (setq load-path (cons (expand-file-name path) load-path)))
(add-search-dir "~/lib/emacs/lisp")

(when (>= emacs-major-version 24)
    (require 'package)


    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                       (not (gnutls-available-p))))
              (proto (if no-ssl "http" "https")))
        ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
        (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
        ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
        (when (< emacs-major-version 24)
            ;; For important compatibility libraries like cl-lib
            (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
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
         groovy-mode
         js3-mode
         json-mode
         json-reformat
         less-css-mode
         let-alist
         lua-mode
         markdown-mode
         php-mode
         pkg-info
         rjsx-mode
         scala-mode2
         seq
         sql-indent
         yaml-mode
         yaml-tomato
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(custom-safe-themes
         (quote
             ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.25)
    '(package-selected-packages
         (quote
             (solarized-theme rjsx-mode yaml-tomato yaml-mode sql-indent scala-mode2 php-mode markdown-mode+ lua-mode json-mode js3-mode groovy-mode flymake-yaml flymake-python-pyflakes flymake-phpcs flymake-php flymake-less flymake-json flymake-jshint flymake-cursor flymake-css flycheck-status-emoji flycheck-pyflakes flycheck-color-mode-line editorconfig column-marker color-theme-solarized)))
 '(solarized-contrast (quote high)))

(add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-mode))

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

; java stuff

(defun java-indent-setup ()
    (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'java-indent-setup)



; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
; (load-theme 'solarized t)
; (set-frame-parameter nil 'background-mode 'dark)
; (set-terminal-parameter nil 'background-mode 'dark)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq auto-mode-alist (append '(("\\.arcconfig" . json-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.arclint" . json-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.apib" . markdown-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.gradle" . groovy-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ino" . c++-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("Jenkinsfile" . groovy-mode)) auto-mode-alist))


;; flycheck stuff
;collections/src/main/java/com/savagebeast/collections/clustr/CollectionsClustrInitializer.java:82:86: warning: '+' should be on a new line.

(when (load "flycheck" t)
    (flycheck-define-checker python-checker "Check yer python"
        :command ("lint-python" source)
        :error-patterns
        (
            (error line-start
                (file-name)
                ":"
                line
                ":"
                column
                ":"
                (minimal-match (message))
                line-end
                )
            (error line-start
                (file-name)
                ":"
                line
                ":"
                (minimal-match (message))
                line-end
                )
            )
        :modes (python-mode cpython-mode))


        (flycheck-define-checker java-checker "Check yer java"
        :command ("lint-java" source-original)
        :error-patterns
        (
            (error line-start
                (file-name)
                ":"
                line
                ":"
                column
                ": "
                (message)
                line-end
                )
            (error line-start
                (file-name)
                ":"
                line
                ": "
                (minimal-match (message))
                line-end
                )
            )
        :modes (java-mode))

    (add-to-list 'flycheck-checkers 'python-checker)
    (add-to-list 'flycheck-checkers 'java-checker)

    (setq flycheck-highlighting-mode 'lines)
    )

;; (warning line-start (file-name) ":" line ":" column ": W: " (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
;; (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": " (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end))
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide '.emacs)
;;; .emacs ends here
