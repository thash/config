; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; TODO: load-path
(let ((dir (expand-file-name "~/.emacs.d/plugins")))
  (add-to-list 'load-path dir)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))


;;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)


(setq make-backup-files nil)
(global-linum-mode t)
(setq linum-format "%3d ")

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-h" 'delete-backward-char)
;; originally 'exchange-point-and-mark
(global-set-key "\C-x\C-x" 'kill-region)
(global-set-key "\C-x:" 'goto-line)

;; tab & space
(setq-default indent-tabs-mode nil)
(custom-set-variables '(tab-width 4))

;; white spaces
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t)

(modify-syntax-entry ?_ "w")

;; C-Space -> select -> then, press C-RET
(setq cua-rectangle-mark-key (kbd "C-RET")) ;; should come before enabling cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; C-x,c,v for cut, copy, paste

;; incremental search buffer items by C-x b
(iswitchb-mode t)

;;; looks
(when (require 'color-theme)
      (color-theme-initialize)
      (color-theme-clarity))

;; GUI
(let ((ws window-system))
  (cond ((eq ws 'ns) ;; mac
         (set-face-attribute 'default nil :family "Ricty" :height 120)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
         (set-foreground-color "LightGray")
         (set-cursor-color "Gray")
         (set-background-color "Black")
         (set-frame-parameter nil 'alpha 80))))

(custom-set-variables
  '(display-time-mode t)
  '(tool-bar-mode nil)
  '(transient-mark-mode t))
(custom-set-faces)


;; auto-complete (elpa)
(require 'auto-complete-config)
(ac-config-default)

;; browse-url (built-in)
(global-set-key "\C-cu" 'browse-url-at-point)

;; rainbow-delimiters (elpa)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
; (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8888")))))

;; cursor-in-brackets (github)
;; https://raw.github.com/yascentur/cursor-in-brackets-el/master/cursor-in-brackets.el
(require 'cursor-in-brackets)
(global-cursor-in-brackets-mode t)

;; yasnippet (elpa)
(require 'yasnippet)
(yas-global-mode 1)


;;; mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Gauche
(require 'cmuscheme) ;; use run-scheme
;; Split window and exec gosh interpreter
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*gosh*"))
  (run-scheme "gosh"))
;; C-c S(open gosh window) -> then, http://goo.gl/uSNTzm
(define-key global-map "\C-cS" 'scheme-other-window)
(show-paren-mode) ;; highlight pair parenthesis

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Golang
(autoload 'go-mode "go-mode" "Mode for golang" t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
