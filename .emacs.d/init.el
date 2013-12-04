; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; load-path
(let ((dir (expand-file-name "~/.emacs.d/plugins")))
  (add-to-list 'load-path dir)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; settings
(setq make-backup-files nil)
(global-linum-mode t)
(setq linum-format "%3d ")

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t)

(cua-mode t)
(setq cua-enable-cua-keys nil)

;; C-x bでミニバッファにバッファ候補を表示
;; (iswitchb-mode t)
;; (iswitchb-default-keybindings)

;; C-kで行全体を削除
(setq kill-whole-line t)

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; packages
(when (require 'color-theme)
      (color-theme-initialize)
      (color-theme-clarity))

;; [lisp] rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
; (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8888")))))

;; mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(autoload 'go-mode "go-mode"
  "Mode for golang" t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
