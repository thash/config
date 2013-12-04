; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; TODO: load-path
;;;;;;;;;;;;;; (let ((dir (expand-file-name "~/.emacs.d/plugins")))
;;;;;;;;;;;;;;   (add-to-list 'load-path dir)
;;;;;;;;;;;;;;   (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;;;;;;;;;;;;;       (normal-top-level-add-subdirs-to-load-path)))

;; settings
(setq make-backup-files nil)
(global-linum-mode t)
(setq linum-format "%3d ")

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-h" 'delete-backward-char)

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

;; looks
(custom-set-variables
  '(display-time-mode t)
  '(tool-bar-mode nil)
  '(transient-mark-mode t))
(custom-set-faces)

(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 70)))

(when (require 'color-theme)
      (color-theme-initialize)
      (color-theme-clarity))

;; [lisp] rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8888")))))

;;; mode
;; Ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Gauche
(setq scheme-program-name "gosh")
(require 'cmuscheme)
;; ウィンドウを２つに分けて、
;; 一方でgoshインタプリタを実行するコマンドを定義します。
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
;; そのコマンドをCtrl-cSで呼び出します。
(define-key global-map
  "\C-cS" 'scheme-other-window)

;; 直前/直後の括弧に対応する括弧を光らせます。
(show-paren-mode)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Golang
(autoload 'go-mode "go-mode"
  "Mode for golang" t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
