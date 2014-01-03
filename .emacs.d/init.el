; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; load-path
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
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)


;; key-chord (elpa)
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

;; evil (elpa)
(require 'evil)
;; ESC keys
(global-set-key "\C- " nil)
(define-key evil-normal-state-map (kbd "C-<SPC>") 'evil-force-normal-state)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'evil-force-normal-state)
(define-key evil-visual-state-map (kbd "C-<SPC>") 'evil-force-normal-state)
(define-key evil-replace-state-map (kbd "C-<SPC>") 'evil-force-normal-state)
(define-key evil-operator-state-map (kbd "C-<SPC>") 'evil-force-normal-state)
(define-key evil-motion-state-map (kbd "C-<SPC>") 'evil-force-normal-state)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
;; Emacs-like edit mode for insert state
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
;; normal state
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "j") 'next-line) ;; visual line
(define-key evil-normal-state-map (kbd "k") 'previous-line)
(key-chord-define evil-normal-state-map " w" 'save-buffer)
(key-chord-define evil-normal-state-map " q" 'delete-window)
(key-chord-define evil-normal-state-map " h" 'windmove-left)
(key-chord-define evil-normal-state-map " j" 'windmove-down)
(key-chord-define evil-normal-state-map " k" 'windmove-up)
(key-chord-define evil-normal-state-map " l" 'windmove-right)
(evil-mode 1)

;; evil-surround (plugins) -- https://github.com/timcharper/evil-surround
(require 'surround)
(global-surround-mode 1)

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
(load-theme 'solarized-dark t) ;; install elpa "color-theme-solarized"
(setq solarized-termcolors 256)

;; Twittering-mode
(defun activate-twittering-mode ()
  (require 'twittering-mode)
  (setq twittering-use-master-password t))

;; GUI
(let ((ws window-system))
  (cond ((eq ws 'ns) ;; mac
         (progn (set-face-attribute 'default nil :family "Ricty" :height 120)
                (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
                (set-frame-parameter nil 'alpha 90)
                (set-scroll-bar-mode nil)
                (activate-twittering-mode)))))

(custom-set-variables
  '(display-time-mode t)
  '(tool-bar-mode nil)
  '(transient-mark-mode t))
(custom-set-faces)


;; auto-complete (elpa)
(require 'auto-complete-config)
(ac-config-default)

;; flx-ido (elpa)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; projectile (elpa) -- uses flx-ido
(projectile-global-mode)
(key-chord-define evil-normal-state-map ",j" 'projectile-find-file)

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
(setq yas-snippet-dirs `("~/.emacs.d/snippets"
                         ,(car (file-expand-wildcards "~/.emacs.d/elpa/yasnippet-*/snippets"))))
(yas-global-mode 1)


;;; eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eshell-cd-to-current-dir ()
  (interactive)
  (let ((dir default-directory))
    (eshell)
    (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt)))
(global-set-key "\C-cd" 'eshell-cd-to-current-dir)

;;; mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)

;; irb session inf-ruby, ac-inf-ruby (elpa)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; rspec-mode (elpa) -- prefix = "C-c ,"
;; v: run current spec, t: toggle spec/target
;; detail: https://github.com/pezra/rspec-mode/blob/master/rspec-mode.el
(eval-after-load 'rspec-mode '(rspec-install-snippets))

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

;; Markdown (elpa)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Golang (elpa)
(autoload 'go-mode "go-mode" "Mode for golang" t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;; Clojure (elpa)
(autoload 'clojure-mode "clojure-mode" "Major mode for clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; Yaml (elpa)
(autoload 'yaml-mode "yaml-mode" "Major mode for yaml" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; PHP (elpa)
(autoload 'php-mode "php-mode" "Major mode for php" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; SCSS (elpa)
(autoload 'scss-mode "scss-mode" "Major mode for scss")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
