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

;; add /usr/local/bin to Emacs PATH (!= (getenv PATH))
(let ((mydir "/usr/local/bin"))
  (setenv "PATH" (concat mydir ":" (getenv "PATH")))
  (setq exec-path (append `(,mydir) exec-path)))

;; rbenv
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") exec-path))

;;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(global-linum-mode t)
(setq linum-format "%3d")
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq display-time-24hr-format t)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; off visual bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; http://www.emacswiki.org/emacs/DiredOmitMode
;; require: M-x load-library RET dired-x RET
(defun my-dired-load-hooks () (require 'dired-x))
(add-hook 'dired-load-hook 'my-dired-load-hooks)
(setq dired-omit-files-p t)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^#\\|^\\..+$")

;;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC q") 'delete-window)
(define-key evil-normal-state-map (kbd "SPC h") 'windmove-left)
(define-key evil-normal-state-map (kbd "SPC j") 'windmove-down)
(define-key evil-normal-state-map (kbd "SPC k") 'windmove-up)
(define-key evil-normal-state-map (kbd "SPC l") 'windmove-right)
(evil-mode 1)

;; evil-surround (plugins) -- https://github.com/timcharper/evil-surround
(require 'surround)
(global-surround-mode 1)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-cl" 'toggle-truncate-lines)
(global-set-key (kbd "C-x F") 'find-file-other-window)
(global-set-key (kbd "C-x B") 'switch-to-buffer-other-window)

;; Command + o instead of C-x o
(global-set-key (kbd "s-o") 'other-window)
;; Prevent unintended quit
(global-set-key (kbd "s-q") nil)

;; tab & space
(setq-default indent-tabs-mode nil)

;; white spaces
(setq-default show-trailing-whitespace t)

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t)

;; recognize _ as a word. (It seems don't work on evil)
(modify-syntax-entry ?_ "w")

;; C-Space -> select -> then, press C-RET
(setq cua-rectangle-mark-key (kbd "C-RET")) ;; should come before enabling cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; C-x,c,v for cut, copy, paste

;; incremental search buffer items by C-x b
(iswitchb-mode t)

(defun window-toggle-division ()
  "Toggle :vs <-> :sp"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "Single Window"))
  (let ((before-height)
        (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer other-buf)
    (other-window -1)))

;; Twittering-mode
(defun activate-twittering-mode ()
  (require 'twittering-mode)
  (setq twittering-use-master-password t))

;;; looks
(global-hl-line-mode t)
(let ((ws window-system))
  (cond ((eq ws 'ns) ;; Mac OS
         (progn ;; (load-theme 'wombat t) ;; elpa 'subatomic-theme'
                (load "/Users/hash/.emacs.d/themes/bubbleberry-theme.el")
                (set-face-attribute 'default nil :family "Ricty" :height 120)
                (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
                (set-frame-parameter nil 'alpha 90)
                (set-scroll-bar-mode nil)
                (activate-twittering-mode)))
        (t (progn (load-theme 'solarized-dark t) ;; elpa 'color-theme-solarized'
                  (setq solarized-termcolors 256)))))

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

;; magit (elpa)
(require 'magit)
(define-key evil-normal-state-map (kbd "C-x g") 'magit-status)
(define-key magit-status-mode-map (kbd "j") 'next-line)
(define-key magit-status-mode-map (kbd "k") 'previous-line)
(define-key magit-status-mode-map (kbd "l") 'magit-show-section)
(define-key magit-status-mode-map (kbd "h") 'magit-hide-section)
(setq auto-mode-alist (remove (rassoc 'git-rebase-mode auto-mode-alist) auto-mode-alist))


;; browse-url (built-in)
(global-set-key "\C-cu" 'browse-url-at-point)

;; rainbow-delimiters (elpa)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)

;; cursor-in-brackets (github)
;; https://raw.github.com/yascentur/cursor-in-brackets-el/master/cursor-in-brackets.el
(require 'cursor-in-brackets)
(global-cursor-in-brackets-mode t)

;; yasnippet (elpa)
(require 'yasnippet)
(setq yas-snippet-dirs `("~/.emacs.d/snippets"
                         ,(car (file-expand-wildcards "~/.emacs.d/elpa/yasnippet-*/snippets"))))
(yas-global-mode 1)

;; ag.el (the_silver_searcher) (elpa)
(setq ag-highlight-search t)
(define-key evil-normal-state-map (kbd ", s") 'ag)

;; helm, (helm-ag) (elpa)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x) ;; extended M-x
(define-key evil-normal-state-map (kbd "C-x b") 'helm-for-files) ;; instead of just opening from buffer list
(define-key evil-normal-state-map (kbd ", j") 'helm-ls-git-ls)

;; popwin (elpa)
(setq pop-up-windows nil)
(require 'popwin nil t)
(when (require 'popwin nil t)
  (setq anything-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("*helm for files*" :height 0.3) popwin:special-display-config)
  (push '("*helm lsgit*" :position left :width 0.2) popwin:special-display-config)
  (push '("*helm M-x*" :height 0.2) popwin:special-display-config)
  (push '("*Completions*" :height 0.4) popwin:special-display-config)
  (push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config))

;; RCIRC: load rcirc setting files (git ignored)
(if (file-exists-p (expand-file-name "~/.emacs.d/ircconf.el"))
    (load (expand-file-name "~/.emacs.d/ircconf.el") t nil nil))


;;; mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-all-to-list (list-var mode patterns)
  (dolist (pattern patterns)
    (add-to-list list-var `(,pattern . ,mode))))

(defun my-buffer-menu-mode-hooks ()
  (local-set-key "k" 'previous-line))
(add-hook 'Buffer-menu-mode-hook 'my-buffer-menu-mode-hooks)

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-all-to-list 'auto-mode-alist 'ruby-mode
                 '("\\.rb$" "\\.rake$"
                   "Rakefile$" "Gemfile$" "Capfile$" "Guardfile$" "Vagrantfile$"))
(setq ruby-insert-encoding-magic-comment nil)

(defun my-ruby-mode-hooks ()
  (require 'rbenv) ;; rbenv.el (elpa)
  (rbenv-use "2.0.0-p353")
  ;; rcodetools (clone from github and cp *.el under plugins dir)
  (add-to-list 'load-path "~/.emacs.d/plugins/rcodetools")
  (require 'rcodetools)) ;; M-x xmp
(add-hook 'ruby-mode-hook 'my-ruby-mode-hooks)

;; irb session inf-ruby, ac-inf-ruby (elpa)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; rspec-mode (elpa) -- prefix = "C-c ,"
;; v: run current spec, t: toggle spec/target
;; detail: https://github.com/pezra/rspec-mode/blob/master/rspec-mode.el
(eval-after-load 'rspec-mode '(rspec-install-snippets))

;; Gauche TODO: activate only on scheme mode.
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
;; C-c C-c -> {p/preview, m/showbuffer, e/export, o/open with Marked(install required)}
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(setq markdown-open-command "/Applications/Marked.app/Contents/Resources/mark")

;; Golang (elpa)
(autoload 'go-mode "go-mode" "Mode for golang" t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;; Clojure (elpa)
(autoload 'clojure-mode "clojure-mode" "Major mode for clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; cider (formely nrepl.el) -> M-x cider

;; Yaml (elpa)
(autoload 'yaml-mode "yaml-mode" "Major mode for yaml" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; PHP (elpa)
(autoload 'php-mode "php-mode" "Major mode for php" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; CoffeeScript
(defun my-coffee-mode-hooks ()
  (setq coffee-tab-width 2))
(add-hook 'coffee-mode-hook 'my-coffee-mode-hooks)

;; SCSS (elpa)
(autoload 'scss-mode "scss-mode" "Major mode for scss")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
;; rainbow-mode (elpa)
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; org-mode (elpa: org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(defun my-org-mode-hooks ()
  (local-set-key "\C-cl" 'org-store-link)
  (local-set-key "\C-cc" 'org-capture)
  (local-set-key "\C-ca" 'org-agenda)
  (local-set-key "\C-cb" 'org-iswitchb))
(add-hook 'org-mode-hook 'my-org-mode-hooks)

;; output file of M-x customize
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/custom.el"))
    (load (expand-file-name custom-file) t nil nil))
