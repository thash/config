;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

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

;; Automatically reload buffers modified by other process.
;; or use emacsclient-server
(global-auto-revert-mode 1)
(setq make-backup-files nil)
(global-linum-mode t)
(setq linum-format "%3d")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
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

;; auto save (https://raw.githubusercontent.com/kentaro/auto-save-buffers-enhanced/master/auto-save-buffers-enhanced.el)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(custom-set-variables
 '(auto-save-buffers-enhanced-interval 5.0))
(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh" "^/scp"))

;; Dired
;; http://www.emacswiki.org/emacs/DiredOmitMode
;; require: M-x load-library RET dired-x RET
(defun my-dired-load-hooks () (require 'dired-x))
(add-hook 'dired-load-hook 'my-dired-load-hooks)
(defun my-dired-mode-hooks ()
  (define-key dired-mode-map (kbd "C-l") 'dired-display-file)
  (define-key dired-mode-map (kbd "C-h") 'dired-jump)
  (define-key dired-mode-map (kbd "C-j") (kbd "j"))
  (define-key dired-mode-map (kbd "C-k") (kbd "k")))

(add-hook 'dired-mode-hook 'my-dired-mode-hooks)
(setq dired-omit-files-p t)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^#\\|^\\..+$")
;; もう片方のdired windowをデフォルトターゲットにする
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-isearch-filenames t)

;; Tramp
(setq tramp-default-method "scp")
(setenv "VM" "/scp:zaim@vagrant.zaim:")

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
;; (define-key evil-insert-state-map (kbd "C-i") 'dabbrev-expand)
(define-key evil-insert-state-map (kbd "C-u") 'delete-backward-char)
;; automatically insert comment to newline, when current line is commented.
(define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

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
;; indent(format) buffer
(define-key evil-normal-state-map (kbd "SPC =")
  (lambda () (interactive) (indent-region (point-min) (point-max))))
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
;; Command + t to open new file
(global-set-key (kbd "s-t") 'find-file)

;; tab & space
(setq-default indent-tabs-mode nil)

;; white spaces
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(defun small-split-vertically ()
  (interactive)
  (split-window-vertically)
  (enlarge-window (/ (window-height (next-window)) 2))
  (other-window 1)) ;; move to the window
(global-set-key (kbd "C-x 2") 'small-split-vertically)

(defun kill-current-buffer ()
  (interactive) (kill-buffer) (kbd "RET"))
(global-set-key (kbd "s-w") 'kill-current-buffer)

;;; looks
(global-hl-line-mode t)
(let ((ws window-system))
  (cond ((eq ws 'ns) ;; Mac OS
         (progn (load-theme 'monokai t) ;; subatomic-theme, wombat, zenburn
                ;; (load "/Users/hash/.emacs.d/themes/bubbleberry-theme.el") ;; LightTable style
                (set-face-attribute 'default nil :family "Ricty" :height 140)
                (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
                (set-frame-parameter nil 'alpha 85)
                (set-scroll-bar-mode nil)
                (set-frame-parameter nil 'fullscreen 'maximized)))
        (t (progn (load-theme 'solarized-dark t) ;; elpa 'color-theme-solarized'
                  (setq solarized-termcolors 256)))))

;; tags(built-in)
(defun my-find-tag ()
  (interactive)
  (find-tag (find-tag-default)))
(define-key evil-normal-state-map (kbd ", l") 'my-find-tag)
(define-key evil-normal-state-map (kbd ", h") 'pop-tag-mark)

;; tabbar(elpa)
(require 'tabbar)
(tabbar-mode 1)
(tabbar-mwheel-mode -1) ;; disable mouse scroll
(setq tabbar-buffer-groups-function nil) ;; disable useless grouping
(dolist (btn '(tabbar-buffer-home-button ;; disable buttons
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(setq tabbar-separator '(1.0))
;; move around tabs by Ctrl + (Shift) + <Tab>
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

(defvar my-tabbar-ignore-names
  '("*scratch*" "*helm" "*Helm" "*Messages*" "*Backtrace*" "*Help*" "*Buffer" "*Compile-Log*" "*tramp/"
    "*gosh*" "*magit-process*" "*Gofmt Errors*" "*RuboCop"))

(defun include (str list)
  (if list (if (string-prefix-p (car list) str) t
             (include str (cdr list)))
    nil))

(defun tabbar-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun tabbar-filter-buffer-list ()
  (tabbar-filter
   (lambda (x)
     (let ((name (format "%s" x)))
        (not (include name my-tabbar-ignore-names))))
   (delq nil
         (mapcar #'(lambda (b)
                     (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                 (buffer-list)))))

(setq tabbar-buffer-list-function 'tabbar-filter-buffer-list)

(set-face-attribute
 'tabbar-default nil
 :family (face-attribute 'default :family)
 :background (face-attribute 'mode-line-inactive :background)
 :height 0.9)
(set-face-attribute
 'tabbar-unselected nil
 :background (face-attribute 'mode-line-inactive :background)
 :foreground (face-attribute 'mode-line-inactive :foreground)
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background (face-attribute 'mode-line :background)
 :foreground (face-attribute 'mode-line :foreground)
 :box nil)


;; auto-complete (elpa)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
;; (setq ac-sources (add-to-list 'ac-sources 'ac-source-filename))
;; explicitly start auto-complete by TAB (added it to use ac-source-filename)
;; (define-key evil-insert-state-map (kbd "TAB") 'ac-start)
(define-key ac-completing-map (kbd "C-<SPC>") 'ac-stop)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; stop auto-complete when insert 'end' in ruby-mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))

;; robe (code navigation, lookup docs) (elpa)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup) ;; robe w/ auto-complete

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

;; git-gutter (elpa)
(require 'git-gutter)
(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:modified-sign " ")
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-"))
(set-face-foreground 'git-gutter:modified "yellow")

;; ediff (built-in)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; browse-url (built-in)
;; NOTE: use C-c C-o to open markdown link
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
(setq ag-reuse-buffers t)
(define-key evil-normal-state-map (kbd ", s") 'ag-project)

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

;; Twitter (elpa) http://www.emacswiki.org/emacs/TwitteringMode-ja
(require 'twittering-mode)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 300)
(define-key twittering-mode-map (kbd "F") 'twittering-favorite)
(define-key twittering-mode-map (kbd "R") 'twittering-native-retweet)
(setq twittering-initial-timeline-spec-string '("T_Hash/ln"))


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
  (rbenv-use-global)
  ;; rcodetools (clone from github and cp *.el under plugins dir)
  (add-to-list 'load-path "~/.emacs.d/plugins/rcodetools")
  (require 'rcodetools)) ;; M-x xmp
(add-hook 'ruby-mode-hook 'my-ruby-mode-hooks)

;; Rubocop (elpa)
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; samrt-compile (elpa)
(require 'smart-compile)
(define-key evil-normal-state-map (kbd "SPC R") 'smart-compile)
(define-key evil-normal-state-map (kbd "SPC r") (kbd "SPC R C-m"))
(setq smart-compile-option-string '("\\.rb\\'" . "ruby %f"))
(setq smart-compile-alist
      (append smart-compile-alist '(("\\.ml\\'" . "ocaml %f")
                                    ("\\.go\\'" . "go run %f"))))

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
(defun my-go-mode-hooks ()
  (setenv "GOPATH" (concat (getenv "HOME") "/.go"))
  (add-hook 'before-save-hook 'gofmt-before-save))
  ;; NOT WORK (require 'go-autocomplete)) ;; installed in plugins dir
(add-hook 'go-mode-hook 'my-go-mode-hooks)
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
(defun my-scss-mode-hooks ()
  (setq css-indent-offset 2))
(add-hook 'scss-mode-hook 'my-scss-mode-hooks)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))
;; rainbow-mode (elpa)
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; org-mode (elpa: org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(defun my-org-mode-hooks ()
  (local-set-key "RET" 'newline)
  (local-set-key "\C-cl" 'org-store-link)
  (local-set-key "\C-cc" 'org-capture)
  (local-set-key "\C-ca" 'org-agenda)
  (local-set-key "\C-cb" 'org-iswitchb))
(add-hook 'org-mode-hook 'my-org-mode-hooks)

;; OCaml (tuareg-mode) (elpa)
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; (autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
;;   "Configuration of imenu for tuareg" t)
;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; Oz(Mozart2)
(defun run-oz-env ()
  (setq load-path (cons "/Applications/Mozart2.app/Contents/Resources//share/mozart/elisp" load-path))
  (load "oz.elc")
  (run-oz))
(defun my-oz-mode-hooks ()
  (define-key oz-mode-map "\C-c\C-b" 'oz-feed-buffer)
  (define-key oz-mode-map "\C-c\C-l" 'oz-feed-line)
  (define-key oz-mode-map "\C-c\C-r" 'oz-feed-region))
(add-hook 'oz-mode-hook 'my-oz-mode-hooks)

;; output file of M-x customize
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/custom.el"))
    (load (expand-file-name custom-file) t nil nil))
