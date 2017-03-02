(setq user-full-name "Abhinav Pandey"
      user-mail-address "abhinav.predicate@gmail.com")

(require 'package)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
       (setenv "PATH" path-from-shell)
       (setq eshell-path-env path-from-shell)
       (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(set 'mac-command-modifier 'meta)
(set     'mac-option-modifier 'super)
(set     'ns-function-modifier 'hyper)
(set     'mac-control-modifier 'control)

(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "M-n") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (forward-line -5)))
(global-set-key (kbd "C-q") (lambda () (interactive) (toggle-read-only)))
(global-set-key (kbd "M-d") (lambda () (interactive) (delete-backward-char 1)))

(defun do-nothing()
  (interactive))

(global-set-key [wheel-left] 'do-nothing)
(global-set-key [wheel-right] 'do-nothing)
(global-set-key [double-wheel-left] 'do-nothing)
(global-set-key [double-wheel-right] 'do-nothing)
(global-set-key [triple-wheel-left] 'do-nothing)
(global-set-key [triple-wheel-right] 'do-nothing)

(setq scroll-step 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(setq tab-width 2)
(setq c-basic-offset 2)
(setq standard-indent 2)
(setq js-indent-level 2)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist '(height . 77))
(add-to-list 'default-frame-alist '(width . 236))

(line-number-mode t)
(blink-cursor-mode 0)
(column-number-mode t)
(transient-mark-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)

(require 'paren)

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

(set-face-attribute 'show-paren-match nil
                    :weight 'extra-bold :foreground "grey" :background "red")

(set-face-attribute 'font-lock-type-face nil :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-string-face nil :foreground "#ff8c1a" :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :foreground "#aaaaaa" :slant 'italic)

(defun increase-display-font()
  (interactive)
  (set-frame-font "DejaVu Sans Mono-11"))

(defun decrease-display-font()
  (interactive)
  (set-frame-font "DejaVu Sans Mono-10"))

(decrease-display-font)

(require 'cl-lib)

(setq projectile-switch-project-action 'neotree-projectile-action)
(projectile-global-mode)

(require 'multi-term)
(set 'multi-term-program "/bin/bash")
(set 'multi-term-buffer-name "term")
(global-set-key (kbd "C-x e") 'multi-term)
(global-set-key (kbd "C-x n") 'multi-term-next)
(global-set-key (kbd "C-x p") 'multi-term-prev)

(require 'neotree)
(require 'all-the-icons)

(setq neo-theme 'icons)
(setq neo-window-width 35)
(setq-default  neo-smart-open t)
(setq neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "\\.elc$"))
(global-set-key [f8] 'neotree-toggle)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'flycheck-mode)

(require 'web-mode)

(defun my/web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (local-set-key (kbd "RET") 'newline-and-indent))

(define-derived-mode web-html-mode web-mode "WebHTML"
  (web-mode)
  (setq web-mode-content-type "html"))

(define-derived-mode web-css-mode web-mode "WebCss"
  (web-mode)
  (setq web-mode-content-type "css"))


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(add-hook 'web-mode-hook  'my/web-mode-hook)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(require 'go-eldoc)
(require 'go-autocomplete)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun my/go-mode-hook ()
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq gofmt-command "goimports")
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (local-set-key (kbd "C-.") 'godef-jump)
  (local-set-key (kbd "C-,") 'pop-tag-mark)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'my/go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(setenv "GOPATH" "/Users/predicate/gocode")
(add-to-list 'exec-path "/Users/predicate/gocode/bin")

(require 'yaml-mode)

(defun my/yaml-mode-hook ()
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'my/yaml-mode-hook)

(require 'thrift-mode)

(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(defun my/python-mode-hook ()
  (require 'elpy)
  (local-set-key (kbd "C-.") 'elpy-goto-definition)
  (local-set-key (kbd "C-,") 'pop-tag-mark)
  (elpy-use-ipython)
  (setq elpy-rpc-timeout 10)
  (setq elpy-modules
        (elpy-module-company elpy-module-eldoc elpy-module-flymake
                             elpy-module-pyvenv elpy-module-yasnippet
                             elpy-module-django elpy-module-sane-defaults))
  (setq elpy-rpc-backend "jedi")
  (elpy-enable))
(add-hook 'python-mode-hook 'my/python-mode-hook)

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(setq helm-split-window-in-side-p t)
(setq helm-autoresize-min-height 20)
(setq helm-autoresize-max-height 20)
(set-face-attribute 'helm-match nil :weight 'bold)

(helm-mode 1)
(helm-projectile-on)
(helm-autoresize-mode)

(require 'popwin)
(popwin-mode 1)

(push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)

(add-hook 'helm-after-initialize-hook (lambda ()
                                        (popwin:display-buffer helm-buffer t)
                                        (popwin-mode -1)))

(add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))

(require 'nlinum)
(setq nlinum-format " %d ")
(setq nlinum-highlight-current-line t)
(set-face-attribute 'nlinum-current-line nil :foreground "red" :weight 'bold)

(global-set-key (kbd "C-x o") 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'auto-complete-config)

(ac-config-default)
(global-auto-complete-mode t)

(setq ac-auto-start t)
(setq ac-ignore-case nil)
(setq ac-auto-show-menu t)
(setq ac-show-menu-immediately-on-auto-complete t)

(custom-set-variables
'(custom-safe-themes
(quote
("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))

(setq sml/theme 'light)
(sml/setup)

(global-flycheck-mode 1)

(winner-mode 1)

(global-subword-mode 1)

(global-hl-line-mode t)
(set-face-background 'hl-line "ffff99") ;; set the face-background for hl-line

(require 'org-bullets)

(defun my/org-mode-hook ()
  (org-bullets-mode 1)
  (visual-line-mode 1)
  (set-face-attribute 'org-level-1 nil :foreground "black" :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "black" :weight 'bold)
  (set-face-attribute 'org-level-3 nil :foreground "black" :weight 'bold :slant 'italic)
  (set-face-attribute 'org-level-4 nil :foreground "black" :slant 'italic)
  (set-face-attribute 'org-level-5 nil :foreground "black" :weight 'light :slant 'italic))

(set 'org-todo-keywords
     '((sequence "TODO" "WAITING" "REVIEW" "|" "DONE" "DELEGATED")))

(set 'org-todo-keyword-faces
     '(("TODO" . (:foreground "red" :weight bold :underline t))
       ("WAITING" . (:foreground "orange" :weight bold :slant italic :underline t))
       ("REVIEW" . (:foreground "orange" :weight bold :slant italic :underline t))))

(set 'org-done-keyword-faces
     '(("DONE" . (:foreground "green" :weight bold :underline t))
       ("DELEGATED" . (:foreground "green" :weight bold :underline t))))

(set 'org-startup-indented 1)
(set 'org-hide-leading-stars t)
(set 'org-src-fontify-natively t)
(set 'org-src-window-setup 'current-window)
(add-hook 'org-mode-hook 'my/org-mode-hook)

(add-hook 'find-file-hook (lambda () (setq buffer-read-only t)))

(defun trailing-whitespace()
  (set 'show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'nlinum-mode)
(add-hook 'prog-mode-hook 'trailing-whitespace)

(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/" t)))

(defun ask-before-closing()
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs?"))
      (save-buffers-kill-emacs)
    (message "Canceled exit")))

(global-set-key (kbd "C-z") 'ask-before-closing)
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(setq sentence-end-double-space nil)

(setq ring-bell-function 'ignore)
