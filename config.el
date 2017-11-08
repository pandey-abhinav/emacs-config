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
    (setq exec-path (split-string path-from-shell path-separator)))
  (exec-path-from-shell-initialize)
  )
(when window-system (set-exec-path-from-shell-PATH))

(set 'mac-command-modifier 'meta)
(set 'mac-option-modifier 'super)
(set 'ns-function-modifier 'hyper)
(set 'mac-control-modifier 'control)

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

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq standard-indent 4)
(setq js-indent-level 4)
(setq-default truncate-lines t)

(defun my/makefile-mode-hook()
  (setq tab-width 4))
(add-hook 'makefile-mode-hook 'my/makefile-mode-hook)

(add-to-list 'default-frame-alist '(height . 71))
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

;; (set-face-attribute 'font-lock-type-face nil :weight 'bold)
;; (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
;; (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :foreground "orange")
(set-face-attribute 'font-lock-constant-face nil :foreground "#e23860")
(set-face-attribute 'font-lock-string-face nil :foreground "sienna" :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :foreground "#bbbbbb" :slant 'oblique)
(set-face-attribute 'font-lock-doc-face nil :foreground "#aaaaaa" :slant 'italic)
(set-face-attribute 'font-lock-builtin-face nil :foreground "#00b3b3")

(defun display-normal()
  (interactive)
  (set-frame-font "Consolas-10"))

(defun display-benq()
  (interactive)
  (set-frame-font "Consolas-11"))

(display-normal)

(require 'cl-lib)

(setq projectile-switch-project-action 'neotree-projectile-action)
(projectile-global-mode)

(require 'multi-term)
(set 'multi-term-program "/bin/bash")
(set 'multi-term-buffer-name "term")
(set 'multi-term-scroll-to-bottom-on-output "others")
(global-set-key (kbd "C-x e") 'multi-term)
(global-set-key (kbd "C-x n") 'multi-term-next)
(global-set-key (kbd "C-x p") 'multi-term-prev)

(require 'neotree)
(require 'all-the-icons)
(setq neo-theme 'icons)
(setq neo-window-width 35)
(setq-default  neo-smart-open t)
(setq neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "\\.elc$"))
(global-set-key (kbd "C-c n") 'neotree-toggle)

(defun my/rjsx-mode-hook()
  ;; (setq flycheck-eslintrc "~/.eslintrc")
  ;; (flycheck-select-checker 'javascript-eslint)
  ;; (flycheck-mode)
  (tern-mode t)
  (eval-after-load 'tern
    '(progn (require 'tern-auto-complete) (tern-ac-setup))))

(autoload 'tern-mode "tern.el" nil t)
(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook 'my/rjsx-mode-hook)

;; (require 'flycheck)
;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
;; (setq js2-mode-show-parse-errors nil)
;; (setq js2-mode-show-strict-warnings nil)
;; (let ((checkers (get 'javascript-eslint 'flycheck-next-checkers)))
;;   (put 'javascript-eslint 'flycheck-next-checkers
;;        (remove '(warning . javascript-jscs) checkers)))
;; (defun setup-js2-mode ()
;;   (flycheck-select-checker 'javascript-eslint)
;;   (flycheck-mode))
;; (add-hook 'js2-mode-hook #'setup-js2-mode)

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
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
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
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
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

(autoload 'asm86-mode "packages/asm86-mode.el")
(setq auto-mode-alist
   (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode))
   auto-mode-alist))

(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(defun my/python-mode-hook ()
  (require 'elpy)
  (setq py-use-font-lock-doc-face-p t)
  (local-set-key (kbd "M-.") 'elpy-goto-definition)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
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
(push '("^\*neotree.+\*$" :regexp t) popwin:special-display-config)
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
(add-to-list 'ac-modes 'thrift-mode)
(setq ac-auto-start t)
(setq ac-ignore-case nil)
(setq ac-auto-show-menu t)
(setq ac-show-menu-immediately-on-auto-complete t)

(custom-set-variables
'(custom-safe-themes
(quote
("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(setq sml/shorten-directory t
      sml/shorten-modes t
      sml/theme 'light
      sml/vc-mode-show-backend t)
(sml/setup)
(set-face-attribute 'mode-line nil
                    :background "wheat1"
                    :box '(:line-width 2 :color "wheat1"))
(set-face-attribute 'mode-line-inactive nil
                    :background "wheat3"
                    :box '(:line-width 2 :color "wheat3"))

(global-flycheck-mode 1)

(winner-mode 1)

(global-subword-mode 1)

(global-hl-line-mode t)
(set-face-background 'hl-line "ffff99") ;; set the face-background for hl-line

(require 'org-bullets)

(defun my/org-mode-hook ()
  (org-bullets-mode 1)
  (visual-line-mode 1)

  (setq org-startup-indented 1)
  (setq org-hide-leading-stars t)
  (setq org-return-follows-link t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-todo-keywords
       '((sequence "TODO" "|" "DONE")))
  (setq org-todo-keyword-faces
       '(("TODO" . (:foreground "red" :weight bold :underline t))))
  (setq org-done-keyword-faces
       '(("DONE" . (:foreground "green" :weight bold :underline t))))
  (setq org-link-abbrev-alist
	'(("quasars"  . "file:/Users/predicate/Uber/Quasars/")))
  ;; (set-face-attribute 'org-block-begin-line nil :weight 'bold)
  ;; (set-face-attribute 'org-block nil :slant 'italic :background "linen")
  ;; (set-face-attribute 'org-block-end-line nil :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.25 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.15 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.1 :slant 'italic)
  (set-face-attribute 'org-level-5 nil :height 1.05 :slant 'italic)
  )

(add-hook 'org-mode-hook 'my/org-mode-hook)

(add-hook 'find-file-hook (lambda () (setq buffer-read-only t)))
(add-hook 'before-save-hook (lambda () (setq buffer-read-only t)))

(setq initial-major-mode 'org-mode)

;; (find-file "~/Google Drive/index.org")

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

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
