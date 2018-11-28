(require 'ace-window)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'cl-lib)
(require 'exec-path-from-shell)
(require 'eclim)
(require 'eclimd)
(require 'flycheck)
(require 'gradle-mode)
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'hive)
(require 'ledger-mode)
(require 'popwin)
(require 'projectile)
(require 'web-mode)
(require 'yaml-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(winner-mode 1)
(projectile-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(helm-mode 1)
(helm-projectile-on)
(helm-autoresize-mode 1)
(desktop-save-mode 1)

(setq user-full-name "Abhinav Pandey")
(setq user-mail-address "abhinav.predicate@gmail.com")

(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)

(setq inhibit-startup-screen t)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq delete-by-moving-to-trash t)
(setq dired-recursive-deletes t)
(setq backup-by-copying t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/saves/" t)))
(setq confirm-kill-emacs 'yes-or-no-p)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

(setq helm-split-window-inside-p t)
(setq helm-autoresize-min-height 30)
(setq helm-autoresize-max-height 30)

(setq magit-log-show-refname-after-summary t)

(setq org-startup-indented 1)
(setq org-hide-leading-stars 1)
(setq org-return-follows-link 1)
(setq org-image-actual-width nil)
(setq org-src-fontify-natively 1)
(setq org-src-tab-acts-natively 1)
(setq org-edit-src-content-indentation 4)
(setq org-src-window-setup 'current-window)
(setq org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE")))
(setq org-todo-keyword-faces '(("TODO" . (:foreground "red" :underline t))
                               ("PROG" . (:foreground "orange" :underline t))))
(setq org-done-keyword-faces '(("DONE" . (:foreground "green" :underline t))))
(setq org-link-abbrev-alist '(("quasars"  . "file:~/Uber/Quasars/")))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-files (list "~/Library/Mobile Documents/com~apple~CloudDocs/notes/todo"))

(set-face-attribute 'default t :font "DejaVu Sans Mono-13")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#00b3b3")
(set-face-attribute 'font-lock-comment-face nil :foreground "#aaaaaa" :slant 'oblique)
(set-face-attribute 'font-lock-constant-face nil :foreground "#e23860")
(set-face-attribute 'font-lock-doc-face nil :foreground "#cccccc" :slant 'italic)
(set-face-attribute 'font-lock-string-face nil :foreground "sienna" :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :foreground "orange")
(set-face-attribute 'org-block nil :slant 'italic)
(set-face-attribute 'org-block-begin-line nil :weight 'bold)
(set-face-attribute 'org-block-end-line nil :weight 'bold)
(set-face-attribute 'org-level-1 nil :weight 'bold :foreground "black")
(set-face-attribute 'org-level-2 nil :weight 'bold :foreground "black")
(set-face-attribute 'org-level-3 nil :weight 'bold :foreground "black")
(set-face-attribute 'org-level-4 nil :weight 'bold :foreground "black")
(set-face-attribute 'org-level-5 nil :weight 'bold :foreground "black")
(set-face-attribute 'helm-match nil :weight 'bold)
(set-face-attribute 'show-paren-match nil :weight 'extra-bold :foreground "grey" :background "red")

(defun do-nothing() (interactive))

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; commands with C-c
(global-set-key (kbd "C-c f") (lambda () (interactive) (message (buffer-file-name))))
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c a") 'org-agenda)

;; commands with C-x
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-n") (lambda () (interactive) (line-move 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (line-move -5)))

(global-set-key [wheel-left] 'do-nothing)
(global-set-key [wheel-right] 'do-nothing)
(global-set-key [double-wheel-left] 'do-nothing)
(global-set-key [double-wheel-right] 'do-nothing)
(global-set-key [triple-wheel-left] 'do-nothing)
(global-set-key [triple-wheel-right] 'do-nothing)

(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode)) ; yaml files
(add-to-list 'auto-mode-alist '("\.asm$" . asm86-mode)) ; asm 86 machine files
(add-to-list 'auto-mode-alist '("\.html$" . web-mode)) ; html files
(add-to-list 'auto-mode-alist '("\.css$" . web-mode)) ; css files
(add-to-list 'auto-mode-alist '("\.scss$" . web-mode)) ; scss files
(add-to-list 'auto-mode-alist '("\.js$" . rjsx-mode)) ; javascript files
(add-to-list 'auto-mode-alist '("\.ledger$" . ledger-mode)) ; ledger files
(add-to-list 'auto-mode-alist '("\.m$" . octave-mode)) ; octave or matlab files

;; (add-hook 'find-file-hook (lambda () (setq buffer-read-only t)))
;; (add-hook 'before-save-hook (lambda () (setq buffer-read-only t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)

(popwin-mode 1)
(push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
(add-hook 'helm-after-initialize-hook
          (lambda ()
            (popwin:display-buffer helm-buffer t)
            (popwin-mode -1)))
(add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

(exec-path-from-shell-copy-env "GOPATH")

(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (require 'go-eldoc))

(defun go-mode-setup ()
  (setq-local tab-width 2)
  (setq-local gofmt-command "goimports")
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (add-to-list 'exec-path "Users/abhinav.pandey/gocode/bin")
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'go-mode-setup)

(add-hook 'java-mode-hook 'eclim-mode)
(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

(with-eval-after-load 'rjsx-mode
  (require 'tern)
  (require 'tern-auto-complete))

(with-eval-after-load 'tern
  (tern-ac-setup))

(defun rjsx-mode-setup()
  (tern-mode t))

(add-hook 'rjsx-mode-hook 'rjsx-mode-setup)

(with-eval-after-load 'python-mode
  (require 'jedi)
  (require 'elpy))

(defun python-mode-setup()
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-timeout 100)
  (setq py-use-font-lock-doc-face-p t)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-django elpy-modules)
  (delete 'elpy-module-yasnippet elpy-modules)
  (add-to-list 'ac-sources 'ac-source-jedi-direct))

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'python-mode-setup)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$"
          ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)))
  (exec-path-from-shell-initialize))
(when window-system (set-exec-path-from-shell-PATH))

(with-eval-after-load 'org-mode
  (require 'org-bullets))

(defun org-mode-setup ()
  (org-bullets-mode 1)
  (org-indent-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'org-mode-setup)

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("beamer"
               "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))


(add-to-list 'org-export-latex-classes
             '("letter"
               "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
