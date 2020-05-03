;(org-babel-load-file "~/.emacs.d/config.org")

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3)) (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(load-theme 'boss t)

(straight-use-package 'use-package)
(eval-and-compile (require 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror) 
(setq make-backup-files nil
      auto-save-default nil
      help-window-select t
      ring-bell-function 'ignore
      tab-width 4
      scroll-step 1
      scroll-margin 5
      show-paren-delay 0
      display-time-default-load-average nil
      display-time-string-forms '((propertize (format-time-string "   %l:%M %p" now)
				              'face 'bold))
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(use-package general
  :straight t)

(use-package exec-path-from-shell
  :straight t
  :config
   (exec-path-from-shell-copy-env "PATH"))

(use-package smooth-scrolling
  :straight t
  :init
   (setq smooth-scroll-margin 5)
  :config
   (smooth-scrolling-mode 1))

(use-package ein
  :straight t
  :init
   (setq ein:jupyter-server-args '("--no-browser")))

(general-define-key
  :keymaps 'ein:notebook-mode-map
   "RET" 'ein:worksheet-execute-cell-and-goto-next)

(use-package eshell
  :init
   (add-hook 'eshell-mode-hook
             (lambda ()
               (define-key eshell-mode-map (kbd "<tab>")
                 (lambda () (interactive) (pcomplete-std-complete)))
	       (eshell/alias "vim" "find-file $1")
               (add-to-list 'eshell-visual-commands "ssh")
               (add-to-list 'eshell-visual-commands "tail")
               (add-to-list 'eshell-visual-commands "top")
               (add-to-list 'eshell-visual-commands "htop"))))

(defun pwd-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (wd (if (equal pwd home) "~" pwd)))
    (cond ((equal "~" wd) "~")
	  ((equal "/" wd) "/")
	  (t (car (last (split-string wd "/")))))))

(setq eshell-prompt-function
  (lambda ()
    (concat
     (user-login-name)
     ": "
     (pwd-home (eshell/pwd))
     " $ "
   )))

(use-package w3m
  :straight t
  :init
   (setq w3m-command (locate-file "w3m" exec-path)
         browse-url-browser-function 'w3m-browse-url
         w3m-session-crash-recovery nil
         w3m-use-cookies t
         w3m-use-tab nil)
   (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

(defun w3m-mode-settings ()
  (evil-normal-state)
  (local-set-key (kbd "RET") 'w3m-view-this-url))
(add-hook 'w3m-mode-hook 'w3m-mode-settings)

(use-package evil-magit
  :straight t)

(use-package elpy
  :straight t
  :config
   (elpy-enable))

(use-package flycheck
  :straight t
  :init
   (setq flycheck-checker 'python-flake8)
  :config
   (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :straight t
  :init
   (setq company-selection-wrap-around t
         company-idle-delay nil
         company-dabbrev-downcase nil)
  :config
   (add-hook 'after-init-hook 'global-company-mode))

(general-define-key
  :keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous)

(general-define-key
  :states 'insert
   "C-n" 'company-complete
   "C-p" 'company-complete)

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun markdown-settings ()
  (make-variable-buffer-local 'tab-width)
  (setq tab-width 2))
(add-hook 'markdown-mode-hook 'markdown-settings)

(use-package helm
  :straight t
  :init
   (setq helm-split-window-inside-p t
	 helm-move-to-line-cycle-in-source t
         helm-autoresize-max-height 0
         helm-autoresize-min-height 20)
  :config
   (helm-mode 1)
   (helm-autoresize-mode 1))

(use-package hlinum
  :straight t
  :init
   (setq linum-format " %d ")
   (add-hook 'prog-mode-hook 'linum-mode)
   (add-hook 'text-mode-hook 'linum-mode)
  :config
   (hlinum-activate))

(use-package linum-relative
  :straight t)

(use-package evil
  :straight t
  :demand t
  :init
   (setq evil-normal-state-tag "Normal"
         evil-insert-state-tag "Insert"
         evil-visual-state-tag "Visual"
         evil-motion-state-tag "Motion"
         evil-emacs-state-tag  "Emacs")
  :config
   (setq-default evil-cross-lines t)
   (evil-mode 1))

(general-define-key
  :states 'normal
   "<remap> <evil-previous-line>" 'evil-previous-visual-line
   "<remap> <evil-next-line>"     'evil-next-visual-line
   "C-v"                          'evil-visual-char
   "v"                            'evil-visual-block)

(defun screen-rename (name)
  (rename-buffer (concat (elscreen-get-screen-nickname
			  (elscreen-get-current-screen))
			 "-"
			 name)))

(defun make-ipython-terminal ()
  (interactive)
  (ansi-term "/bin/bash")
  (screen-rename "ipython")
  (comint-send-string (current-buffer) "ipython\n"))

(general-define-key
  :states '(normal insert motion)
   "RET" nil
   "C-;" 'evil-ex
   "C-b" 'helm-mini
   "C-f" 'helm-find-files
   "C-d" (lambda () (interactive) (dired default-directory))
   "C-t" (lambda () (interactive) (call-interactively 'eshell)
                                  (screen-rename "term"))
   "C-r" (lambda () (interactive) (call-interactively 'rename-buffer))
   "C-p" 'make-ipython-terminal
   "C-q" 'evil-delete-buffer
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right
   "C-g" 'magit-status

   "C-+" 'evil-window-increase-height
   "C-_" 'evil-window-decrease-height
   "C->" 'evil-window-increase-width
   "C-<" 'evil-window-decrease-width)

(use-package elscreen
  :straight t
  :init
   (global-unset-key (kbd "C-s"))
   (setq elscreen-display-tab nil)
  :config
   (use-package elscreen-separate-buffer-list
     :straight t
     :config
      (elscreen-separate-buffer-list-mode))
   (elscreen-start)
   (elscreen-screen-nickname "misc"))

(defun screen-shell ()
  (interactive)
  (call-interactively 'eshell)
  (screen-rename "term")
  (split-window-horizontally))

(defun elscreen-create-rename-split ()
  (interactive)
  (elscreen-create)
  (catch 'quit
	(call-interactively 'elscreen-screen-nickname)
	(when (equal (elscreen-get-screen-nickname (elscreen-get-current-screen))
		      "q")
	      (progn (elscreen-kill)
		     (throw 'quit nil)))
	(call-interactively 'cd)
	(screen-shell)))

(defun w3m-buffer-goto-url ()
  (interactive)
  (w3m)
  (call-interactively 'w3m-goto-url))

(general-define-key
  :states '(normal insert motion)
  :prefix "C-x"
   "f" 'elscreen-find-and-goto-by-buffer
   "n" 'elscreen-create-rename-split
   "r" 'elscreen-screen-nickname
   "l" 'elscreen-select-and-goto
   "b" 'w3m-buffer-goto-url
   ";" 'elscreen-toggle
   "q" 'elscreen-kill
   "d" 'cd)

(defun toggle-relative-line-numbers ()
  (interactive)
  (if (eq linum-format 'linum-relative)
    (setq linum-format " %d ")
    (linum-relative-on)))

(defun toggle-spell-check ()
  (interactive)
  (command-execute (if (bound-and-true-p prog-mode)
                     'flyspell-prog-mode
                     'flyspell-mode)))

(general-define-key
  :states 'normal
  :prefix "SPC"
   "ss" 'ispell-word
   "sb" 'ispell-buffer
   "sc" 'toggle-spell-check
   "ea" (lambda () (interactive) (ein:jupyter-server-start :no-popup))
   "et" 'ein:jupyter-server-stop
   "en" 'ein:notebooklist-new-notebook
   "eo" 'ein:notebooklist-open-notebook
   "el" 'ein:notebooklist-open
   "i"  'clone-indirect-buffer-other-window
   "n"  'toggle-relative-line-numbers
   "f"  'find-file-at-point
   "c"  'check-parens
   "u"  'redo)

(defun my-buffer-kill ()
  (if (bound-and-true-p ein:notebook-multilang-mode)
    (ein:notebook-kill-kernel-then-close-command)
    (kill-this-buffer)))

(defun save-kill-buffer ()
  (interactive)
  (call-interactively 'evil-write)
  (my-buffer-kill))

(evil-ex-define-cmd "q"  'kill-this-buffer)          ; kill buffer, leave window
(evil-ex-define-cmd "wq" 'save-kill-buffer)          ; save-kill buffer, leave window

(define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
(global-set-key (kbd "C-x s") (kbd "C-x 3 C-x o"))   ; split window vertically
(global-set-key (kbd "C-x v") (kbd "C-x 2 C-x o"))   ; split window horizontally
(global-set-key (kbd "C-x x") 'delete-window)        ; delete window not buffer

;; Terminal Settings
(global-set-key (kbd "C-x C-l") (kbd "C-z C-l C-z")) ; clear screen
(global-set-key (kbd "C-x C-a") (kbd "C-z C-a C-z")) ; beginning of line
(global-set-key (kbd "C-x C-e") (kbd "C-z C-e C-z")) ; end of line
(global-set-key (kbd "C-x C-k") (kbd "C-z C-k C-z")) ; delete to end of line
(global-set-key (kbd "C-x C-u") (kbd "C-z C-u C-z")) ; delete to beginning of line

(general-define-key
  :states  'insert
  :keymaps 'term-raw-map
   "<backtab>" 'term-send-raw
   "C-r"       'term-send-raw
   "C-a"       'term-send-raw
   "C-e"       'term-send-raw
   "C-c"       'term-send-raw
   "s-v"       'term-paste)

(defun term-mode-settings ()
  "Kill terminal w/o prompt"
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-mode-hook 'term-mode-settings)

(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

(defun eshell-mode-bindings ()
  (evil-local-set-key 'normal (kbd "RET") (kbd "GA"))
  (evil-local-set-key 'insert (kbd "C-r") 'eshell-previous-matching-input))
(add-hook 'eshell-mode-hook 'eshell-mode-bindings)

;; General Setup
(kill-buffer "*scratch*")
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(electric-indent-mode 1)
(display-time-mode 1)
(global-visual-line-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default mode-line-format
  (list "    "
	'(:eval (propertize
                  (concat "[" (elscreen-get-screen-nickname
                                   (elscreen-get-current-screen)) "]")
                  'face 'bold))
        "    %e"
        '(:eval (abbreviate-file-name
                  (file-name-nondirectory
                    (directory-file-name default-directory))))
        "    "
	'(:eval (when (bound-and-true-p linum-mode)
		      (cond ((buffer-modified-p) "[+] ")
			    (buffer-read-only "[x] "))))
        '(:eval mode-line-buffer-identification 'face 'bold)
	"    "
        '(:eval (if (bound-and-true-p linum-mode)
		    "Line: %l Col: %c"
		    "-"))
	"    "
        '(:eval (propertize evil-mode-line-tag 'face 'bold))
	"   "
	'(:eval (if vc-mode vc-mode " -"))
        mode-line-misc-info))

(screen-shell)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
