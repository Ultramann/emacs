(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(load-theme 'boss t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror) 

(setq inhibit-splash-screen t inhibit-startup-message t inhibit-startup-echo-area-message t
      make-backup-files nil auto-save-default nil help-window-select t
      ring-bell-function 'ignore
      tab-width 4
      tab-stop-list '(0 4 8)
      scroll-step 1 scroll-margin 5 show-paren-delay 0
      display-time-default-load-average nil
      frame-inhibit-implied-resize t
      display-time-string-forms '((propertize (format-time-string "   %l:%M %p" now)
                                              'face 'bold)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(display-time-mode 1)
(global-visual-line-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defun reload-init () (interactive) (load-file "~/.emacs.d/init.el"))
(use-package general
  :ensure t
  :config
  (general-evil-setup t))

(use-package hydra
  :ensure t
  :config
  (load "~/.emacs.d/hydras.el"))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-normal-state-tag "Normal"
        evil-insert-state-tag "Insert"
        evil-visual-state-tag "Visual"
        evil-motion-state-tag "Motion"
        evil-emacs-state-tag  "Emacs")
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq-default evil-cross-lines t)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" '(lambda () (interactive)
			      (save-buffer)
			      (kill-this-buffer)))
  (general-define-key
    :states 'normal
    [remap evil-previous-line] 'evil-previous-visual-line
    [remap evil-next-line]     'evil-next-visual-line
    "C-v"                      'evil-visual-char
    "v"                        'evil-visual-block)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "u" 'redo)
  (general-define-key
   :states 'insert
   "TAB" 'tab-to-tab-stop)
  (evil-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "PATH"))

(use-package elscreen
  :ensure t
  :init
  (global-unset-key (kbd "C-s"))
  (setq elscreen-display-tab nil)
  :config
  (use-package elscreen-separate-buffer-list
    :ensure t
    :config
    (elscreen-separate-buffer-list-mode))
  (elscreen-start)
  (elscreen-screen-nickname "misc"))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :config
  (general-define-key
   :states 'normal
   "/" 'swiper
   ))

(use-package ein
  :ensure t
  :init
   (setq ein:jupyter-server-args '("--no-browser")))
(general-define-key
  :keymaps 'ein:notebook-mode-map
   "RET" 'ein:worksheet-execute-cell-and-goto-next)
(general-define-key
  :states 'normal
  :prefix "SPC"
   "ea" 'ein:run
   "et" 'ein:jupyter-server-stop
   "ed" 'ein:dev-start-debug
   "en" 'ein:notebooklist-new-notebook
   "eo" 'ein:notebooklist-open-notebook
   "el" 'ein:log-pop-to-all-buffer)

(use-package hlinum
  :ensure t
  :init
  (setq linum-format " %d ")
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  :config
  (hlinum-activate))

(use-package counsel
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(general-define-key
  :states 'normal
  :keymaps 'override  ; For Dired
  :prefix "SPC"
  "w"  'cg-window/body
  "o"  'evil-ex-nohighlight
  "rd" 'rainbow-delimiters-mode)

(general-define-key
  :states '(normal insert motion)
  "C-b"   'ivy-switch-buffer
  "C-S-b" 'buffer-menu
  "C-f"   'counsel-find-file
  "C-h"   'evil-window-left
  "C-j"   'evil-window-down
  "C-k"   'evil-window-up
  "C-l"   'evil-window-right)

(use-package dired-single
  :ensure t
  :config
  (general-define-key
   :states 'normal 
   :keymaps 'dired-mode-map
   "RET" 'dired-single-buffer
   "^" 'dired-single-up-directory))

(defun elscreen-create-rename ()
  (interactive)
  (elscreen-create)
  (catch 'quit
	(call-interactively 'elscreen-screen-nickname)
	(when (equal (elscreen-get-screen-nickname (elscreen-get-current-screen))
		      "q")
	      (progn (elscreen-kill)
		     (throw 'quit nil)))))

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
		"   "
		'(:eval (if vc-mode vc-mode " -"))
		mode-line-misc-info))

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
