;; TODO: magit, w3m tabs, copy of buffer, evil-surround, evil-goggles
;; w3m filters: https://www.emacswiki.org/emacs/WThreeMFilters
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
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
(defun print-type (var) (print (type-of var)))

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
        evil-emacs-state-tag  "Emacs"
	evil-want-keybinding nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq-default evil-cross-lines t)
  ;; maybe there's a better way to do this:
  ;; https://github.com/emacs-evil/evil/issues/622#issuecomment-598841628
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
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t)

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
    "/" 'swiper))

;; w3m
(use-package w3m
  :ensure t
  :after evil-collection
  :init
  (setq w3m-command (locate-file "w3m" exec-path)
        browse-url-browser-function 'w3m-browse-url
        w3m-session-crash-recovery nil
        w3m-use-cookies t
        w3m-use-tab nil)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'w3m-mode-map
   "C-s" 'w3m-submit-form)
  (evil-collection-init 'w3m))

(defun google (query)
  (interactive "sgoogle: ")
  (catch 'quit
	(when (string= query "q")
	      (throw 'quit nil)))
  (w3m)
  (w3m-goto-url (concat "google.com/search?q=" query)))

;; eshell
(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
	      ;; local-set-key? update with general? But it complains about
	      ;; the mode map being nil...
              (define-key eshell-mode-map (kbd "TAB")
                (lambda () (interactive) (pcomplete-std-complete)))
	      (eshell/alias "vim" "find-file $1")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "top")
              (add-to-list 'eshell-visual-commands "htop"))))

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

;; magit
(use-package evil-magit
  :ensure t)
;; this is needed because q and wq are globally set in the evil config
(defun magit-ex-cmd ()
  (make-local-variable 'evil-ex-commands)
  (setq evil-ex-commands
	(mapcar (lambda (cmd) (cons (car cmd) (cdr cmd)))
		(default-value 'evil-ex-commands)))
  (evil-ex-define-cmd "q"  'with-editor-cancel)
  (evil-ex-define-cmd "wq" '(lambda () (interactive)
			      (save-buffer)
			      (with-editor-finish))))
(add-hook 'git-commit-mode-hook 'magit-ex-cmd)

(use-package rainbow-delimiters
  :ensure t)

;; python
(add-hook 'python-mode-hook
  (lambda () (setq-local tab-width 4)
	     (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(general-define-key
  :states 'insert
  :keymaps 'python-mode-map
  "RET" 'newline-and-indent)

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; leader
(general-define-key
  :states 'normal
  :keymaps 'override  ; For Dired
  :prefix "SPC"
  "w"  'cg-window/body
  "o"  'evil-ex-nohighlight
  "g"  'google
  "s"  'eshell
  "rd" 'rainbow-delimiters-mode)

;; control
(general-define-key
  :states '(normal insert motion)
  "C-b"   'ivy-switch-buffer
  "C-S-b" 'buffer-menu
  "C-f"   'counsel-find-file
  "C-g"   'magit-status
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
