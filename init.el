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
      display-time-string-forms '((format-time-string "%l:%M %p" now)))

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

(use-package transient
  ;; simple examples of transient: https://github.com/Silex/docker.el
  :ensure t
  :config
  (general-define-key
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one))

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
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "PATH"))

(use-package perspective
  :ensure t
  :init
  (setq persp-initial-frame-name "misc"
	persp-show-modestring nil)
  :config
  (persp-mode))

(defun toggle-window-fullscreen ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
      (progn (window-configuration-to-register '_)
  	     (delete-other-windows))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (evil-collection-init 'ivy))
;; TODO: take a look at how ivy and swiper work together
;; esp re: escape exiting minibuffer from evil-collection init
(use-package swiper
  :ensure t
  :config
  (ivy-mode 1))

(use-package dired-single
  :ensure t
  :config
  (general-define-key
    :states 'normal
    :keymaps 'dired-mode-map
    "o"   'dired-find-file-other-window
    "m"   'dired-mark
    "d"   'dired-flag-file-deletion
    "x"   'dired-do-flagged-delete
    "u"   'dired-unmark
    "g"   'dired-do-find-regexp
    "s"   'dired-do-find-regexp-and-replace
    "C"   'dired-do-copy
    "R"   'dired-do-rename
    "T"   'dired-do-touch
    "DEL" 'dired-unmark-backward
    "RET" 'dired-single-buffer
    "^"   'dired-single-up-directory))

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

; w3m
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
    "s" 'w3m-submit-form)
  (evil-collection-init 'w3m))

(defun google (query)
  (interactive "sgoogle: ")
  (unless (string= query "q")
    (w3m)
    (w3m-goto-url (concat "google.com/search?q=" query))))

(defun google-tab (query)
  (interactive "sgoogle: ")
  (unless (string= query "q")
    (w3m)
    (w3m-goto-url-new-session (concat "google.com/search?q=" query))))

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

;; terminal
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

; magit
(use-package evil-magit
  :ensure t
  :config
  (general-define-key
   :states 'normal
   :keymaps 'magit-diff-mode-map
   "J" 'magit-section-forward
   "K" 'magit-section-backward))
;; this is needed because q and wq are globally set in the evil config
(defun magit-ex-cmd ()
  (make-local-variable 'evil-ex-commands)
  (setq evil-ex-commands
	(mapcar (lambda (cmd) (cons (car cmd) (cdr cmd)))
		(default-value 'evil-ex-commands)))
  (evil-ex-define-cmd "q"  'with-editor-cancel)
  (evil-ex-define-cmd "wq" 'with-editor-finish))
(add-hook 'git-commit-mode-hook 'magit-ex-cmd)

;; docker
(use-package docker
  :ensure t)
(define-transient-command cg-docker-compose-up ()
  "Transient for \"docker-compose up\"."
  :man-page "docker-compose up"
  :value '("--build")
  ["Arguments"
   ("-b" "Build" "--build")
   ("-c" "Scale" "--scale " transient-read-number-N0)
   ("-d" "Detach" "-d")
   ("-f" "Force recreate" "--force-recreate")
   ("-n" "No deps" "--no-deps")
   ("-r" "Remove orphans" "--remove-orphans")
   ("-t" "Timeout" "--timeout " transient-read-number-N0)]
  ["Actions"
   ("u" "All services" docker-compose-run-action-for-all-services)
   ("U" "Up" docker-compose-run-action-for-one-service)])

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
  "/" 'swiper
  "w" 'cg-window/body
  "r" 'cg-run/body
  "o" 'evil-ex-nohighlight
  "u" 'redo
  "g" 'google
  "G" 'google-tab)

;; control
(general-define-key
  :states '(normal insert motion)
  :keymaps 'override
  "C-b" 'persp-ivy-switch-buffer
  "C-f" 'counsel-find-file
  "C-g" 'magit-status
  "C-s" 'shell-command
  "C-a" 'async-shell-command
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right)

(defun remove-git (branch-string)
   (replace-regexp-in-string " Git\\(:\\|-\\)" "" branch-string))

(setq-default mode-line-format
  (list "   "
	'(:eval (propertize (concat "[" (persp-current-name) "]") 'face 'bold))
        "   %e"
        '(:eval (abbreviate-file-name
                 (file-name-nondirectory
                  (directory-file-name default-directory))))
        "   "
	'(:eval (propertize
		 (when (bound-and-true-p linum-mode)
		       (cond ((buffer-modified-p) "[+] ")
			      (buffer-read-only "[x] ")))
		'face 'bold))
        '(:eval (propertize (buffer-name) 'face 'bold))
 	"   "
        '(:eval (when (bound-and-true-p linum-mode)
			"L-%l C-%c   "))
	'(:eval (propertize (if vc-mode
				(remove-git vc-mode)
			        "-")
			    'face 'bold))
	"  "
	mode-line-misc-info))

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
