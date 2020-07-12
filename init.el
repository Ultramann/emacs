;; TODO: w3m tabs, icicles, copy of buffer, evil-surround, evil-goggles
;; w3m filters: https://www.emacswiki.org/emacs/WThreeMFilters

;; packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(if (package-installed-p 'use-package)
  (package-refresh-contents t)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; theme
(load-theme 'boss t)

;; random
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      help-window-select t
      ring-bell-function 'ignore
      scroll-step 1
      scroll-margin 5
      display-time-default-load-average nil
      frame-inhibit-implied-resize t)
(set-frame-font "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; convenience
(defalias 'yes-or-no-p 'y-or-n-p)
(defun reload-init () (interactive) (load-file "~/.emacs.d/init.el"))
(defun print-type (var) (print (type-of var)))
(defun setup-tabs (width)
  ;; Unsure why I can't use setq-local, it throws an error, this works though
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (mapcar (lambda (tab-stop) (* width tab-stop))
		        '(1 2 3))
        indent-tabs-mode nil))

;; prog and text modes
(defun prog-text-settings ()
  ;; line numbers
  (display-line-numbers-mode)
  ;; trailing whitespace
  (setq-local show-trailing-whitespace t)
  ;; tab is goes to tab stops
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  ;; default tab setup
  (setup-tabs 4))
(add-hook 'prog-mode-hook 'prog-text-settings)
(add-hook 'text-mode-hook 'prog-text-settings)

;; full screen
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(fringe-mode 0) ;; might want to remove for git/flycheck/flake8

;; time
(setq display-time-string-forms '((format-time-string "%I:%M %p" now)))
(display-time-mode 1)

;; general
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

;; evil
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
  ;; Evil searching
  (defun evil-ex-search-symbol-forward () (evil-ex-search-word-forward nil 'symbol))
  ;; Define symbol search motions explicitly, bind to * and #. These are just copies
  ;; of evil-ex-search-word-for/backward from evil-commands.el with 'symbol explictly
  ;; passed to evil-ex-start-word-search. Default emacs word searching is bound through
  ;; leader 8 and 3 (space instead of shift) in leader section
  (evil-define-motion evil-ex-search-symbol-forward (count &optional symbol)
    "Search for the next occurrence of symbol under the cursor."
    :jump t
    :type exclusive
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (evil-ex-start-word-search nil 'forward count 'symbol))
  (evil-define-motion evil-ex-search-symbol-backward (count &optional symbol)
    "Search for the next occurrence of word under the cursor."
    :jump t
    :type exclusive
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (evil-ex-start-word-search nil 'backward count 'symbol))
  (setq-default evil-cross-lines t)
  ;; maybe there's a better way to do this:
  ;; https://github.com/emacs-evil/evil/issues/622#issuecomment-598841628
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" (lambda () (interactive)
			     (save-buffer)
			     (kill-this-buffer)))
  (general-define-key
    :states 'normal
    [remap evil-previous-line] 'evil-previous-visual-line
    [remap evil-next-line]     'evil-next-visual-line
    "C-v"                      'evil-visual-char
    "v"                        'evil-visual-block
    "*"                        'evil-ex-search-symbol-forward
    "#"                        'evil-ex-search-symbol-backward)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t)

;; parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

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
  ;; Only setup perspective on startup
  (when (not persp-mode) (persp-mode)))

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

(use-package company
  :ensure t
  :init
  (setq company-selection-wrap-around t
        company-idle-delay nil
        company-dabbrev-downcase nil
        company-backends '(company-dabbrev-code
                           company-dabbrev))
  ;; for "buffer local" company backends see:
  ;; github.com/company-mode/company-mode/issues/839
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

;; dired
(use-package dired-single
  :ensure t
  :config
  (general-define-key  ;; TODO: consider doing this with a hydra
    :states 'normal
    :keymaps 'dired-mode-map
    "c"   'dired-do-copy
    "C"   'dired-create-directory
    "R"   'dired-do-rename
    "T"   'dired-do-touch
    "d"   'dired-flag-file-deletion
    "D"   'dired-flag-files-regexp
    "x"   'dired-do-flagged-delete
    "g"   'dired-do-find-regexp
    "s"   'dired-do-find-regexp-and-replace
    "m"   'dired-mark
    "M"   'dired-mark-files-regexp
    "o"   'dired-find-file-other-window
    "r"   'revert-buffer
    "u"   'dired-unmark
    "DEL" 'dired-unmark-backward
    "RET" 'dired-single-buffer
    "^"   'dired-single-up-directory))

(use-package counsel
  :ensure t)

;; w3m
(use-package w3m
  :ensure t
  :defer t
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

;; Make these two function higher order taking
;; 'w3m-goto-url(-new-session) as an argument. Not sure if this
;; how hard this will be with the interactive parameter
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
(add-hook 'eshell-mode-hook
          (lambda ()
            ;; There's and issue with the way that eshell set's up its
            ;; mode map, have to do manually with a hook. See:
            ;; https://github.com/noctuid/general.el/issues/80
            (evil-define-key 'insert 'eshell-mode-map
              "TAB" (lambda () (interactive) (pcomplete-std-complete)))
            (evil-local-set-key 'normal (kbd "RET") (kbd "GA"))
	    (eshell/alias "vim" "find-file $1")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")
            (add-to-list 'eshell-visual-commands "htop")))

;; terminal
(general-define-key
  :states  'insert
  :keymaps 'term-raw-map
  "<backtab>" 'term-send-raw
  "C-r"       'term-send-raw
  "C-a"       'term-send-raw
  "C-e"       'term-send-raw
  "C-c"       'term-send-raw
  "C-v"       'term-paste)

(general-define-key
  :states  'normal
  :keymaps 'term-raw-map
  "k" 'scroll-down
  "j" 'scroll-up)

(defun term-mode-settings ()
  "Kill terminal w/o prompt"
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-mode-hook 'term-mode-settings)

(defun exit-term-settings ()
  "Kill terminal buffer with `exit` at prompt"
  (let* ((buff (current-buffer))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(if (string= event "finished\n")
	    (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'exit-term-settings)

;; magit
(use-package evil-magit
  :ensure t
  :defer 1
  :config
  (general-define-key
   :states 'normal
   :keymaps 'magit-diff-mode-map
   "J" 'magit-section-forward
   "K" 'magit-section-backward)
  (general-define-key
   :states 'normal
   :keymaps 'magit-status-mode-map
   "J" 'magit-section-forward
   "K" 'magit-section-backward)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
;; this is needed because q and wq are globally set in the evil config
(defun magit-ex-cmd ()
  (make-local-variable 'evil-ex-commands)
  (setq evil-ex-commands
	(mapcar (lambda (cmd) (cons (car cmd) (cdr cmd)))
		(default-value 'evil-ex-commands)))
  (evil-ex-define-cmd "q"  'with-editor-cancel)
  (evil-ex-define-cmd "wq" 'with-editor-finish))
(add-hook 'git-commit-mode-hook 'magit-ex-cmd)
;; from doom-emacs
(defun magit-update-vc ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (vc-refresh-state))))
(add-hook 'magit-post-refresh-hook #'magit-update-vc)

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
  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(general-define-key
  :states 'insert
  :keymaps 'python-mode-map
  "RET" 'newline-and-indent)

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
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
  "c" 'comment-dwim
  "b" 'browse-url-at-point ;; TODO: might be worth making a w3m hydra at this point
  "g" 'google
  "G" 'google-tab
  "8" 'evil-ex-search-word-forward
  "3" 'evil-ex-search-word-backward)

(general-define-key
  :states 'visual
  :prefix "SPC"
  "c" 'comment-dwim)

;; control
(general-define-key
  :states '(normal insert motion)
  :keymaps 'override
  "C-b" 'persp-ivy-switch-buffer
  "C-f" 'counsel-find-file
  "C-g" 'magit-status
  "C-;" 'help
  "C-u" 'evil-scroll-up
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right)

;; modeline
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
		 (if (bound-and-true-p display-line-numbers-mode)
		     (cond ((buffer-modified-p) "[+] ")
			   (buffer-read-only "[x] ")
			   (t ""))
		     "")
		 'face 'bold))
        '(:eval (propertize (buffer-name) 'face 'bold))
 	"   "
        '(:eval (when (bound-and-true-p display-line-numbers-mode)
			"-%l|%c   "))
	'(:eval (propertize (if vc-mode
				(remove-git vc-mode)
			        "-")
		 'face 'bold))
	"   "
	mode-line-misc-info))
