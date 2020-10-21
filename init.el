;;; init.el --- Cary's emacs init file  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: w3m tabs, icicles, evil-surround, evil-goggles
;; hydras examples for buffer navigation
;; w3m filters: https://www.emacswiki.org/emacs/WThreeMFilters
;; (winner mode 1), then (winner-undo to restore window config
;; pyvenv.el:174/310 have super cool methods of interactively passing multiple
;;                   parameters to a function, could definitely use this

;;; Code:

(defvar first-load t)  ;; set to nil at end of init if t

;; packages
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
      frame-inhibit-implied-resize t)
(set-frame-font "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; convenience
(defalias 'yes-or-no-p 'y-or-n-p)

(defun reload-init ()
  "Reload init file."
  (interactive) (load-file "~/.emacs.d/init.el"))

(defun print-type (var)
  "Prints type of passed variable, VAR."
  (print (type-of var)))

;; full screen
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; time
(defvar display-time-default-load-average nil)
(defvar display-time-string-forms '((format-time-string "%I:%M %p" now)))
(display-time-mode 1)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH")) ;; "PYENV_ROOT"))
  (exec-path-from-shell-initialize))

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
    "<escape>" #'transient-quit-one))

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
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" (lambda () (interactive)
			     (save-buffer)
			     (kill-this-buffer)))
  (general-define-key
    :states 'normal
    [remap evil-previous-line] #'evil-previous-visual-line
    [remap evil-next-line]     #'evil-next-visual-line
    "C-v"                      #'evil-visual-char
    "v"                        #'evil-visual-block
    "gb"                       #'pop-tag-mark
    "'"                        #'evil-jump-backward-swap
    "*"                        #'evil-ex-search-symbol-forward
    "#"                        #'evil-ex-search-symbol-backward)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t)

;; parenthesis
(defvar show-paren-delay 0)
(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t)

;; tabs
(defun set-up-tab-width (width)
  "Set tab stop list to multiples of WIDTH, and evil shift width to WIDTH."
  (setq-local tab-stop-list (mapcar (lambda (tab-stop) (* width tab-stop))
                                    '(1 2 3)))
  (setq-local evil-shift-width width))

;; prog and text modes
(defun prog-text-settings ()
  "Set up defaults for programming and text modes."
  ;; line numbers
  (display-line-numbers-mode)
  ;; trailing whitespace
  (setq-local show-trailing-whitespace t)
  ;; Don't use tabs, just spaces
  (setq-local indent-tabs-mode nil)
  ;; tab is goes to tab stops
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  ;; default tab setup
  (set-up-tab-width 4))
(add-hook 'prog-mode-hook #'prog-text-settings)
(add-hook 'text-mode-hook #'prog-text-settings)

;; fringes
(fringe-mode 0)

(defun enable-window-fringes ()
  "Enable fringes in window."
  (setq left-fringe-width 8))

(use-package perspective
  :ensure t
  :defines persp-make-variable-persp-local  ;; for pyvenv
  :init
  (setq persp-initial-frame-name "misc"
	persp-show-modestring nil)
  :config
  ;; Only set up perspective on startup
  (when first-load (persp-mode)))
  ;; This advice didn't do what I wanted, but these are good examples
  ;; (advice-add #'persp-activate :around (lambda (orig_fun &rest r)
  ;;                                        (print process-environment)
  ;;                                        (apply orig_fun r)
  ;;                                        (print process-environment)))
  ;; Keeping to see the formatting and _r to appease the linter
  ;; (advice-add #'persp-new :after (lambda (&rest _r) (cd "~/developer")))

(use-package ivy
  :ensure t
  :init
  (defvar ivy-case-fold-search-default nil)
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
  :defines company-dabbrev-downcase
  :init
  (setq company-selection-wrap-around t
        company-idle-delay nil
        company-dabbrev-downcase nil
        company-backends '(company-dabbrev-code
                           company-dabbrev))
  ;; for "buffer local" company backends see:
  ;; github.com/company-mode/company-mode/issues/839
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(general-define-key
  :keymaps 'company-active-map
  "C-n" #'company-select-next
  "C-p" #'company-select-previous)

(general-define-key
  :states 'insert
  "C-n" #'company-complete
  "C-p" #'company-complete)

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (setq-default
   flycheck-disabled-checkers '(python-pylint python-mypy))
  (global-flycheck-mode))

(use-package counsel
  :ensure t)

;; w3m
(use-package w3m
  :ensure t
  :defer t
  :after evil-collection
  :defines w3m-session-crash-recovery
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
    "s" #'w3m-submit-form
    "c" (lambda () (interactive)
                (async-shell-command
                 (concat "wget -P ~ " (w3m-anchor)))))
  (evil-collection-init 'w3m))

;; Make these two function higher order taking
;; 'w3m-goto-url(-new-session) as an argument. Not sure if this
;; how hard this will be with the interactive parameter
(defun google (query)
  "Search for QUERY with google in w3m buffer."
  (interactive "sgoogle: ")
  (unless (string= query "q")
    (w3m)
    (w3m-goto-url (concat "google.com/search?q=" query))))

(defun google-tab (query)
  "Search for QUERY with google in new w3m buffer."
  (interactive "sgoogle: ")
  (unless (string= query "q")
    (w3m)
    (w3m-goto-url-new-session (concat "google.com/search?q=" query))))

;; eshell
(use-package eshell
  :defines eshell-prompt-function)

(defun eshell-mode-settings ()
  "Eshell mode settings."
  ;; There's and issue with the way that eshell set's up its
  ;; mode map, have to do manually with a hook. See:
  ;; https://github.com/noctuid/general.el/issues/80
  (evil-define-key 'insert 'eshell-mode-map
      (kbd "TAB") (lambda () (interactive) (pcomplete-std-complete)))
  (evil-local-set-key 'normal (kbd "RET") (kbd "GA"))
  (defvar eshell-visual-commands)
  (add-to-list 'eshell-visual-commands "tail")
  (add-to-list 'eshell-visual-commands "top")
  (add-to-list 'eshell-visual-commands "htop")
  (eshell/alias "vim" "find-file $1"))
(add-hook 'eshell-mode-hook #'eshell-mode-settings)

(defun pwd-home (cwd)
  "Show home directory as ~ in eshell current working directory, CWD."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (wd (if (equal cwd home) "~" cwd)))
    (cond ((equal "~" wd) "~")
	  ((equal "/" wd) "/")
	  (t (car (last (split-string wd "/")))))))

(setq eshell-prompt-function
  (lambda ()
    (concat
     (user-login-name)
     ": "
     (pwd-home (eshell/pwd))
     " $ ")))

;; terminal
(general-define-key
  :states  'insert
  :keymaps 'term-raw-map
  "<S-tab>" (lambda () (interactive) (term-send-raw-string "\033[Z"))
  "C-r"     #'term-send-raw
  "C-a"     #'term-send-raw
  "C-e"     #'term-send-raw
  "C-c"     #'term-send-raw
  "C-v"     #'term-paste)

(defun term-mode-settings ()
  "Kill terminal w/o prompt."
  (auto-fill-mode -1)  ;; required to make EOL stuff work at prompt input
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
(add-hook 'term-mode-hook #'term-mode-settings)

(defun exit-term-settings ()
  "Kill terminal buffer with `exit` at prompt."
  (let* ((buff (current-buffer))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(if (string= event "finished\n")
	    (kill-buffer ,buff))))))
(add-hook 'term-exec-hook #'exit-term-settings)

(defun term-normal-state ()
  "Enable `term-line-mode' when in normal state in `term-mode' buffer.
Also make the buffer read only."
  (term-line-mode)
  (read-only-mode 1))

(defun term-insert-state ()
  "Enable `term-char-mode' when in insert state in a `term-mode' buffer."
  (when (get-buffer-process (current-buffer))
    (read-only-mode -1)
    (term-char-mode)))

(defun term-evil-bindings ()
  "Enable term support for vim and hybrid editing styles."
  (add-hook 'evil-hybrid-state-entry-hook #'term-insert-state nil t)
  (add-hook 'evil-insert-state-entry-hook #'term-insert-state nil t)
  (add-hook 'evil-hybrid-state-exit-hook #'term-normal-state nil t)
  (add-hook 'evil-insert-state-exit-hook #'term-normal-state nil t))

(defvar term-char-mode-point-at-process-mark t)
(add-hook 'term-mode-hook 'term-evil-bindings)

;; dired
(use-package dired-single
  :ensure t
  :init
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :config
  (general-define-key  ;; TODO: consider doing this with a hydra
    :states 'normal
    :keymaps 'dired-mode-map
    "c"   #'dired-do-copy
    "C"   #'dired-create-directory
    "d"   #'dired-flag-file-deletion
    "D"   #'dired-flag-files-regexp
    "g"   #'dired-do-find-regexp
    "s"   #'dired-do-find-regexp-and-replace
    "m"   #'dired-mark
    "M"   #'dired-mark-files-regexp
    "o"   #'dired-find-file-other-window
    "r"   #'revert-buffer
    "R"   #'dired-do-rename
    "t"   #'dired-toggle-marks
    "T"   #'dired-do-touch
    "u"   #'dired-unmark
    "U"   #'dired-unmark-all-marks
    "x"   #'dired-do-flagged-delete
    "DEL" #'dired-unmark-backward
    "RET" #'dired-single-buffer
    "^"   #'dired-single-up-directory))

(evil-collection-init 'proced)

;; xref -- used when grepping from dired
(defun xref-show-location-at-point-other-window ()
  "Display the source of xref at point in the other window."
  (interactive)
  (let* ((xref (xref--item-at-point))
         (xref--current-item xref)
         (location (xref-item-location xref))
         (marker (xref-location-marker location))
         (buf (marker-buffer marker)))
    (when xref
      (switch-to-buffer-other-window buf)
      (xref--show-pos-in-buf marker buf))))

(defun xref-next-line-other-window ()
  "Move to the next xref and display its source in other window."
  (interactive)
  (xref--search-property 'xref-item)
  (xref-show-location-at-point-other-window))

(defun xref-prev-line-other-window ()
  "Move to the previous xref and display its source in other window."
  (interactive)
  (xref--search-property 'xref-item t)
  (xref-show-location-at-point-other-window))

(general-define-key
 :states 'normal
 :keymaps 'xref--xref-buffer-mode-map
 "RET" #'xref-quit-and-goto-xref
 "TAB" #'xref-show-location-at-point-other-window
 "n"   #'xref-next-line
 "N"   #'xref-prev-line
 "r"   #'xref-query-replace-in-results)

;; magit
(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t
  :defer 1
  :config
  (general-define-key
   :states 'normal
   :keymaps 'magit-diff-mode-map
   "J" #'magit-section-forward
   "K" #'magit-section-backward)
  (general-define-key
   :states 'normal
   :keymaps 'magit-status-mode-map
   "J" #'magit-section-forward
   "K" #'magit-section-backward)
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t)
  (add-hook 'magit-mode-hook 'enable-window-fringes))

(defun magit-ex-cmd ()
  "Make q & wq work as expected.
Needed because they are globally set in the evil config."
  (setq-local evil-ex-commands
	(mapcar (lambda (cmd) (cons (car cmd) (cdr cmd)))
		(default-value 'evil-ex-commands)))
  (evil-ex-define-cmd "q"  'with-editor-cancel)
  (evil-ex-define-cmd "wq" 'with-editor-finish))
(add-hook 'git-commit-mode-hook #'magit-ex-cmd)

(defun magit-update-vc ()
  "Update magit status on file save.  From doom-emacs."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (vc-refresh-state))))
(add-hook 'magit-post-refresh-hook #'magit-update-vc)

;; python
(add-hook 'python-mode-hook
          (lambda ()
            (setq flycheck-checker 'python-flake8)
            (enable-window-fringes)
            (make-local-variable 'write-file-functions)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; re-define function that generates python buffer name
;; necessary so that elpy send code functionality sends
;; to the right buffer
(eval-after-load "python"
  '(defun python-shell-get-process-name (dedicated)
     (if dedicated
         (format "%s[%s]" (short-name "python") (buffer-name))
       (short-name "python"))))

(general-define-key
  :states 'insert
  :keymaps 'python-mode-map
  "RET" #'newline-and-indent)

(use-package elpy
  :ensure t
  :defer t
  :defines elpy-rpc-backend
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq elpy-rpc-backend "jedi"
        elpy-shell-echo-output nil
        elpy-rpc-virtualenv-path 'current)
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

(general-define-key
  :states 'normal
  :keymaps 'python-mode-map
  "gD" #'elpy-doc
  "gd" #'elpy-goto-definition)

(advice-add
 #'elpy-shell-switch-to-shell
 :after
 (lambda (&rest _r) (goto-char (point-max))))

(use-package pyvenv ;; https://ddavis.io/posts/emacs-python-lsp/
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (defvar python-shell-virtualenv-path nil)  ;; may not need this?
  (defvar python-shell-virtualenv-root nil)  ;; may not need this?
  (setq pyvenv-mode-line-indicator "")
  :config
  (mapcar
   'persp-make-variable-persp-local
   `(python-shell-virtualenv-path  ;; may not need this?
     python-shell-virtualenv-root  ;; may not need this?
     pyvenv-workon       pyvenv-activate
     pyvenv-virtual-env  pyvenv-virtual-env-name
     exec-path           pyvenv-old-exec-path
     eshell-path-env     pyvenv-old-eshell-path
     process-environment pyvenv-old-process-environment)))

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (add-hook 'gfm-mode-hook (lambda () (set-up-tab-width 2)))
  (add-hook 'markdown-mode-hook (lambda () (set-up-tab-width 2))))

;; yaml
(use-package yaml-mode
  :ensure t
  :defer t)

;; go
(use-package go-mode
  :defer t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (general-define-key
    :states 'normal
    :keymaps 'go-mode-map
    "gd" #'godef-jump
    "gf" #'godef-describe))

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

;; leader
(general-define-key
  :states 'normal
  :keymaps 'override  ;; for dired
  :prefix "SPC"
  "/" #'swiper
  "e" #'eval-last-sexp
  "w" #'cg-window/body
  "r" #'cg-run/body
  "o" #'evil-ex-nohighlight
  "u" #'redo
  "c" #'comment-dwim
  "b" #'browse-url-at-point  ;; TODO: might be worth making a w3m hydra at this point
  "p" #'cg-pyvenv/body
  "g" #'google
  "G" #'google-tab
  "j" #'evil-scroll-down
  "k" #'evil-scroll-up
  "8" #'evil-ex-search-word-forward
  "3" #'evil-ex-search-word-backward)

(general-define-key
  :states 'visual
  :prefix "SPC"
  "c" #'comment-dwim)

;; control
(general-define-key
  :states '(normal insert motion)
  :keymaps 'override
  "C-b" #'persp-ivy-switch-buffer
  "C-f" #'counsel-find-file
  "C-g" #'magit-status
  "C-;" #'help
  "C-u" #'evil-scroll-up
  "C-h" #'evil-window-left
  "C-j" #'evil-window-down
  "C-k" #'evil-window-up
  "C-l" #'evil-window-right)

;; modeline
(defun remove-git (branch-string)
  "Remove unnecessary Git<punctuation mark> from BRANCH-STRING, `vc-mode` string."
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
                      (concat "-%l|%c:"
                              (format "%d" (/ (point) 0.01 (point-max)))
                              "%%   ")))
	'(:eval (propertize (if vc-mode
				(remove-git vc-mode)
			        "-")
		 'face 'bold))
	"   "
	mode-line-misc-info))
(when first-load (progn (cd "~") (setq first-load nil)))

(provide 'init)
;;; init.el ends here
