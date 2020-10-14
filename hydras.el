;;; init.el --- Cary's hydras file  -*- lexical-binding: t; -*-

;;; Commentary:
;;^ are a special no width character to help visual spacing in definitions

;;; Code:

;; ----------- Functions -----------

(defun short-name (name &optional unique)
  "Get, unique if UNIQUE, name for buffer with base NAME and short persp at end.
Useful for adding to buffers with multiple instances like shells."
  (let* ((name-list (split-string (persp-current-name) "[-_ ]+"))
         (short-name (if (> (length name-list) 1)
                         (mapconcat (lambda (p) (substring p nil 1)) name-list "")
                       (car name-list)))
         (persp-short-name (concat name "-" short-name))
         (buffer-names (mapcar #'buffer-name (buffer-list)))
         (total (length (seq-filter (lambda (v) (string-prefix-p persp-short-name v))
                                    buffer-names)))
         (short-name-end (if (and unique (> total 0))
                             (concat "-" (number-to-string total))
                           "")))
      (concat persp-short-name short-name-end)))

(defun new-terminal ()
  "Make an ansi terminal buffer."
  ;; Note, using bash as the terminal in the ansi-term emulator, for some reason,
  ;; requires reall specific character sequences while customizing the prompt,
  ;; if \[ and \] are used then ansi-term seems to get confused when the line end
  ;; is reached while inputting a command and wraps on the same line, overwriting
  ;; the beginning of the prompt. Using \001 and \002 instead seems to fix this:
  ;; blue="\001$(tput setaf 4)\002"
  ;; reset="\001$(tput sgr0)\002"
  ;; export PS1="${blue}Ultramann: \W \$ ${reset}"
  ;; unset blue reset
  (interactive)
  (let ((process-environment (seq-filter (lambda (v) (string-prefix-p "HOME=" v))
                                         process-environment)))
    (ansi-term (concat
                (if (file-exists-p "/usr/local/bin/bash")
                    "/usr/local"
                  "")
                "/bin/bash")
               (short-name "term" t))))

(defun new-eshell ()
  "Open a new eshell instance."
  (interactive)
  (eshell)
  (rename-buffer (concat "*" (short-name "eshell" t) "*")))

(defun new-python ()
  "Open python shell based off current perspective."
  (interactive)
  (run-python)
  (rename-buffer (concat "*" (short-name "python" t) "*")))

(defun cg-persp-new (name)
  "Make new pespective given by NAME."
  (interactive "sNew perspective name: ")
  (let ((already-exists (member name (persp-names))))
    (persp-switch name)
    (when (not already-exists)
      (find-file "~/developer"))))

(defun toggle-window-fullscreen ()
  "Toggle current window to full screen."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
      (progn (window-configuration-to-register '_)
  	     (delete-other-windows))))

;; ------------ Hydras ------------

;; Window
(defhydra split-horizontal (:hint nil
			    :foreign-keys warn
			    :exit t
			    :post (progn (split-window-right)
					 (call-interactively 'evil-window-right)))
  "
 ^ ^  Horizonal
 ^-^------------^-^----------------^-^----------
 _b_: buffer    _g_: google        _e_: eshell
 _f_: file      _G_: google-tab    _t_: terminal
 _n_: none

 [_<escape>_]: quit
"
  ("b" persp-ivy-switch-buffer)
  ("f" counsel-find-file)
  ("g" google)
  ("G" google-tab)
  ("e" new-eshell)
  ("t" new-terminal)
  ("n" nil)
  ("<escape>" delete-window :color teal))

(defhydra split-vertical (:hint nil
			  :foreign-keys warn
			  :exit t
			  :post (progn (split-window-below)
				       (call-interactively 'evil-window-down)))
  "
 ^ ^  Vertical
 ^-^------------^-^----------------^-^----------
 _b_: buffer    _g_: google        _e_: eshell
 _f_: file      _G_: google-tab    _t_: terminal
 _n_: none

 [_<escape>_]: quit
"
  ("b" persp-ivy-switch-buffer)
  ("f" counsel-find-file)
  ("g" google)
  ("G" google-tab)
  ("e" new-eshell)
  ("t" new-terminal)
  ("n" nil)
  ("<escape>" delete-window :color teal))

(defhydra other-split (:hint nil
			:foreign-keys warn
			:exit t
			:post (switch-to-buffer-other-window (current-buffer)))
  "
 ^ ^  Other Split
 ^-^------------^-^----------------^-^----------
 _b_: buffer    _g_: google        _e_: eshell
 _f_: file      _G_: google-tab    _t_: terminal
 _n_: none

 [_<escape>_]: quit
"
  ("b" persp-ivy-switch-buffer)
  ("f" counsel-find-file)
  ("g" google)
  ("G" google-tab)
  ("e" new-eshell)
  ("t" new-terminal)
  ("n" nil)
  ("<escape>" delete-window :color teal))

(defhydra text-resize (:hint nil :foreign-keys warn :quit-key "<escape>")
  "
 ^ ^  Resize Text
 ^-^------------^-^---------
 _=_: bigger    _-_: smaller

"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("<escape>" nil "quit"))

(defhydra split-resize (:hint nil :foreign-keys warn :quit-key "<escape>")
  "
  ^ ^  Resize Split
  ^-^------------^-^-------------^-^------------^-^---------
  _l_: wider    _h_: narrower    _k_: taller    _j_: shorter
  _b_: balance

"
  ("l" enlarge-window-horizontally)
  ("h" shrink-window-horizontally)
  ("k" enlarge-window)
  ("j" shrink-window)
  ("b" balance-windows)
  ("<escape>" nil "quit"))

(defhydra cg-window (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
 ^ ^  Window        ^ ^  Split             ^ ^  Display
 ^-^----------------^-^--------------------^-^--------------------
 _w_: switch        _s_: horizonal         _d_: rainbow delimiters
 _l_: last          _v_: vertical          _t_: text size
 _r_: rename        _o_: other             _N_: toggle line numbers
 _n_: new           _f_: fullscreen
 _K_: kill          _k_: kill
 ^ ^                _R_: resize
"
  ("w" persp-switch)
  ("l" persp-switch-last)
  ("r" persp-rename)
  ("n" cg-persp-new)
  ("K" persp-kill)

  ("s" split-horizontal/body)
  ("v" split-vertical/body)
  ("o" other-split/body)
  ("f" toggle-window-fullscreen)
  ("k" delete-window)
  ("R" split-resize/body)

  ("d" rainbow-delimiters-mode)
  ("t" text-resize/body)
  ("N" display-line-numbers-mode)

  ("<escape>" nil "quit"))

;; Run
(defhydra cg-run (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
 ^ ^  Run
 ^-^---------------^-^------------------^-^----------------
 _e_: eshell       _s_: sync-shell      _p_: python
 _t_: terminal     _a_: async-shell     _d_: docker-compose
"
  ("e" new-eshell)
  ("t" new-terminal)
  ("s" shell-command)
  ("a" async-shell-command)  ;; want to make interactive wrapper for start-process to replace this
  ("d" cg-docker-compose-up)
  ("p" new-python)
  ("<escape>" nil "quit"))

;; Pyenv
(defhydra cg-pyvenv (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
 ^ ^  Pyvenv: %`pyvenv-virtual-env-name
 ---------------------------------
 _a_: activate
 _d_: deactivate
"
  ("a" pyvenv-workon)
  ("d" pyvenv-deactivate)
  ("<escape>" nil "quit"))

(provide 'hydras)
;;; hydras ends here
