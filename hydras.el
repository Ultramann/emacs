;;; init.el --- Cary's hydras file  -*- lexical-binding: t; -*-

;;; Commentary:
;;^ are a special no width character to help visual spacing in definitions

;;; Code:

;; ----------- Functions -----------

(defun get-short-persp-current-name ()
  "Get shortened name of current perspective.
Useful for adding to buffers with multiple instances like shells."
  (split-string (persp-current-name) "[-_ ]+")) ;; WIP

(defun new-terminal ()
  "Make an ansi terminal buffer."
  (interactive)
  (let ((process-environment '("HOME=/Users/carygoltermann")))
    (ansi-term (concat
                (if (file-exists-p "/usr/local/bin/bash")
                    "/usr/local"
                  "")
                "/bin/bash")
               "term"))
  )

(defun new-eshell ()
  "Open a new eshell instance."
  (interactive)
  (eshell 'N))

(defun cg-run-python ()
  "Open python shell based off current perspective."
  (get-short-persp-current-name)) ;; WIP

(defun cg-persp-new (name)
  "Make new pespective given by NAME."
  (interactive "sNew perspective name: ")
  (persp-switch name)
  (find-file "~/developer"))

;; ------------ Hydras ------------

;; Window
(defhydra split-horizontal (:hint nil
			    :foreign-keys warn
			    :exit t
			    :post (progn (split-window-right)
					 (call-interactively 'evil-window-right)))
  "
^Horizonal
^-^------------^-^----------------^-^--------
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
^Vertical
^-^------------^-^----------------^-^--------
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
^Resize Text
^-^------------^-^---------
_=_: bigger    _-_: smaller
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("<escape>" nil "quit"))

(defhydra split-resize (:hint nil :foreign-keys warn :quit-key "<escape>")
  "
^Resize Split
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
^Window^           ^Split^                ^Display^
^-^----------------^-^--------------------^-^--------------------
_w_: switch        _s_: horizonal         _d_: rainbow delimiters
_l_: last          _v_: vertical          _t_: text size
_r_: rename        _k_: kill              _N_: toggle line numbers
_n_: new           _f_: fullscreen
_K_: kill          _R_: resize
"
  ("w" persp-switch)
  ("l" persp-switch-last)
  ("r" persp-rename)
  ("n" cg-persp-new)
  ("K" persp-kill)

  ("s" split-horizontal/body)
  ("v" split-vertical/body)
  ("k" delete-window)
  ("f" toggle-window-fullscreen)
  ("R" split-resize/body)

  ("d" rainbow-delimiters-mode)
  ("t" text-resize/body)
  ("N" display-line-numbers-mode)

  ("<escape>" nil "quit"))

;; Run
(defhydra cg-run (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
^Run
^-^---------------^-^------------------^-^----------------
_e_: eshell       _s_: sync-shell      _p_: python
_t_: terminal     _a_: async-shell     _d_: docker-compose
"
  ("e" new-eshell)
  ("t" new-terminal)
  ("s" shell-command)
  ("a" async-shell-command)  ;; want to make interactive wrapper for start-process to replace this
  ("d" cg-docker-compose-up)
  ("p" (lambda () (interactive) (run-python nil t)))
  ("<escape>" nil "quit"))

;; Pyenv
(defhydra cg-pyvenv (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
Pyvenv: %`pyvenv-virtual-env-name
---------------------------------
_a_: activate
_d_: deactivate
"
  ("a" pyvenv-workon)
  ("d" pyvenv-deactivate)
  ("<escape>" nil "quit"))

(provide 'hydras)
;;; hydras ends here
