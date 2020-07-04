;;^ are a special no width character to help visual spacing in definitions

(defun terminal ()
  (interactive)
  (ansi-term "/bin/bash")
  (rename-buffer "*term*"))

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
  ("e" eshell)
  ("t" terminal)
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
  ("e" eshell)
  ("t" terminal)
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

(defhydra split-resize (:hint nil
			 :foreign-keys warn
			 :quit-key "<escape>")
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
^Window^         ^Split^            ^Text^
^-^--------------^-^----------------^--^--------------------
_w_: switch       _s_: horizonal    _rd_: rainbow delimiters
_l_: last         _v_: vertical     _rt_: resize text
_R_: rename       _k_: kill
_K_: kill         _f_: fullscreen
^^               _rs_: resize
"
  ("w"  persp-switch)
  ("l"  persp-switch-last)
  ("R"  persp-rename)
  ("K"  persp-kill)
  ("s"  split-horizontal/body)
  ("v"  split-vertical/body)
  ("k"  delete-window)
  ("f"  toggle-window-fullscreen)
  ("rs" split-resize/body)
  ("rt" text-resize/body)
  ("rd" rainbow-delimiters-mode)

  ("<escape>" nil "quit"))

;; Run
(defhydra cg-run (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
^Run
^-^----------------^-^-----------------^-^----------------
_e_: eshell        _t_: terminal       _d_: docker-compose
_s_: sync-shell    _a_: async-shell
"
  ("e" eshell)
  ("t" terminal)
  ("d" cg-docker-compose-up)
  ("s" shell-command)
  ("a" sync-shell-command)
  ("<escape>" nil "quit"))
