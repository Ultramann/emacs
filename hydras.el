;; Window
(defhydra split-horizontal (:hint nil
			    :foreign-keys warn
			    :exit t
			    :post (progn (split-window-right)
					 (call-interactively 'evil-window-right)))
  "
^Horizonal^
------------------------------------------
_b_: buffer    _g_: google        _s_: eshell
_f_: file      _G_: google-tab    _n_: none
[_<escape>_]: quit
"
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("g" google)
  ("G" google-tab)
  ("s" eshell)
  ("n" nil)
  ("<escape>" delete-window :color teal))

(defhydra split-vertical (:hint nil
			  :foreign-keys warn
			  :exit t
			  :post (progn (split-window-below)
				       (call-interactively 'evil-window-down)))
  "
^Vertical^
------------------------------------------------------
_b_: buffer    _g_: google        _s_: eshell
_f_: file      _G_: google-tab    _n_: none
[_<escape>_]: quit
"
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("g" google)
  ("G" google-tab)
  ("s" eshell)
  ("n" nil)
  ("<escape>" delete-window :color teal))

(defhydra text-resize (:hint nil :foreign-keys warn :quit-key "<escape>")
  "
^Resize Text^
---------------------------
_=_: bigger    _-_: smaller
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("<escape>" nil "quit"))

(defhydra buffer-resize (:hint nil
			 :foreign-keys warn
			 :quit-key "<escape>")
  "
^Resize Buffer^
----------------------------------------------------------
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
^Window^         ^Split^           ^Resize^
----------------------------------------------
_n_: new         _s_: horizonal    _b_: buffer
_l_: last        _v_: vertical     _t_: text
_a_: list all    _k_: kill
_K_: kill
"
  ("a" elscreen-select-and-goto)
  ("l" elscreen-toggle)
  ("n" elscreen-create-rename)
  ("K" elscreen-kill)
  ("s" split-horizontal/body)
  ("v" split-vertical/body)
  ("k" delete-window)
  ("b" buffer-resize/body)
  ("t" text-resize/body)
  ("<escape>" nil "quit"))

(defhydra docker-compose (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
^Docker Compose^
----------------------------------
_b_: build    _u_: up    _d_: down
"
  ("b" docker-compose-build)
  ("u" docker-compose-up)
  ("d" docker-compose-down)
  ("<escape>" nil "quit"))

(defhydra docker (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
^Docker^
--------------------------------------
_b_: build    _r_: run    _c_: compose
"
  ("b" docker-build)
  ("r" docker-run)
  ("c" docker-compose/body)
  ("<escape>" nil "quit"))

(defhydra cg-run (:hint nil :foreign-keys warn :exit t :quit-key "<escape>")
  "
^Run^
-------------------------------------------
_e_: eshell    _t_: terminal    _d_: docker
"
  ("t" (lambda () (interactive) (ansi-term "/bin/bash")))
  ("e" eshell)
  ("d" docker/body)
  ("<escape>" nil "quit"))
