(defhydra split-horizontal (:hint nil
			    :foreign-keys warn
			    :exit t
			    :post (progn (split-window-right)
					 (call-interactively 'evil-window-right)))
  "
^Horizonal^
-------------------------------------
_b_: buffer    _f_: file    _n_: none
[_q_]: quit
"
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("n" nil)
  ("q" delete-window))

(defhydra split-vertical (:hint nil
			  :foreign-keys warn
			  :exit t
			  :post (progn (split-window-below)
				       (call-interactively 'evil-window-down)))
  "
^Vertical^
-------------------------------------
_b_: buffer    _f_: file    _n_: none
[_q_]: quit
"
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("n" nil)
  ("q" delete-window))

(defhydra text-resize (:hint nil :foreign-keys warn :quit-key "q")
  "
^Resize Text^
---------------------------
_=_: bigger    _-_: smaller
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil "quit"))

(defhydra buffer-resize (:color teal :hint nil :foreign-keys warn :quit-key "q")
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
  ("q" nil "quit"))

(defhydra cg-window (:hint nil :foreign-keys warn :exit t :quit-key "q")
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
  ("q" nil "quit"))

(defhydra cg-ipython-notebook (:hint nil :quit-key "q")
  "
 Operations on Cells^^^^^^            On Worksheets^^^^              Other
 ----------------------------^^^^^^   ------------------------^^^^   ----------------------------------^^^^
 [_k_/_j_]^^     select prev/next     [_h_/_l_]   select prev/next   [_t_]^^         toggle output
 [_K_/_J_]^^     move up/down         [_H_/_L_]   move left/right    [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_1_.._9_]  open [1st..last]   [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_+_/_-_]   create/delete      [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           ^^^^                           [_x_]^^         close notebook
 [_u_]^^^^       change type          ^^^^                           [_q_]^^         quit transient-state
 [_RET_]^^^^     execute"
        ("q" nil :exit t)
        ("?" spacemacs//ipython-notebook-ms-toggle-doc)
        ("h" ein:notebook-worksheet-open-prev-or-last)
        ("j" ein:worksheet-goto-next-input)
        ("k" ein:worksheet-goto-prev-input)
        ("l" ein:notebook-worksheet-open-next-or-first)
        ("H" ein:notebook-worksheet-move-prev)
        ("J" ein:worksheet-move-cell-down)
        ("K" ein:worksheet-move-cell-up)
        ("L" ein:notebook-worksheet-move-next)
        ("t" ein:worksheet-toggle-output)
        ("d" ein:worksheet-kill-cell)
        ("R" ein:worksheet-rename-sheet)
        ("y" ein:worksheet-copy-cell)
        ("p" ein:worksheet-yank-cell)
        ("o" ein:worksheet-insert-cell-below)
        ("O" ein:worksheet-insert-cell-above)
        ("u" ein:worksheet-change-cell-type)
        ("RET" ein:worksheet-execute-cell-and-goto-next)
        ;; Output
        ("C-l" ein:worksheet-clear-output)
        ("C-S-l" ein:worksheet-clear-all-output)
        ;;Console
        ("C-o" ein:console-open)
        ;; Merge cells
        ("C-k" ein:worksheet-merge-cell)
        ("C-j" spacemacs/ein:worksheet-merge-cell-next)
        ;; Notebook
        ("C-s" ein:notebook-save-notebook-command)
        ("C-r" ein:notebook-rename-command)
        ("1" ein:notebook-worksheet-open-1th)
        ("2" ein:notebook-worksheet-open-2th)
        ("3" ein:notebook-worksheet-open-3th)
        ("4" ein:notebook-worksheet-open-4th)
        ("5" ein:notebook-worksheet-open-5th)
        ("6" ein:notebook-worksheet-open-6th)
        ("7" ein:notebook-worksheet-open-7th)
        ("8" ein:notebook-worksheet-open-8th)
        ("9" ein:notebook-worksheet-open-last)
        ("+" ein:notebook-worksheet-insert-next)
        ("-" ein:notebook-worksheet-delete)
        ("x" ein:notebook-close))
