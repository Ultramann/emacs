;; Set up, but not colors, taken from https://github.com/jonathanchu/atom-one-dark-theme.
;; list-faces-display is useful for defining colors for new faces

(deftheme boss
  "Boss color them, created by Cary Goltermann")

(defvar boss-colors-alist
  '(("boss-accent"   . "#A2ABB3")
    ("boss-fg"       . "#ABB2BF")
    ("boss-bg"       . "#000016")
    ("boss-bg-1"     . "#121417")
    ("boss-bg-hl"    . "#00254A")
    ("boss-gutter"   . "#131C33")
    ("boss-base-1"   . "#3DDDFF")
    ("boss-base-2"   . "#2BBCFF")
    ("boss-base-3"   . "#2B92FF")
    ("boss-mono-1"   . "#ABB2BF")
    ("boss-mono-2"   . "#828997")
    ("boss-mono-3"   . "#5C6370")
    ("boss-mono-4"   . "#26303B")
    ("boss-blue"     . "#61AFEF")
    ("boss-purple"   . "#C678DD")
    ("boss-purple-1" . "#995CB6")
    ("boss-green"    . "#61EF71")
    ("boss-yellow"   . "#FFFE21")
    ("boss-yellow-1" . "#C69D00")
    ("boss-red-1"    . "#C03A3A")
    ("boss-red-2"    . "#E76D6D")
    ("boss-pink"     . "#F40C61")
    ("boss-gray"     . "#3E4451")
    ("boss-silver"   . "#AAAAAA")
    ("boss-black"    . "#0F1011"))
  "List of Boss colors.")

(defmacro boss-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    boss-colors-alist))
     ,@body))

(boss-with-color-variables
  (custom-theme-set-faces
   'boss
   `(default ((t (:foreground ,boss-fg :background ,boss-bg))))
   `(success ((t (:foreground ,boss-green))))
   `(warning ((t (:foreground ,boss-red-2))))
   `(error ((t (:foreground ,boss-red-1 :weight bold))))
   `(link ((t (:foreground ,boss-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,boss-blue :underline t :weight normal))))
   `(cursor ((t (:background ,boss-accent))))
   `(fringe ((t (:background ,boss-bg))))
   `(region ((t (:background ,boss-gray))))
   `(highlight ((t (:background ,boss-gray))))
   `(hl-line ((t (:background ,boss-bg-hl))))
   `(vertical-border ((t (:foreground ,boss-mono-3))))
   `(secondary-selection ((t (:background ,boss-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,boss-silver))))
   `(show-paren-mismatch ((t :background ,boss-pink)))
   `(show-paren-match ((t :foreground ,boss-pink)))

   `(font-lock-builtin-face ((t (:foreground ,boss-base-1))))
   `(font-lock-comment-face ((t (:foreground ,boss-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,boss-purple-1))))
   `(font-lock-keyword-face ((t (:foreground ,boss-red-1))))
   `(font-lock-preprocessor-face ((t (:foreground ,boss-mono-2))))
   `(font-lock-string-face ((t (:foreground ,boss-base-3))))
   `(font-lock-type-face ((t (:foreground ,boss-yellow-1))))
   `(font-lock-constant-face ((t (:foreground ,boss-base-2))))
   `(font-lock-variable-name-face ((t (:inherit (defualt)))))
   `(font-lock-warning-face ((t (:foreground ,boss-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:foreground ,boss-gutter :background ,boss-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,boss-gutter))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,boss-fg :background ,boss-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,boss-mono-2 :background ,boss-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,boss-fg :background ,boss-gray))))
   `(company-tooltip-mouse ((t (:background ,boss-gray))))
   `(company-tooltip-common ((t (:foreground ,boss-red-2 :background ,boss-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,boss-red-2 :background ,boss-gray))))
   `(company-preview ((t (:background ,boss-bg))))
   `(company-preview-common ((t (:foreground ,boss-red-2 :background ,boss-bg))))
   `(company-scrollbar-fg ((t (:background ,boss-mono-1))))
   `(company-scrollbar-bg ((t (:background ,boss-bg-1))))

   ;; compilation
   `(compilation-face ((t (:foreground ,boss-fg))))
   `(compilation-line-number ((t (:foreground ,boss-mono-2))))
   `(compilation-column-number ((t (:foreground ,boss-mono-2))))

   ;; isearch
   `(isearch ((t (:foreground ,boss-bg :background ,boss-yellow))))
   `(isearch-fail ((t (:foreground ,boss-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,boss-yellow :background ,boss-bg))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; helm
   `(helm-header ((t (:foreground ,boss-mono-2
                      :background ,boss-bg
                      :underline nil
                      :box (:line-width 6 :color ,boss-bg)))))
   `(helm-source-header ((t (:foreground ,boss-red-2
                             :background ,boss-bg
                             :underline nil
                             :weight bold
                             :box (:line-width 6 :color ,boss-bg)))))
   `(helm-selection ((t (:background ,boss-gray))))
   `(helm-selection-line ((t (:background ,boss-gray))))
   `(helm-visible-mark ((t (:foreground ,boss-bg :foreground ,boss-red-2))))
   `(helm-candidate-number ((t (:foreground ,boss-base-2 :background ,boss-bg-1))))
   `(helm-separator ((t (:background ,boss-bg :foreground ,boss-red-1))))
   `(helm-M-x-key ((t (:foreground ,boss-red-1))))
   `(helm-bookmark-addressbook ((t (:foreground ,boss-red-1))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,boss-purple))))
   `(helm-bookmark-info ((t (:foreground ,boss-green))))
   `(helm-bookmark-man ((t (:foreground ,boss-red-2))))
   `(helm-bookmark-w3m ((t (:foreground ,boss-purple))))
   `(helm-match ((t (:foreground ,boss-base-1))))
   `(helm-ff-directory ((t (:foreground ,boss-base-2 :background ,boss-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,boss-fg :background ,boss-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,boss-green :background ,boss-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,boss-red-1 :background ,boss-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,boss-red-2 :background ,boss-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,boss-bg :background ,boss-red-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,boss-red-1))))
   `(helm-buffer-process ((t (:foreground ,boss-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,boss-fg))))
   `(helm-buffer-size ((t (:foreground ,boss-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,boss-purple))))
   `(helm-grep-cmd-line ((t (:foreground ,boss-base-2))))
   `(helm-grep-file ((t (:foreground ,boss-fg))))
   `(helm-grep-finish ((t (:foreground ,boss-green))))
   `(helm-grep-lineno ((t (:foreground ,boss-mono-2))))
   `(helm-grep-finish ((t (:foreground ,boss-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,boss-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,boss-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,boss-red-2 :weight bold))))

   ;; magit
   `(magit-section-highlight ((t (:background ,boss-bg-hl))))
   `(magit-section-heading ((t (:foreground ,boss-red-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,boss-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,boss-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,boss-red-2 :background ,boss-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,boss-mono-2 :background ,boss-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,boss-mono-1 :background ,boss-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,boss-purple :background ,boss-mono-3))))
   `(magit-diff-context ((t (:foreground ,boss-fg))))
   `(magit-diff-context-highlight ((t (:background ,boss-bg-1 :foreground ,boss-fg))))
   `(magit-diffstat-added ((t (:foreground ,boss-green))))
   `(magit-diffstat-removed ((t (:foreground ,boss-red-1))))
   `(magit-process-ok ((t (:foreground ,boss-green))))
   `(magit-process-ng ((t (:foreground ,boss-red-1))))
   `(magit-log-author ((t (:foreground ,boss-red-2))))
   `(magit-log-date ((t (:foreground ,boss-mono-2))))
   `(magit-log-graph ((t (:foreground ,boss-silver))))
   `(magit-sequence-pick ((t (:foreground ,boss-red-2))))
   `(magit-sequence-stop ((t (:foreground ,boss-green))))
   `(magit-sequence-part ((t (:foreground ,boss-red-1))))
   `(magit-sequence-head ((t (:foreground ,boss-blue))))
   `(magit-sequence-drop ((t (:foreground ,boss-red-1))))
   `(magit-sequence-done ((t (:foreground ,boss-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,boss-mono-2))))
   `(magit-bisect-good ((t (:foreground ,boss-green))))
   `(magit-bisect-skip ((t (:foreground ,boss-red-1))))
   `(magit-bisect-bad ((t (:foreground ,boss-red-1))))
   `(magit-blame-heading ((t (:background ,boss-bg-1 :foreground ,boss-mono-2))))
   `(magit-blame-hash ((t (:background ,boss-bg-1 :foreground ,boss-purple))))
   `(magit-blame-name ((t (:background ,boss-bg-1 :foreground ,boss-red-2))))
   `(magit-blame-date ((t (:background ,boss-bg-1 :foreground ,boss-mono-3))))
   `(magit-blame-summary ((t (:background ,boss-bg-1 :foreground ,boss-mono-2))))
   `(magit-dimmed ((t (:foreground ,boss-mono-2))))
   `(magit-hash ((t (:foreground ,boss-purple))))
   `(magit-tag  ((t (:foreground ,boss-red-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,boss-green :weight bold))))
   `(magit-branch-local   ((t (:foreground ,boss-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,boss-blue :weight bold :box t))))
   `(magit-head           ((t (:foreground ,boss-blue :weight bold))))
   `(magit-refname        ((t (:background ,boss-bg :foreground ,boss-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,boss-bg :foreground ,boss-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,boss-bg :foreground ,boss-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,boss-green))))
   `(magit-signature-bad       ((t (:foreground ,boss-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,boss-red-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,boss-base-2))))
   `(magit-cherry-equivalent   ((t (:foreground ,boss-purple))))
   `(magit-reflog-commit       ((t (:foreground ,boss-green))))
   `(magit-reflog-amend        ((t (:foreground ,boss-purple))))
   `(magit-reflog-merge        ((t (:foreground ,boss-green))))
   `(magit-reflog-checkout     ((t (:foreground ,boss-blue))))
   `(magit-reflog-reset        ((t (:foreground ,boss-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,boss-purple))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,boss-green))))
   `(magit-reflog-remote       ((t (:foreground ,boss-base-2))))
   `(magit-reflog-other        ((t (:foreground ,boss-base-2))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,boss-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,boss-purple))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,boss-blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,boss-base-2))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,boss-green))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,boss-red-1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,boss-red-2))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,boss-red-1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,boss-red-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,boss-mono-1))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,boss-mono-2))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,boss-mono-3))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,boss-black))))

   ;; term
   `(term-color-black ((t :foreground ,boss-mono-1)))
   `(term-color-blue ((t :foreground ,boss-base-3)))
   `(term-color-cyan ((t :foreground ,boss-base-2)))
   `(term-color-green ((t :foreground ,boss-green)))
   `(term-color-magenta ((t :foreground ,boss-purple)))
   `(term-color-red ((t :foreground ,boss-red-2)))
   `(term-color-white ((t :foreground ,boss-fg)))
   `(term-color-yellow ((t (:foreground ,boss-red-1))))

   ;; linum
   `(linum ((t (:foreground ,boss-silver :background ,boss-gutter))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,boss-bg :background ,boss-silver))))

   ;; w3m
   `(w3m-anchor ((t :foreground ,boss-base-3)))
   `(w3m-image-anchor ((t :foreground ,boss-green :background ,boss-bg)))
   `(header-line ((t :background ,boss-gutter)))
   `(w3m-header-line-location-content ((t :foreground ,boss-base-2 :background ,boss-gutter)))
   `(w3m-header-line-location-title ((t :foreground ,boss-yellow-1 :background ,boss-gutter)))
   ))


(boss-with-color-variables
  (custom-theme-set-variables
   'boss
   `(fci-rule-color ,boss-gray)
   ))

(defvar boss-theme-force-faces-for-mode t)

(defun boss-theme-change-faces-for-mode ()
  (interactive)
  (and (eq boss-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(js2-mode))
         ;; boss-red-1
         (face-remap-add-relative 'font-lock-constant-face :foreground "#D19A66")
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
         ;; boss-mono-1
         (face-remap-add-relative 'font-lock-variable-name-face :foreground "#ABB2BF"))
        )))

(add-hook 'after-change-major-mode-hook 'boss-theme-change-faces-for-mode)

(provide-theme 'boss)
