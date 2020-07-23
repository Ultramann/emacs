;;; package --- Cary's custom color theme

;;; Commentary:
;; Set up, but not colors, taken from https://github.com/jonathanchu/atom-one-dark-theme.
;; list-colors-display show all the current color options
;; list-faces-display (counsel-faces) shows colors for all current faces
;; rainbow-mode highlights color words and definitions with the color
;; (counsel-)describe-face shows all information about face for word under point

;;; Code:

(deftheme boss
  "Boss color theme, created by Cary Goltermann")

(defvar boss-colors-alist
  '(("boss-fg"            . "#C6C6C6")
    ("boss-bg"            . "#000000")
    ("boss-bg-shadow"     . "#1C1C1C")
    ("boss-bg-light"      . "#262626")
    ("boss-bg-highlight"  . "#3A3A3A")
    ("boss-mono-bridge"   . "#4F4F4F")
    ("boss-mono-dark"     . "#767676")
    ("boss-mono-med"      . "#9E9E9E")
    ("boss-mono-light"    . "#B2B2B2")
    ("boss-mono-white"    . "#FFFFFF")
    ("boss-blue-light"    . "#5fd7ff")
    ("boss-blue-med"      . "#00AFFF")
    ("boss-blue-dark"     . "#0087FF")
    ("boss-purple"        . "#AA78DD")
    ("boss-purple-dark"   . "#875FFF")
    ("boss-purple-bright" . "#D700D7")
    ("boss-green"         . "#00af87")
    ("boss-green-bright"  . "#00FF5F")
    ("boss-yellow"        . "#D7AF00")
    ("boss-yellow-bright" . "#FFFF00")
    ("boss-red"           . "#AF5F5F")
    ("boss-red-bright"    . "#FF005F")
    ("boss-pink"          . "#FF00AF"))
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
   `(default ((t :foreground ,boss-fg :background ,boss-bg)))
   `(success ((t :foreground ,boss-green-bright)))
   `(warning ((t :foreground ,boss-red-bright)))
   `(error ((t :foreground ,boss-red :weight bold)))
   `(link ((t :foreground ,boss-blue-med :underline t :weight bold)))
   `(link-visited ((t :foreground ,boss-blue-med :underline t :weight normal)))
   `(cursor ((t :background ,boss-fg)))
   `(fringe ((t :background ,boss-bg)))
   `(region ((t :background ,boss-bg-highlight)))
   `(highlight ((t :background ,boss-bg-highlight)))
   `(vertical-border ((t :foreground ,boss-mono-dark)))
   `(secondary-selection ((t :background ,boss-mono-dark)))
   `(trailing-whitespace ((t :foreground ,boss-red-bright :underline t)))

   `(query-replace ((t :inherit isearch)))
   `(minibuffer-prompt ((t :foreground ,boss-mono-light)))
   `(show-paren-mismatch ((t :background ,boss-pink)))
   `(show-paren-match ((t :foreground ,boss-pink)))
   `(rainbow-delimiters-depth-1-face ((t :foreground ,boss-red-bright)))
   `(rainbow-delimiters-depth-2-face ((t :foreground ,boss-purple-bright)))
   `(rainbow-delimiters-depth-3-face ((t :foreground ,boss-blue-light)))
   `(rainbow-delimiters-depth-4-face ((t :foreground ,boss-green-bright)))
   `(rainbow-delimiters-depth-5-face ((t :foreground ,boss-yellow-bright)))
   `(rainbow-delimiters-depth-6-face ((t :foreground ,boss-red-bright)))
   `(rainbow-delimiters-depth-7-face ((t :foreground ,boss-purple-bright)))
   `(rainbow-delimiters-depth-8-face ((t :foreground ,boss-blue-light)))
   `(rainbow-delimiters-depth-9-face ((t :foreground ,boss-green-bright)))
   `(rainbow-delimiters-unmached-face ((t :background ,boss-pink)))
   `(rainbow-delimiters-mismached-face ((t :background ,boss-pink)))

   `(font-lock-builtin-face ((t :foreground ,boss-blue-light)))
   `(font-lock-comment-face ((t :foreground ,boss-mono-dark)))
   `(font-lock-comment-delimiter-face ((default :inherit font-lock-comment-face)))
   `(font-lock-doc-face ((t :inherit font-lock-string-face)))
   `(font-lock-function-name-face ((t :foreground ,boss-purple)))
   `(font-lock-keyword-face ((t :foreground ,boss-red)))
   `(font-lock-preprocessor-face ((t :foreground ,boss-mono-med)))
   `(font-lock-string-face ((t :foreground ,boss-blue-dark)))
   `(font-lock-type-face ((t :foreground ,boss-yellow)))
   `(font-lock-constant-face ((t :foreground ,boss-blue-med)))
   `(font-lock-variable-name-face ((t :inherit default)))
   `(font-lock-warning-face ((t :foreground ,boss-mono-dark :weight bold)))

   ;; company-mode
   `(company-tooltip ((t :foreground ,boss-fg :background ,boss-bg-highlight)))
   `(company-tooltip-annotation ((t :inherit company-tooltip)))
   `(company-tooltip-selection ((t :foreground ,boss-blue-light :background ,boss-mono-bridge :weight bold)))
   `(company-tooltip-common ((t :inherit company-tooltip :foreground ,boss-blue-dark)))
   `(company-tooltip-common-selection ((t :inherit company-tooltip-selection)))
   `(company-tooltip-mouse ((t :inherit company-tooltip :background ,boss-mono-bridge)))
   `(company-preview ((t :background ,boss-bg)))
   `(company-preview-common ((t :foreground ,boss-blue-med :background ,boss-bg)))
   `(company-scrollbar-fg ((t :background ,boss-mono-light)))
   `(company-scrollbar-bg ((t :background ,boss-mono-dark)))

   ;; compilation
   `(compilation-face ((t :foreground ,boss-fg)))
   `(compilation-info ((t :background ,boss-bg-light :foreground ,boss-green)))
   `(compilation-line-number ((t :foreground ,boss-mono-med)))
   `(compilation-column-number ((t :foreground ,boss-mono-med)))

   ;; search
   `(isearch ((t :foreground ,boss-bg :background ,boss-yellow-bright)))
   `(isearch-fail ((t :foreground ,boss-red-bright :background nil)))
   `(lazy-highlight ((t :foreground ,boss-yellow-bright :inhert region)))
   `(evil-ex-substitute-matches ((t :foreground ,boss-yellow-bright :background ,boss-bg)))
   `(match ((t :foreground ,boss-yellow-bright :background ,boss-bg :underline t)))

   ;; dired-mode
   '(dired-directory ((t :inherit font-lock-keyword-face)))
   '(dired-flagged ((t :inherit diff-hl-delete)))
   `(dired-marked ((t :foreground ,boss-purple)))
   `(dired-symlink ((t :foreground ,boss-green)))

   ;; ivy
   `(ivy-current-match ((t :foreground ,boss-blue-dark :weight bold)))
   `(ivy-minibuffer-match-face-1 ((t :background ,boss-bg :foreground ,boss-yellow-bright)))
   `(ivy-minibuffer-match-face-2 ((t :inherit ivy-minibuffer-match-face-1)))
   `(ivy-minibuffer-match-face-3 ((t :inherit ivy-minibuffer-match-face-1)))
   `(ivy-minibuffer-match-face-4 ((t :inherit ivy-minibuffer-match-face-1)))

   ;; headers
   `(diff-file-header ((t :background ,boss-bg-light)))
   `(header-line ((t :background ,boss-bg)))
   `(magit-header-line ((t :foreground ,boss-blue-dark)))

   ;; git-commit
   `(git-commit-comment-action  ((t :foreground ,boss-green :weight bold)))
   `(git-commit-comment-branch  ((t :foreground ,boss-blue-med :weight bold)))
   `(git-commit-comment-heading ((t :foreground ,boss-red :weight bold)))

   ;; magit
   `(magit-section-heading ((t :foreground ,boss-red :weight bold)))
   `(magit-diff-revision-summary-highlight ((t :foreground ,boss-red :weight bold)))
   `(magit-diff-file-heading ((t :weight bold)))
   `(magit-diff-file-heading-highlight ((t :background ,boss-bg-light :weight bold)))
   `(magit-diff-file-heading-selection ((t :foreground ,boss-red :background ,boss-bg-light :weight bold)))
   `(magit-diff-added ((t :foreground ,boss-green)))
   `(magit-diff-added-highlight ((t :foreground ,boss-green)))
   `(magit-diff-removed ((t :foreground ,boss-red)))
   `(magit-diff-removed-highlight ((t :foreground ,boss-red)))
   `(magit-diff-hunk-heading ((t :background ,boss-bg-light :foreground ,boss-mono-light)))
   `(magit-diff-hunk-heading-highlight ((t :foreground ,boss-bg :background ,boss-mono-light)))
   `(magit-diff-hunk-heading-selection ((t :foreground ,boss-purple :background ,boss-mono-dark)))
   `(magit-diff-context ((t :foreground ,boss-fg)))
   `(magit-diff-context-highlight ((t :foreground ,boss-fg)))
   `(magit-diffstat-added ((t :foreground ,boss-green)))
   `(magit-diffstat-removed ((t :foreground ,boss-red)))
   `(magit-process-ok ((t :foreground ,boss-green)))
   `(magit-process-ng ((t :foreground ,boss-red)))
   `(magit-log-author ((t :foreground ,boss-red-bright)))
   `(magit-log-date ((t :foreground ,boss-mono-med)))
   `(magit-log-graph ((t :foreground ,boss-mono-light)))
   `(magit-sequence-pick ((t :foreground ,boss-red-bright)))
   `(magit-sequence-stop ((t :foreground ,boss-green)))
   `(magit-sequence-part ((t :foreground ,boss-red)))
   `(magit-sequence-head ((t :foreground ,boss-blue-med)))
   `(magit-sequence-drop ((t :foreground ,boss-red)))
   `(magit-sequence-done ((t :foreground ,boss-mono-med)))
   `(magit-sequence-onto ((t :foreground ,boss-mono-med)))
   `(magit-bisect-good ((t :foreground ,boss-green-bright)))
   `(magit-bisect-skip ((t :foreground ,boss-red)))
   `(magit-bisect-bad ((t :foreground ,boss-red)))
   `(magit-blame-heading ((t :background ,boss-mono-dark :foreground ,boss-mono-med)))
   `(magit-blame-hash ((t :background ,boss-mono-dark :foreground ,boss-purple)))
   `(magit-blame-name ((t :background ,boss-mono-dark :foreground ,boss-red-bright)))
   `(magit-blame-date ((t :background ,boss-mono-dark :foreground ,boss-mono-dark)))
   `(magit-blame-summary ((t :background ,boss-mono-dark :foreground ,boss-mono-med)))
   `(magit-dimmed ((t :foreground ,boss-mono-med)))
   `(magit-hash ((t :foreground ,boss-purple)))
   `(magit-tag ((t :foreground ,boss-red :weight bold)))
   `(magit-branch-remote ((t :foreground ,boss-red :weight bold)))
   `(magit-branch-local ((t :foreground ,boss-blue-med :weight bold)))
   `(magit-branch-current ((t :foreground ,boss-blue-med :weight bold :box t)))
   `(magit-head ((t :foreground ,boss-blue-med :weight bold)))
   `(magit-refname ((t :background ,boss-bg :foreground ,boss-fg :weight bold)))
   `(magit-refname-stash ((t :background ,boss-bg :foreground ,boss-fg :weight bold)))
   `(magit-refname-wip ((t :background ,boss-bg :foreground ,boss-fg :weight bold)))
   `(magit-signature-good ((t :foreground ,boss-green-bright)))
   `(magit-signature-bad ((t :foreground ,boss-red)))
   `(magit-signature-untrusted ((t :foreground ,boss-red)))
   `(magit-cherry-unmatched  ((t :foreground ,boss-blue-med)))
   `(magit-cherry-equivalent ((t :foreground ,boss-purple)))
   `(magit-reflog-commit ((t :foreground ,boss-green-bright)))
   `(magit-reflog-amend ((t :foreground ,boss-purple)))
   `(magit-reflog-merge ((t :foreground ,boss-green-bright)))
   `(magit-reflog-checkout ((t :foreground ,boss-blue-med)))
   `(magit-reflog-reset ((t :foreground ,boss-red)))
   `(magit-reflog-rebase ((t :foreground ,boss-purple)))
   `(magit-reflog-cherry-pick ((t :foreground ,boss-green-bright)))
   `(magit-reflog-remote ((t :foreground ,boss-blue-med)))
   `(magit-reflog-other ((t :foreground ,boss-blue-med)))

   ;; markdown
   `(markdown-list-face ((t :foreground ,boss-red)))

   ;; hydra
   `(hydra-face-teal ((t :foreground ,boss-blue-light)))
   `(hydra-face-amaranth ((t :foreground ,boss-red-bright)))

   ;; eshell
   `(eshell-prompt ((t :foreground ,boss-blue-dark)))
   `(eshell-ls-executable ((t :foreground ,boss-green-bright)))
   `(eshell-ls-directory ((t :foreground ,boss-blue-med)))
   `(eshell-ls-archive ((t :foreground ,boss-fg)))
   `(eshell-ls-backup ((t :foreground ,boss-fg)))
   `(eshell-ls-clutter ((t :foreground ,boss-fg)))
   `(eshell-ls-missing ((t :foreground ,boss-fg)))
   `(eshell-ls-product ((t :foreground ,boss-fg)))
   `(eshell-ls-readonly ((t :foreground ,boss-fg)))
   `(eshell-ls-special ((t :foreground ,boss-fg)))
   `(eshell-ls-symlink ((t :foreground ,boss-fg)))
   `(eshell-ls-unreadable ((t :foreground ,boss-fg)))
   `(nobreak-space ((t :foreground nil :background nil)))

   ;; terminal
   `(term-color-black ((t :foreground ,boss-fg)))
   `(term-color-white ((t :background ,boss-bg-highlight)))
   `(term-color-blue ((t :foreground ,boss-blue-dark)))
   `(term-color-cyan ((t :foreground ,boss-blue-light)))
   `(term-color-green ((t :foreground ,boss-green)))
   `(term-color-magenta ((t :foreground ,boss-purple)))
   `(term-color-red ((t :foreground ,boss-red)))
   `(term-color-yellow ((t :foreground ,boss-yellow)))

   ;; w3m
   `(w3m-anchor ((t :foreground ,boss-blue-med)))
   `(w3m-image-anchor ((t :foreground ,boss-mono-dark :background ,boss-bg)))
   `(w3m-arrived-anchor ((t :foreground ,boss-purple :background ,boss-bg)))
   `(w3m-header-line-content ((t :foreground ,boss-blue-light :background ,boss-bg-light)))
   `(w3m-header-line-title ((t :foreground ,boss-yellow :background ,boss-bg-light)))

   ;; line-numbers
   `(line-number ((t :foreground ,boss-mono-light :background ,boss-bg-light)))
   `(line-number-current-line
     ((t :foreground ,boss-mono-white :background ,boss-bg-highlight :weight bold)))

   ;; mode-line
   `(mode-line ((t :background ,boss-mono-light :foreground ,boss-bg-light)))
   `(mode-line-buffer-id ((t :weight bold)))
   `(mode-line-emphasis ((t :weight bold)))
   `(mode-line-inactive ((t :background ,boss-bg-shadow)))
   ))

(provide-theme 'boss)
