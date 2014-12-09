(deftheme badwolf "Badwolf color Theme")

(let ((class '((class color) (min-colors 89)))
      (bwc-plain "#f8f6f2")
      (bwc-snow "#ffffff")
      (bwc-coal "#000000")
      (bwc-brightgravel "#d9cec3")
      (bwc-lightgravel "#998f84")
      (bwc-gravel "#857f78")
      (bwc-mediumgravel "#666462")
      (bwc-deepgravel "#45413b")
      (bwc-deepergravel "#35322d")
      (bwc-darkgravel "#242321")
      (bwc-blackgravel "#1c1b1a")
      (bwc-blackestgravel "#141413")
      (bwc-dalespale "#fade3e")
      (bwc-dirtyblonde "#f4cf86")
      (bwc-taffy "#ff2c4b")
      (bwc-saltwatertaffy "#8cffba")
      (bwc-tardis "#0a9dff")
      (bwc-darktardis "#005fff")
      (bwc-orange "#ffa724")
      (bwc-lime "#aeee00")
      (bwc-dress "#ff9eb8")
      (bwc-toffee "#b88853")
      (bwc-coffee "#c7915b")
      (bwc-darkroast "#88633f")
      (bwc-term-blue "#6298c8")
      (bwc-term-green "#82d92f")
      (bwc-term-yellow "#f3e14c")
      (bwc-term-red "#e5261f"))

  (defface powerline-normal-1
    `((t (:foreground ,bwc-blackgravel :background ,bwc-lime :weight bold)))
    "Powerline normal 1")
  (defface powerline-normal-2
    `((t (:foreground ,bwc-dirtyblonde :background ,bwc-deepgravel)))
    "Powerline normal 2")
  (defface powerline-normal-3
    `((t (:foreground ,bwc-saltwatertaffy :background ,bwc-darkgravel)))
    "Powerline normal 3")
  (defface powerline-insert-1
    `((t (:foreground ,bwc-blackgravel :background ,bwc-tardis :weight bold)))
    "Powerline insert 1")
  (defface powerline-replace-1
    `((t (:foreground ,bwc-blackgravel :background ,bwc-dress :weight bold)))
    "Powerline replace 1")
  (defface powerline-visual-1
    `((t (:foreground ,bwc-blackgravel :background ,bwc-orange :weight bold)))
    "Powerline visual 1")
  (defface powerline-emacs-1
    `((t (:foreground ,bwc-blackgravel :background ,bwc-brightgravel :weight bold)))
    "Powerline emacs 1")
  (defface powerline-insert-2
    `((t (:foreground ,bwc-dirtyblonde :background ,bwc-darktardis)))
    "Powerline insert 2")
  (defface powerline-visual-2
    `((t (:foreground ,bwc-blackgravel :background ,bwc-dalespale)))
    "Powerline visual 2")
  (defface powerline-insert-3
    `((t (:foreground ,bwc-tardis :background ,bwc-darkgravel)))
    "Powerline insert 3")
  (defface powerline-visual-3
    `((t (:foreground ,bwc-blackgravel :background ,bwc-toffee)))
    "Powerline visual 3")

  (custom-theme-set-faces
   'badwolf
   `(default ((t (:inherit nil :foreground ,bwc-plain :background ,bwc-blackestgravel))))
   `(cursor ((t (:background ,bwc-tardis))))
   `(region ((t (:foreground nil :background ,bwc-mediumgravel ))))
   `(fringe ((t (:background ,bwc-blackestgravel))))
   `(minibuffer-prompt ((t (:foreground ,bwc-lime))))
   `(link ((t (:foreground ,bwc-lightgravel :underline t))))
   `(link-visited ((t (:inherit link :foreground ,bwc-orange))))
   `(highlight ((t (:foreground ,bwc-coal :background ,bwc-dalespale))))
   `(hl-line ((t (:inherit nil :background ,bwc-darkgravel))))
   `(linum ((t (:foreground ,bwc-mediumgravel))))
   `(isearch ((t (:foreground ,bwc-coal :background ,bwc-dalespale :weight bold))))
   `(lazy-highlight ((t (:foreground ,bwc-coal :background, bwc-dalespale :weight bold))))
   `(mode-line
     ((t (:box (:line-width -1 :style released-button)
               :foreground ,bwc-brightgravel :background ,bwc-darkgravel))))
   `(mode-line-inactive
     ((t (:box (:line-width -1 :style released-button)
               :foreground ,bwc-snow :background ,bwc-deepgravel))))

   `(font-lock-comment-face ((t (:foreground ,bwc-lightgravel :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,bwc-lightgravel :slant italic))))
   `(font-lock-doc-face ((t (:foreground ,bwc-dirtyblonde))))
   `(font-lock-string-face ((t (:foreground ,bwc-dirtyblonde :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,bwc-orange))))
   `(font-lock-variable-name-face ((t (:foreground ,bwc-dress))))
   `(font-lock-builtin-face ((t (:foreground ,bwc-taffy))))
   `(font-lock-keyword-face ((t (:foreground ,bwc-lime :weight bold))))
   `(font-lock-type-face ((t (:foreground ,bwc-saltwatertaffy))))
   `(font-lock-constant-face ((t (:foreground ,bwc-toffee :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,bwc-dress :weight bold))))
   `(show-paren-match ((t (:background ,bwc-tardis :weight bold))))
   `(show-paren-mismatch ((t (:background ,bwc-taffy :weight bold))))

   `(haskell-interactive-face-compile-error ((t (:foreground ,bwc-taffy :weight bold))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,bwc-snow))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,bwc-orange))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,bwc-saltwatertaffy))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,bwc-dress))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,bwc-term-blue))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,bwc-coffee))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,bwc-dirtyblonde))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,bwc-term-green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,bwc-term-yellow))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,bwc-lightgravel))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,bwc-saltwatertaffy))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,bwc-taffy :weight bold))))

   `(web-mode-doctype-face ((t (:foreground ,bwc-lime :weight bold))))
   `(web-mode-html-tag-face ((t (:foreground ,bwc-coffee :weight bold))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,bwc-coffee :weight bold))))
   `(web-mode-html-attr-name-face ((t (:foreground ,bwc-toffee))))

   `(term-color-blue ((t (:foreground ,bwc-term-blue))))
   `(term-color-green ((t (:foreground ,bwc-term-green))))
   `(term-color-yellow ((t (:foreground ,bwc-term-yellow))))
   `(term-color-red ((t (:foreground ,bwc-term-red))))

   `(whitespace-tab ((t (:background ,bwc-blackestgravel :foreground ,bwc-deepgravel))))
   `(whitespace-newline ((t (:foreground ,bwc-deepgravel))))

   `(linum-relative-current-face
     ((t (:background ,bwc-blackestgravel :foreground ,bwc-dalespale :weight bold))))

   `(company-tooltip ((t (:background ,bwc-darkgravel :foreground ,bwc-brightgravel))))
   `(company-tooltip-common ((t (:background ,bwc-darkgravel :foreground ,bwc-taffy :weight bold))))
   `(company-tooltip-selection ((t (:background ,bwc-deepgravel :foreground ,bwc-brightgravel))))
   `(company-tooltip-common-selection
     ((t (:background ,bwc-deepgravel :foreground ,bwc-taffy :weight bold))))
   `(company-scrollbar-bg ((t (:background ,bwc-deepgravel :foreground ,bwc-snow))))
   `(company-scrollbar-fg ((t (:background ,bwc-lightgravel :foreground ,bwc-snow))))
   `(company-preview ((t (:background ,bwc-darkgravel :foreground ,bwc-term-blue :weight bold))))
   `(company-preview-common
     ((t (:background ,bwc-darkgravel :foreground ,bwc-taffy :weight bold))))
   `(company-preview-search
     ((t (:background ,bwc-darkgravel :foreground ,bwc-dalespale :weight bold))))

   `(magit-branch ((t (:background ,bwc-blackestgravel :foreground ,bwc-term-blue :weight bold))))
   `(magit-log-sha1 ((t (:background ,bwc-blackestgravel :foreground ,bwc-taffy))))
   `(magit-section-title
     ((t (:background ,bwc-blackestgravel :foreground ,bwc-dalespale :weight bold))))
   `(magit-diff-add ((t (:background ,bwc-blackestgravel :foreground ,bwc-term-green))))
   `(magit-diff-del ((t (:background ,bwc-blackestgravel :foreground ,bwc-term-red))))
   `(magit-diff-none ((t (:background ,bwc-blackestgravel :foreground ,bwc-lightgravel))))
   `(magit-item-highlight ((t (:background ,bwc-blackestgravel :foreground ,bwc-snow :weight bold))))
   `(magit-diff-file-header ((t (:background ,bwc-blackestgravel :foreground ,bwc-saltwatertaffy))))
   `(magit-diff-hunk-header ((t (:background ,bwc-blackestgravel :foreground ,bwc-saltwatertaffy))))

   `(font-latex-verbatim-face ((t (:inherit nil :foreground "burlywood"))))

   `(org-date ((t (:background ,bwc-blackestgravel :foreground ,bwc-saltwatertaffy :underline nil))))
   `(org-level-3 ((t (:background ,bwc-blackestgravel :foreground ,bwc-lime :weight normal))))
   `(org-level-4 ((t (:background ,bwc-blackestgravel :foreground ,bwc-coffee))))
   `(org-level-6 ((t (:background ,bwc-blackestgravel :foreground ,bwc-tardis :weight normal)))))

  (custom-set-faces
   `(ein:cell-input-area ((t (:background ,bwc-blackestgravel :inherit nil))))
   `(ein:cell-input-prompt ((t (:foreground ,bwc-orange :background nil :inherit nil))))
   `(ein:cell-output-prompt ((t (:foreground ,bwc-taffy :background nil :inherit nil))))
   `(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
   `(ac-candidate-face ((t (:background ,bwc-lightgravel))))
   `(ac-selection-face ((t (:foreground ,bwc-coal :background ,bwc-orange))))
   `(flymake-errline ((t (:background nil :underline ,bwc-taffy ))))
   `(flymake-warnline ((t (:background nil :underline ,bwc-dress ))))

   `(helm-selection ((t (:inherit nil :background ,bwc-deepgravel :underline nil))))
   `(helm-source-header
     ((t (:inherit nil :foreground ,bwc-saltwatertaffy :background ,bwc-blackestgravel
                   :family "Source Code Pro" :height 1.2))))
   `(helm-M-x-key ((t (:inherit nil :foreground ,bwc-orange :underline nil))))
   `(helm-candidate-number
     ((t (:inherit nil :foreground ,bwc-dalespale :background ,bwc-darkgravel :weight bold))))
   `(helm-buffer-saved-out
     ((t (:inherit nil :foreground ,bwc-taffy :background ,bwc-blackestgravel :weight bold))))
   `(helm-ff-directory
     ((t (:inherit nil :foreground ,bwc-term-blue :background ,bwc-blackestgravel :weight bold))))
   `(helm-buffer-directory
     ((t (:inherit nil :foreground ,bwc-term-blue :background ,bwc-blackestgravel :weight bold))))
   `(helm-ff-file ((t (:inherit nil :foreground ,bwc-snow :background ,bwc-blackestgravel))))
   `(helm-match
     ((t (:inherit nil :foreground ,bwc-dalespale :background ,bwc-blackestgravel :weight bold))))
   `(elm-grep-lineno
     ((t (:inherit nil :foreground ,bwc-mediumgravel :background ,bwc-blackestgravel))))
   `(helm-moccur-buffer
     ((t (:inherit nil :foreground ,bwc-saltwatertaffy :background ,bwc-blackestgravel :underline nil))))))


(provide-theme 'badwolf)
