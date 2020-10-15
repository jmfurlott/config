;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joe Furlott"
      user-mail-address "joe.furlott@datadoghq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn)

(custom-set-faces!
  '(org-hide :foreground "#3F3F3F")
  '(cursor :foreground "#FFA500")
  '(cursor :background "#FFA500")
)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-journal-dir "~/org/journal")
(use-package! org-superstar
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-hide-leading-stars t)
)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 15))
(setq doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :size 14))

;; For doom-modeline
(set-face-attribute 'mode-line nil :height 130)

(add-hook 'js2-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'typescript-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'web-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'scss-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'javascript-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'markdown-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'js-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'ruby-mode-hook 'display-fill-column-indicator-mode)



;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(evil-ex-define-cmd "W" 'evil-write)
(setq evil-want-fine-undo t)
(global-set-key (kbd "C-x C-c") 'nil)


;; (treemacs-follow-mode t)

(setq auth-sources '("~/.authinfo"))

(map! :leader (:prefix ("r" . "eradio") :desc "Play a radio channel" "p" 'eradio-play))
(map! :leader (:prefix ("r" . "eradio") :desc "Stop the radio player" "s" 'eradio-stop))
(setq eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")          ;; electronica with defcon-speaker bumpers
			("vaporwaves - soma fm" . "https://somafm.com/vaporwaves.pls")      ;; vaporwaves
                       ))
