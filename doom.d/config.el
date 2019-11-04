;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-theme 'hc-zenburn)
(setq evil-normal-state-cursor '(box "#ffa500")
      evil-insert-state-cursor '(bar "#ffa500")
      evil-visual-state-cursor '(hollow "#ffa500"))

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 16))

(setq evil-want-fine-undo t)
(evil-ex-define-cmd "W" 'evil-write)
(global-vi-tilde-fringe-mode -1)

(global-set-key (kbd "C-x C-c") 'nil)

(setq doom-modeline-height 1)
(set-face-attribute 'mode-line nil :height 80)
(set-face-attribute 'mode-line-inactive nil :height 80)
