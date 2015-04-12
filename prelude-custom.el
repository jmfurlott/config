(defun eshell/clear ()
          "To clear the eshell buffer."
            (interactive)
              (let ((inhibit-read-only t))
                   (erase-buffer)))


(add-to-list 'default-frame-alist '(font .  "ProggyCleanTT-12" ))

(set-face-bold-p 'bold nil)

(set-frame-parameter (selected-frame) 'alpha '(90 75))
(add-to-list 'default-frame-alist '(alpha 90 75))

;; aliases
(defalias 'ff 'find-file)
(defalias 'ffow 'find-file-other-window)

;; hide scroll bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq prelude-theme 'darkburn)
