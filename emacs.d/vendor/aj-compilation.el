;; Compilation mode
;; Compilation ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq compilation-scroll-output t)
;;(setq compilation-window-height nil)

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))


(defvar aj-compilation-saved-window-configuration nil
  "Previous window conf from before a compilation")

(defvar aj-compile-command ""
  "The compile command used by compilation-start since
  `compile-command' is only saved by `compile' command.")

;; Hide *compilation* buffer if compile didn't give erros
(defadvice compilation-start (before aj-compilation-save-window-configuration(command comint))
  "Save window configuration before compilation in
`aj-compilation-saved-window-configuration'"

  ;; compile command is not saved in compilation-start function only in
  ;; compile function (rgrep only uses compilation-start)
  (setq aj-compile-command command)
  ;; Save window configuration
  (setq aj-compilation-saved-window-configuration
        (current-window-configuration)))
(ad-activate 'compilation-start)

;; compilation-handle-exit returns (run-hook-with-args
;; 'compilation-finish-functions cur-buffer msg) Could use but it only
;; got a string describing status
(defadvice compilation-handle-exit
  (after aj-compilation-exit-function(process-status exit-status msg))
  "Hack to restore window conf"
  (let ((hide (string-match "find" aj-compile-command)))
    (when (and (eq process-status 'exit)
               (zerop exit-status)
               ;; Not nil and not 0 means that command was "find" at
               ;; pos 0 which means that I don't want to restore the
               ;; layout
               (not (and (integerp hide) (zerop hide))))
      (set-window-configuration aj-compilation-saved-window-configuration))))
(ad-activate 'compilation-handle-exit)

(provide 'aj-compilation)
