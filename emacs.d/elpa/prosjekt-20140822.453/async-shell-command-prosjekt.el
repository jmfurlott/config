;;; async-shell-command-prosjekt.el --- Adds support for running a background
;;; command in prosjekt.

;; This file is NOT part of GNU emacs.

;;; Commentary:
;; This adds a new function prosjekt-run-async-shell-command which
;; allows you to run a process in the background and get notified if it
;; succeeds or fails. As a bonus, if you use popwin, you get a nice
;; popup

;;; Code:

(defun prosjekt-display-process-status (description process)
  (message "%s... Process status: %S"
           description
           (process-status process)))

(defun prosjekt-run-async-shell-command (description command &optional on-success on-error)
  "Run an arbitrary shell command asynchronously. Calls 0-ary
  functions on-success and on-error when appropriate"
  (let ((cwd default-directory)
        (project-name (cdr (assoc "name" prosjekt-proj)))
        (process-buffer "*prosjekt-async-command*"))
    (unwind-protect
        (progn
          (cd prosjekt-proj-dir)
          (let ((process (start-process-shell-command project-name
                                                      process-buffer
                                                      command)))
            (prosjekt-display-process-status description process)
            (run-with-timer 1 nil
                            'prosjekt-check-process-status
                            description process on-success on-error)))
      (cd cwd))))

(defun prosjekt-check-process-status (description process on-success on-error)
  (prosjekt-display-process-status description process)
  (case (process-status process)
    (exit (if (eql 0 (process-exit-status process))
              (progn (and on-success (funcall on-success))
                     (message "%s: Success!" description)
                     (kill-buffer (process-buffer process)))
            (and on-error (funcall on-error))
            (message "%s: Failed!" description)
            (display-buffer (process-buffer process))))
    (run (run-with-timer 1 nil
                         'prosjekt-check-process-status
                         description process on-success on-error))))

(provide 'async-shell-command-prosjekt)

;;; async-shell-command-prosjekt.el ends here
