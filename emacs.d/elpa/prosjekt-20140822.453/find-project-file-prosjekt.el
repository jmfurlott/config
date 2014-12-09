;;; find-project-file-prosjekt.el --- Finds a file from the current project,
;;; using completing-read (or ido-completing-read as appropriate).

;; This file is NOT part of GNU emacs.

;;; Commentary:

;;; Code:

;;;###autoload
(defun prosjekt-find-project-file ()
  "Find a file in the current project."
  (interactive)
  (let* ((completion-list (prosjekt-proj-files))
         (prompt "Find file: ")
         (choice (if (and (boundp 'ido-mode) ido-mode)
                     (ido-completing-read prompt completion-list nil t)
                   (completing-read prompt completion-list nil t))))
    (find-file (concat (file-name-as-directory prosjekt-proj-dir) choice))))

(provide 'find-project-file-prosjekt)

;;; find-project-file-prosjekt.el ends here
