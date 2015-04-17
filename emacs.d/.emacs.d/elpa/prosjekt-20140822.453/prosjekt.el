;;; prosjekt.el --- a software project tool for emacs
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.3
;; URL: https://github.com/abingham/prosjekt
;; Package-Requires: ((dash "2.8.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2012 Austin Bingham
;;
;;; Commentary:
;;
;; Description:
;;
;; prosjekt is a simple software project management tool. A project
;; in prosjekt comprises 1) a top-level directory, 2) a collection
;; of files belonging to the project, and 3) a set of commands that
;; can be executed.
;;
;; For more details, see the project page at
;; https://github.com/abingham/prosjekt.
;;
;; Installation:
;;
;; Copy prosjekt.el to some location in your emacs load path. Then add
;; "(require 'prosjekt)" to your emacs initialization (.emacs,
;; init.el, or something). 
;; 
;; Installation (anything integration):
;; 
;; Prosjekt comes with integration with anything
;; (http://emacswiki.org/emacs/Anything). To enable this, copy
;; anything-prosjekt.el to your emacs load path. Then add "(require
;; 'anything-prosjekt)" to you emacs initialization. This provides the
;; anything sources "anything-c-source-prosjekt-files" and
;; "anything-c-source-prosjekt-projects".
;;
;; Example config:
;; 
;;   (require 'prosjekt)
;;   (require 'anything-prosjekt)
;; 
;;   (require 'anything)
;;   (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
;;   (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)
;; 
;; Tool descriptions: 
;;
;; The ":tools" section of a project defines commands which are
;; associated with the project. Each tool has a name, a function run
;; for the tool, an optional sequence of keybindings. A tool description looks like this:
;;   ((:name . "name of tool")
;;    (:command ...tool function...))
;;    (:keys ...list of keybinding...))
;;
;; for example:
;;
;;   ((:name . "git status")
;;    (:command git-status)
;;    (:keys "[f5]"))
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'thingatpt) ; for read-from-whole-string
(require 'dash)	     ; for -any?

(defmacro prosjekt-with-cfg (&rest body)
    "Load the global config, bind it to `cfg`, run BODY, and save the global config."
    `(let ((cfg (prosjekt-cfg-load)))
       (progn ,@body)
       (prosjekt-cfg-save cfg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC API                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; minor-mode-map-alist entry
(defvar prosjekt-mode t)

(defvar prosjekt-open-hooks '()
  "Hooks run after any project is opened.")

(defvar prosjekt-close-hooks '()
  "Hooks run before any project is closed.")

(defvar prosjekt-ignore-dirs '(".svn" ".git" ".hg" ".bzr" ".cvs")
  "Directories which are ignored when populating projects.")

(defconst prosjekt-format-version 3
  "The current format version for projects.")

;;;###autoload
(defun prosjekt-new (directory name)
  "Create a new project."
  (interactive
   (list
    (read-directory-name "Create project in directory: ")
    (read-string "Project name: ")))
  (prosjekt-with-cfg
   (let ((proj-list (prosjekt-cfg-project-list cfg))
	 (proj-file (expand-file-name (concat name ".prosjekt") directory)))
     
					; TODO: Give option to overwrite.
     (if (file-exists-p proj-file)
	 (error "Project file %s already exists." proj-file))
     
     (prosjekt-close)
     
					; Write the new project file
     (prosjekt-write-object-to-file
      (prosjekt-default-project)
      proj-file)
     
					; Update the global project list
     (prosjekt-cfg-add-project cfg proj-file)
     
					; Load it like normal
     (prosjekt-open proj-file))))

;;;###autoload
(defun prosjekt-delete (filename)
  "Delete an existing project."
  (interactive
   (list
    (read-file-name "Delete project: ")))

  (prosjekt-with-cfg
   (let ((filename (expand-file-name filename))
	 (proj-list (prosjekt-cfg-project-list cfg)))
					; First, close the current
					; project if it's the one
					; being deleted.
     (ignore-errors
       (if (equal prosjekt-proj-file filename)
	   (prosjekt-close)))

					; Update the global project
					; list
     (prosjekt-cfg-remove-project cfg filename))))

;;;###autoload
(defun prosjekt-open-recent (filename)
  "Open a project named PROJ in the recent-list."
  (interactive
   (list
    (completing-read "Open project: " 
		     (prosjekt-cfg-project-list (prosjekt-cfg-load)))))
  (prosjekt-open filename))

;;;###autoload
(defun prosjekt-open (filename)
  "Open the project defined in FILENAME."
  (interactive
   (list
    (read-file-name "Project file: ")))
  (prosjekt-close)
  (setq prosjekt-proj-file filename)
  (setq prosjekt-proj-dir (file-name-directory filename))
  (setq prosjekt-proj 
	(prosjekt-upgrade 
	 (prosjekt-read-object-from-file prosjekt-proj-file)))
  (prosjekt-setkeys (prosjekt-proj-tools))
  (prosjekt-set-hooks)
  (let ((curfile (prosjekt-proj-curfile)))
    (if curfile 
	(find-file 
	 (expand-file-name curfile prosjekt-proj-dir))))
  (mapc 'funcall prosjekt-open-hooks)
  (mapc 'funcall (prosjekt-proj-open-hooks)))

(defun prosjekt-upgrade-from (project from_version)
  "Upgrade a project format (except for the version tag itself) by one version."
  (message "Upgrading project format from version %s to version %s" from_version (1+ from_version))
  (cond ((= from_version 1)
	 (cons (list :ignores) 
	       (assq-delete-all :populate-spec project)))
	((= from_version 2)
	 (cons (list :includes) project))
	(t project)))

(defun prosjekt-upgrade (project)
  "Upgrade a project to the current format."
  (let ((from_version (cdr (assoc :version project))))
    (cond ((= from_version prosjekt-format-version)
	   project)
	  ((> from_version prosjekt-format-version)
	   (error "Project format is from the future! Upgrade prosjekt."))
	  (t (let ((project (prosjekt-upgrade-from project from_version)))
	       (setcdr (assoc :version project) (+ from_version 1))
	       (prosjekt-upgrade project))))))

;;;###autoload
(defun prosjekt-clone (directory name clone_from)
  "Clone a new project from an existing project."
  (interactive
   (list
    (read-directory-name 
     "Create project in directory: ")
    (read-string 
     "Project name: ")
    (read-file-name
     "Clone from existing project: ")))

  (prosjekt-with-cfg
   (let* ((proj (prosjekt-read-object-from-file clone_from))
	  (proj-file (expand-file-name (concat name ".prosjekt") directory)))

					; close any existing project
     (prosjekt-close)

					; activate the new project
					; specification
     (setq prosjekt-proj-file proj-file)
     (setq prosjekt-proj-dir directory)
     (setq prosjekt-proj proj)

					; Activate the keybindings for
					; the new project
     (prosjekt-setkeys (prosjekt-proj-tools))
     
					; Update the global project
					; list
     (prosjekt-cfg-add-project cfg proj-file))))

;;;###autoload
(defun prosjekt-save ()
  "Save the current project."
  (interactive)
  (if prosjekt-proj
      (prosjekt-write-object-to-file
       prosjekt-proj
       prosjekt-proj-file)))

(defmacro defun-autosave (name args &rest body)
  "Define a function which automatically calls 'prosjekt-save at
the end"
  `(defun ,name ,args ,@body (prosjekt-save)))

;;;###autoload
(defun prosjekt-close ()
  "Close the current project."
  (interactive)

  ; Run global close hooks
  (mapc 'funcall prosjekt-close-hooks)

  ; Run project close hooks if there's an active project.
  (if prosjekt-proj
      (mapc 'funcall (prosjekt-proj-close-hooks)))

  (prosjekt-save)
  (setq prosjekt-proj nil)
  (setq prosjekt-proj-file nil)
  (setq prosjekt-proj-dir nil)
  (prosjekt-reset-keys)
  (prosjekt-clear-hooks)
  )

; TODO: Normalize the error messages. "No project open." everywhere,
; or whatever.
(defun-autosave prosjekt-clear ()
  "Remove all files from the current project."
  (interactive)
  (prosjekt-proj-clear-file-hash))

;;;###autoload
(defun prosjekt-setup ()
  "Edit the project configuration in a new buffer."
  (interactive)
  (unless prosjekt-proj (error "No current project."))
  (cond ((buffer-live-p prosjekt-buffer)
	 (switch-to-buffer prosjekt-buffer))
	(t
	 (setq prosjekt-buffer (get-buffer-create "*prosjekt*"))
	 (switch-to-buffer prosjekt-buffer)
	 (emacs-lisp-mode)

	 (let ((keymap (make-sparse-keymap)))
	   (define-key keymap [escape] 'prosjekt-setup-save-and-close)
	   (define-key keymap [C-escape] 'prosjekt-setup-save)
	   (use-local-map keymap))
	 
	 (insert 
	  (pp-to-string 
	   (reduce 
	    (lambda (seq key) (assq-delete-all key seq))
	    prosjekt-private-fields 
	    :initial-value (copy-alist prosjekt-proj))))

	 (goto-char (point-min))
	 ) ; t
	)  ; cond
  )        ; defun

(defun-autosave prosjekt-add (f)
  "Add a file to the current project."
  (interactive
   (let ((_ (unless prosjekt-proj (error "No project open."))))
     (list
      (read-file-name "Add file to project: " nil nil t nil))))
  
  (prosjekt-insert-file f))

(defun-autosave prosjekt-populate (dir ignores includes)
  "Add all files under DIR which a) matches a pattern in INCLUDES
and b) matches no pattern in IGNORES"
  (unless prosjekt-proj-dir (error "No project opened."))
  (prosjekt-walk-path
   dir
   (lambda (dir file)
     (let* ((fullname (concat (file-name-as-directory dir) file))
	    (is-dir (file-directory-p fullname))
	    (ignored (-any? (lambda (p) (string-match p fullname)) ignores))
	    (included (-any? (lambda (p) (string-match p fullname)) includes)))
       (if (and 
	    (not is-dir)
	    (not ignored) 
	    included)
	   (prosjekt-insert-file fullname)))
     t)))

;;;###autoload
(defun prosjekt-repopulate ()
  "Repopulate the project."
  (interactive)
  (unless prosjekt-proj-dir (error "No project opened."))
  (prosjekt-clear)
  (prosjekt-populate 
   prosjekt-proj-dir
   (prosjekt-proj-ignores)
   (prosjekt-proj-includes)))

;;;###autoload
(defun prosjekt-run-tool-by-name (name)
  (interactive
   (list
    (completing-read "Command name: "
                     (prosjekt-tool-names))))
  (let ((tool (prosjekt-find-tool-by-name name)))
    (if tool
        (let* ((default-directory (or prosjekt-proj-dir default-directory))
               (command (cdr (assoc :command tool)))
               (is-interactive (interactive-form command)))
          (if is-interactive
              (call-interactively command)
            (eval command))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; active-project related stuff.

(defvar prosjekt-proj nil
  "The current project definition.")

(defvar prosjekt-proj-dir nil
  "The directory of the current project.")

(defun prosjekt-proj-check ()
  "Check if a project is currently open, throwing an error if not."
  (unless prosjekt-proj 
    (error "No project is open.")))

(defun prosjekt-proj-get-item_ (name)
  (prosjekt-proj-check)
  (cdr (assoc name prosjekt-proj)))

(defun prosjekt-proj-set-item_ (name val)
  (prosjekt-proj-check)
  (setcdr (assoc name prosjekt-proj) val))

(defun prosjekt-proj-tools ()
  (prosjekt-proj-get-item_ :tools))

(defun prosjekt-proj-curfile ()
  (prosjekt-proj-get-item_ :curfile))

(defun prosjekt-proj-set-curfile (fname)
  (prosjekt-proj-set-item_ :curfile fname))

(defun prosjekt-proj-open-hooks ()
  (prosjekt-proj-get-item_ :open-hooks))

(defun prosjekt-proj-close-hooks ()
  (prosjekt-proj-get-item_ :close-hooks))

(defun prosjekt-proj-ignores ()
  (prosjekt-proj-get-item_ :ignores))

(defun prosjekt-proj-includes ()
  (prosjekt-proj-get-item_ :includes))

(defun prosjekt-proj-file-hash ()
  (prosjekt-proj-get-item_ :files))

(defun prosjekt-proj-clear-file-hash ()
  (prosjekt-proj-set-item_ 
   :files 
   (make-hash-table :test 'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION DETAILS: Users should not generally need to call or look    ;;
;; below here.                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global config-related functionality

(defun prosjekt-cfg-add-project (cfg filename)
  "Add recent-file FILENAME to global config CFG."
  (let ((plist (assoc :project-list cfg)))
    (setcdr plist (cons filename (cdr plist)))))

(defun prosjekt-cfg-remove-project (cfg filename)
  "Remove recent-file FILENAME from global config CFG."
  (let ((plist (assoc :project-list cfg)))
    (setcdr plist
	    (remove* filename
		     (cdr plist)
		     :test 'equal))))

; TODO: Add clean-recent-files which is run at start?

(defun prosjekt-cfg-project-list (cfg)
  (cdr (assoc :project-list cfg)))

(defun prosjekt-cfg-load () 
  "Read the global prosjekt configuration from file."
  (let ((fname (prosjekt-cfg-file)))
    (if (file-exists-p fname)
	(prosjekt-read-object-from-file fname)
      (prosjekt-cfg-default))))

(defun prosjekt-cfg-file ()
  "Get the global configuration filename (~/.emacs.d/prosjekt.lst)"
  (expand-file-name 
   "prosjekt.lst"
   (if (boundp 'user-emacs-directory) 
       user-emacs-directory
     "~/.emacs.d/"
     )))

(defun prosjekt-cfg-save (cfg)
  "Save the global config (`prosjekt-config`) to file."
  (prosjekt-write-object-to-file
   cfg
   (prosjekt-cfg-file)))

(defun prosjekt-cfg-default ()
  '((:version . 1)
    (:project-list)
    (:last-open)))

; TODO: Include name in project definition.
(defun prosjekt-default-project ()
  (let ((files (make-hash-table :test 'equal)))
    (list
     '(:tools 
       ((:name . "sample command") 
	(:command message "add your command here") 
	(:keys "[f6]")))
     (cons :files files)
     '(:curfile . nil)
     '(:ignores ".*~")
     '(:includes ".*")
     '(:open-hooks)
     '(:close-hooks)
     `(:version . ,prosjekt-format-version)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attach prosjekt into other hooks.

(defun prosjekt-set-hooks ()
  (add-hook 'find-file-hook 'prosjekt-find-file-hook))

(defun prosjekt-clear-hooks ()
  (remove-hook 'find-file-hook 'prosjekt-find-file-hook))

(defun prosjekt-find-file-hook ()
  (let* ((abs_fname (buffer-file-name (current-buffer)))
	 (rel_fname (file-relative-name abs_fname prosjekt-proj-dir)))
    (if (gethash rel_fname (prosjekt-proj-file-hash))
	(prosjekt-proj-set-curfile rel_fname))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; active-project related stuff.

(defvar prosjekt-private-fields '(:curfile :files :version)
  "Project fields which are not displayed in the setup buffer.")

(defvar prosjekt-buffer nil
  "The buffer for prosjekt editing tasks.")

(defvar prosjekt-proj-file nil
  "The filename of the current project.")

(defun-autosave prosjekt-setup-save ()
  "Save the prosjekt-buffer contents and the new project definition."
  (interactive) ; this is needed because we bind this method to a key.
  (unless prosjekt-buffer (error "No edit in progress."))
  (unless prosjekt-proj-file (error "No current project."))
  (switch-to-buffer prosjekt-buffer)
  (let ((new-proj (read-from-whole-string (buffer-string))))
    (setq prosjekt-proj
	  (reduce (lambda (seq key)
                    (let ((private_value (assoc key prosjekt-proj)))
                      (if private_value
                          (cons (assoc key prosjekt-proj) seq)
                        seq)))
		  prosjekt-private-fields
		  :initial-value new-proj)))
  (minibuffer-message "New configuration enabled.")

  ; Update key bindings with edits
  ; TODO: Other edits to take care of?
  (prosjekt-setkeys (prosjekt-proj-tools)))

;;;###autoload
(defun prosjekt-setup-save-and-close () 
  "Save the prosjekt-buffer contents and the new project definition,
and kill that buffer."
  (interactive) ; this is needed because we bind this method to a key.
  (prosjekt-setup-save)
  (kill-buffer prosjekt-buffer)
  (setq prosjekt-buffer nil)
  )

(defun prosjekt-proj-files ()
  "Get the list of files in the active project."
  (if prosjekt-proj
      (prosjekt-hash-keys (prosjekt-proj-file-hash))))

(defun prosjekt-insert-file (f)
  (let ((files (prosjekt-proj-file-hash))
	(rel_file (file-relative-name f prosjekt-proj-dir)))
    (unless (gethash rel_file files)
      (puthash rel_file 0 files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for reading elisp code from files

;; TODO: Are there better, standard versions of these functions?
(defun prosjekt-read-object-from-file (filename)
  "Read FILENAME's complete contents and 'read' them as a lisp
  object."
  (with-temp-buffer
    (insert-file-contents filename)
    (read (buffer-string))))

(defun prosjekt-write-object-to-file (object filename)
  "Write STRING as the contents of FILENAME."
   (with-temp-buffer
     (insert (pp-to-string object))
     (when (file-writable-p filename)
       (write-file filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for dealing with the keymap and bindings.

(defun prosjekt-get-mode-map ()
  "Get the (mode . keymap) cell from minor-mode-map-alist.
This will initialize the entry if needed."
  (let ((m (assoc 'prosjekt-mode minor-mode-map-alist)))
    (or	m
	(let ((mode (cons 'prosjekt-mode (make-sparse-keymap))))
	  (car (push mode minor-mode-map-alist))))))

(defun prosjekt-reset-keys ()
  "Clear the keybindings for the minor mode."
  (setcdr (prosjekt-get-mode-map) (make-sparse-keymap)))

(defun prosjekt-setkeys (tools)
  "Set a series of tools in the minor mode.
TOOLS is a list of keybinding descriptions."
  (let ((keymap (cdr (prosjekt-get-mode-map))))
    (dolist (tool tools)
      (let* ((keys (cdr (assoc :keys tool)))
	     (command (cdr (assoc :command tool)))
	     (is-interactive (interactive-form command)))
	(dolist (key keys)
	  (let ((key (read key)))
	    (lexical-let ((command command)
			  (is-interactive is-interactive))
	      (define-key keymap key
		(lambda ()
		  (interactive)
		  (let ((default-directory (or prosjekt-proj-dir default-directory)))
		    (if is-interactive
			(call-interactively command)
		      (eval command))))))))))))

(defun prosjekt-tool-names ()
  (let ((tools (prosjekt-proj-tools)))
    (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)))

(defun prosjekt-find-tool-by-name (name)
  (let ((tools (prosjekt-proj-tools)))
    (car 
     (remove-if-not 
      (lambda (tool) 
	(string= (cdr (assoc :name tool)) name)) 
      tools))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Populate support

; TODO: Add support for skipping common "untracked" directories,
; e.g. .hg, .svn, etc.

; TODO: Some sort of auto-populate? Picks up everything that matches
; common source-file extensions

(defun prosjekt-walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
	 (or (char-equal ?/ (aref dir(1- (length dir))))
	     (setq dir (file-name-as-directory dir)))
	 (let ((lst (directory-files dir nil nil t))
	       fullname file)
	   (while lst
	     (setq file (car lst))
	     (setq lst (cdr lst))
	     (cond ((or (member file '("." ".."))
			(member file prosjekt-ignore-dirs)))
		   (t
		    (and (funcall action dir file)
			 (setq fullname (concat dir file))
			 (file-directory-p fullname)
			 (prosjekt-walk-path fullname action)))))))
	(t
	 (funcall action
		  (file-name-directory dir)
		  (file-name-nondirectory dir)))))

(defun prosjekt-add-if (p dir file)
  "If FILE matches the regex P, DIR/FILE is added to the project."
  (if (string-match p file)
      (prosjekt-insert-file (concat dir file))
      't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other misc. functions

(defun prosjekt-hash-keys (h)
  "Get the list keys from hash-table H."
  (let (keys)
    (maphash (lambda (k v) (setq keys (cons k keys))) h)
    keys))

(defun prosjekt-alist-transpose (a)
  (mapcar (lambda (x) (cons (cdr x) (car x))) a))

; Add the "ext" directory to the load path. This makes it more
; convenient for users to load extensions.
(if load-file-name
    (add-to-list 'load-path
		 (concat
		  (file-name-directory load-file-name)
		  "/ext")))

(provide 'prosjekt)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prosjekt.el ends here
