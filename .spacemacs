;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (vimscript
   ;; default 'unused
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ruby
     ruby-on-rails
     php
     ;; emoji
     yaml
     php
     html
     csv
     nginx
     sql
     (gtags :variables gtags-enable-by-default nil)
     (typescript :variables
                 typescript-fmt-on-save t)
     syntax-checking
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      company-idle-delay 0.5
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence "kj")
     ;; better-defaults
     emacs-lisp
     ;; react
     git
     ;; clojure
     (javascript :variables
        node-add-modules-path t)
     org
     ;; deft
     markdown
     python
     ; themes-megapack
     (shell :variables shell-default-term-shell "/bin/zsh"
            shell-default-shell 'eshell)
     ;; ranger
     ; (elfeed :variables
     ;         elfeed-feeds '("http://planet.emacsen.org/atom.xml"
     ;                        ))
     )


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer.  If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(fullframe
                                      inf-ruby
                                      tramp-term
                                      wttrin
                                      rjsx-mode
                                      restclient
                                      ruby-hash-syntax
                                      counsel-dash
                                      nubox
                                      nodejs-repl
                                      polymode
                                      prettier-js
                                      vimish-fold
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, thansi-term-color-vectore first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         hc-zenburn
                         dracula
                         sanityinc-tomorrow-night
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font `("DejaVu Sans Mono"
                               :size ,(if (<= (x-display-pixel-width) 1920)
                                          16.0
                                        16.0)
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   ;; Name of the default layout (default "Default")
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   ;; dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   ;; dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   ;; dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   ;; dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2
   typescript-indent-level 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   clojure-defun-style-default-indent t
   evil-escape-key-sequence nil;"kj"
   evil-symbol-word-search t
   evil-search-module 'isearch
   wgrep-auto-save-buffer t
   git-commit-finish-query-functions '()
   init-file-debug t
   rspec-autosave-buffer t
   projectile-enable-caching nil
   shell-pop-autocd-to-working-dir nil
   doc-view-continuous t
   evil-want-fine-undo t)

  ;; (with-eval-after-load 'web-mode
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (setq-default
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   ns-right-option-modifier nil
   explicit-shell-file-name "/bin/bash"
   evil-ex-search-highlight-all nil)

  ;; jmfurlott
  (setq powerline-default-separator 'nil)
  (spaceline-compile)
  ;; aliases
  (defalias 'ff 'find-file)
  (defalias 'ffow 'find-file-other-window)
  (defalias 'l 'ls)
  ;; (spacemacs/toggle-line-numbers) ;; enable line numbers
  ;; (setq-default dotspacemacs-line-numbers t)

  ;; Less coarse undo via evil.  More close to vim style
  (setq evil-want-fine-undo t)

  ;; disable C-x C-c from closing all of emacs
  (global-set-key (kbd "C-x C-c") 'nil)

  ;; Capital :W writes to file like :w
  (evil-ex-define-cmd "W" 'evil-write)

  ;; Fill column indicator at L80
  (setq fci-rule-width 6)
  (setq fci-rule-color "#8faf9f")
  (add-hook 'js2-mode-hook 'fci-mode)
  (add-hook 'typescript-mode-hook 'fci-mode)
  (add-hook 'web-mode-hook 'fci-mode)
  (add-hook 'scss-mode-hook 'fci-mode)
  (add-hook 'python-mode-hook 'fci-mode)
  (add-hook 'javascript-mode-hook 'fci-mode)
  (add-hook 'markdown-mode-hook 'fci-mode)
  (add-hook 'js-mode-hook 'fci-mode)
  (add-hook 'org-mode-hook 'fci-mode)
  (add-hook 'ruby-mode-hook 'fci-mode)
  (add-hook 'react-mode-hook 'fci-mode)
  (add-hook 'rjsx-mode-hook 'fci-mode)

  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)))

  (require 'prettier-js)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'react-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '(
    ;; "--trailing-comma" "all"
    "--print-width" "120"
    "--single-quote"
    "--tab-width" "2"
  ))

  ;; Indenting guide
  (indent-guide-global-mode)

  (evil-set-initial-state 'ibuffer-mode 'normal)
  (global-set-key "\C-x\C-b" 'ido-switch-buffer)

  ;; Deft mode
  (setq deft-extensions '("org" "txt" "tex" "sql" "md"))
  (setq deft-directory "~/icloud/notes")

  (add-hook 'js2-mode-hook    'subword-mode)
  (add-hook 'rjsx-mode-hook    'subword-mode)
  (add-hook 'typescript-mode-hook    'subword-mode)
  (add-hook 'react-mode-hook    'subword-mode)
  (add-hook 'web-mode-hook    'subword-mode)
  (add-hook 'css-mode-hook    'subword-mode)
  (add-hook 'python-mode-hook    'subword-mode)
  (add-hook 'ruby-mode-hook    'subword-mode)
  (add-hook 'web-mode-hook    'subword-mode)

  (defun npm-format ()
    (interactive)
    (message "Running npm run format in the repo" (buffer-file-name))
    (shell-command (concat "npm run format" (buffer-file-name))))

  (defun eslint-fix-file ()
    (interactive)
    (message "eslint --fixing the file" (buffer-file-name))
    (shell-command (concat "`npm bin`/eslint --fix " (buffer-file-name))))

  (defun eslint-fix-file-and-revert ()
    (interactive)
    (eslint-fix-file)
    (revert-buffer t t))

  ;; (add-hook 'rjsx-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

  ;; (add-hook 'js2-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

  ;; asok
  ;; https://github.com/asok/dot-files/blob/master/.spacemacs

  (with-eval-after-load 'evil
    (evil-define-key 'visual global-map (kbd "v") #'er/expand-region)

    (evil-define-key 'insert global-map (kbd "C-j") #'yas-expand)
    (evil-define-key 'normal global-map (kbd "C-j") #'yas-expand)
    (evil-define-key 'normal global-map (kbd "M-p") (lambda () (interactive) (set-mark-command 4)))

    (add-to-list 'evil-normal-state-modes 'shell-mode)
    (add-to-list 'evil-emacs-state-modes 'term-mode)

    (add-hook 'term-mode-hook #'evil-emacs-state)
    )

  (with-eval-after-load 'shell-pop
    (defun asok/shell-pop-emacs-state-maybe ()
      (if (eq shell-default-shell 'ansi-term)
          (evil-emacs-state)))

    (add-hook 'shell-pop-in-after-hook #'asok/shell-pop-emacs-state-maybe))


  (fullframe wttrin wttrin-exit)

  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)


  ;; (defun asok/detect-react-buffer ()
  ;;   (string-match-p ".*^import React" (buffer-string)))

  ;; Polymode
  (define-hostmode poly-typescript-hostmode :mode 'typescript-mode)
  (define-innermode poly-rjsx-innermode
    :mode 'rjsx-mode
    :head-matcher "*"
    :tail-matcher "*"
    :head-mode 'host
    :tail-mode 'host)


  (define-polymode poly-ts-mode
    :hostmode 'poly-typescript-hostmode
    :innermodes '(poly-rjsx-innermode))


  ;; Change modes per file extension
  ;; (add-to-list 'magic-mode-alist '(".*\n?import React" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  ;; (when
  ;;     (string= (file-name-base) '.js') (add-to-list 'magic-mode-alist '(".*\n?import React" . rjsx-mode))
  ;; )

  (global-vi-tilde-fringe-mode -1)

  ;; (with-eval-after-load 'term
  ;;   (evil-define-key 'normal term-raw-map (kbd "i") #'evil-emacs-state)
  ;;   (evil-define-key 'normal term-raw-map (kbd "a") '(lambda ()
  ;;                                                      (interactive)
  ;;                                                      (evil-emacs-state)
  ;;                                                      (forward-char 1)))
  ;;   (evil-define-key 'emacs term-raw-map (kbd "ESC") #'evil-normal-state))

  ;; (with-eval-after-load 'volatile-highlights
  ;;   (vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
  ;;   (vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
  ;;   (vhl/give-advice-to-make-vhl-on-changes evil-paste-pop))

  ;; Typescript
  (add-hook 'before-save-hook 'tide-format-before-save)



  (with-eval-after-load 'compilation-mode
    (defun endless/send-input (input &optional nl)
      "Send INPUT to the current process.
Interactively also sends a terminating newline."
      (interactive "MInput: \nd")
      (let ((string (concat input (if nl "\n"))))
        ;; This is just for visual feedback.
        (let ((inhibit-read-only t))
          (insert-before-markers string))
        ;; This is the important part.
        (process-send-string
         (get-buffer-process (current-buffer))
         string)))

    (defun endless/send-self ()
      "Send the pressed key to the current process."
      (interactive)
      (endless/send-input
       (apply #'string
              (append (this-command-keys-vector) nil))))

    (define-key compilation-mode-map (kbd "C-c i")
      #'endless/send-input)

    (dolist (key '("\C-d" "\C-j" "y" "n"))
      (define-key compilation-mode-map key
        #'endless/send-self)))

  (with-eval-after-load 'counsel-dash
    (setq counsel-dash-browser-func 'eww))

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?λ) prettify-symbols-alist)))

  (defun asok/paste-and-reload ()
    (interactive)
    (evil-paste-after)
    (web-mode-reload))

  (evil-define-key 'normal web-mode-map (kbd "p") #'asok/paste-and-reload)

  (with-eval-after-load 'rjsx-mode
    (evil-define-key 'normal rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag))


  ;; (require 'gotham-theme)
  ;; (custom-theme-set-faces 'gotham '(js2-object-property ((t (:inherit 'font-lock-type-face)))))

  ;; (flycheck-add-mode 'rjsx-mode)

  ;; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  ;;   "Create parent directory if not exists while visiting file."
  ;;   (unless (file-exists-p filename)
  ;;     (let ((dir (file-name-directory filename)))
  ;;       (unless (file-exists-p dir)
  ;;         (make-directory dir)))))
    (show-smartparens-global-mode -1)
    (global-highlight-parentheses-mode +1)


    ;; better(ment) ruby setup
    (spacemacs/set-leader-keys-for-major-mode 'enh-ruby-mode "ru" 'rbenv-use-corresponding)
    (add-hook 'enh-ruby-mode-hook
              (lambda ()
                (when (file-exists-p (concat (projectile-project-root) "Gemfile.lock"))
                  (shell-command-to-string (concat "grep -o -m1 'rubocop' " (projectile-project-root) "Gemfile.lock"))
                  (make-variable-buffer-local 'flycheck-command-wrapper-function)
                  (setq flycheck-command-wrapper-function
                        (lambda (command)
                          (append '("bundle" "exec") command))))))
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile-rails feature-mode helm-gtags ggtags fold-this evil-vimish-fold vimish-fold rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler prettier-js dash-docs emoji-cheat-sheet-plus company-emoji polymode treemacs-projectile treemacs-evil treemacs ht pfuture lv transient flycheck-pos-tip pos-tip tide typescript-mode flycheck yaml-mode phpunit phpcbf php-auto-yasnippets drupal-mode sesman php-extras php-mode xterm-color white-sand-theme rebecca-theme org-category-capture alert log4e gntp org-mime markdown-mode skewer-mode json-snatcher json-reformat js2-mode parent-mode request haml-mode gitignore-mode flx exotica-theme ghub anzu evil undo-tree simple-httpd ace-jump-mode noflet powerline popwin diminish autothemer web-completion-data dash-functional tern company hydra edn paredit peg eval-sexp-fu highlight spinner clojure-mode epl bind-map bind-key yasnippet packed anaconda-mode pythonic f dash s avy auto-complete popup pug-mode persp-mode move-text moe-theme live-py-mode hy-mode gruvbox-theme evil-surround evil-escape eshell-prompt-extras dumb-jump cython-mode color-theme-sanityinc-tomorrow inflections cider counsel swiper ivy helm-dash elfeed iedit smartparens goto-chg helm helm-core multiple-cursors projectile org-plus-contrib magit magit-popup git-commit async zonokai-theme zenburn-theme zen-and-art-theme yapfify wttrin ws-butler with-editor winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme tramp-term toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode ruby-hash-syntax rjsx-mode reverse-theme restclient restart-emacs ranger rainbow-delimiters railscasts-theme queue pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme planet-theme pkg-info pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paradox orgit organic-green-theme org-projectile org-present org-pomodoro org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme nubox nodejs-repl noctilux-theme niflheim-theme nginx-mode neotree naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ inf-ruby indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy fullframe flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu espresso-theme eshell-z esh-help emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies dracula-theme django-theme deft define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme csv-mode counsel-dash company-web company-tern company-statistics company-anaconda column-enforce-mode color-theme-sanityinc-solarized coffee-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(projectile-use-git-grep t)
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#313131")))))
