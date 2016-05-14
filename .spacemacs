;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     git
     html
     javascript
     (shell :variables shell-default-term-shell "/bin/zsh")
     erc
     rcirc
     restclient
     ;; chrome
     markdown
     org
     deft
     ;; auto-completion
     ;; clojure
     ;; colors
     ruby-on-rails
     ;; react
     ;; latex
     ;; elm
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes ' (atom-one-dark zenburn base16-ocean-dark)
   dotspacemacs-themes '(zenburn)
   ;; dotspacemacs-themes '(zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("DejaVu Sans Mono"
   ;;                             :size 14
   ;;                             :weight Light
   ;;                             :width normal
   ;;                             :powerline-scale 1)

   dotspacemacs-default-font '("Hack"
                               :size 16
                               :weight Light
                               :width normal
                               :powerline-scale 1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode t
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   dotspacemacs-elpa-https nil
   )
   ;; User initialization goes here
   ;; (add-to-list 'load-path "~/.emacs.d/private/twittering-mode")
   ;; (add-to-list 'load-path "~/.emacs.d/private/android-mode")
   ;; (load "~/.emacs.d/private/react-mode/react.el")
   ;; (load "~/.emacs.d/private/hackernews.el/hackernews.el")
)

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
   (setq powerline-default-separator 'nil)
  ;; aliases
  (defalias 'ff 'find-file)
  (defalias 'ffow 'find-file-other-window)
  (defalias 'l 'ls)
  (spacemacs/toggle-line-numbers) ;; enable line numbers
  (setq-default dotspacemacs-line-numbers t)

  ;; disable C-x C-c from closing all of emacs
  (global-set-key (kbd "C-x C-c") 'nil)

  ;; Fill column indicator at L80
  (setq fci-rule-width 6)
  (setq fci-rule-color "#8faf9f")
  (add-hook 'js2-mode-hook 'fci-mode)
  (add-hook 'web-mode-hook 'fci-mode)
  (add-hook 'scss-mode-hook 'fci-mode)
  (add-hook 'python-mode-hook 'fci-mode)
  (add-hook 'javascript-mode-hook 'fci-mode)
  (add-hook 'markdown-mode-hook 'fci-mode)
  (add-hook 'js-mode-hook 'fci-mode)
  (add-hook 'org-mode-hook 'fci-mode)
  (add-hook 'ruby-mode-hook 'fci-mode)

  ;; Indenting guide
  (indent-guide-global-mode)

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  ;; ibuffer
  (evil-set-initial-state 'ibuffer-mode 'normal)

  ;; Indentation from
  ;; http://blog.binchen.org/posts/easy-indentation-setup-in-emacs-for-web-development.html
  (defun my-setup-indent (n)
    ;; web development
    (setq coffee-tab-width n) ; coffeescript
    (setq javascript-indent-level n) ; javascript-mode
    (setq js-indent-level n) ; js-mode
    (setq js2-basic-offset n) ; js2-mode
    (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
    (setq web-mode-css-indent-offset n) ; web-mode, css in html file
    (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
    (setq css-indent-offset n) ; css-mode
    )

  (defun my-office-code-style ()
    (interactive)
    (message "Office code style!")
    (setq indent-tabs-mode t) ; use tab instead of space
    (my-setup-indent 4) ; indent 4 spaces width
    )

  (defun my-personal-code-style ()
    (interactive)
    (message "Indentation set to two")
    (setq indent-tabs-mode nil) ; use space instead of tab
    (my-setup-indent 2) ; indent 2 spaces width
    )

  ;; call indentation
  (my-personal-code-style)

  ;; Deft mode
  (setq deft-extensions '("org" "txt" "tex"))
  (setq deft-directory "~/icloud/notes")

  (global-set-key "\C-x\C-b" 'ido-switch-buffer)

  ;; For storing erc passwords
  ;; (require 'erc-services)
  ;; (erc-services-mode 1)

  ;; (setq erc-prompt-for-nickserv-password nil)
  ;; (setq erc-nickserv-passwords
  ;;       `((freenode
  ;;          (("jmfurlott" . ,freenode-pass)))
  ;;         ))

  ;; (defmacro erc-autojoin (&rest args)
  ;;   `(add-hook 'erc-after-connect
  ;;              '(lambda (server nick)
  ;;                 (cond
  ;;                  ,@(mapcar (lambda (servers+channels)
  ;;                              (let ((servers (car servers+channels))
  ;;                                    (channels (cdr servers+channels)))
  ;;                                `((member erc-session-server ',servers)
  ;;                                  (mapc 'erc-join-channel ',channels))))
  ;;                            args)))))
  ;; (erc-autojoin
  ;;   (("irc.freenode.net")
  ;;    "reactjs" "emacs" "postgresql" "Node.js" "programming"))
  ;; (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  (add-hook 'js2-mode-hook    'subword-mode)
  (add-hook 'web-mode-hook    'subword-mode)

  ;; Function for easy json formatting
  (defun json-format ()
    (interactive)
    (save-excursion
      (shell-command-on-region (mark) (point) "underscore pretty" (buffer-name) t)
      )
    )

  ;; Support for running git-grep in it's own buffer
  (defcustom git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
    "Switches to pass to `git grep'."
    :type 'string)

  (defcustom git-grep-default-work-tree (expand-file-name "~/work/adtrack")
    "Top of your favorite git working tree.  \\[git-grep] will search from here if it cannot figure out where else to look."
    :type 'directory
    )

  (when (require 'vc-git nil t)

    ;; Uncomment this to try out the built-in-to-Emacs function.
    ;;(defalias 'git-grep 'vc-git-grep)

    (defun gg (command-args)
      (interactive
       (let ((root (vc-git-root default-directory)))
         (when (not root)
           (setq root git-grep-default-work-tree)
           (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root))
         (list (read-shell-command "Run git-grep (like this): "
                                   (format (concat
                                            "cd %s && "
                                            "git grep %s -e %s")
                                           root
                                           git-grep-switches
                                           (let ((thing (and

                                          ; don't snarf stuff from the
                                          ; buffer if we're not looking
                                          ; at a file.  Perhaps we
                                          ; should also check to see if
                                          ; the file is part of a git
                                          ; repo.
                                                         buffer-file-name
                                                         (thing-at-point 'symbol))))
                                             (or (and thing (progn
                                                              (set-text-properties 0 (length thing) nil thing)
                                                              (shell-quote-argument (regexp-quote thing))))
                                                 "")))
                                   'git-grep-history))))
      (let ((grep-use-null-device nil))
        (grep command-args))))

)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(neo-theme (quote ascii))
 '(package-selected-packages
   (quote
    (rcirc-notify rcirc-color rake inflections f alert log4e gntp json-snatcher json-reformat parent-mode pkg-info epl request flx iedit highlight popup async anzu with-editor hydra spinner magit-popup git-commit restclient deft s inf-ruby multiple-cursors dash avy yasnippet magit powerline smartparens projectile helm helm-core xterm-color ws-butler window-numbering web-mode web-beautify volatile-highlights vi-tilde-fringe toc-org tern tagedit spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters projectile-rails popwin persp-mode pcre2el paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js2-mode js-doc jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md flx-ido fill-column-indicator feature-mode fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emmet-mode define-word coffee-mode clean-aindent-mode chruby bundler buffer-move bracketed-paste auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line quelpa package-build use-package which-key bind-key bind-map evil zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
