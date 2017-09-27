;; -*- mode: emacs-lisp -*-
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
     ;; editor
     auto-completion
     emoji
     ibuffer
     ivy
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-term-shell "/run/current-system/sw/bin/fish")
     spell-checking
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     themes-megapack

     ;; notes
     org

     ;; markup
     html
     markdown
     yaml

     ;; languages
     emacs-lisp
     (haskell :variables haskell-process-type 'ghci)
     shell-scripts
     sql

     ;; misc
     finance
     (mu4e :variables
           mu4e-installation-path "/run/current-system/sw/share/emacs/site-lisp/mu4e"
           mu4e-enable-notifications t
           mu4e-enable-mode-line t)
     )
   dotspacemacs-additional-packages '(wttrin)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(smartparens)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
   This function is called at the very startup of Spacemacs initialization
   before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'haskell-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(dracula)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Knack Nerd Font"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
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
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   )
  )

(defun dotspacemacs/user-config ()
  ;; system
  (add-to-list 'exec-path "~/.local/bin")
  (remove-hook 'haskell-mode-hook 'flyspell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-fci-mode) ;; 80 columns
  (setq flycheck-pos-tip-timeout 30)

  ;; clock
  (setq display-time-day-and-date t)
  (display-time)

  ;; japanese keyboard
  (global-set-key (quote [134217893]) "\\")
  (defun insert-backs ()
    "insert back-slash"
    (interactive)
    (insert "\\ "))
  (global-set-key (kbd "M-:") 'insert-backs)

  ;; weather
  (setq wttrin-default-cities '("Osaka"))

  ;; haskell
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (setq haskell-indent 2)

  ;; org-mode
  (with-eval-after-load 'org
    (setq org-startup-truncated nil)
    ;;(setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
    (setq org-agenda-window-setup 'current-window)
    (setq org-todo-keywords
          '((sequence "TODO(t)" "ACTIVE(a)" "ONHOLD(h@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
    (setq org-fontify-done-headline t)
    (setq spaceline-org-clock-p t))

  ;; hledger
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-binary-path "hledger")

  ;; mu4e
  ;;(setq mu4e-mu-binary "/run/current-system/sw/bin/mu"
  ;;      mu4e-maildir "~/.mail"
  ;;      mu4e-attachment-dir  "~/Downloads"
  ;;      mu4e-get-mail-command "mbsync -a"
  ;;      mu4e-update-interval 30
  ;;      mu4e-compose-signature-auto-include nil
  ;;      mu4e-html2text-command "w3m -dump -T text/html"
  ;;      mu4e-view-prefer-html t
  ;;      mu4e-view-show-images t
  ;;      mu4e-view-show-addresses t
  ;;      mu4e-hide-index-messages t
  ;;      mu4e-headers-auto-update t
  ;;      mu4e-headers-include-related nil
  ;;      mu4e-headers-skip-duplicates t
  ;;      mu4e-compose-dont-reply-to-self t
  ;;      mu4e-sent-messages-behavior 'delete
  ;;      mu4e-split-view 'horizontal
  ;;      mu4e-show-images t
  ;;      message-cite-reply-position 'above
  ;;      message-kill-buffer-on-exit t
  ;;      mu4e-index-update-error-warning nil)
  ;;(with-eval-after-load 'mu4e
  ;;  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  ;;  (setq mu4e-contexts
  ;;        `(,(make-mu4e-context
  ;;            :name "gmail"
  ;;            :enter-func (lambda () (mu4e-message "Switch to the gmail context"
  ;;                                    mu4e-refile-folder "/gmail/.all"
  ;;                                    mu4e-drafts-folder "/gmail/.drafts"
  ;;                                    mu4e-sent-folder   "/gmail/.sent"
  ;;                                    mu4e-maildir-shortcuts
  ;;                                    '(("/gmail/INBOX"   . ?i)
  ;;                                      ("/gmail/.all"    . ?a)
  ;;                                      ("/gmail/.sent"   . ?s)
  ;;                                      ("/gmail/.drafts" . ?d)))
  ;;            :leave-func (lambda () mu4e-message "Left gmail context")
  ;;            :match-func (lambda (msg)
  ;;                          (when msg
  ;;                            (mu4e-message-contact-field-matches msg
  ;;                              :to "mojobojo@gmail.com")))
  ;;            :vars '((user-mail-address . "mojobojo@gmail.com")
  ;;                    (user-full-name	   . "Brian Jones")))
  ;;          ,(make-mu4e-context
  ;;            :name "uncannyworks"
  ;;            :enter-func (lambda () (mu4e-message "Switch to the uncannyworks context"
  ;;                                    mu4e-refile-folder "/uncannyworks/.all"
  ;;                                    mu4e-drafts-folder "/uncannyworks/.drafts"
  ;;                                    mu4e-sent-folder   "/uncannyworks/.sent"
  ;;                                    mu4e-maildir-shortcuts
  ;;                                    '(("/uncannyworks/INBOX"   . ?i)
  ;;                                      ("/uncannyworks/.all"    . ?a)
  ;;                                      ("/uncannyworks/.sent"   . ?s)
  ;;                                      ("/uncannyworks/.drafts" . ?d)))
  ;;            :leave-func (lambda () mu4e-message "Left uncannyworks context")
  ;;            :match-func (lambda (msg)
  ;;                          (when msg
  ;;                            (mu4e-message-contact-field-matches msg
  ;;                              :to "brian.jones@uncannyworks.com")))
  ;;          :vars '((user-mail-address . "brian.jones@uncannyworks.com")
  ;;                  (user-full-name	  . "Brian Jones"))))))))
  ;;(with-eval-after-load 'mu4e-alert
  ;;  (mu4e-alert-set-default-style 'notifications))
  ;;(setq message-send-mail-function 'message-send-mail-with-sendmail
  ;;      sendmail-program "msmtp")
  ;;(require 'gnus-dired)
  ;;(defun gnus-dired-mail-buffers ()
  ;;  "Return a list of active message buffers."
  ;;  (let (buffers)
  ;;    (save-current-buffer
  ;;      (dolist (buffer (buffer-list t))
  ;;        (set-buffer buffer)
  ;;        (when (and (derived-mode-p 'message-mode)
  ;;                   (null message-sent-message-via))
  ;;          (push (buffer-name buffer) buffers))))
  ;;    (nreverse buffers)))
  ;;(setq gnus-dired-mail-mode 'mu4e-user-agent)
  ;;(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; gpg
  ;;(setq epg-gpg-program "gpg2")
  ;;(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  ;;(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
)

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (wttrin web-mode use-package tao-theme spaceline powerline monokai-theme markdown-toc markdown-mode ledger-mode julia-mode ivy-hydra intero inkpot-theme hl-todo gruvbox-theme gruber-darker-theme google-translate flycheck-haskell evil-unimpaired evil-mc evil-matchit evil-ediff espresso-theme eshell-prompt-extras esh-help dumb-jump darktooth-theme darkokai-theme cyberpunk-theme color-theme-sanityinc-tomorrow alect-themes aggressive-indent ghc haskell-mode company counsel smartparens highlight evil undo-tree flycheck helm helm-core avy org-plus-contrib haml-mode ivy dash yasnippet zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler wolfram-mode winum white-sand-theme which-key wgrep volatile-highlights vi-tilde-fringe vala-snippets vala-mode uuidgen underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org thrift tangotango-theme tango-plus-theme tango-2-theme tagedit symon swiper sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stan-mode sql-indent spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex slim-mode shell-pop seti-theme scss-mode scad-mode sass-mode reverse-theme restart-emacs request rebecca-theme rainbow-delimiters railscasts-theme qml-mode purple-haze-theme pug-mode professional-theme popwin planet-theme pkgbuild-mode phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme password-generator paradox organic-green-theme org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term mu4e-maildirs-extension mu4e-alert move-text monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme matlab-mode material-theme majapahit-theme madhat2r-theme macrostep lush-theme lorem-ipsum logcat linum-relative link-hint light-soap-theme less-css-mode kivy-mode jbeans-theme jazz-theme ivy-purpose ir-black-theme insert-shebang info+ indent-guide impatient-mode ibuffer-projectile hungry-delete hoon-mode hlint-refactor hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-make hc-zenburn-theme haskell-snippets grandshell-theme goto-chg gotham-theme golden-ratio gnuplot gh-md gandalf-theme fuzzy flyspell-correct-ivy flycheck-pos-tip flycheck-ledger flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-z emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav editorconfig ebuild-mode dracula-theme django-theme diminish define-word darkmine-theme darkburn-theme dante dakrone-theme counsel-projectile company-web company-statistics company-shell company-ghci company-ghc company-emoji company-cabal column-enforce-mode color-theme-sanityinc-solarized cmm-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme autothemer auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme afternoon-theme adaptive-wrap ace-window ace-link ac-ispell))))
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
    (spinner flyspell-correct pos-tip flx iedit anzu f s epl web-completion-data bind-map auto-complete popup yaml-mode wttrin which-key web-mode use-package tao-theme spaceline powerline solarized-theme slim-mode rainbow-mode rainbow-identifiers phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme org-projectile org-category-capture org-plus-contrib obsidian-theme mustang-theme mu4e-alert monokai-theme mmm-mode markdown-toc markdown-mode ledger-mode jbeans-theme ivy-hydra intero insert-shebang inkpot-theme info+ ibuffer-projectile hlint-refactor hl-todo highlight-parentheses highlight-numbers gruvbox-theme gruber-darker-theme gotham-theme google-translate flycheck-haskell fill-column-indicator evil-nerd-commenter evil-mc evil-matchit evil-ediff espresso-theme eshell-prompt-extras esh-help dumb-jump darktooth-theme darkokai-theme dakrone-theme cyberpunk-theme counsel-projectile counsel swiper color-theme-sanityinc-tomorrow color-identifiers-mode busybee-theme auto-compile packed apropospriate-theme alect-themes aggressive-indent ace-window ghc haskell-mode company smartparens highlight evil undo-tree flycheck ivy yasnippet projectile helm helm-core avy async alert hydra haml-mode dash zonokai-theme zenburn-theme zen-and-art-theme xterm-color ws-butler winum wgrep volatile-highlights vi-tilde-fringe uuidgen underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs request rainbow-delimiters railscasts-theme purple-haze-theme pug-mode professional-theme popwin planet-theme pkg-info persp-mode pcre2el pastels-on-dark-theme parent-mode paradox org-present org-pomodoro org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme noctilux-theme niflheim-theme neotree naquadah-theme multi-term mu4e-maildirs-extension move-text monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme macrostep lush-theme lorem-ipsum log4e linum-relative link-hint light-soap-theme less-css-mode jazz-theme ir-black-theme indent-guide hungry-delete htmlize ht hindent highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-make hc-zenburn-theme haskell-snippets grandshell-theme goto-chg golden-ratio gnuplot gntp gh-md gandalf-theme fuzzy flyspell-correct-ivy flycheck-pos-tip flycheck-ledger flx-ido flatui-theme flatland-theme fish-mode firebelly-theme farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-z emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dracula-theme django-theme diminish define-word darkmine-theme darkburn-theme company-web company-statistics company-shell company-ghci company-ghc company-emoji company-cabal column-enforce-mode color-theme-sanityinc-solarized cmm-mode clues-theme clean-aindent-mode cherry-blossom-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme autothemer auto-yasnippet auto-highlight-symbol auto-dictionary anti-zenburn-theme ample-zen-theme ample-theme afternoon-theme adaptive-wrap ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
