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
     ;; --------------------------------------------------------
      ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
      auto-completion
      dash
     (colors :variables
             colors-enable-nyan-cat-progress-bar t
             colors-enable-rainbow-identifiers t)
     ;; better-defaults
      (git :variables
           git-gutter-use-fringe t
           git-enable-github-support t)
      markdown
      beeminder
      emacs-lisp
      clojure
      editorconfig
      gtags
      org
      python
      slime
      javascript
      ;; jason-js
      jason-eyebrowse
      jason-org
      jason-python
      jason-smartparens
      jason-web
      ruby

     ;; haskell
      syntax-checking
      markdown
      themes-megapack
      restclient
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
   dotspacemacs-themes '(zenburn
                         leuven
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   dotspacemacs-inactive-transparency 50
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq helm-imenu-fuzzy-match t)
  ;; This function is wayyyyyy too slow.
  (defun magit-wazzup())

  ;; Matching everywhere!
  (global-evil-matchit-mode 1)
  (golden-ratio-mode 1)

  ;; Paredit stuff
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)

  ;; Expand a snippet by name. Mostly useful when you've selected a region
  ;; and you want your snippet to use.
  (evil-leader/set-key "oe" 'helm-yas-complete)

  ;; I want my line numbers.
  (add-hook 'prog-mode-hook
            '(lambda ()
               (progn
                 (linum-mode 1)
                 (yas-minor-mode 1))
               )
            )

  ;; Gah, so annoying
  ;; ehh, edoesn't actually work
  (evil-search-highlight-persist nil)


  ;; yasnippet
  ;; make it work with evil visual
  (setq jason-yas-delete-region nil)
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (setq jason-yas-delete-region t)
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m)))))

  (add-hook 'yas-after-exit-snippet-hook
            #'(lambda ()
                (when jason-yas-delete-region
                  (delete-region (mark) (point))
                  (setq jason-yas-delete-region nil))))

  ;; Term settings
  ;; Let all snippets be expanded in term-mode b/c we could be using
  ;; any language.
  (add-hook 'shell-mode-hook
            '(lambda ()
               (yas-minor-mode 1)
               (yas-activate-extra-mode 'python-mode)))

  ;; Temporary tags stuff. I'll move this out eventually
  (setq twice-root "~/twicevm/twice-web-root/")
  (defun build-ctags ()
    (interactive)
    (message "building project tags")
    (shell-command
     ;; -e Generate etags
     ;; +f Generate file name as part of tag
     (concat "ack -f | ctags -e -R --extra=+f --exclude=test --exclude=.git --exclude=public -L - " twice-root "TAGS " twice-root))
    (visit-project-tags)
    (message "tags built successfully"))

  (defun visit-project-tags ()
    (interactive)
    ;; TODO, don't hardcode path separator
    (let ((tags-file (concat twice-root "TAGS")))
      (visit-tags-table tags-file)
      (message (concat "Loaded " tags-file))))

  (evil-leader/set-key
    ;; t for tag
    "of" 'find-tag
    )
  ;; In term-mode, Take result of snippet and set it to clipboard, then paste the result
  ;; Create new buffer. Enable term-mode Expand snippet in that buffer. Switch back to original buffer. Insert tmp buffer's
  ;; content. Remove tmp buffer.
  (setq jason-yas-term-use-extra-hook nil)
  (defun jason-yas/yas-term-expand ()
    (interactive)
    (progn
      (message "jason-yas/yas-term-expand")
      (setq jason-yas-term-use-extra-hook t)
      (setq jason-yas-term-old-buffer (current-buffer))
      (get-buffer-create "jason-yas-tmp")
      (switch-to-buffer "jason-yas-tmp")
      (erase-buffer)
      ;; (set-buffer "jason-yas-tmp")
      (yas-minor-mode 1)
      (yas-activate-extra-mode 'python-mode)
      (yas-activate-extra-mode 'term-mode)
      (spacemacs/helm-yas)
      )
    )

  (defun jason-yas/copy-expanded-snippet ()
    ;; debug code
    (when jason-yas-term-use-extra-hook
      (set-buffer "jason-yas-tmp")
      (let ((expanded-snippet (buffer-string)))
        (message "hook function called")
        (setq jason-yas-term-use-extra-hook nil)
        ;; (term-line-mode)
        (switch-to-buffer jason-yas-term-old-buffer)
        ;; (kill-new expanded-snippet)
        ;; (yank)
        (term-send-raw-string expanded-snippet)
        ;; Insert so we can press enter
        ;; Doesn't work for some reason
        ;; (evil-insert)
        ;; press enter
        )))
  (add-hook 'yas-after-exit-snippet-hook
            'jason-yas/copy-expanded-snippet)

  (evil-leader/set-key
    ;; x for expand
    "ox" 'jason-yas/yas-term-expand
    )
  ;; Toggle modeline clock on by default
  (setq spacemacs-mode-line-org-clock-current-taskp t)
  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(fringe-mode 6 nil (fringe))
 '(linum-format " %7d ")
 '(org-agenda-start-with-clockreport-mode t)
 '(org-clock-continuously t)
 '(org-clock-history-length 23)
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote bh/clock-in-to-next))
 '(org-clock-into-drawer t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-clock-sound t)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(enh-ruby-op-face ((t (:foreground "#DD1100"))))
 '(enh-ruby-string-delimiter-face ((t (:foreground "cornsilk3")))))
