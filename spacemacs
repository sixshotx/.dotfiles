;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  ;; Need to do this setup here because layer code uses it.
  (setq auth-tokens-path "~/Dropbox/auth_tokens.el")
  (if (file-exists-p auth-tokens-path)
      (load-file auth-tokens-path)
    nil)

  (setq
   pushbullet
   (mapconcat
    'identity
    (list "/usr/local/bin/python ~/.emacs.d/pushbullet_wrapper.py"
          (getenv "PUSHBULLET_API_KEY"))
    " "))
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; (default nil)
   dotspacemacs-enable-lazy-installation nil
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     (clojure :variables
              clojure-enable-fancify-symbols t)
     chinese
     colors
     chrome
     dash
     dockerfile
     elm
     emacs-lisp
     gtags
     (git :variables
          git-gutter-use-fringe t
          git-enable-github-support t)
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence-delay 0.1)
     ;; better-defaults
     emacs-lisp
     emoji
     haskell
     html
     hydra
     lispy
     lua
     octave
     (org :variables
          org-enable-reveal-js-support t
          org-reveal-root (concat "file://"
                                  (file-truename (file-name-as-directory (expand-file-name "~/.emacs.d/reveal.js"))))
          )
     ;; To use the python layer, you have to install these dependencies manually:
     ;; pip install jedi==0.8.1 json-rpc==1.8.1 service_factory==0.1.2
     markdown
     norang
     python
     javascript
     ;; jason-eyebrowse
     jason-org
     ;; jason-python
     ruby
     react
     semantic
     spacemacs-layouts
     shell
     osx
     markdown
     restclient
     syntax-checking
     themes-megapack
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'diff-hl)
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(alert bbdb f s swiper beeminder jsx-mode
                                            editorconfig crux)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(helm-gitignore)
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
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
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
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
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
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
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
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
;;;;;;;;;;;;;;;;;;;;;;
  ;; Editing defaults ;;
;;;;;;;;;;;;;;;;;;;;;;
  ;; In Unix, a file is defined as having a newline: http://bit.ly/22huFrs
  (setq require-final-newline t)

  ;; Navigate by visual line rather than absolute line.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (add-hook 'prog-mode-hook
            '(lambda ()
               (progn
                 ;; Always have cursor's line be in the center of the screen
                 (centered-cursor-mode 1))))

;;;;;;;;;;;;;;;
  ;; Yasnippet ;;
;;;;;;;;;;;;;;;
  ;; Selecting a block of text and then expanding a snippet around it doesn't
  ;; work in evil visual mode.
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
;;;;;;;;;;;;;;
  ;; Terminal ;;
;;;;;;;;;;;;;;
  ;; Term settings
  ;; Allow expanding python snippets in term-mode. The idea was that we could
  ;; use yasnippets in a terminal, but there are too many weirdnesses around
  ;; using a terminal in emacs for that to be worth it right now.
  (add-hook 'shell-mode-hook
            '(lambda ()
               (yas-minor-mode 1)
               (yas-activate-extra-mode 'python-mode)))

  ;; In term-mode, Take result of snippet and set it to clipboard,
  ;; then paste the result Create new buffer. Enable term-mode.
  ;; Expand snippet in that buffer. Switch back to original buffer.
  ;; Insert tmp buffer's content. Remove tmp buffer.
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
      (spacemacs/helm-yas))
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
    "ox" 'jason-yas/yas-term-expand)


;;;;;;;;;;
  ;; Text ;;
;;;;;;;;;;
  (evil-leader/set-key
    ;; t for tag
    "of" 'find-tag
    "i RET" '(lambda ()
               (interactive)
               (insert "\n"))
    "i;" 'insert-semicolon
    "i," 'insert-comma
    )

  (defun insert-end-of-line-char (char)
    "Inserts the given character at the end of the line"
    (interactive "s")
    (save-excursion
      (let (start end)
        (save-excursion (beginning-of-line)
                        (setq start (point)))
        (save-excursion (end-of-line)
                        (setq end (point)))
        (replace-regexp "$" char nil start end))))
  (defun insert-semicolon ()
    (interactive)
    (insert-end-of-line-char ";"))
  (defun insert-comma ()
    (interactive)
    (insert-end-of-line-char ","))
  (defun insert-newline ()
    (insert-end-of-line-char "\n"))

  ;; Whitespace
  (add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;
  ;; Modeline ;;
;;;;;;;;;;;;;;
  ;; Toggle modeline clock on by default
  (setq spacemacs-mode-line-org-clock-current-taskp t)
  ;; Disable showing minor modes
  (setq spacemacs-mode-line-minor-modesp nil)

  ;; Hybrid s-expression
  (evil-leader/set-key
    "ohk" 'sp-kill-hybrid-sexp
    "oht" 'sp-transpose-hybrid-sexp
    "ohp" 'sp-push-hybrid-sexp
    "ohs" 'sp-slurp-hybrid-sexp
    "oh>" 'sp-indent-adjust-sexp
    "oh<" 'sp-dedent-adjust-sexp)


;;;;;;;;;;;;;;;;;;;;;
  ;; Version control ;;
;;;;;;;;;;;;;;;;;;;;;

  (setq vc-follow-symlinks t)

;;;;;;;;;;;;;;;;;;;;;;;
  ;; Language-specific ;;
;;;;;;;;;;;;;;;;;;;;;;;

  ;; Indent amount hooks
  ;; Maybe make a config file for different languages that evil mode
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq evil-shift-width python-indent))))
  (add-hook 'ruby-mode-hook
            (function (lambda ()
                        (setq evil-shift-width ruby-indent-level))))
  (add-hook 'yaml-mode-hook
            (function (lambda ()
                        (setq evil-shift-width yaml-indent-offset))))
  (add-hook 'json-mode-hook
            (function (lambda ()
                        (setq js-indent-level 2)
                        (setq evil-shift-width 2))))
;;;;;;;;;;
  ;; Evil ;;
;;;;;;;;;;
  ;; Put H and L to go to line start and end, respectively.
  (define-key evil-normal-state-map "H" "^")
  (define-key evil-normal-state-map "L" "$")

  ;; For quick recordings just type qq to start recording, then q to stop. You
  ;; don't have to worry about the name this way (you just named the recording
  ;; 'q'). Now, to play back the recording you just type Q. This will redefine the
  ;; standard meaning of 'Q', but all that does is enter "Ex" mode which I can live
  ;; without.
  (define-key evil-normal-state-map "Q" "@q")

  ;; To copy text to the end-of-line, you can press y$ or you can use the
  ;; following and press Y instead. This mapping sets up Y to be consistent with
  ;; the C and D operators, which act from the cursor to the end of the line. The
  ;; default behavior of Y is to yank the whole line.
  (define-key evil-normal-state-map "Y" "y$")

  ;; TODO Map an insert mode keybinding to go to
  (define-key evil-insert-state-map (kbd "<C-return>")
    (lambda ()
      (end-of-line)
      (newline-and-indent)))

;;;;;;;;;;;;;;;;
  ;; Screenshot ;;
;;;;;;;;;;;;;;;;
  ;; Take a screenshot and save it to dropbox.
  (defun take-screenshot ()
    (interactive)
    (let ((screenshot-path (expand-file-name (concat "~/Dropbox/Screenshots/" (md5 (current-time-string)) ".png") )))
      (call-process "screencapture" nil nil nil "-i" screenshot-path)
      (kill-new screenshot-path)))

  ;; Possibly fixes https://github.com/syl20bnr/spacemacs/issues/1300
  (savehist-mode -1)

  ;; Customize helm
  ;; Decide on this later
  ;; Remap SPC / to search the current buffer
  ;; (define-key evil-normal-state-map (kbd "SPC /") 'helm-occur)
  ;; (define-key evil-normal-state-map (kbd "SPC p /") 'spacemacs/helm-projectile-smart-do-search)
  ;; helm-regexp is pretty sexy http://tuhdo.github.io/helm-intro.html
  ;; SPC r r for helm-register: view the contents of registers
  ;; SPC r y for helm-kill-ring: use helm to choose the thing to paste.
  ;; helm-top is amazing too

  ;; No idea where this is getting set, but I don't want it.
  (setq helm-always-two-windows nil)

  ;; Org-babel
  ;; Requirements for Babel code execution for each language.
  ;; Clojure requirements
  (setq org-babel-clojure-backend 'cider)
  (setq nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-popup-stacktraces nil
        cider-repl-popup-stacktraces t)
  (require 'ob-clojure)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (clojure . t)))

  ;; Syntax highlight org babel blocks of code.
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate nil)
  ;; Tabe indents in src blocks just like in the real file.
  (setq org-src-tab-acts-natively t)

  ;; Org-pomodoro
  (setq org-pomodoro-length 45)
  (setq org-pomodoro-short-break-length 15)
  (setq org-pomodoro-long-break-length 15)
  ;; Better lisp
  (setq evil-move-cursor-back nil)
  ;; Stop getting dbus errors when doing an org-pomodoro.
  (setq notify-method 'notify-via-growl)
  ;; Open these buffers as soon as they're available

;;;;;;;;;
  ;; Git ;;
;;;;;;;;;
  (setq diff-hl-side 'right)

;;;;;;;;;;;;;;
  ;; Projectile
;;;;;;;;;;;;;;
  (evil-leader/set-key
    "pw" 'helm-projectile-find-other-file)
  (define-key evil-normal-state-map (kbd "SPC p /") 'spacemacs/helm-projectile-smart-do-search)
  ;; Allow switching between js, scss, and html files more easily.

  ;; TODO - this doesn't work right now
  ;; (eval-after-load 'helm-projectile
  ;;   (progn
  ;;       (add-to-list 'projectile-other-file-alist '("js" "scss"))
  ;;       (add-to-list 'projectile-other-file-alist '("scss" "js"))))

;;;;;;;;;;
  ;; HTML ;;
;;;;;;;;;;
  ;; (evil-define-key 'normal html-mode-map
  ;;   (kbd "j") 'sp-next-sexp
  ;;   (kbd "k") 'sp-previous-sexp
  ;;   (kbd "l") 'sp-down-sexp
  ;;   (kbd "h") 'sp-up-sexp)

;;;;;;;;;;;
  ;; Ediff ;;
;;;;;;;;;;;
  ;; http://oremacs.com/2015/01/17/setting-up-ediff/

  ;; Uses custom-set variable value if it exists, otherwise just setq's.
  (defmacro csetq (variable value)
    `(funcall (or (get ',variable 'custom-set)
                  'set-default)
              ',variable ,value))
  ;; Don't manage control panel in a separate frame.
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Split windows horizontally so it's easier to follow changes.
  (csetq ediff-split-window-function 'split-window-horizontally)
  ;; Ignore whitespace in diff. Useful in lisp and languages where whitespace isn't significant.
  ;; (csetq ediff-diff-options "-w")

  ;; Use 'j' and 'k' to navigate
  (defun jy-ediff-hook ()
    (ediff-setup-keymap)
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))
  (add-hook 'ediff-mode-hook 'jy-ediff-hook)

  ;; Restore window configuration when quitting ediff.
  ;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

  ;; React/Web stuff
  ;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  ;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
  ;;   (if (equal web-mode-content-type "jsx")
  ;;       (let ((web-mode-enable-part-face nil))
  ;;         ad-do-it)
  ;;     ad-do-it))
  ;; Javascript default settings
  ;; Set default indentation to 2 spaces

  ;; Stop showing js2 errors, we should just always use eslint instead.
  (setq js2-basic-offset 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq standard-indent 2)
  (defun turn-on-aggressive-indent ()
    (progn
      (aggressive-indent-mode)))
  ;; Can't use aggressive indent mode for scss because the indentation is not perfect.
  ;; (add-hook 'scss-mode-hook 'turn-on-aggressive-indent)

  (setq web-mode-markup-indent-offset 2)
  (defun buffer-mode (buffer-or-string)
    "Returns the major mode associated with a buffer."
    (with-current-buffer buffer-or-string
      major-mode))

  ;; Toggles between js and html mode so we can edit react jsx easily
  (defun toggle-js-html-mode ()
    (interactive)
    (cond ((eq 'js2-mode major-mode) (html-mode))
          ((eq 'html-mode major-mode) (js2-mode))))
  (evil-leader/set-key "ot" 'toggle-js-html-mode)

  ;; Ivy
  ;; Turn on ivy
  ;; (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; Use ivy for projectile
  (setq projectile-completion-system 'ivy)
  ;; Use ivy for magit
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Bind this away from helm-m-x
  (global-set-key "\M-x" 'execute-extended-command)
  (global-set-key [remap ido-find-file] 'find-file)
  (global-set-key "\M-p" 'pop-to-mark-command)
  ;; Remap SPC b b from helm-mini to ivy buffer
  (evil-leader/set-key "b b" 'ivy-switch-buffer)

  ;; Don't use IDO; that way
  (setq org-completion-use-ido nil)

  ;; To use these actions, type M-o and then the action
  ;; (ivy-set-actions
  ;;  'ivy-switch-buffer
  ;;  '(("k"
  ;;     (lambda (x)
  ;;       (kill-buffer x)
  ;;       (ivy--reset-state ivy-last))
  ;;     "kill")
  ;;    ("o"
  ;;     ivy--switch-buffer-other-window-action
  ;;     "other")))

  (evil-leader/set-key
    "pb"  'projectile-switch-to-buffer
    "pd"  'projectile-find-dir
    "pf"  'projectile-find-file
    "pF"  'projectile-find-file-other-window
    "ph"  'projectile
    "pp"  'projectile-switch-project
    "pr"  'projectile-recentf
    "pv"  'vc
    "sgp" 'projectile-grep)

  ;; Regexp stuff
  (require 're-builder)
  (setq reb-re-syntax 'string)

;;;;;;;;;
  ;; Avy ;;
;;;;;;;;;
  (evil-leader/set-key
    ;; SPC SPC
    "SPC" 'avy-goto-char-2)
  (setq avy-style 'de-bruijn)
  (setq avy-keys (number-sequence ?a ?z))

  (defun prelude-copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))
;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Custom org-pomodoro ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (load-file "~/.dotfiles/emacs.spacemacs.d.private/jason-org/local/norang/norang.el")
  (load-file "~/.dotfiles/emacs.spacemacs.d.private/jason-org/local/org-pomodoro/org-pomodoro.el")
  (setq org-pomodoro-start-sound "~/.dotfiles/emacs.spacemacs.d.private/jason-org/local/org-pomodoro/resources/marine_gogogo.wav")
  (setq org-pomodoro-long-break-sound "~/.dotfiles/emacs.spacemacs.d.private/jason-org/local/org-pomodoro/resources/reap_the_whirlwind.wav")
  (setq org-pomodoro-short-break-sound "~/.dotfiles/emacs.spacemacs.d.private/jason-org/local/org-pomodoro/resources/reap_the_whirlwind.wav")

  (require 'beeminder)
  (setq beeminder-username "zinbiel")
  (setq beeminder-auth-token (getenv "BEEMINDER_AUTH_TOKEN"))
  (setq org-pomodoro-finished-hook
        (lambda ()
          (message "Pomodoro done. Hook running.")
          (beeminder-add-data "tockmore" 1 (current-time-string))
          (shell-command (concat pushbullet " --tock=tock_done"))))

  (defun jason/pomodoro-break-finished ()
    "Makes a notification"
    (message "Pomodoro break finished. Hook running.")
    (shell-command (concat pushbullet " --tock=break_done")))

  (setq org-pomodoro-break-finished-hook 'jason/pomodoro-break-finished)

  ;; Remap sexp commands to be more vim-like
  (global-set-key "\C-\M-j" 'forward-sexp)
  (global-set-key "\C-\M-k" 'backward-sexp)
  (global-set-key "\C-\M-l" 'down-list)
  (global-set-key "\C-\M-h" 'backward-up-list)
  ;; Web-mode
  (setq web-mode-comment-face '((t (:foreground "#ddd"))))

;;;;;;;;;;;
  ;; Theme ;;
;;;;;;;;;;;
  ;; This makes the mode-line legible
  ;; (moe-theme-set-color 'magenta)
  ;; Highlights the entire expression in parentheses
  ;; (show-paren-mode t)

  ;; Doesn't work well with spacemacs-dark
  ;; (setq show-paren-style 'expression)

  ;;  Python
  ;; Create an inferior python process just once. An inferior python process is
  ;; needed for eldoc and anaconda-mode.
  (defun run-python-once ()
    (remove-hook 'python-mode-hook 'run-python-once)
    (run-python))
  (add-hook 'python-mode-hook 'run-python-once)
  ;; http://stackoverflow.com/questions/22817120/how-can-i-save-evil-mode-vim-style-macros-to-my-init-el
  (fset 'mymacro
        [?i ?f ?o ?o ?b ?a ?r escape])
  ;; Change dict assignment to literal. Positiion cursor on first character of line that is an assignment.
  (evil-set-register ?d
                     [?w ?d ?f ?\[ ?f ?\] ?d ?f ?= ?i ?: escape ?A ?, escape ?j ?0])


  ;; Stop asking about whether local variables are safe.
  (setq enable-local-variables :all)

  ;; Commented out so that
  ;; Setting this so that Tramp uses bash
  ;; (setq explicit-shell-file-name "/bin/bash")
  ;; (push
  ;;  (cons
  ;;   "docker"
  ;;   '((tramp-login-program "docker")
  ;;     (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
  ;;     (tramp-remote-shell "/bin/sh")
  ;;     (tramp-remote-shell-args ("-i") ("-c"))))
  ;;  tramp-methods)


  ;; (defadvice tramp-completion-handle-file-name-all-completions
  ;;     (around dotemacs-completion-docker activate)
  ;;   "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
  ;;   a list of active Docker container names, followed by colons."
  ;;   (if (equal (ad-get-arg 1) "/docker:")
  ;;       (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
  ;;              (dockernames (cl-remove-if-not
  ;;                            #'(lambda (dockerline) (string-match ":$" dockerline))
  ;;                            (split-string dockernames-raw "\n"))))
  ;;         (setq ad-return-value dockernames))
  ;;     ad-do-it))

  (defun count-buffers (&optional display-anyway)
    "Display or return the number of buffers."
    (interactive)
    (let ((buf-count (length (buffer-list))))
      (if (or (interactive-p) display-anyway)
          (message "%d buffers in this Emacs" buf-count)) buf-count))

  ;; Enable company globally so that auto-completion works in modes
  ;; where it's not explicitly supported, like less-mode.
  ;; (global-company-mode)
  ;; (global-auto-complete-mode)

  (defun large-file-hook ()
    "If a file is over a given size, make the buffer read only."
    (when (> (buffer-size) (* 1024 1024))
      ;; Don't allow file editing
      (setq buffer-read-only t)
      (buffer-disable-undo)
      ;; Fundamental mode doesn't font lock
      (fundamental-mode)
      (linum-mode -1)))
  (add-hook 'find-file-hook 'large-file-hook)

;;;;;;;;;;;;;;;
  ;; Git/Magit ;;
;;;;;;;;;;;;;;;
  (setq magit-blame-heading-format "%-20a %C %s %H")
  ;; Chrome layer listj
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . org-mode)
          ("everstring\\.atlassian\\.net . org-mode")))

  (require 'editorconfig)
  (editorconfig-mode 1)

  ;; Ag doesn't search dotfiles and dot-directories by default. Adding the
  ;; --hidden option will search these files. It will still ignore paths
  ;; matched in the .agignore file, so I've added .git/ to that ag won't
  ;; search .git/
  (setq helm-ag-command-option "--hidden")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(fringe-mode 6 nil (fringe))
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-mode-show-parse-errors nil)
 '(linum-format " %7d ")
 '(org-clock-auto-clock-resolution t)
 '(org-clock-continuously nil)
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
 '(package-selected-packages
   (quote
    (swiper ivy clj-refactor cider tern haskell-mode company helm yasnippet magit magit-popup git-commit async f inf-ruby emoji-cheat-sheet-plus company-emoji editorconfig flycheck-elm elm-mode gmail-message-mode ham-mode html-to-markdown edit-server zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sws-mode sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance stekene-theme srefactor spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restclient restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa pyvenv pytest pyenv-mode py-autopep8 purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy pastels-on-dark-theme paradox page-break-lines osx-trash orgit organic-green-theme org-repo-todo org-present org-plus-contrib org-bullets org-autolist open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode live-py-mode lispy linum-relative link-hint light-soap-theme leuven-theme less-css-mode launchctl jsx-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md ggtags gandalf-theme flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dracula-theme dockerfile-mode django-theme diff-hl define-word dash-at-point darktooth-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web company-tern company-statistics company-quickhelp colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme browse-at-remote bracketed-paste birds-of-paradise-plus-theme beeminder badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alert alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-link ace-jump-helm-line ac-ispell)))
 '(python-shell-interpreter "ipython")
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((web-mode-markup-indent-offset . 4)
     (sgml-basic-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 4096)) (:foreground "#c6c6c6" :background "#303030")) (((class color) (min-colors 256)) (:foreground "#c6c6c6" :background "#303030")) (((class color) (min-colors 89)) (:foreground "#c6c6c6" :background "#303030"))))
 '(ace-jump-face-foreground ((((class color) (min-colors 89)) (:foreground "#ff8700" :bold t))))
 '(company-tooltip-common ((((class color) (min-colors 89)) (:background "#6c6c6c" :foreground "#afd7ff"))))
 '(company-tooltip-common-selection ((((class color) (min-colors 89)) (:background "#005f87" :foreground "#afd7ff" :bold t)))))
