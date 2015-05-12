;; TODO Not sure if I need these
;; (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
;; (add-hook 'org-mode-hook (lambda () (whitespace-mode 0)))

; Agenda hooks
;; (add-hook 'org-agenda-mode-hook (lambda () (visual-line-mode 1)))

;; Use sticky agenda's so they persist. Useful to have an agenda view per buffer and just navigate among them

; Navigation
(defun org-goto-last-heading ()
  (interactive)
  (org-forward-heading-same-level 1)     ; 1. Move to next tree
  (outline-previous-visible-heading 1)   ; 2. Move to last heading in previous tree
  (let ((org-special-ctrl-a/e t))        ; 3. Ignore tags when
    (org-end-of-line)))                  ;    moving to the end of the line

; Replace prelude's smart open above w/ org mode's insert heading
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [remap prelude-smart-open-line-above] 'org-insert-todo-heading-respect-content)
             (define-key org-mode-map [remap prelude-google] 'org-goto-last-heading)))
(setq org-startup-indented t)
(setq org-agenda-follow-mode t)

;; Org mode
; Necessary org global commands
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

; Org mode todo settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "CODE REVIEW(c)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" ))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "PaleVioletRed" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CODE REVIEW" :foreground "orange" :weight bold)
              ("WAITING" :foreground "forest green" :weight bold))))
(setq org-tag-faces
      (quote (("@home" :foreground "light green" :weight bold)
              ("@work" :foreground "light red" :weight bold)
              ("@errand" :foreground "SteelBlue1" :weight bold)
              ("scheduled" :foreground "yellow1" :weight bold)
              ("today" :foreground "OrangeRed2" :weight bold)
              ("week" :foreground "salmon1" :weight bold))))

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
;; I use C-c c to start capture mode
(setq org-default-notes-file "~/Dropbox/org/refile.org")
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f11>") 'org-clock-goto)

;; Org capture
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n")
              ("h" "Habit" entry (file "~/Dropbox/org/life.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ("j" "Journal" entry (file+datetree "~/Dropbox/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "Twice One-Offs" entry (file+olp "~/Dropbox/org/twice.org" "Twice Root" "One-offs")
               "* TODO %? :week:")
              ("p" "Programming Productivity" entry (file+olp "~/Dropbox/org/life.org" "Life" "Extra Programming")
               "* TODO %?"))))

;; Org habits
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;; Org clocking
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
             
; Org agenda
(global-set-key (kbd "<f12>") 'org-agenda)
(setq org-agenda-files (quote ("~/Dropbox/org")))
; Replace prelude's smart open above w/ org mode's insert heading
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [remap prelude-smart-open-line-above] 'org-insert-todo-heading-respect-content)))

                                        ; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-habit org-checklist org-depend)))

; Org-mode specific shortcuts
(add-hook 'org-mode-hook 
          (lambda ()
            (define-key org-mode-map [remap move-text-up] 'org-move-subtree-up)
            (define-key org-mode-map [remap move-text-down] 'org-move-subtree-down)
            (local-set-key "\M-n" 'outline-next-visible-heading)
            (local-set-key "\M-p" 'outline-previous-visible-heading)
            ;; table
            (local-set-key "\C-\M-w" 'org-table-copy-region)
            (local-set-key "\C-\M-y" 'org-table-paste-rectangle)
            (local-set-key "\C-\M-l" 'org-table-sort-lines)
            ;; display images
            (local-set-key "\M-I" 'org-toggle-iimage-in-org)
            ;; fix tab
            (local-set-key "\C-y" 'yank)
            (local-set-key (kbd "<C-M-return>") 'org-insert-todo-subheading)))

; Text editing
; delete whitespace/word
;(setq viper-mode t)
; (require 'viper)
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(defun skip-waiting ()
  "Skip trees that aren't waiting"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (re-search-forward ":waiting:" subtree-end t)
        nil
      subtree-end)))

(defun jason-org/skip-unless-clocked-in-today ()
  "Skip function. Only see tasks that
    - have a today tag
    - have a clock entry for today"
  (if (member "today" (org-get-tags-at))
      nil
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             ;; Get LOGBOOK
             (logbook-start (save-excursion (re-search-forward ":LOGBOOK:" subtree-end t)))
             ;; Get timestamp start and end position.
             (timestamp-start-pos (save-excursion
                                    (progn
                                      (re-search-forward "CLOCK: \\[" subtree-end t))))
             (timestamp-end-pos (save-excursion
                                  (progn
                                    (re-search-forward "\\]" subtree-end t))))
             )
        (if (and timestamp-start-pos timestamp-end-pos)
            ;; Get timestamp str itself
            (condition-case nil
                (progn
                  (let* ((timestamp-str (buffer-substring timestamp-start-pos (- timestamp-end-pos 1)))
                         ;; Convert timestamp str to elisp time
                         (timestamp (org-time-string-to-time timestamp-str)))
                    ;;(message "return value is %S" (jason-org/is-today timestamp))
                    ;; Skip if we don't find a timestamp
                    (if (jason-org/is-today timestamp)
                        ;; Don't skip
                        nil
                      ;; Skip
                      subtree-end
                      )))
              (error subtree-end))
          subtree-end))))

(defun jason-org/is-today (timestamp)
  "Takes a timestamp and return t if timestamp occurs during the current day"
  (let*
      (;; Get today's day number
       (today-day-num (nth 3 (decode-time (current-time))))
       (timestamp-day-num (nth 3 (decode-time timestamp)))
       (ret (eq timestamp-day-num today-day-num)))
    ;; debug
    ;;(message "Today's day num %d" today-day-num)
    ;; (message "Passed in timestamp's day num: %d" timestamp-day-num)
    ;; (message "ret: %S" ret)
    ret
    ))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("X" "Xtra agenda" todo "TODO"
               ((org-agenda-overriding-header "Xtra header")
                (org-agenda-skip-function 'skip-waiting)))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("p" "Today Block Agenda"
               (
                ;; Don't have anything actually on the agenda: we have this here so we can see the clock report.
                (agenda "" nil)
                (tags-todo "+twice"
                           ((org-agenda-overriding-header "Twice")
                            ;;(org-agenda-skip-function 'jason-org/skip-unless-clocked-in-today)
                            ))
                (tags-todo "+life"
                           ((org-agenda-overriding-header "Life")
                            ;; Interestingly, putting this here also uses it for the Twice agenda block, but
                            ;; putting it with the common settings below doesn't work.
                            (org-agenda-skip-function 'jason-org/skip-unless-clocked-in-today)
                            )))
               ;; Settings that apply to the entire block agenda
               (;;(org-agenda-tag-filter '("+today"))
                (org-agenda-overriding-columns-format "%80ITEM(Task) %10Effort(Effort) %10CLOCKSUM_T(Today)")
                (org-agenda-files '("~/Dropbox/org/life.org" "~/Dropbox/org/twice.org"))
                (org-agenda-clockreport-parameter-plist
                 '(:maxlevel 6 :properties ("MAX_EFFORT" "Effort" "CLOCKSUM" "CLOCKSUM_T")))
                )
               )
              (" " "Agenda"
               ((agenda "" nil)
                (tags-todo "refile"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo 'DONE))))
                (todo "WAITING"
                      ((org-agenda-overriding-header "Waiting")))
                (todo "CODE REVIEW"
                      ((org-agenda-overriding-header "Code Review")))
                ;; Stuck projects
                ;; (tags-todo "-CANCELLED/!"
                ;;            ((org-agenda-overriding-header "Stuck Projects")
                ;;             (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                ;;             (org-agenda-sorting-strategy
                ;;              '(category-keep))))
                ;; Projects
                ;; (tags-todo "-HOLD-CANCELLED/!"
                ;;            ((org-agenda-overriding-header "Projects")
                ;;             (org-agenda-skip-function 'bh/skip-non-projects)
                ;;             (org-tags-match-list-sublevels 'indented)
                ;;             (org-agenda-sorting-strategy
                ;;              '(category-keep))))
                ;; Project Next Tasks
                ;; (tags-todo "-CANCELLED/!NEXT"
                ;;            ((org-agenda-overriding-header (concat "Project Next Tasks"
                ;;                                                   (if bh/hide-scheduled-and-waiting-next-tasks
                ;;                                                       ""
                ;;                                                     " (including WAITING and SCHEDULED tasks)")))
                ;;             (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                ;;             (org-tags-match-list-sublevels t)
                ;;             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-sorting-strategy
                ;;              '(todo-state-down effort-up category-keep))))
                ;; Project Subtasks
                ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                ;;            ((org-agenda-overriding-header (concat "Project Subtasks"
                ;;                                                   (if bh/hide-scheduled-and-waiting-next-tasks
                ;;                                                       ""
                ;;                                                     " (including WAITING and SCHEDULED tasks)")))
                ;;             (org-agenda-skip-function 'bh/skip-non-project-tasks)
                ;;             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-sorting-strategy
                ;;              '(category-keep))))
                ;; Standalone Tasks
                ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                ;;            ((org-agenda-overriding-header (concat "Standalone Tasks"
                ;;                                                   (if bh/hide-scheduled-and-waiting-next-tasks
                ;;                                                       ""
                ;;                                                     " (including WAITING and SCHEDULED tasks)")))
                ;;             (org-agenda-skip-function 'bh/skip-project-tasks)
                ;;             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-sorting-strategy
                ;;              '(category-keep))))
                ;; Waiting and Postponed Tasks
                ;; (tags-todo "-CANCELLED+WAITING|HOLD/!"
                ;;            ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                ;;                                                   (if bh/hide-scheduled-and-waiting-next-tasks
                ;;                                                       ""
                ;;                                                     " (including WAITING and SCHEDULED tasks)")))
                ;;             (org-agenda-skip-function 'bh/skip-non-tasks)
                ;;             (org-tags-match-list-sublevels nil)
                ;;             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                ;; (tags "-REFILE/"
                ;;       ((org-agenda-overriding-header "Tasks to Archive")
                ;;        (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                ;;        (org-tags-match-list-sublevels nil)))
                (tags-todo "week"
                           ((org-agenda-overriding-header "Tasks to do this week")
                            (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo '("DONE" "CODE REVIEW")))))
                (todo "NEXT"
                     ((org-agenda-overriding-header "All Next Tasks")))
                (tags-todo "today"
                     ((org-agenda-overriding-header "Today tasks")
                      (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo '("DONE" "CODE REVIEW")))))
                nil)))))

;(setq org-clock-sound "/Users/jason/Downloads/Sound/Terran/Goliath/TGoRdy00.wav")
(setq org-clock-sound t)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)
                                      (" " . bh/show-org-agenda)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . org-set-tags-command)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))
; Clocking
; Org mode will resolve idle time if I'm away for more than n minutes.
(setq org-clock-idle-time 10)

; Mobile org required setup
;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Public/mobile_org_inbox/mobile_inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;; Set to public b/c otherwise there are permissions issues.
(setq org-mobile-directory "~/Dropbox/Public/MobileOrg")
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
                                       ; global Effort estimate values
                                        ; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))
; Sexy autosyncing
;; Fork the work (async) of pushing to mobile
;; https://gist.github.com/3111823 ASYNC org mobile push...
;(require 'gnus-async) 
;; Define a timer variable
;; (defvar org-mobile-push-timer nil
;;   "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")
;; Push to mobile when the idle timer runs out
;; (defun org-mobile-push-with-delay (secs)
;;    (when org-mobile-push-timer
;;     (cancel-timer org-mobile-push-timer))
;;   (setq org-mobile-push-timer
;;         (run-with-idle-timer
;;          (* 1 secs) nil 'org-mobile-push)))
;; After saving files, start an idle timer after which we are going to push 
;; (add-hook 'after-save-hook 
;;  (lambda () 
;;    (if (or (eq major-mode 'org-mode) (eq major-mode 'org-agenda-mode))
;;      (dolist (file (org-mobile-files-alist))
;;        (if (string= (expand-file-name (car file)) (buffer-file-name))
;;            (org-mobile-push-with-delay 10)))
;;      )))
;; Run after midnight each day (or each morning upon wakeup?).
;; (run-at-time "00:01" 86400 '(lambda () (org-mobile-push-with-delay 1)))
;; Run 1 minute after launch, and once a day after that.
;; (run-at-time "1 min" 86400 '(lambda () (org-mobile-push-with-delay 1)))

;; watch mobileorg.org for changes, and then call org-mobile-pull
;; http://stackoverflow.com/questions/3456782/emacs-lisp-how-to-monitor-changes-of-a-file-directory
;;(defun install-monitor (file secs)
;;   (run-with-timer
;;    0 secs
;;    (lambda (f p)
;;      (unless (< p (second (time-since (elt (file-attributes f) 5))))
;;        (org-mobile-pull)))
;;    file secs))
;; (defvar monitor-timer (install-monitor (concat org-mobile-directory "/mobileorg.org") 30)
;;   "Check if file changed every 30 s.")

; Miscellaneous agenda settings
;; Diary
(setq org-agenda-diary-file "~/Dropbox/org/diary.org")

;; Refile
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation



; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)
(setq org-refile-allow-creating-parent-nodes t)

(setq org-ctrl-k-protect-subtree t)
; Org mode attachments
(setq org-file-apps '((auto-mode . default)
 ("\\.mm\\'" . default)
 ("\\.x?html?\\'" . default)
 ("\\.pdf\\'" . default)))

; org tags
; Fast tag setting/unsetting
(setq org-tag-alist (quote ((:startgroup)
                            ("@home" . ?H)
                            ("@work" . ?W)
                            ("@errand" . ?E)
                            (:endgroup)
                            ("today" . ?t)
                            ("week" . ?w)
                            )))

                                        ; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

                                        ; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
; Deft
;(require 'deft)
;(setq deft-extension "txt")
;(setq deft-directory "~/Dropbox/nvalt_notes/")

;(setq org-blank-before-new-entry nil)

; Mac notifications for org mode
;(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '10) ;; warn every 10 minutes from t - appt-message-warning-time
(setq
  appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
  appt-display-mode-line nil     ;; don't show in the modeline
  appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; set up the call to terminal-notifier
;(defvar my-notifier-path 
;  "~/terminal-notifier_1.4.2/terminal-notifier.app/Contents/MacOS/terminal-notifier")  
(defvar my-notifier-path
  "/Users/jason/.rvm/gems/ruby-2.1.0@global/bin/terminal-notifier")
(defun my-appt-send-notification (title msg)
  (shell-command (concat my-notifier-path " -message " msg " -title " title)))

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification 
    (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
    (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))

;; Get rid of whitespace mode. Annoying in org files
;; Clocking settings
(provide 'jason)
