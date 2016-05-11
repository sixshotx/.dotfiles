(add-hook 'org-finalize-agenda-hook
          (lambda ()
            (message "org agenda hook")
            ;; Don't split long lines across multiple visual lines in agenda
            ;; because it breaks the clock report table.
            (visual-line-mode 1)))

;; Navigation
(defun org-goto-last-heading ()
  "Go to last heading in the current subtree"
  (interactive)
  ;; 1. Move to next tree
  (org-forward-heading-same-level 1)
  ;;  2. Move to last heading in previous tree
  (outline-previous-visible-heading 1)
  ;; 3. Ignore tags when moving to the end of the line.
  (let ((org-special-ctrl-a/e t))
    (org-end-of-line)))

(setq org-tag-alist (quote ((:startgroup)
                            ("@home" . ?H)
                            ("@work" . ?W)
                            ("@errand" . ?E)
                            (:endgroup)
                            ("today" . ?t)
                            ("overdue" . ?o)
                            ("week" . ?w)
                            )))
;; I use C-c c to start capture mode
(setq org-default-notes-file "~/Dropbox/org/refile.org")

;; Org capture
;; http://orgmode.org/manual/Capture-templates.html
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
               "* TODO %?")
              ;; For one-off reminders. Templates here will
              ("r" "Reminder/Scheduled" entry
               (file+olp "~/Dropbox/org/life.org" "Life" "Reminders")
               "* TODO %?\nSCHEDULED: %T")
              ;; ("r" "Reminder" entry
              ;;  (file+olp "~/Dropbox/org/life.org" "Life" "Reminders")
              ;;  ;; Using %a doesn't work in format-time-string here.
              ;;  "* TODO %?\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %H:%M>\")")
              )))

;; Org habits
;; Position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;; Org agenda
(global-set-key (kbd "<f12>") 'org-agenda)
;; Enable habit tracking (and a bunch of other modules)
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
            (local-set-key "\M-I" 'org-toggle-image-in-org)
            ;; fix tab
            (local-set-key "\C-y" 'yank)
            (local-set-key (kbd "<C-M-return>") 'org-insert-todo-subheading)))

                                        ; Text editing

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(defun skip-waiting ()
  "Skip trees that aren't waiting"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (re-search-forward ":waiting:" subtree-end t)
        nil
      subtree-end)))

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

;; Use multiples of 0:45 to support tocks.
(setq org-global-properties (quote (("Effort_ALL" . "0:45 1:30 2:15 3:00 3:45 4:30 5:15 6:00")
                                    ("STYLE_ALL" . "habit")
                                    ("EFFORT_T_ALL" . "0:45 1:30 2:15 3:00 3:45 4:30 5:15 6:00"))))
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
;; Org mode attachments
(setq org-file-apps '((auto-mode . default)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . default)
                      ("\\.jpg\\'" . default)
                      ("\\.jpeg\\'" . default)
                      ("\\.png\\'" . default)))


;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; For tag searches ignore tasks with scheduled and deadline dates
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


;; TODO: Fix this later
;; (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; set up the call to terminal-notifier
;; Set terminal-notifier notification preferences in System Preferences if you want them to be sticky.
(defalias 'sqa 'shell-quote-argument)
(defun my-appt-send-notification (title msg)
  (shell-command (concat pushbullet " --title " (sqa title) " --message " (sqa msg))))

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification
   (format "'%s'" msg)
   (format "'in %s minutes'" min-to-app)
   ))
(setq appt-disp-window-function (function my-appt-display))

(defun jy/next-friday ()
  "Return the ISO timestamp of the next Friday from today"
  (let* ((day-of-week-today (nth 6 (decode-time (current-time))))
         (day-of-week-friday 5)
         (days-till-friday (- day-of-week-friday day-of-week-today))

         ;; Time as an elisp time object
         (time-till-friday (days-to-time days-till-friday))
         (time-friday (time-add (current-time) time-till-friday))
         (time-friday-timestamp (format-time-string "%Y-%m-%d" time-friday)))
    (progn
      time-friday-timestamp)))

(defun jy/org-set-deadline-to-end-of-week (arg)
  "Set this headline's deadline to the end of this week"
  (interactive "P")
  (progn
    (org-deadline nil (jy/next-friday))))

;; Get rid of whitespace mode. Annoying in org files
;; Clocking settings
(provide 'jason)
