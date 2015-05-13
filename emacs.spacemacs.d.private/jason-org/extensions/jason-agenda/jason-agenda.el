(defun jason-org/skip-entry-unless-clocked-in-today ()
  "Skip function. Only see tasks that
    - have a today tag
    - have a clock entry for today"
  (let* ((entry-end (save-excursion (outline-next-heading) (1- (point))))
         ;; Get LOGBOOK
         ;; Get timestamp start and end position.
         (timestamp-start-pos (re-search-forward "CLOCK: \\[" entry-end t))
         (timestamp-end-pos (save-excursion
                              (re-search-forward "\\]" entry-end t)))
         skip timestamp-str timestamp
         )
    (if (and timestamp-start-pos timestamp-end-pos)
        ;; Get timestamp str itself
        (progn
          (setq timestamp-str (buffer-substring timestamp-start-pos (- timestamp-end-pos 1)))
          (print "timestamp-str")
          (print timestamp-str)
          ;; Convert timestamp str to elisp time
          (setq timestamp (org-time-string-to-time timestamp-str))
          ;;(message "return value is %S" (jason-org/is-today timestamp))
          ;; Skip if we don't find a timestamp
          (setq skip (not (jason-org/is-today timestamp)))
          (and skip entry-end))
      entry-end)))

(defun jason-org/skip-entry-unless-today-tag ()
  (let ((end (save-excursion (org-end-of-subtree t)))
        (entry-end (save-excursion (outline-next-heading) (1- (point))))
        skip-not-today skip-not-clocked-today)
    (save-excursion
      (setq skip-not-today (not (member "today" (org-get-tags-at)))))
    (and skip-not-today entry-end)))

(defun jason-org/skip-nothing () nil)
(defun jason-org/skip-everything ()
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    subtree-end))

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
                ;;(agenda "" nil)
                (tags-todo "+twice"
                           ((org-agenda-overriding-header "Twice")))
                ;; (tags-todo "+life"
                ;;            ((org-agenda-overriding-header "Life")))
                )
               ;; Settings that apply to the entire block agenda
               ((org-agenda-skip-function
                 (lambda ()
                   (and (jason-org/skip-entry-unless-today-tag) (jason-org/skip-entry-unless-clocked-in-today))))
                (org-agenda-overriding-columns-format "%70ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Clocksum) %10CLOCKSUM_T")
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
                (tags-todo "week"
                           ((org-agenda-overriding-header "Tasks to do this week")
                            (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo '("DONE" "CODE REVIEW")))))
                (todo "NEXT"
                     ((org-agenda-overriding-header "All Next Tasks")))
                (tags-todo "today"
                     ((org-agenda-overriding-header "Today tasks")
                      (org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo '("DONE" "CODE REVIEW")))))
                nil)))))

(provide 'jason-agenda)
