;;;;;;;;;;;;;;;;;;;;;;;
;; Org Todo Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Keywords after "|" are terminal
;; STATE(@/!).
;; @ means adding a time and prompting user to enter a tnote
;; ! means only adding a time.
;; X/Y means "do X when entering a state" and Y means "do Y" iff the target
;; state does not define X
(setq org-todo-keywords
      ;; TODO means stuff to do that we haven't started.
      ;; NEXT means that we've started doing it and might need to finish it.
      ;; WAITING means that we're waiting on external input before we can
      ;; continue working on it.
      ;; CANCELED means we don't want to do it at all
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "|" "CANCELED(c@)" ))))
;; Allows changing todo states with S-left and S-right w/o triggering things like
;; notes and state triggers.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "PaleVioletRed" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CODE_REVIEW" :foreground "orange" :weight bold)
              ("CANCELED" :foreground "light green" :weight bold)
              ("WAITING" :foreground "forest green" :weight bold))))
;; Org tags
(setq org-tag-faces
      (quote (("@home" :foreground "light green" :weight bold)
              ("@work" :foreground "light red" :weight bold)
              ("@errand" :foreground "SteelBlue1" :weight bold)
              ("scheduled" :foreground "yellow1" :weight bold)
              ("today" :foreground "OrangeRed2" :weight bold)
              ("overdue" :foreground "Red" :weight bold)
              ("week" :foreground "salmon1" :weight bold))))

;; Don't allow marking a headline as done until its children
;; are marked done.
(setq org-enforce-todo-dependencies t)

;; Don't allow marking a headline done until its child
;; checkboxes are done.
(setq org-enforce-todo-checkbox-dependencies t)
