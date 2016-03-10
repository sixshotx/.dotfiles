(add-hook 'org-mode-hook
          (lambda ()
            (message "org mode hook")
            ;; Split long lines across multiple visual lines since text describing
            ;; tasks can and should be long.
            (visual-line-mode 1)
            ;; Disable linum-mode for org-mode. Adding line numbers to large buffers is super slow.
            (linum-mode 0)

            ;; Replace prelude's smart open above w/ org mode's insert heading
            (define-key org-mode-map [remap prelude-smart-open-line-above] 'org-insert-todo-heading-respect-content)
            (define-key org-mode-map [remap prelude-google] 'org-goto-last-heading)
            ))

;; Log state changes into a logbook
(setq org-log-into-drawer t)
;; Start out indented since it's hard to understand org files
;; when they're not indented.
(setq org-startup-indented t)
