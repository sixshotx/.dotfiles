;;; packages.el --- hydra Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq hydra-packages
    '(
      hydra
      ;; package hydras go here
      ))

;; List of packages to exclude.
(setq hydra-excluded-packages '())

;; For each package, define a function hydra/init-<package-hydra>
;;
;; (defun hydra/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package


(defun hydra/init-hydra ()
  "Initialize my package"
  (defhydra hydra-helm-like-unite ()
    "vim movement"
    ("?" helm-help "help")
    ("<escape>" keyboard-escape-quit "exit")
    ("<SPC>" helm-toggle-visible-mark "mark")
    ("a" helm-toggle-all-marks "(un)mark all")
    ;; not sure if there's a better way to do this
    ("/" (lambda ()
           (interactive)
           (execute-kbd-macro [?\C-s]))
     "search")
    ("v" helm-execute-persistent-action)
    ("gg" helm-beginning-of-buffer "top")
    ("G" helm-end-of-buffer "bottom")
    ("j" helm-next-line "down")
    ("k" helm-previous-line "up")
    ("i" nil "cancel"))

  ;; to escape
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "<escape>") 'hydra-helm-like-unite/body))
  ;; or with key-chord.el; suggested by ReneFroger
  ;; (key-chord-define minibuffer-local-map "jk" 'hydra-helm-like-unite/body)

  (defhydra hydra-buffer-menu (:color pink
                                      :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

  (define-key Buffer-menu-mode-map (kbd "<escape>") 'hydra-buffer-menu/body)

  ;; (defhydra hydra-org (:color blue)
  ;;   "org"
  ;;   ("c" "capture" org-capture ))

  ;; (evil-leader/set-key-for-mode 'org-mode
  ;;   "<escape>" 'hydra-org/body)
  ;; Org mode stuff that we want to be accessible from every buffer.
  (defhydra hydra-global-org (:color blue
                                     :hint nil)
    "
Timer^^        ^Clock^         ^Capture^
--------------------------------------------------
 _a_genda      _w_ clock in    _c_ capture
               _o_ clock out   _l_ast capture
               _j_ clock goto
               _z_ clock resolve
"
    ("a" org-agenda)
    ;; Using 'i' causes an error here for some reason.
    ("w" (org-clock-in '(4)))
    ("o" org-clock-out)
    ("z" org-resolve-clocks)
    ;; Visit the clocked task from any buffer
    ("j" org-clock-goto)
    ("c" org-capture)
    ("l" org-capture-goto-last-stored))

  ;; Red - Stay in hydra after running command
  (defhydra hydra-org-timestamp (:color red)
    "org timestamp"
    ("l" org-shiftright "shift right")
    ("h" org-shiftleft "shift left")
    ("k" org-shiftup "shift up")
    ("j" org-shiftdown "shift down")
    ("q" nil "quit" :color blue))

  (evil-leader/set-key "oo" 'hydra-global-org/body)
  ;; Jump between git hunks and goto/revert them.
  (defhydra hydra-git ()
    "git"
    ("j" diff-hl-next-hunk "next")
    ("k" diff-hl-previous-hunk "prev")
    ("g" diff-hl-diff-goto-hunk "goto")
    ("r" diff-hl-revert-hunk "revert"))
  (evil-leader/set-key "og" 'hydra-git/body)

  ;; TODO: set delete-by-moving-to-trash to t
  (defhydra hydra-dired (:color pink :hint nil)
    "
Deletion^^             ^Marks^  ^Opening^          ^Navigation^ ^Operations^
    flag _d_eletion    _m_ark _o_ther window    _h_ up dir
_u_nflag deletion      _J_ next marked file
    flag _r_egexp      _K_ previous marked file
    _x_ delete flagged _R_ filename regexp
                       _g_ contents regexp
"
    ;; Deletion
    ("d" dired-flag-file-deletion)
    ("u" dired-unmark)
    ("r" dired-flag-files-regexp)
    ("x" dired-do-flagged-delete)
    ;; Marks
    ("m" dired-mark)
    ("J" dired-next-marked-file)
    ("K" dired-prev-marked-file)
    ("R" dired-mark-files-regexp)
    ("g" dired-mark-files-containing-regexp)
    ;; Opening
    ("o" dired-display-file)
    ;; Navigation
    ("h" dired-up-directory)
    ("q" nil "quit" :color blue))

  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "<escape>") 'hydra-dired/body))

  (defhydra hydra-org-heading (:color blue)
    "
Movement^^               ^More^
_j_ move subree down      _y_ cut subtree
_k_ move subtree up       _p_ paste subtree
_h_ move subtree left
_l_ move subtree right
"
    ("j" org-move-subtree-down :color red)
    ("k" org-move-subtree-up :color red)
    ("h" org-promote-subtree :color red)
    ("l" org-demote-subtree :color red)
    ("$" org-goto-last-heading)
    ("y" org-cut-subtree)
    ("p" org-paste-subtree)
    ("i" org-insert-heading-after-current "heading insert")
    ("I" org-insert-heading "heading insert before")
    ("d" org-cut-subtree "delete subtree")
    ("o" org-insert-todo-heading-respect-content "todo insert below"))

  (defhydra hydra-org-clock (:color blue)
    "org clock"
    ("i" org-clock-in)
    ("o" org-clock-out))

  (defhydra hydra-org (:color blue)
    "
Editing^^          ^Visibility^           ^Navigation^
  _w_ refile         indirect _b_uffer      _u_p
  _c_apture
  _d_eadline (eow)
  _e_xport
  e_f_fort
  _:_ tag
  _p_omodoro
"
    ;; ("'" org-edit-special)
    ("w" org-refile)
    ("c" org-capture)
    ("d" jy/org-set-deadline-to-end-of-week)
    ("e" org-export-dispatch)
    ("f" org-set-effort)
    (":" org-set-tags)
    ("b" org-tree-to-indirect-buffer)
    ("p" org-pomodoro)
    ;; Nested hydras
    ("h" hydra-org-heading/body "heading" :exit t)
    ("c" hydra-org-clock/body "clock" :exit t)
    ("t" hydra-org-timestamp/body "timestamp" :exit t)
    ("u" outline-up-heading))
  (evil-leader/set-key "," 'hydra-org/body)

  ;; Hit "v" in the agenda view.
  ;; (with-eval-after-load 'org-agenda
  ;;   (define-key org-agenda-mode-map
  ;;     "v" 'hydra-org-agenda-view/body))

  (defun org-agenda-cts ()
    (let ((args (get-text-property
                 (min (1- (point-max)) (point))
                 'org-last-args)))
      (nth 2 args)))

  (defhydra hydra-org-agenda-view (:hint nil)
    "
_d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week       _[_: inactive      _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
_m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view
     (if (eq 'day (org-agenda-cts))
         "[x]" "[ ]"))
    ("w" org-agenda-week-view
     (if (eq 'week (org-agenda-cts))
         "[x]" "[ ]"))
    ("t" org-agenda-fortnight-view
     (if (eq 'fortnight (org-agenda-cts))
         "[x]" "[ ]"))
    ("m" org-agenda-month-view
     (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
    ("y" org-agenda-year-view
     (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
    ("l" org-agenda-log-mode
     (format "% -3S" org-agenda-show-log))
    ("L" (org-agenda-log-mode '(4)))
    ("c" (org-agenda-log-mode 'clockcheck))
    ("f" org-agenda-follow-mode
     (format "% -3S" org-agenda-follow-mode))
    ("a" org-agenda-archives-mode)
    ("A" (org-agenda-archives-mode 'files))
    ("r" org-agenda-clockreport-mode
     (format "% -3S" org-agenda-clockreport-mode))
    ("e" org-agenda-entry-text-mode
     (format "% -3S" org-agenda-entry-text-mode))
    ("g" org-agenda-toggle-time-grid
     (format "% -3S" org-agenda-use-time-grid))
    ("D" org-agenda-toggle-diary
     (format "% -3S" org-agenda-include-diary))
    ("!" org-agenda-toggle-deadlines)
    ("["
     (let ((org-agenda-include-inactive-timestamps t))
       (org-agenda-check-type t 'timeline 'agenda)
       (org-agenda-redo)))
    ("q" (message "Abort") :exit t))

  )
