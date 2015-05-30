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
    (define-key helm-map (kbd "<escape>") 'hydra-helm-like-unite/body)
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

    (defhydra hydra-global-org (:color blue
                                       :hint nil)
      "
Timer^^        ^Clock^         ^Capture^
--------------------------------------------------
s_t_art        _w_ clock in    _c_apture
 _s_top        _o_ clock out   _l_ast capture
_r_eset        _j_ clock goto
_p_rint
"
      ("t" org-timer-start)
      ("s" org-timer-stop)
      ;; Need to be at timer
      ("r" org-timer-set-timer)
      ;; Print timer value to buffer
      ("p" org-timer)
      ("w" (org-clock-in '(4)))
      ("o" org-clock-out)
      ;; Visit the clocked task from any buffer
      ("j" org-clock-goto)
      ("c" org-capture)
      ("l" org-capture-goto-last-stored))

    (evil-leader/set-key "oo" 'hydra-global-org/body))
