;;; packages.el --- jason-org Layer packages File for Spacemacs
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

(defvar jason-org-packages
  '(
    ;; package jason-orgs go here
    appt
    ;; Needed for norang
    bbdb
    moe-theme
    (norang :location local)
    org-autolist
    (org-checklist :location local)
    (org-depend :location local)
    (org-pomodoro :location local)
    ;; Installed by org-plus-contrib
    (org-subtask-reset :location built-in)
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar jason-org-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function jason-org/init-<package-jason-org>
;;
;; (defun jason-org/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun jason-org/init-org-autolist ()
  "Calvin's autolist mode"
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  (org-autolist-mode)
  )

(defun jason-org/init-moe-theme ()
  ""
  (use-package moe-theme))

(defun jason-org/init-bbdb ()
  ""
  (use-package bbdb))

(defun jason-org/init-org-depend ()
  "Initialize my extension"
  (use-package org-depend)
  )

(defun jason-org/init-org-checklist ()
  "Initialize my extension"
  (use-package org-checklist)
  )

(defun jason-org/init-org-subtask-reset ()
  "Initialize my extension"
  (use-package org-subtask-reset)
  )

(defun jason-org/init-norang ()
  ""
  (message "trying to load norang")
  (use-package norang
    :config
    (message "done loading norang")))

(defun jason-org/init-org-pomodoro ()
  "Initialize my extension"
  (message "loading org-pomodoro")
  (use-package org-pomodoro
    :config
    (message "loading org-pomodoro")
    ))
