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
    (jason :location local)
    (clock-settings :location local)
    (jason-agenda :location local)
    moe-theme
    org-autolist
    (org-bh :location local)
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

(defun jason-org/init-org-bh ()
  "Initialize my extension"
  ;; Don't defer any of these b/c we want org mode to be available at all time
  (use-package org-bh)
  )

(defun jason-org/init-jason ()
  "Initialize my extension"
  ;; Don't defer any of these b/c we want org mode to be available at all time
  (eval-after-load 'org-bh
    (use-package jason))
  )

(defun jason-org/init-clock-settings ()
  (with-eval-after-load 'org-bh
        (use-package clock-settings
          :defer t
          :init
          ;; This block executes before the package has been loaded
          :config
          ;; This block executes after the package has been loaded
          )))
(defun jason-org/init-jason-settings ()
  (with-eval-after-load 'org
      (use-package jason-settings
        :defer t
        :init
        ;; This block executes before the package has been loaded
        :config
        ;; This block executes after the package has been loaded
        )))

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

(defun jason-org/init-jason-agenda ()
  "Initialize my extension"
  (eval-after-load 'org-agenda
    (use-package jason-agenda))
  )

(defun jason-org/init-org-pomodoro ()
  "Initialize my extension"
  (message "loading org-pomodoro")
  (use-package org-pomodoro
    :config
    (message "loading org-pomodoro")
    ))
