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
    org-autolist
    moe-theme
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
