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
    org
    org-autolist
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

(defun jason-org/init-org ()
  "Init my really custom org mode setup"
  ;; TODO
  ;; These are temporarily hardcoded. I'll just leave these here for now and switch if this
  ;; project gets stable
  ;; Some customizations originally from custom.el of my old emacs setup.
  (setq fill-column 85)
  (setq org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 6)))
  (setq org-agenda-log-mode-items (quote (clock)))
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-span (quote day))
  (setq org-agenda-start-with-clockreport-mode t)
  )

(defun jason-org/init-org-autolist ()
  "Calvin's autolist mode"
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  (org-autolist-mode)
  )

