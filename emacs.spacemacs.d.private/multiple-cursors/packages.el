;;; packages.el --- multiple-cursors Layer packages File for Spacemacs
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
(defvar multiple-cursors-packages
  '(
    ;; package multiple-cursorss go here
    multiple-cursors
    ))

;; List of packages to exclude.
(defvar multiple-cursors-excluded-packages '())

;; For each package, define a function multiple-cursors/init-<package-multiple-cursors>
;;
;; (defun multiple-cursors/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun multiple-cursors/init-multiple-cursors ()
  "Initialize `multiple cursors"
  (setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
  (eval-after-load 'evil
    (progn
      (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
      (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state)))
  )
