;;; packages.el --- screenshot Layer packages File for Spacemacs
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
(defvar screenshot-packages
  '(
    org-screenshot
    ;; package screenshots go here
    ))

;; List of packages to exclude.
(defvar screenshot-excluded-packages '())

;; For each package, define a function screenshot/init-<package-screenshot>
;;
;; (defun screenshot/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun screenshot/init-org-screenshot
    (setq org-screenshot-command-line "screencapture -i %f")
)
