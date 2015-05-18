;;; packages.el --- jason-ggtags Layer packages File for Spacemacs
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
(defvar jason-ggtags-packages
  '(
    ;; package jason-ggtagss go here
    ggtags
    ))

;; List of packages to exclude.
(defvar jason-ggtags-excluded-packages '())

;; For each package, define a function jason-ggtags/init-<package-jason-ggtags>
;;
;; (defun jason-ggtags/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
