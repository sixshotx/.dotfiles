;;; extensions.el --- jason-haskell Layer extensions File for Spacemacs
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

(defvar jason-haskell-pre-extensions
  '(
    ;; pre extension jason-haskells go here
    )
  "List of all extensions to load before the packages.")

(defvar jason-haskell-post-extensions
  '(
    ;; post extension jason-haskells go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function jason-haskell/init-<extension-jason-haskell>
;;
;; (defun jason-haskell/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
