;;; packages.el --- jason-smartparens Layer packages File for Spacemacs
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

(defvar jason-smartparens-packages
  '(
    ;; package jason-smartparenss go here
    smartparens
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar jason-smartparens-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function jason-smartparens/init-<package-jason-smartparens>
;;
;; (defun jason-smartparens/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun jason-smartparens/init-smartparens ()
  "Initialize my package"
        (evil-leader/set-key
         ;; hybrid-sexp kill
        "ohk" 'sp-kill-hybrid-sexp
         ;; hybrid-sexp transpose (switch current hsexp w/ previous one)
        "oht" 'sp-transpose-hybrid-sexp
        ;; hybrid-sexp push (push current hsexp ahead of the one in front, pushing
        ;; it back
        "ohp" 'sp-push-hybrid-sexp
        ;; hybrid-sexp slurp. Slurps in the next sexp into the current one.
        "ohs" 'sp-slurp-hybrid-sexp))
