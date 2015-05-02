;;; packages.el --- jason-python Layer packages File for Spacemacs
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

(defvar jason-python-packages
  '(
    ;; package jason-pythons go here
    ;; pymacs
    ;; ropemacs
    ;; elpy
    jedi
    ;; Automatically formats code to be pep8 compliant
    py-autopep8
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar jason-python-excluded-packages ()
  "List of packages to exclude.")

;; For each package, define a function jason-python/init-<package-jason-python>
;;
;; (defun jason-python/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

;; (defun jason-ropemacs/init-pymacs ()
;;   "Some stuff"
;;   (pymacs-load "ropemacs" "rope-")
;;   )

(defun jason-python/init-jedi ()
  "Init elpy, an ll-in-one python dev environment"
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  )


(defun jason-python/init-py-autopep8 ()
  "Init"
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

  (setq py-autopep8-options '("--ignore=E309,E301" "--max-line-length=85"))
  )
