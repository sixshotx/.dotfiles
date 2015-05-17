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
    pymacs
    ;; elpy
    ;; Mm, makes everythying too slow
    ;; jedi
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
  "Init jedi"
  (use-package jedi
    :defer t
    :init
    (progn
      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:complete-on-dot t)
      (add-hook 'python-mode-hook 'indent-guide-mode)
      )))


(defun jason-python/init-py-autopep8 ()
  "Automatically format python files to comply with pep8"
  (use-package autopep8
    :defer t
    :init (progn
            (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
            (setq py-autopep8-options '("--ignore=E309,E301" "--max-line-length=85")))
    ))

(defun jason-python/init-pymacs ()
  "Refactoring support"
  (use-package pymacs
    :defer t
    :init (progn
            (pymacs-load "ropemacs" "rope-")
            ;; Disable default shortcuts
            (setq ropemacs-enable-shortcuts nil)
            (evil-leader/set-key-for-mode 'python-mode
              ;; *r*efactor *e*xtract *v*ariable
              "mrev" 'rope-extract-variable
              ;; *r*efacttor *e*xtraact *m*ethod
              "mrem" 'rope-extract-method
              ;; *r*efactor *r*ename
              "mrr" 'rope-rename

              ;; *find* *o*currences
              "m/o" 'rope-find-occurrences
              ))
    )))
