;;; packages.el --- jason-js Layer packages File for Spacemacs
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

(defvar jason-js-packages
  '(
    ;; package jason-jss go here
    ;; The famous js2-mode that steve yegge loves.
    js2-mode
    ;; A dependency of ac-js2
    skewer-mode
    ;; Autocomplete for js2
    ac-js2
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar jason-js-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function jason-js/init-<package-javascript>
;;
;; (defun jason-js/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun jason-js/init-js2-mode '()
  "Sets up js2-mode"
  ;; Install as major mode for js files.
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Install as minor mode for js linting.
  ;; Set node as interpreter.
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
  )

(defun jason-js/init-ac-js2 '()
  "Sets up ac-js2"
  ;; Enable js2 autocomplete.
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  )
