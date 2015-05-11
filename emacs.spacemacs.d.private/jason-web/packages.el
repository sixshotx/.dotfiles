;;; packages.el --- jason-web Layer packages File for Spacemacs
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

(defvar jason-web-packages
  '(
    ;; package jason-webs go here
    emmet-mode
    ac-emmet
    web-mode
    scss-mode
    jade-mode
    sws-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar jason-web-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function jason-web/init-<package-jason-web>
;;
;; (defun jason-web/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun jason-web/init-web-mode ()
  "Init web stuff"
  ;; Use web-mode for these files.
  (add-to-list 'auto-mode-alist '("\\.htmlmk\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jqt\\'" . web-mode))

  (setq web-mode-content-types-alist
  '(("html" . "\\.jqt\\'")
    ))

  ;; Associate template engines
  (setq web-mode-engines-alist
        '(("mako"    . "\\.htmlmk\\'")
          ("handlebars" . "\\.jqt\\'")))

  ;; Emmet and snippets
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
  (setq web-mode-markup-indent-offset 4)
)

(defun jason-web/init-scss-mode ()
  "scss"
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

(defun jason-web/init-jade-mode ()
  "scss"
  (require 'jade-mode)
)

(defun jason-web/init-sws-mode ()
  "scss"
  (require 'sws-mode)
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))
)
