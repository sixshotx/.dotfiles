;;; extensions.el --- jason-org Layer extensions File for Spacemacs
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

(defvar jason-org-pre-extensions
  '(
    ;; pre extension jason-orgs go here
    )
  "List of all extensions to load before the packages.")

(defvar jason-org-post-extensions
  '(
    ;; Functions defined by this guy: http://doc.norang.ca/org-mode.html
    org-bh
    org-depend
    org-checklist
    org-subtask-reset
    jason
    jason-agenda
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function jason-org/init-<extension-jason-org>
;;
;; (defun jason-org/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun jason-org/init-org-bh ()
  "Initialize my extension"
  ;; Don't defer any of these b/c we want org mode to be available at all time
  (use-package org-bh)
  )

(defun jason-org/init-jason ()
  "Initialize my extension"
  ;; Don't defer any of these b/c we want org mode to be available at all time
  (eval-after-load 'org-bh
    (use-package jason))
  )

(defun jason-org/init-org-depend ()
  "Initialize my extension"
  (use-package org-depend)
  )

(defun jason-org/init-org-checklist ()
  "Initialize my extension"
  (use-package org-checklist)
  )

(defun jason-org/init-org-subtask-reset ()
  "Initialize my extension"
  (use-package org-subtask-reset)
)

(defun jason-org/init-jason-agenda ()
  "Initialize my extension"
  (eval-after-load 'org-agenda
    (use-package jason-agenda))
  )

