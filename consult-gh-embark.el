;;; consult-gh-embark.el --- Emabrk Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.34") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

(require 'embark)

;;; Add embark menus if embark is available
(with-eval-after-load 'consult-gh

(defun consult-gh-embark-get-ssh-link (repo)
  "Get the ssh based link of the repo"
  (kill-new (concat "git@github.com:" (string-trim  (consult-gh--output-cleanup (substring-no-properties repo))) ".git")))

(defun consult-gh-embark-get-https-link (repo)
  "Get the ssh based link of the repo"
  (kill-new (concat "https://github.com/" (string-trim (consult-gh--output-cleanup (substring-no-properties repo))) ".git")))

(defun consult-gh-embark-get-straight-usepackage (repo)
  "Close tab."
  (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties repo))))
         (package (car (last (split-string reponame "\/"))))
         )
    (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" reponame  "\")\n)"))))

(defun consult-gh-embark-clone-repo (repo)
  "Clone the repo at point"
  (funcall (consult-gh--clone-repo-action) repo))


(defvar-keymap consult-gh-embark-actions
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "s" #'consult-gh-embark-get-ssh-link
  "h" #'consult-gh-embark-get-https-link
  "e" #'consult-gh-embark-get-straight-usepackage
  "c" #'consult-gh-embark-clone-repo)

(add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-actions)))

(provide 'consult-gh-embark)
