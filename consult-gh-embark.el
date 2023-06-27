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
(require 'consult-gh)

(defun consult-gh-embark-open-in-browser (cand)
  "Open the link in browser"
  (let* ((repo (get-text-property 0 :repo cand))
         (issue (or (get-text-property 0 :issue cand) nil))
         (path (or (get-text-property 0 :path cand) nil)))
    (if issue
        (consult-gh--call-process "issue" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties issue))
      (if path
        (browse-url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/HEAD/" path))
        (consult-gh--call-process "repo" "view" "--web" (substring repo))))))

(defun consult-gh-embark-get-ssh-link (cand)
  "Copy the ssh based link of the repo to `kill-ring'."
  (kill-new (concat "git@github.com:" (string-trim  (get-text-property 0 :repo cand))) ".git"))

(defun consult-gh-embark-get-https-link (cand)
  "Copy the http based link of the repo to `kill-ring'."
  (kill-new (concat "https://github.com/" (string-trim (get-text-property 0 :repo cand)) ".git")))

(defun consult-gh-embark-get-url-link (cand)
  "Copy the http based link of the repo to `kill-ring'."
  (kill-new (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim (get-text-property 0 :repo cand)) "--no-browser"))))

(defun consult-gh-embark-get-org-link (cand)
  "Copy the http based link of the repo to `kill-ring'."
  (let* ((repo (get-text-property 0 :repo cand))
         (url  (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (package (car (last (split-string repo "\/")))))
  (kill-new (concat "[[" url "][" package "]]"))))

(defun consult-gh-embark-get-straight-usepackage-link (cand)
  "Copy a drop-in straight use package setup of this repo to `kill-ring'."
  (let* ((repo (get-text-property 0 :repo cand))
         (package (car (last (split-string repo "\/"))))
         )
    (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" repo  "\")\n)"))))

(defun consult-gh-embark-get-other-repos-by-same-user (cand)
  "List other repos by the same user/organization as the repo at point."
  (let* ((repo  (get-text-property 0 :repo cand))
         (user (car (split-string repo "\/"))))
    (consult-gh-orgs `(,user))))

(defun consult-gh-embark-view-issues-of-repo (cand)
  "View issues of the repo at point."
  (let* ((repo (get-text-property 0 :repo cand))
         )
    (consult-gh-issue-list `(,repo))))

(defun consult-gh-embark-clone-repo (cand)
  "Clone the repo at point."
  (funcall (consult-gh--repo-clone-action) (get-text-property 0 :repo cand)))


(defun consult-gh-embark-fork-repo (cand)
  "Fork the repo at point."
  (funcall (consult-gh--repo-fork-action) (get-text-property 0 :repo cand)))

(defun consult-gh-embark-save-file (cand)
  "Save the file at point."
  (funcall (consult-gh--files-save-file-action) cand))

(defvar-keymap consult-gh-embark-actions-map
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "l h" #'consult-gh-embark-get-https-link
  "l s" #'consult-gh-embark-get-ssh-link
  "l l" #'consult-gh-embark-get-url-link
  "l o" #'consult-gh-embark-get-org-link
  "l e" #'consult-gh-embark-get-straight-usepackage-link
  "c" #'consult-gh-embark-clone-repo
  "f" #'consult-gh-embark-fork-repo
  "x" #'consult-gh-embark-get-other-repos-by-same-user
  "z" #'consult-gh-embark-view-issues-of-repo
  "o" #'consult-gh-embark-open-in-browser
)

(add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-actions-map))



(defvar-keymap consult-gh-embark-files-actions-map
  :doc "Keymap for consult-gh-embark-files"
  :parent consult-gh-embark-actions-map
  "s" #'consult-gh-embark-save-file)

(add-to-list 'embark-keymap-alist '(consult-gh-files . consult-gh-embark-files-actions-map))


(provide 'consult-gh-embark)
