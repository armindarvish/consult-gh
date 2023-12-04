;;; consult-gh-embark.el --- Embark Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (consult "0.34"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

;;; Requirements
(require 'embark)
(require 'consult-gh)

;;; Define Embark Action Functions
(defun consult-gh-embark-add-repo-to-known-repos (cand)
  "Adds repo to `consult-gh--known-repos-list'."
  (let* ((repo (get-text-property 0 :repo cand)))
    (add-to-list 'consult-gh--known-repos-list repo))
  )

(defun consult-gh-embark-remove-repo-from-known-repos (cand)
  "Removes repo from `consult-gh--known-repos-list'."
  (let* ((repo (get-text-property 0 :repo cand)))
    (setq consult-gh--known-repos-list (delete repo consult-gh--known-repos-list))
    ))

(defun consult-gh-embark-add-org-to-known-orgs (cand)
  "Adds org to `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (add-to-list 'consult-gh--known-orgs-list (format "%s" org)))
  )

(defun consult-gh-embark-remove-org-from-known-orgs (cand)
  "Removes org from `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (setq consult-gh--known-orgs-list (delete org consult-gh--known-orgs-list))
    )
  )

(defun consult-gh-embark-add-org-to-default-list (cand)
  "Adds org to `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (add-to-list 'consult-gh-default-orgs-list (format "%s" org)))
  )

(defun consult-gh-embark-remove-org-from-default-list (cand)
  "Removes org from `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (setq consult-gh-default-orgs-list (delete org consult-gh-default-orgs-list))
    )
  )

(defun consult-gh-embark-open-in-browser (cand)
  "Opens the link in browser"
  (let* ((repo (get-text-property 0 :repo cand))
         (issue (or (get-text-property 0 :issue cand) nil))
         (pr (or (get-text-property 0 :pr cand) nil))
         (path (or (get-text-property 0 :path cand) nil))
         )
    (cond
     (issue
      (consult-gh--call-process "issue" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties issue)))
     (path
      (browse-url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/HEAD/" path)))
     (pr
      (consult-gh--call-process "pr" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties pr)))
     (t
      (consult-gh--call-process "repo" "view" "--web" (substring repo))))
    ))

(defun consult-gh-embark-default-action (cand)
  "Opens the link in an Emacs buffer"
  (let* ((repo (get-text-property 0 :repo cand))
         (user (get-text-property 0 :user cand))
         (package (get-text-property 0 :package cand))
         (url (or (get-text-property 0 :url cand) nil))
         (issue (or (get-text-property 0 :issue cand) nil))
         (pr (or (get-text-property 0 :pr cand) nil))
         (path (or (get-text-property 0 :path cand) nil))
         (branch (or (get-text-property 0 :branch cand) nil))
         (code (or (get-text-property 0 :code cand) nil))
         (newcand (cons cand `(:repo ,repo :user ,user :package ,package :url ,url :path ,path :branch ,branch :issue ,issue :pr ,pr :code ,code)))
         )
    (cond
     (code
      (funcall consult-gh-code-action newcand))
     (issue
      (funcall consult-gh-issue-action newcand))
     (pr
      (funcall consult-gh-pr-action newcand))
     (path
      (funcall consult-gh-file-action newcand))
     (t
      (funcall consult-gh-repo-action newcand)))
    ))


(defun consult-gh-embark-get-ssh-link (cand)
  "Copys the ssh based clone link of the repo to `kill-ring'."
  (kill-new (concat "git@github.com:" (string-trim  (get-text-property 0 :repo cand))) ".git"))

(defun consult-gh-embark-get-https-link (cand)
  "Copys the http based clone link of the repo to `kill-ring'."
  (kill-new (concat "https://github.com/" (string-trim (get-text-property 0 :repo cand)) ".git")))

(defun consult-gh-embark-get-url-link (cand)
  "Copys the url link of candidate to `kill-ring'.

The candidate can be a repo, issue, PR, file path, or a branch."
  (let* ((repo (get-text-property 0 :repo cand))
         (issue (or (get-text-property 0 :issue cand) nil))
         (pr (or (get-text-property 0 :pr cand) nil))
         (path (or (get-text-property 0 :path cand) nil))
         (branch (or (get-text-property 0 :branch cand) nil)))
    (cond
     (issue
      (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/issues/%s" issue))))
     (path
      (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) (format "/blob/%s/%s" (or branch "HEAD") path))))
     (pr
      (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" pr))))
     (t
      (kill-new (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))))
    ))

(defun consult-gh-embark-get-org-link (cand)
  "Copy the org style link for the repo's url to `kill-ring'."
  (let* ((repo (get-text-property 0 :repo cand))
         (url  (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (package (car (last (split-string repo "\/")))))
    (kill-new (concat "[[" url "][" package "]]"))))

(defun consult-gh-embark-get-straight-usepackage-link (cand)
  "Copys a drop-in straight use package setup of this repo to `kill-ring'."
  (let* ((repo (get-text-property 0 :repo cand))
         (package (car (last (split-string repo "\/"))))
         )
    (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" repo "\")\n)"))))

(defun consult-gh-embark-get-other-repos-by-same-user (cand)
  "List other repos by the same user/organization as the repo at point."
  (let* ((repo (get-text-property 0 :repo cand))
         (user (car (split-string repo "\/"))))
    (consult-gh-repo-list user)))

(defun consult-gh-embark-view-issues-of-repo (cand)
  "Browse issues of the repo at point."
  (let ((repo (or (get-text-property 0 :repo cand))))
    (consult-gh-issue-list repo)))

(defun consult-gh-embark-view-prs-of-repo (cand)
  "Browse PRs of the repo at point."
  (let ((repo (or (get-text-property 0 :repo cand))))
    (consult-gh-pr-list repo)))

(defun consult-gh-embark-view-files-of-repo (cand)
  "Browses files of the repo at point."
  (let ((repo (or (get-text-property 0 :repo cand) (consult-gh--nonutf-cleanup cand))))
    (consult-gh-find-file repo)))

(defun consult-gh-embark-clone-repo (cand)
  "Clones the repo at point."
  (let ((repo (get-text-property 0 :repo cand)))
    (funcall #'consult-gh--repo-clone-action (cons repo `(:repo ,repo)))))

(defun consult-gh-embark-fork-repo (cand)
  "Forks the repo at point."
  (let ((repo (get-text-property 0 :repo cand)))
    (funcall #'consult-gh--repo-fork-action (cons repo `(:repo ,repo)))))

(defun consult-gh-embark-save-file (cand)
  "Saves the file at point."
  (let* ((repo (get-text-property 0 :repo cand))
         (path (get-text-property 0 :path cand))
         (url (get-text-property 0 :url cand))
         (size (get-text-property 0 :size cand)))
    (funcall #'consult-gh--files-save-file-action (cons path `(:repo ,repo :path ,path :url ,url :size ,size)))))

;;; Define Embark Keymaps

(defvar-keymap consult-gh-embark-general-actions-map
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "b r r" #'consult-gh-embark-add-repo-to-known-repos
  "b r k" #'consult-gh-embark-remove-repo-from-known-repos
  "b o o" #'consult-gh-embark-add-org-to-known-orgs
  "b o k" #'consult-gh-embark-remove-org-from-known-orgs
  "b o d" #'consult-gh-embark-add-org-to-default-list
  "b o D" #'consult-gh-embark-remove-org-from-default-list
  "f f" #'consult-gh-embark-view-files-of-repo
  "l h" #'consult-gh-embark-get-https-link
  "l s" #'consult-gh-embark-get-ssh-link
  "l l" #'consult-gh-embark-get-url-link
  "l o" #'consult-gh-embark-get-org-link
  "l u" #'consult-gh-embark-get-straight-usepackage-link
  "r c" #'consult-gh-embark-clone-repo
  "r f" #'consult-gh-embark-fork-repo
  "r r" #'consult-gh-embark-get-other-repos-by-same-user
  "r i" #'consult-gh-embark-view-issues-of-repo
  "r p" #'consult-gh-embark-view-prs-of-repo
  "o" #'consult-gh-embark-open-in-browser
  )

(add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-general-actions-map))

(defvar-keymap consult-gh-embark-orgs-actions-map
  :doc "Keymap for consult-gh-embark-orgs"
  :parent consult-gh-embark-general-actions-map)

(add-to-list 'embark-keymap-alist '(consult-gh-orgs . consult-gh-embark-orgs-actions-map))

(defvar-keymap consult-gh-embark-repos-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-gh-repos . consult-gh-embark-repos-actions-map))


(defvar-keymap consult-gh-embark-files-actions-map
  :doc "Keymap for consult-gh-embark-files"
  :parent consult-gh-embark-general-actions-map
  "s" #'consult-gh-embark-save-file)

(add-to-list 'embark-keymap-alist '(consult-gh-files . consult-gh-embark-files-actions-map))

(defvar-keymap consult-gh-embark-issues-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-gh-issues . consult-gh-embark-issues-actions-map))

(defvar-keymap consult-gh-embark-prs-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-gh-prs . consult-gh-embark-prs-actions-map))

(defvar-keymap consult-gh-embark-codes-actions-map
  :doc "Keymap for consult-gh-embark-codes"
  :parent consult-gh-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-gh-codes . consult-gh-embark-codes-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-gh-repos . consult-gh-embark-default-action))
(add-to-list 'embark-default-action-overrides '(consult-gh-issues . consult-gh-embark-default-action))
(add-to-list 'embark-default-action-overrides '(consult-gh-prs . consult-gh-embark-default-action))
(add-to-list 'embark-default-action-overrides '(consult-gh-files . consult-gh-embark-default-action))
(add-to-list 'embark-default-action-overrides '(consult-gh-codes . consult-gh-embark-default-action))

;;; Provide `consul-gh-embark' module

(provide 'consult-gh-embark)

;;; consult-gh-embark.el ends here
