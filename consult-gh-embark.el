;;; consult-gh-embark.el --- Embark Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.0
;; Package-Requires: ((emacs "29.1") (consult-gh "2.0") (embark-consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides embark actions for consult-gh.
;; (see URL `https://github.com/armindarvish/consult-gh' for more info).

;;; Code:

;;; Requirements
(require 'consult-gh)
(require 'embark-consult)

;;; Define Embark Action Functions
(defun consult-gh-embark-add-repo-to-known-repos (cand)
  "Add CAND repo to `consult-gh--known-repos-list'."
  (let* ((repo (get-text-property 0 :repo cand)))
    (add-to-list 'consult-gh--known-repos-list repo)))

(defun consult-gh-embark-remove-repo-from-known-repos (cand)
  "Remove CAND repo from `consult-gh--known-repos-list'."
  (let* ((repo (get-text-property 0 :repo cand)))
    (setq consult-gh--known-repos-list (delete repo consult-gh--known-repos-list))))

(defun consult-gh-embark-add-org-to-known-orgs (cand)
  "Add CAND org to `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (add-to-list 'consult-gh--known-orgs-list (format "%s" org))))

(defun consult-gh-embark-remove-org-from-known-orgs (cand)
  "Remove CAND org from `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (setq consult-gh--known-orgs-list (delete org consult-gh--known-orgs-list))))

(defun consult-gh-embark-add-org-to-default-list (cand)
  "Add CAND org to `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (add-to-list 'consult-gh-default-orgs-list (format "%s" org))))

(defun consult-gh-embark-remove-org-from-default-list (cand)
  "Remove CAND org from `consult-gh--known-orgs-list'."
  (let* ((org (get-text-property 0 :user cand)))
    (setq consult-gh-default-orgs-list (delete org consult-gh-default-orgs-list))))

(defun consult-gh-embark-open-in-browser (cand)
  "Open CAND link in browser."
  (let* ((repo (get-text-property 0 :repo cand))
         (class (or (get-text-property 0 :class cand) nil))
         (number (or (get-text-property 0 :number cand) nil))
         (path (or (get-text-property 0 :path cand) nil)))
    (pcase class
      ("issue"
       (consult-gh--call-process "issue" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
      ("file"
       (funcall (or consult-gh-browse-url-func #'browse-url) (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/HEAD/" path)))
      ("pr"
       (consult-gh--call-process "pr" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
      (_
       (consult-gh--call-process "repo" "view" "--web" (substring repo))))))

(defun consult-gh-embark-default-action (cand)
  "Open CAND link in an Emacs buffer."
  (let* ((class (get-text-property 0 :class cand)))
    (pcase class
      ("code"
       (funcall consult-gh-code-action cand))
      ("issue"
       (funcall consult-gh-issue-action cand))
      ("pr"
       (funcall consult-gh-pr-action cand))
      ("file"
       (funcall consult-gh-file-action cand))
      ("notification"
       (funcall consult-gh-notifications-action cand))
      ("dashboard"
       (funcall consult-gh-dashboard-action cand))
      (_
       (funcall consult-gh-repo-action cand)))))


(defun consult-gh-embark-get-ssh-link (cand)
  "Copy CAND ssh link to `kill-ring'."
  (kill-new (concat "git@github.com:" (string-trim  (get-text-property 0 :repo cand))) ".git"))

(defun consult-gh-embark-get-https-link (cand)
  "Copy CAND http link to `kill-ring'."
  (kill-new (concat "https://github.com/" (string-trim (get-text-property 0 :repo cand)) ".git")))

(defun consult-gh-embark-get-url-link (cand)
  "Copy CAND url link to `kill-ring'.

The candidate can be a repo, issue, PR, file path, or a branch."
  (let* ((repo (get-text-property 0 :repo cand))
         (class (or (get-text-property 0 :class cand) nil))
         (number (or (get-text-property 0 :number cand) nil))
         (path (or (get-text-property 0 :path cand) nil))
         (branch (or (get-text-property 0 :branch cand) nil)))
    (pcase class
      ("issue"
       (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/issues/%s" number))))
      ("file"
       (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) (format "/blob/%s/%s" (or branch "HEAD") path))))
      ("pr"
       (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" number))))
      (_
       (kill-new (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))))))

(defun consult-gh-embark-get-org-link (cand)
  "Copy the org style link for the CAND url to `kill-ring'."
  (let* ((repo (get-text-property 0 :repo cand))
         (url  (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (package (car (last (split-string repo "\/")))))
    (kill-new (concat "[[" url "][" package "]]"))))

(defun consult-gh-embark-get-straight-usepackage-link (cand)
  "Copy a drop-in straight use package setup of CAND to `kill-ring'."
  (let* ((repo (get-text-property 0 :repo cand))
         (package (car (last (split-string repo "\/")))))
    (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" repo "\")\n)"))))

(defun consult-gh-embark-get-other-repos-by-same-user (cand)
  "List other repos by the same user/organization as CAND at point."
  (let* ((repo (get-text-property 0 :repo cand))
         (user (car (split-string repo "\/"))))
    (consult-gh-repo-list user)))

(defun consult-gh-embark-view-issues-of-repo (cand)
  "Browse issues of CAND repo at point."
  (let ((repo (or (get-text-property 0 :repo cand))))
    (consult-gh-issue-list repo)))

(defun consult-gh-embark-view-prs-of-repo (cand)
  "Browse PRs of CAND repo at point."
  (let ((repo (or (get-text-property 0 :repo cand))))
    (consult-gh-pr-list repo)))

(defun consult-gh-embark-view-files-of-repo (cand)
  "Browse files of CAND at point."
  (let ((repo (or (get-text-property 0 :repo cand) (consult-gh--nonutf-cleanup cand))))
    (consult-gh-find-file repo)))

(defun consult-gh-embark-clone-repo (cand)
  "Clone the CAND repo at point."
  (funcall #'consult-gh--repo-clone-action cand))

(defun consult-gh-embark-fork-repo (cand)
  "Fork the CAND repo at point."
  (funcall #'consult-gh--repo-fork-action cand))

(defun consult-gh-embark-save-file (cand)
  "Save the CAND file at point."
  (funcall #'consult-gh--files-save-file-action cand))


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
  "o" #'consult-gh-embark-open-in-browser)

(defvar-keymap consult-gh-embark-orgs-actions-map
  :doc "Keymap for consult-gh-embark-orgs"
  :parent consult-gh-embark-general-actions-map)

(defvar-keymap consult-gh-embark-repos-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map)

(defvar-keymap consult-gh-embark-files-actions-map
  :doc "Keymap for consult-gh-embark-files"
  :parent consult-gh-embark-general-actions-map
  "s" #'consult-gh-embark-save-file)

(defvar-keymap consult-gh-embark-issues-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map)

(defvar-keymap consult-gh-embark-prs-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map)

(defvar-keymap consult-gh-embark-codes-actions-map
  :doc "Keymap for consult-gh-embark-codes"
  :parent consult-gh-embark-general-actions-map)


(defvar-keymap consult-gh-embark-notifications-actions-map
  :doc "Keymap for consult-gh-embark-notifications"
  :parent consult-gh-embark-general-actions-map)

;;; Define consult-gh-embark minor-mode

(defun consult-gh-embark--mode-on ()
  "Enable `consult-gh-embark-mode'."
  (add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-general-actions-map))
  (add-to-list 'embark-keymap-alist '(consult-gh-orgs . consult-gh-embark-orgs-actions-map))
  (add-to-list 'embark-keymap-alist '(consult-gh-repos . consult-gh-embark-repos-actions-map))
  (add-to-list 'embark-keymap-alist '(consult-gh-files . consult-gh-embark-files-actions-map))
  (add-to-list 'embark-keymap-alist '(consult-gh-issues . consult-gh-embark-issues-actions-map))
  (add-to-list 'embark-keymap-alist '(consult-gh-prs . consult-gh-embark-prs-actions-map))
  (add-to-list 'embark-keymap-alist '(consult-gh-codes . consult-gh-embark-codes-actions-map))
  (add-to-list 'embark-default-action-overrides '(consult-gh-repos . consult-gh-embark-default-action))
  (add-to-list 'embark-default-action-overrides '(consult-gh-issues . consult-gh-embark-default-action))
  (add-to-list 'embark-default-action-overrides '(consult-gh-prs . consult-gh-embark-default-action))
  (add-to-list 'embark-default-action-overrides '(consult-gh-files . consult-gh-embark-default-action))
  (add-to-list 'embark-default-action-overrides '(consult-gh-codes . consult-gh-embark-default-action))
  (add-to-list 'embark-default-action-overrides '(consult-gh-notifications . consult-gh-embark-default-action)))

(defun consult-gh-embark--mode-off ()
  "Disable `consult-gh-embark-mode'."
  (setq  embark-keymap-alist (seq-difference embark-keymap-alist '((consult-gh . consult-gh-embark-general-actions-map)
                                                                   (consult-gh-orgs . consult-gh-embark-orgs-actions-map)
                                                                   (consult-gh-repos . consult-gh-embark-repos-actions-map)
                                                                   (consult-gh-files . consult-gh-embark-files-actions-map)
                                                                   (consult-gh-issues . consult-gh-embark-issues-actions-map)
                                                                   (consult-gh-prs . consult-gh-embark-prs-actions-map))))
  (setq embark-default-action-overrides (seq-difference embark-default-action-overrides
                                                        '((consult-gh-repos . consult-gh-embark-default-action)
                                                          (consult-gh-issues . consult-gh-embark-default-action)
                                                          (consult-gh-prs . consult-gh-embark-default-action)
                                                          (consult-gh-files . consult-gh-embark-default-action)
                                                          (consult-gh-codes . consult-gh-embark-default-action)
                                                          (consult-gh-notifications . consult-gh-embark-default-action)))))

(defun consult-gh-embark-unload-function ()
  "Unload function for `consult-gh-embark'."
  (consult-gh-embark--mode-off))

;;;###autoload
(define-minor-mode consult-gh-embark-mode
  "Use embark with `consult-gh' for extra actions."
  :init-value nil
  :global t
  :group 'consult-gh
  :lighter " consult-gh-embark"
  (if consult-gh-embark-mode
      (consult-gh-embark--mode-on)
    (consult-gh-embark--mode-off)))


;;; Provide `consul-gh-embark' module

(provide 'consult-gh-embark)

;;; consult-gh-embark.el ends here
