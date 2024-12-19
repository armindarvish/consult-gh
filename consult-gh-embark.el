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
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand)))
      (add-to-list 'consult-gh--known-repos-list repo))))

(defun consult-gh-embark-remove-repo-from-known-repos (cand)
  "Remove CAND repo from `consult-gh--known-repos-list'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand)))
      (setq consult-gh--known-repos-list (delete repo consult-gh--known-repos-list)))))

(defun consult-gh-embark-add-org-to-known-orgs (cand)
  "Add CAND org to `consult-gh--known-orgs-list'."
  (when (stringp cand)
    (let* ((org (get-text-property 0 :user cand)))
      (add-to-list 'consult-gh--known-orgs-list (format "%s" org)))))

(defun consult-gh-embark-remove-org-from-known-orgs (cand)
  "Remove CAND org from `consult-gh--known-orgs-list'."
  (when (stringp cand)
    (let* ((org (get-text-property 0 :user cand)))
      (setq consult-gh--known-orgs-list (delete org consult-gh--known-orgs-list)))))

(defun consult-gh-embark-add-org-to-favorite-list (cand)
  "Add CAND org to `consult-gh--known-orgs-list'."
  (when (stringp cand)
    (let* ((org (get-text-property 0 :user cand)))
      (add-to-list 'consult-gh-favorite-orgs-list (format "%s" org)))))

(define-obsolete-function-alias #'consult-gh-embark-add-org-to-default-list #'consult-gh-embark-add-org-to-favorite-list "2.0")

(defun consult-gh-embark-remove-org-from-favorite-list (cand)
  "Remove CAND org from `consult-gh--known-orgs-list'."
  (when (stringp cand)
    (let* ((org (get-text-property 0 :user cand)))
      (setq consult-gh-favorite-orgs-list (delete org consult-gh-favorite-orgs-list)))))

(define-obsolete-function-alias #'consult-gh-embark-remove-org-from-default-list #'consult-gh-embark-remove-org-from-favorite-list "2.0")

(defun consult-gh-embark-default-action (cand)
  "Open CAND link in an Emacs buffer."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand)))
      (consult-gh-with-host
       (consult-gh--auth-account-host)
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
          (funcall consult-gh-repo-action cand)))))))

(defun consult-gh-embark-show-preview (cand)
  "Open preview of CAND."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand)))
      (consult-gh-with-host
       (consult-gh--auth-account-host)
       (pcase class
         ("code"
          (funcall (consult-gh--code-state) 'preview cand))
         ("issue"
          (funcall (consult-gh--issue-state) 'preview cand))
         ("pr"
          (funcall (consult-gh--pr-state) 'preview cand))
         ("file"
          (funcall (consult-gh--file-state) 'preview cand))
         ("notification"
          (funcall (consult-gh--notification-state) 'preview cand))
         ("dashboard"
          (funcall (consult-gh--dashboard-state) 'preview cand))
         (_
          (funcall (consult-gh--repo-state) 'preview cand)))))))

(defun consult-gh-embark-get-title (cand)
  "Get the title of CAND."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (repo (get-text-property 0 :repo cand))
           (title (get-text-property 0 :title cand))
           (number (get-text-property 0 :number cand))
           (path (get-text-property 0 :path cand))
           (title (pcase class
                    ((or "file" "code")
                     (format ("#%s" path)))
                    ((or "issue" "pr")
                     (format ("#%s: %s" number title)))
                    (_ (format "%s" repo)))))
      (string-trim title))))

(defun consult-gh-embark-insert-title (cand)
  "Insert the title of CAND at point."
  (when (stringp cand)
    (if-let* ((title (consult-gh-embark-get-title cand)))
      (embark-insert (list (string-trim title))))))

(defun consult-gh-embark-copy-title-as-kill (cand)
  "Copy the title of CAND to `kill-ring'."
  (when (stringp cand)
    (if-let* ((title (consult-gh-embark-get-title cand)))
      (kill-new (string-trim title)))))

(defun consult-gh-embark-get-url-link (cand)
  "Get url link of CAND.

The candidate can be a repo, issue, PR, file path, or a branch."
   (when (stringp cand)
     (consult-gh-with-host
      (consult-gh--auth-account-host)
      (let* ((repo (get-text-property 0 :repo cand))
             (class (or (get-text-property 0 :class cand) nil))
             (number (or (get-text-property 0 :number cand) nil))
             (path (or (get-text-property 0 :path cand) nil))
             (branch (or (get-text-property 0 :branch cand) nil)))
     (pcase class
       ("issue"
        (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/issues/%s" number))))
       ((or "file" "code")
        (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) (format "/blob/%s/%s" (or branch "HEAD") path))))
       ("pr"
        (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" number))))
       (_
        (string-trim (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))))))))


(defun consult-gh-embark-open-in-system-browser (cand)
  "Open the url link of CAND in the system's default browser."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (class (or (get-text-property 0 :class cand) nil))
           (number (or (get-text-property 0 :number cand) nil))
           (path (or (get-text-property 0 :path cand) nil)))
      (consult-gh-with-host
       (consult-gh--auth-account-host)
       (pcase class
         ("issue"
          (consult-gh--call-process "issue" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
         ("file"
          (funcall (or consult-gh-browse-url-func #'browse-url) (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/HEAD/" path)))
         ("pr"
          (consult-gh--call-process "pr" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
         (_
          (consult-gh--call-process "repo" "view" "--web" (substring repo))))))))

(defun consult-gh-embark-open-in-default-browser (cand)
  "Open the url link of CAND with `consult-gh-browse-url-func'."
  (when (stringp cand)
    (let* ((url (consult-gh-embark-get-url-link cand)))
      (funcall consult-gh-browse-url-func url))))

(defun consult-gh-embark-copy-url-link-as-kill (cand)
  "Copy url link of CAND to `kill-ring'.

CAND can be a repo, issue, PR, file path, ..."
   (when (stringp cand)
      (let* ((link (consult-gh-embark-get-url-link cand)))
        (kill-new link))))

(defun consult-gh-embark-copy-url-org-link-as-kill (cand)
  "Copy the org-format url link of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (url (consult-gh-embark-get-url-link cand))
           (title (consult-gh-embark-get-title cand))
           (path (or (get-text-property 0 :path cand) nil))
           (branch (or (get-text-property 0 :branch cand) nil))
           (str nil))

      (pcase class
        ((or "issue" "pr")
          (setq title (concat repo "/" title)))
        ((or "file" "code")
         (setq title (concat repo ":" branch "/" path))))

      (when url
        (setq str
              (cond
               ((and url title) (format " [[%s][%s]] " url title))
               (url (format " [[%s]] " url))
               (t nil))))

      (when (stringp str)
        (kill-new str)))))

(defun consult-gh-embark-insert-url-link (cand)
  "Insert the url of CAND at point."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (url (consult-gh-embark-get-url-link cand))
           (title (consult-gh-embark-get-title cand))
           (path (or (get-text-property 0 :path cand) nil))
           (branch (or (get-text-property 0 :branch cand) nil)))

      (pcase class
        ((or "issue" "pr")
          (setq title (concat repo "/" title)))
        ((or "file" "code")
         (setq title (concat repo ":" branch "/" path))))

      (when url
        (cond
         ((derived-mode-p 'org-mode)
          (insert (cond
                   ((and url title) (format " [[%s][%s]] " url title))
                   (url (format " [[%s]] " url))
                   (t ""))))
         ((derived-mode-p 'markdown-mode)
          (insert (cond
                   ((and url title) (format " [%s](%s) " url title))
                   (url (format " <%s> " url))
                   (t ""))))
         (t
          (insert (cond
                   ((and url title) (format " %s (%s) " title  url))
                   (url (format " %s " url))
                   (t "")))))))))

(defun consult-gh-embark-copy-https-link-as-kill (cand)
  "Copy http link of CAND to `kill-ring'."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (get-text-property 0 :repo cand)))
     (kill-new (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser"))  ".git"))))))

(defun consult-gh-embark-copy-ssh-link-as-kill (cand)
  "Copy shh link of CAND to `kill-ring'."
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((repo (get-text-property 0 :repo cand)))
     (kill-new (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" (string-trim repo) "--json" "sshUrl") :sshUrl))))))

(defun consult-gh-embark-copy-straight-usepackage-link-as-kill (cand)
  "Copy a drop-in straight use-package script of CAND to `kill-ring'."
    (when (stringp cand)
  (let* ((repo (get-text-property 0 :repo cand))
         (package (car (last (split-string repo "\/")))))
    (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" repo "\")\n)")))))

(defun consult-gh-embark-copy-usepackage-link-as-kill (cand)
  "Copy a drop-in use-package script of CAND to `kill-ring'."
    (when (stringp cand)
  (let* ((repo (get-text-property 0 :repo cand))
         (package (car (last (split-string repo "\/")))))
    (kill-new (concat "(use-package " package)))))

(defun consult-gh-embark-get-other-repos-by-same-user (cand)
  "List other repos by the same user/organization as CAND at point."
    (when (stringp cand)
  (let* ((repo (get-text-property 0 :repo cand))
         (user (car (split-string repo "\/"))))
    (consult-gh-repo-list user))))

(defun consult-gh-embark-view-issues-of-repo (cand)
  "Browse issues of CAND repo at point."
    (when (stringp cand)
  (let ((repo (or (get-text-property 0 :repo cand))))
    (consult-gh-issue-list repo))))

(defun consult-gh-embark-view-prs-of-repo (cand)
  "Browse PRs of CAND repo at point."
    (when (stringp cand)
  (let ((repo (or (get-text-property 0 :repo cand))))
    (consult-gh-pr-list repo))))

(defun consult-gh-embark-view-files-of-repo (cand)
  "Browse files of CAND at point."
    (when (stringp cand)
  (let ((repo (or (get-text-property 0 :repo cand) (consult-gh--nonutf-cleanup cand))))
    (consult-gh-find-file repo))))

(defun consult-gh-embark-clone-repo (cand)
  "Clone the CAND repo at point."
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh--repo-clone-action cand))))

(defun consult-gh-embark-fork-repo (cand)
  "Fork the CAND repo at point."
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh--repo-fork-action cand))))

(defun consult-gh-embark-save-file (cand)
  "Save the CAND file at point."
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh--files-save-file-action cand))))

(defun consult-gh-embark-create-issue (cand)
  "Create an issue in repo of CAND"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let ((repo (get-text-property 0 :repo cand)))
   (funcall #'consult-gh-issue-create repo)))))

(defun consult-gh-embark-create-pr (cand)
  "Create a pull request in repo of CAND"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let ((repo (get-text-property 0 :repo cand)))
   (funcall #'consult-gh-pr-create repo)))))


(defun consult-gh-embark-toggle-issue-open (cand)
  "Close/Re-open the issue in CAND"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((state (get-text-property 0 :state cand)))
     (if (equal state "OPEN")
         (funcall #'consult-gh-issue-close cand)
       (funcall #'consult-gh-issue-reopen cand))))))

(defun consult-gh-embark-toggle-issue-pin (cand)
  "Pin/Unpin the issue in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (when-let* ((repo (get-text-property 0 :repo cand))
         (number (get-text-property 0 :number cand))
         (state (when (and repo number)
                  (consult-gh--json-to-hashtable  (consult-gh--command-to-string "issue" "view" number "--repo" repo "--json" "isPinned") :isPinned))))
     (if (eq state 't)
         (funcall #'consult-gh-issue-unpin cand)
       (funcall #'consult-gh-issue-pin cand))))))

(defun consult-gh-embark-toggle-issue-lock (cand)
  "Lock/Unlock the issue in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (when-let* ((repo (get-text-property 0 :repo cand))
               (number (get-text-property 0 :number cand))
               (state (when (and repo number)
                  (consult-gh--json-to-hashtable  (consult-gh--api-command-string (format "/repos/%s/issues/%s" repo number)) :locked))))
     (if (eq state 't)
         (funcall #'consult-gh-issue-unlock cand)
       (funcall #'consult-gh-issue-lock cand))))))

(defun consult-gh-embark-transfer-issue (cand)
  "Transfer the issue in CAND"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-issue-transfer cand))))

(defun consult-gh-embark-delete-issue (cand)
  "Delete the issue in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-issue-delete cand))))

(defun consult-gh-embark-develop-issue (cand)
  "Make a linked branch for the issue in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-issue-develop cand))))

(defun consult-gh-embark-edit-issue (cand)
  "Make a linked branch for the issue in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-issue-edit cand))))

(defun consult-gh-embark-comment-on-issue (cand)
  "Edit the issue in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let ((repo (get-text-property 0 :repo cand))
         (number (get-text-property 0 :number cand)))
     (with-current-buffer (funcall #'consult-gh--issue-view repo number)
       (consult-gh-topics-comment-create))))))

(defun consult-gh-embark-edit-pr (cand)
  "Edit the pull request in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-pr-edit cand))))

(defun consult-gh-embark-merge-pr (cand)
  "Merge the pull request in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-pr-merge cand))))

(defun consult-gh-embark-review-pr (cand)
  "Review the pull request in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-pr-review cand))))

(defun consult-gh-embark-comment-on-pr (cand)
  "Make a linked branch for the issue in CAND"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let ((repo (get-text-property 0 :repo cand))
         (number (get-text-property 0 :number cand)))
     (with-current-buffer (funcall #'consult-gh--pr-view repo number)
       (consult-gh-topics-comment-create))))))

(defun consult-gh-embark-toggle-pr-open (cand)
  "Close/Re-open the pull request in CAND"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((state (get-text-property 0 :state cand)))
     (if (equal state "OPEN")
         (funcall #'consult-gh-pr-close cand)
       (funcall #'consult-gh-pr-reopen cand))))))

(defun consult-gh-embark-toggle-pr-lock (cand)
  "Lock/Unlock the pull request in CAND"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (when-let* ((repo (get-text-property 0 :repo cand))
               (number (get-text-property 0 :number cand))
               (state (when (and repo number)
                  (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/repos/%s/pulls/%s" repo number)) :locked))))
     (if (eq state 't)
         (funcall #'consult-gh-issue-unlock cand)
       (funcall #'consult-gh-issue-lock cand))))))

(defun consult-gh-embark-toggle-pr-draft (cand)
  "Toggle the pull request in CAND as draft/ready"
    (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (when-let* ((repo (get-text-property 0 :repo cand))
         (number (get-text-property 0 :number cand))
         (state (when (and repo number)
                  (consult-gh--json-to-hashtable  (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" "isDraft") :isDraft))))
     (if (eq state 't)
         (funcall #'consult-gh-issue-mark-ready cand)
       (funcall #'consult-gh-issue-mark-draft cand))))))


(defun consult-gh-embark-toggle-notification-read (cand)
  "Mark the notification in CAND as read/unread"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let ((state (get-text-property 0 :state cand)))
     (if (equal state "Unread")
         (consult-gh--notifications-mark-as-read cand))))))

(defun consult-gh-embark-notification-toggle-subscription (cand)
  "Mark the notification in CAND as read/unread"
  (when (stringp cand)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((thread (get-text-property 0 :thread cand))
          (subscription (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/notifications/threads/%s/subscription" thread)) :subscribed)))
     (if (eq subscription 't)
         (consult-gh--notifications-unsubscribe cand)
       (consult-gh--notifications-subscribe cand))))))
;;; Define Embark Keymaps

(defvar-keymap consult-gh-embark-general-actions-map
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "b r r" #'consult-gh-embark-add-repo-to-known-repos
  "b r k" #'consult-gh-embark-remove-repo-from-known-repos
  "b o o" #'consult-gh-embark-add-org-to-known-orgs
  "b o k" #'consult-gh-embark-remove-org-from-known-orgs
  "b o d" #'consult-gh-embark-add-org-to-favorite-list
  "b o D" #'consult-gh-embark-remove-org-from-favorite-list
  "c i" #'consult-gh-embark-create-issue
  "c p" #'consult-gh-embark-create-pr
  "f f" #'consult-gh-embark-view-files-of-repo
  "i t" #'consult-gh-embark-insert-title
  "i u" #'consult-gh-embark-insert-url-link
  "l h" #'consult-gh-embark-copy-https-link-as-kill
  "l s" #'consult-gh-embark-copy-ssh-link-as-kill
  "l l" #'consult-gh-embark-copy-url-link-as-kill
  "l o" #'consult-gh-embark-copy-url-org-link-as-kill
  "l u" #'consult-gh-embark-copy-straight-usepackage-link-as-kill
  "o o" #'consult-gh-embark-open-in-system-browser
  "o O" #'consult-gh-embark-open-in-default-browser
  "o p" #'consult-gh-embark-show-preview
  "r c" #'consult-gh-embark-clone-repo
  "r f" #'consult-gh-embark-fork-repo
  "r r" #'consult-gh-embark-get-other-repos-by-same-user
  "r i" #'consult-gh-embark-view-issues-of-repo
  "r p" #'consult-gh-embark-view-prs-of-repo
  "w t" #'consult-gh-embark-copy-title-as-kill
  "w u" #'consult-gh-embark-copy-url-link-as-kill
  "w h" #'consult-gh-embark-copy-https-link-as-kill
  "w s" #'consult-gh-embark-copy-ssh-link-as-kill
  "w o" #'consult-gh-embark-copy-url-org-link-as-kill)

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
  :parent consult-gh-embark-general-actions-map
  "c c" #'consult-gh-embark-comment-on-issue
  "g D" #'consult-gh-embark-delete-issue
  "g e" #'consult-gh-embark-edit-issue
  "g g" #'consult-gh-embark-develop-issue
  "g L" #'consult-gh-embark-toggle-issue-lock
  "g O" #'consult-gh-embark-toggle-issue-open
  "g P" #'consult-gh-embark-toggle-issue-pin
  "g T" #'consult-gh-embark-transfer-issue)



(defvar-keymap consult-gh-embark-prs-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "c c" #'consult-gh-embark-comment-on-pr
  "c r" #'consult-gh-embark-review-pr
  "g e" #'consult-gh-embark-edit-pr
  "g D" #'consult-gh-embark-toggle-pr-draft
  "g L" #'consult-gh-embark-toggle-pr-lock
  "g m" #'consult-gh-embark-merge-pr
  "g O" #'consult-gh-embark-toggle-pr-open)

(defvar-keymap consult-gh-embark-codes-actions-map
  :doc "Keymap for consult-gh-embark-codes"
  :parent consult-gh-embark-general-actions-map)


(defvar-keymap consult-gh-embark-notifications-actions-map
  :doc "Keymap for consult-gh-embark-notifications"
  :parent consult-gh-embark-general-actions-map
  "g r" #'consult-gh-embark-toggle-notification-read
  "g s" #'consult-gh-embark-notification-toggle-subscription)

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
  (add-to-list 'embark-keymap-alist '(consult-gh-notifications . consult-gh-embark-notifications-actions-map))
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
