;;; consult-gh-embark.el --- Embark Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.5
;; Package-Requires: ((emacs "29.4") (consult "2.0") (consult-gh "2.5") (embark-consult "1.1"))
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
(require 'embark-consult)
(require 'consult-gh)

;;; Define Embark Action Functions

;;;; Default Actions
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
         ("release"
          (funcall consult-gh-release-action cand))
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
          (funcall (consult-gh--notifications-state) 'preview cand))
         ("dashboard"
          (funcall (consult-gh--dashboard-state) 'preview cand))
         ("release"
          (funcall (consult-gh--release-state) 'preview cand))
         (_
          (funcall (consult-gh--repo-state) 'preview cand)))))))

;;;; Add/Remove from Favorites
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

(define-obsolete-function-alias 'consult-gh-embark-add-org-to-default-list #'consult-gh-embark-add-org-to-favorite-list "2.0")

(defun consult-gh-embark-remove-org-from-favorite-list (cand)
  "Remove CAND org from `consult-gh--known-orgs-list'."
  (when (stringp cand)
    (let* ((org (get-text-property 0 :user cand)))
      (setq consult-gh-favorite-orgs-list (delete org consult-gh-favorite-orgs-list)))))

(define-obsolete-function-alias 'consult-gh-embark-remove-org-from-default-list #'consult-gh-embark-remove-org-from-favorite-list "2.0")

;;;; Get other props
(defun consult-gh-embark-get-title (cand)
  "Get the title of CAND.

When `current-prefix-args' is non-nil, add repository's name at the front."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (repo (get-text-property 0 :repo cand))
           (title (get-text-property 0 :title cand))
           (number (get-text-property 0 :number cand))
           (path (get-text-property 0 :path cand))
           (title (pcase class
                    ((or "file" "code")
                     (if current-prefix-arg
                         (format "%s/%s" repo path)
                       (format "%s" path)))
                    ((or "issue" "pr")
                     (if current-prefix-arg
                         (format "%s/#%s: %s" repo number title)
                       (concat "#" (format "%s: %s" number title))))
                    (_ (format "%s" repo)))))
      (string-trim title))))

(defun consult-gh-embark-get-url (cand)
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
          (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/blob/%s/%s" (or branch "HEAD") path))))
         ("pr"
          (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" number))))
         (_
          (string-trim (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))))))))


(defun consult-gh-embark-get-other-repos-by-same-user (cand)
  "List other repos by the same user/organization as CAND at point."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (user (consult-gh--get-username repo)))
      (consult-gh-repo-list user))))

(defun consult-gh-embark-get-user-name (cand)
  "Get the name of the user from CAND at point."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (user (consult-gh--get-username repo)))
      (consult-gh--get-user-fullname user))))

(defun consult-gh-embark-get-user-email (cand)
  "Get the email of the user from CAND at point."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (user (consult-gh--get-username repo)))
      (consult-gh--get-user-email user))))

(defun consult-gh-embark-get-user-link (cand)
  "Get the email of the user from CAND at point."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (user (consult-gh--get-username repo)))
      (consult-gh--get-user-link user))))

;;;; Open Actions
(defun consult-gh-embark-open-in-system-browser (cand)
  "Open the url link of CAND in the system's default browser."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (class (or (get-text-property 0 :class cand) nil))
           (number (or (get-text-property 0 :number cand) nil))
           (path (or (get-text-property 0 :path cand) nil))
           (branch (or (get-text-property 0 :branch cand) nil)))
      (consult-gh-with-host
       (consult-gh--auth-account-host)
       (pcase class
         ("issue"
          (consult-gh--call-process "issue" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
         ("file"
          (funcall #'browse-url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) (format "/blob/%s/" (or branch "HEAD")) path)))
         ("pr"
          (consult-gh--call-process "pr" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
         (_
          (consult-gh--call-process "repo" "view" "--web" (substring repo))))))))

(defun consult-gh-embark-open-in-default-browser (cand)
  "Open the url link of CAND with `consult-gh-browse-url-func'."
  (when (stringp cand)
    (let* ((url (consult-gh-embark-get-url cand)))
      (funcall consult-gh-browse-url-func url))))

(defun consult-gh-embark-open-repo-in-system-browser (cand)
  "Open the url link for user in CAND in the system's default browser."
  (when (stringp cand)
    (if-let* ((repo (get-text-property 0 :repo cand)))
        (consult-gh-with-host
         (consult-gh--auth-account-host)
         (consult-gh--call-process "repo" "view" "--web" (substring repo)))
      (message "No repo at point!"))))

(defun consult-gh-embark-open-repo-in-default-browser (cand)
  "Open the url link for user in CAND in the system's default browser."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (if-let* ((repo (get-text-property 0 :repo cand))
               (url (string-trim (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))))
         (funcall consult-gh-browse-url-func url)
       (message "No repo link at point!")))))

(defun consult-gh-embark-user-open-in-system-browser (cand)
  "Open the url link for user in CAND in the system's default browser."
  (when (stringp cand)
    (if-let* ((url (consult-gh-embark-get-user-link cand)))
        (funcall #'browse-url url)
      (message "No user's link at point!"))))

(defun consult-gh-embark-user-open-in-default-browser (cand)
  "Open the url link for user in CAND in the system's default browser."
  (when (stringp cand)
    (if-let* ((url (consult-gh-embark-get-user-link cand)))
        (funcall consult-gh-browse-url-func url)
      (message "No user's link at point!"))))

;;;; View Actions
(defun consult-gh-embark-view-readme-of-repo (cand)
  "Open readme of CAND repo at point."
  (when (stringp cand)
    (consult-gh--repo-view-action cand)))

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

(defun consult-gh-embark-view-issues-involves-user (cand)
  "Browse issues involving the user in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (if-let* ((repo (get-text-property 0 :repo cand))
               (user (consult-gh--get-username repo))
               (candidates (consult--slow-operation "Collecting issues ..."
                             (cl-loop for item in (split-string (consult-gh--command-to-string "search" "issues" "--involves" user) "[\r\n]+" t)
                                      collect (consult-gh--search-issues-format item "" nil)))))
         (consult--read candidates
                        :prompt "Select Issue: "
                        :lookup #'consult--lookup-member
                        :state (funcall #'consult-gh--issue-state)
                        :group #'consult-gh--issue-group
                        :require-match t
                        :category 'consult-gh-issues
                        :preview-key consult-gh-preview-key
                        :sort nil)
       (message "No user at point!")))))

(defun consult-gh-embark-view-prs-involves-user (cand)
  "Search pullrequests involving the user in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (if-let* ((repo (get-text-property 0 :repo cand))
               (user (consult-gh--get-username repo))
               (candidates (consult--slow-operation "Collecting pullrequests ..."
                             (cl-loop for item in (split-string (consult-gh--command-to-string "search" "prs" "--involves" user) "[\r\n]+" t)
                                      collect (consult-gh--search-prs-format item "" nil)))))
         (consult--read candidates
                        :prompt "Select PullRequest: "
                        :lookup #'consult--lookup-member
                        :state (funcall #'consult-gh--pr-state)
                        :group #'consult-gh--pr-search-group
                        :require-match t
                        :category 'consult-gh-prs
                        :preview-key consult-gh-preview-key
                        :sort nil)
       (message "No user at point!")))))

(defun consult-gh-embark-view-user-assignment (cand)
  "Search issues and prs assigned to the user in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (if-let* ((repo (get-text-property 0 :repo cand))
               (user (consult-gh--get-username repo))
               (candidates (consult--slow-operation "Collecting..."
                             (cl-loop for item in (split-string (consult-gh--command-to-string "search" "issues" "--include-prs" "--involves" user) "[\r\n]+" t)
                                      collect (consult-gh--search-issues-include-prs-format item "" nil)))))
         (consult--read candidates
                        :prompt "Select PullRequest: "
                        :lookup #'consult--lookup-member
                        :state (funcall #'consult-gh--dashboard-state)
                        :group #'consult-gh--dashboard-group
                        :require-match t
                        :category 'consult-gh-dashboard
                        :preview-key consult-gh-preview-key
                        :sort nil)
       (message "No user at point!")))))

(defun consult-gh-embark-view-files-of-repo (cand)
  "Browse files of CAND at point."
  (when (stringp cand)
    (let ((repo (or (get-text-property 0 :repo cand) (consult-gh--nonutf-cleanup cand))))
      (consult-gh-find-file repo))))

(defun consult-gh-embark-view-pr-diff (cand)
  "View the diff of a pull request in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh--pr-view-diff-action cand))))

;;;; Copy/Kill Actions
(defun consult-gh-embark-copy-title-as-kill (cand)
  "Copy the title of CAND to `kill-ring'."
  (when (stringp cand)
    (if-let* ((title (consult-gh-embark-get-title cand)))
        (kill-new (string-trim title)))))

(defun consult-gh-embark-copy-url-as-kill (cand)
  "Copy url link of CAND to `kill-ring'.

CAND can be a repo, issue, PR, file path, ..."
  (when (stringp cand)
    (let* ((url (consult-gh-embark-get-url cand)))
      (kill-new url))))

(defun consult-gh-embark-copy-org-link-as-kill (cand)
  "Copy the org-format url link of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (repo (get-text-property 0 :repo cand))
           (url (consult-gh-embark-get-url cand))
           (title (consult-gh-embark-get-title cand))
           (path (or (get-text-property 0 :path cand) nil))
           (branch (or (get-text-property 0 :branch cand) nil))
           (str nil))

      (pcase class
        ((or "issue" "pr")
         (when (not current-prefix-arg) (setq title (concat repo "/" title))))
        ((or "file" "code")
         (when (not current-prefix-arg) (setq title (concat repo "/" branch "/" path)))))

      (when url
        (setq str
              (cond
               ((and url title) (format " [[%s][%s]] " url title))
               (url (format " [[%s]] " url))
               (t nil))))

      (when (stringp str)
        (kill-new str)))))

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
  "Copy a drop-in “straight use-package” script of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (package (car (last (split-string repo "\/")))))
      (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" repo "\")\n)")))))

(defun consult-gh-embark-copy-usepackage-link-as-kill (cand)
  "Copy a drop-in “use-package” script of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (package (car (last (split-string repo "\/")))))
      (kill-new (concat "(use-package " package)))))

(defun consult-gh-embark-copy-file-contents-as-kill (cand)
  "Copy the contents of CAND file to `kill-ring'."
  (when (stringp cand)
    (let ((url (get-text-property 0 :url cand)))
      (when (and url (stringp url))
        (kill-new (consult-gh--files-get-content url))))))

(defun consult-gh-embark-copy-user-as-kill (cand)
  "Copy the user in CAND to `kill-ring'."
  (when (stringp cand)
    (when-let* ((repo (get-text-property 0 :repo cand))
                (user (consult-gh--get-username repo)))
      (kill-new user))))

(defun consult-gh-embark-copy-user-name-as-kill (cand)
  "Copy the name of the user in CAND to `kill-ring'."
  (when (stringp cand)
    (when-let ((name (consult-gh-embark-get-user-name cand)))
      (kill-new name))))

(defun consult-gh-embark-copy-user-email-as-kill (cand)
  "Copy the email of the user in CAND to `kill-ring'."
  (when (stringp cand)
    (when-let ((email (consult-gh-embark-get-user-email cand)))
      (kill-new email))))

(defun consult-gh-embark-copy-user-link-as-kill (cand)
  "Copy the link of the user page in CAND to `kill-ring'."
  (when (stringp cand)
    (when-let ((link (consult-gh-embark-get-user-link cand)))
      (kill-new link))))

;;;; Insert Actions
(defun consult-gh-embark-insert-title (cand)
  "Insert the title of CAND at point."
  (when (stringp cand)
    (if-let* ((title (consult-gh-embark-get-title cand)))
        (embark-insert (list (string-trim title))))))

(defun consult-gh-embark-insert-url (cand)
  "Insert the url of CAND at point."
  (when (stringp cand)
    (if-let* ((url (consult-gh-embark-get-url cand)))
        (embark-insert (list (string-trim url))))))

(defun consult-gh-embark-insert-link (cand)
  "Insert the link of CAND at point.

In `org-mode' or `markdown-mode',the link is formatted accordingly."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (repo (get-text-property 0 :repo cand))
           (url (consult-gh-embark-get-url cand))
           (title (consult-gh-embark-get-title cand))
           (path (or (get-text-property 0 :path cand) nil))
           (branch (or (get-text-property 0 :branch cand) nil)))

      (pcase class
        ((or "issue" "pr")
         (when (not current-prefix-arg) (setq title (concat repo "/" title))))
        ((or "file" "code")
         (when (not current-prefix-arg) (setq title (concat repo "/" branch "/" path)))))

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

(defun consult-gh-embark-insert-file-contents (cand)
  "Insert the contents of CAND file at point."
  (when (stringp cand)
    (let ((url (get-text-property 0 :url cand)))
      (when (and url (stringp url))
        (embark-insert (consult-gh--files-get-content url))))))

(defun consult-gh-embark-insert-user (cand)
  "Insert the user in CAND at point."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (user (consult-gh--get-username repo)))
      (if user (embark-insert (list user))
        (message "No user at point!")))))

(defun consult-gh-embark-insert-user-name (cand)
  "Insert the name of the user in CAND at point."
  (when (stringp cand)
    (if-let ((user (consult-gh-embark-get-user-name cand)))
        (embark-insert (list user))
      (message "No user at point!"))))

(defun consult-gh-embark-insert-user-email (cand)
  "Insert the email of the user in CAND at point."
  (when (stringp cand)
    (if-let ((email (consult-gh-embark-get-user-email cand)))
        (embark-insert (list email))
      (message "No email found for user at point!"))))

(defun consult-gh-embark-insert-user-link (cand)
  "Copy the org-format url link of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (title (consult-gh--get-username repo))
           (url (consult-gh-embark-get-user-link cand)))
      (if url
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
                     (t "")))))
        (message "No link found for user at point!")))))

;;;; Svae/Clone/Fork Actions

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

(defun consult-gh-embark-download-release (cand)
  "Download the CAND release at point."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh--release-download-action cand))))

;;;; Create Actions

(defun consult-gh-embark-create-repo (cand)
  "Create a new repo with CAND as name."
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (funcall #'consult-gh-repo-create (and (stringp cand) (string-remove-prefix (consult-gh--get-split-style-character) (substring-no-properties cand))))))

(defun consult-gh-embark-create-issue (cand)
  "Create an issue in repo of CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand))
            (class (get-text-property 0 :class cand))
            (title (pcase class
                     ((or "repo" "issue") nil)
                     ("pr" (get-text-property 0 :title cand))
                     (_ (or (get-text-property 0 :title cand) (string-remove-prefix (consult-gh--get-split-style-character) (substring-no-properties cand))))))
            (ref (pcase class
                   ((or "pr" "notification" "dashboard" "code" "file")
                    (consult-gh-embark-get-url cand))
                   (_ nil)))
            (body (when ref (not (string-empty-p ref)) (format "%s" ref))))
       (funcall #'consult-gh-issue-create repo title body)))))

(defun consult-gh-embark-create-pr (cand)
  "Create a pull request in repo of CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand))
            (class (get-text-property 0 :class cand))
            (title (pcase class
                     ((or "repo" "pr") nil)
                     ("issue" (get-text-property 0 :title cand))
                     (_ (or (get-text-property 0 :title cand) (string-remove-prefix (consult-gh--get-split-style-character) (substring-no-properties cand))))))
            (ref (pcase class
                   ((or "issue" "notification" "dashboard" "code" "file")
                    (consult-gh-embark-get-url cand))
                   (_ nil)))
            (body (when ref (not (string-empty-p ref)) (format "%s" ref))))
       (funcall #'consult-gh-pr-create repo title body)))))

(defun consult-gh-embark-create-release (cand)
  "Create a release in repo of CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand)))
       (funcall #'consult-gh-release-create repo)))))

;;;; Edit Issue Actions

(defun consult-gh-embark-toggle-issue-open (cand)
  "Close/Re-open the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((state (get-text-property 0 :state cand)))
       (if (equal state "OPEN")
           (funcall #'consult-gh-issue-close cand)
         (funcall #'consult-gh-issue-reopen cand))))))

(defun consult-gh-embark-toggle-issue-pin (cand)
  "Pin/Unpin the issue in CAND."
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
  "Lock/Unlock the issue in CAND."
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
  "Transfer the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-issue-transfer cand))))

(defun consult-gh-embark-delete-issue (cand)
  "Delete the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-issue-delete cand))))

(defun consult-gh-embark-develop-issue (cand)
  "Make a linked branch for the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-issue-develop cand))))

(defun consult-gh-embark-edit-issue (cand)
  "Make a linked branch for the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-issue-edit cand))))

(defun consult-gh-embark-comment-on-issue (cand)
  "Edit the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (get-text-property 0 :repo cand))
           (number (get-text-property 0 :number cand)))
       (with-current-buffer (funcall #'consult-gh--issue-view repo number)
         (consult-gh-topics-comment-create))))))

;;;; Edit PR Actions

(defun consult-gh-embark-edit-pr (cand)
  "Edit the pull request in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-pr-edit cand))))

(defun consult-gh-embark-merge-pr (cand)
  "Merge the pull request in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-pr-merge cand))))

(defun consult-gh-embark-review-pr (cand)
  "Review the pull request in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-pr-review cand))))

(defun consult-gh-embark-comment-on-pr (cand)
  "Make a linked branch for the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (get-text-property 0 :repo cand))
           (number (get-text-property 0 :number cand)))
       (with-current-buffer (funcall #'consult-gh--pr-view repo number)
         (consult-gh-topics-comment-create))))))

(defun consult-gh-embark-toggle-pr-open (cand)
  "Close/Re-open the pull request in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((state (get-text-property 0 :state cand)))
       (if (equal state "OPEN")
           (funcall #'consult-gh-pr-close cand)
         (funcall #'consult-gh-pr-reopen cand))))))

(defun consult-gh-embark-toggle-pr-lock (cand)
  "Lock/Unlock the pull request in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (when-let* ((repo (get-text-property 0 :repo cand))
                 (number (get-text-property 0 :number cand))
                 (state (when (and repo number)
                          (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/repos/%s/pulls/%s" repo number)) :locked))))
       (if (eq state 't)
           (funcall #'consult-gh-pr-unlock cand)
         (funcall #'consult-gh-pr-lock cand))))))

(defun consult-gh-embark-toggle-pr-draft (cand)
  "Toggle the pull request in CAND as draft/ready."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (when-let* ((repo (get-text-property 0 :repo cand))
                 (number (get-text-property 0 :number cand))
                 (state (when (and repo number)
                          (consult-gh--json-to-hashtable  (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" "isDraft") :isDraft))))
       (if (eq state 't)
           (funcall #'consult-gh-pr-mark-ready cand)
         (funcall #'consult-gh-pr-mark-draft cand))))))

(defun consult-gh-embark-link-pr-to-issue (cand)
  "Link CAND to Issue or PR.

CAND can be a PR or an issue."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (save-match-data
       (when-let* ((repo (get-text-property 0 :repo cand))
                   (number (get-text-property 0 :number cand))
                   (class (get-text-property 0 :class cand)))
         (pcase class
           ("pr"
            (let* ((issue (get-text-property 0 :number (consult-gh-issue-list repo t)))
                   (info (consult-gh--pr-read-json repo number))
                   (old-body (when (and issue (not (string-empty-p issue)) (hash-table-p info)) (gethash :body info)))
                   (new-body (when (and issue (not (string-match (format "\\(close\\|closes\\|closed\\|fix\\|fixes\\|fixed\\) #%s" issue) old-body)))
                               (concat (when old-body old-body)
                                       (format "\n\nclose #%s" issue)))))
              (when (and number new-body)
                (apply #'consult-gh--command-to-string "pr" "edit" number (list "--repo" repo "--body" (substring-no-properties new-body))))))
           ("issue"
            (let* ((pr (consult-gh-pr-list repo t))
                   (pr-number (get-text-property 0 :number pr))
                   (info (when (and pr pr-number) (consult-gh--pr-read-json repo pr-number)))
                   (old-body (when (and pr pr-number (hash-table-p info)) (gethash :body info)))
                   (new-body (when (and pr-number (not (string-match (format "\\(close\\|closes\\|closed\\|fix\\|fixes\\|fixed\\) #%s" number) old-body)))
                               (concat (when old-body old-body)
                                       (format "\n\nclose #%s" number)))))
              (when (and pr-number new-body)
                (apply #'consult-gh--command-to-string "pr" "edit" pr-number (list "--repo" repo "--body" (substring-no-properties new-body))))))
           (_ nil)))))))

;;;; Edit Notification Actions
(defun consult-gh-embark-toggle-notification-read (cand)
  "Mark the notification in CAND as read/unread."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((state (get-text-property 0 :state cand)))
       (if (equal state "Unread")
           (consult-gh--notifications-mark-as-read cand))))))

(defun consult-gh-embark-notification-toggle-subscription (cand)
  "Mark the notification in CAND as read/unread."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((thread (get-text-property 0 :thread cand))
            (subscription (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/notifications/threads/%s/subscription" thread)) :subscribed)))
       (if (eq subscription 't)
           (consult-gh--notifications-unsubscribe cand)
         (consult-gh--notifications-subscribe cand))))))

;;;; Edit Release Actions

(defun consult-gh-embark-mark-release-draft (cand)
  "Un(mark) the release in CAND as draft."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((type (get-text-property 0 :type cand)))
       (when (equal type "release")
         (funcall #'consult-gh-release-mark-draft cand))))))

(defun consult-gh-embark-toggle-release-prerelease (cand)
  "Mark/Unmark the release in CAND as prerelease."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((type (get-text-property 0 :type cand)))
       (when (equal type "release")
         (funcall #'consult-gh-release-toggle-prerelease cand))))))

(defun consult-gh-embark-mark-release-latest (cand)
  "Mark the release in CAND as latest."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (when-let* ((type (get-text-property 0 :type cand)))
       (when (equal type "release")
         (funcall #'consult-gh-release-mark-latest cand))))))

(defun consult-gh-embark-publish-release (cand)
  "Publish the release in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (when-let* ((type (get-text-property 0 :type cand)))

       (when (equal type "release")
        (funcall #'consult-gh-release-publish cand))))))

(defun consult-gh-embark-delete-release (cand)
  "Delete the release in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-release-delete cand))))

(defun consult-gh-embark-edit-release (cand)
  "Edit the release in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-release-edit cand))))

;;;; Other Actions

(defun consult-gh-embark-email-user (cand)
  "Insert the email of the user in CAND at point."
  (when (stringp cand)
    (if-let ((email (consult-gh-embark-get-user-email cand)))
        (compose-mail email)
      (message "No email at point!"))))

(defun consult-gh-embark-search-code-in-repo (cand)
  "Search for code in CAND repo at point."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (or (get-text-property 0 :repo cand) (substring-no-properties cand))))
       (consult-gh-search-code nil repo)))))

;;; Define Embark Keymaps

;;;; General Keymaps
;;;;; Bookmark Menu Keymap
(defvar-keymap consult-gh-embark-bookmark-repos-menu-map
  :doc "Keymap for bookmarking repos menu"
  :parent nil
  "r" '("add to known repos" . consult-gh-embark-add-repo-to-known-repos)
  "k" '("remove from known repos" . consult-gh-embark-remove-repo-from-known-repos))

(fset 'consult-gh-embark-bookmark-repos-menu-map consult-gh-embark-bookmark-repos-menu-map)

(defvar-keymap consult-gh-embark-bookmark-orgs-menu-map
  :doc "Keymap for bookmarking orgs menu"
  :parent nil
  "o" '("add to known orgs" . consult-gh-embark-add-org-to-known-orgs)
  "k" '("remove from known orgs" . consult-gh-embark-remove-org-from-known-orgs)
  "d" '("add to favorite orgs" . consult-gh-embark-add-org-to-favorite-list)
  "D" '("remove from favorite orgs" . consult-gh-embark-remove-org-from-favorite-list))

(fset 'consult-gh-embark-bookmark-orgs-menu-map consult-gh-embark-bookmark-orgs-menu-map)

(defvar-keymap consult-gh-embark-bookmarks-menu-map
  :doc "Keymap for bookmarks menu"
  :parent nil
  "r" '("bookmark repos" . consult-gh-embark-bookmark-repos-menu-map)
  "o" '("bookmark orgs" . consult-gh-embark-bookmark-orgs-menu-map))

(fset 'consult-gh-embark-bookmarks-menu-map consult-gh-embark-bookmarks-menu-map)

;;;;; Copy Menu Keymap
;;;;;; Copy  User's Info Keymap
(defvar-keymap consult-gh-embark-user-copy-menu-map
  :doc "Keymap for copying user info as kill menu"
  :parent nil
  "w" '("GitHubusername" . consult-gh-embark-copy-user-as-kill)
  "n" '("full name" . consult-gh-embark-copy-user-name-as-kill)
  "e" '("email" . consult-gh-embark-copy-user-email-as-kill)
  "l" '("user page" . consult-gh-embark-copy-user-link-as-kill))

(fset 'consult-gh-embark-user-copy-menu-map consult-gh-embark-user-copy-menu-map)
(defvar-keymap consult-gh-embark-copy-menu-map
  :doc "Keymap for copy-as-kill menu"
  :parent nil
  "t" '("title" . consult-gh-embark-copy-title-as-kill)
  "u" '("url link" . consult-gh-embark-copy-url-as-kill)
  "h" '("https link" . consult-gh-embark-copy-https-link-as-kill)
  "s" '("ssh link" . consult-gh-embark-copy-ssh-link-as-kill)
  "o" '("org-mode link" . consult-gh-embark-copy-org-link-as-kill)
  "U" '("user's info" . consult-gh-embark-user-copy-menu-map))

(fset 'consult-gh-embark-copy-menu-map consult-gh-embark-copy-menu-map)

;;;;; Create Menu Keymap
(defvar-keymap consult-gh-embark-create-menu-map
  :doc "Keymap for create menu"
  :parent nil
  "i" '("create issue" . consult-gh-embark-create-issue)
  "p" '("create pull request" . consult-gh-embark-create-pr)
  "r" '("create repo" . consult-gh-embark-create-repo)
  "R" '("cretae release" . consult-gh-embark-create-release)
  "c" '("create a clone of repo" . consult-gh-embark-clone-repo)
  "f" '("create a fork repo" . consult-gh-embark-fork-repo))

(fset 'consult-gh-embark-create-menu-map consult-gh-embark-create-menu-map)

;;;;; Edit  Menu Keymap
(defvar-keymap consult-gh-embark-edit-menu-map
  :doc "Keymap for edit menu"
  :parent nil)

(fset 'consult-gh-embark-edit-menu-map consult-gh-embark-edit-menu-map)

;;;;; Find Menu Keymap
(defvar-keymap consult-gh-embark-find-menu-map
  :doc "Keymap for find menu"
  :parent nil
  "f" '("find file (browse files of repo)" . consult-gh-embark-view-files-of-repo)
  "r" '("find repos by user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("find issues of repo" . consult-gh-embark-view-issues-of-repo)
  "p" '("find prs of repo" . consult-gh-embark-view-prs-of-repo)
  "c" '("find code in repo" . consult-gh-embark-search-code-in-repo))

(fset 'consult-gh-embark-find-menu-map consult-gh-embark-find-menu-map)

;;;;; Insert Menu Keymap
;;;;;; Insert  User's Info Keymap
(defvar-keymap consult-gh-embark-user-insert-menu-map
  :doc "Keymap for inserting user info menu"
  :parent nil
  "u" '("insert GitHub username" .  consult-gh-embark-insert-user)
  "n" '("insert ful name" . consult-gh-embark-insert-user-name)
  "e" '("insert email" . consult-gh-embark-insert-user-email)
  "l" '("insert user url" . consult-gh-embark-insert-user-link))

(fset 'consult-gh-embark-user-insert-menu-map consult-gh-embark-user-insert-menu-map)

(defvar-keymap consult-gh-embark-insert-menu-map
  :doc "Keymap for insert menu"
  :parent nil
  "t" '("insert title" . consult-gh-embark-insert-title)
  "u" '("insert url" . consult-gh-embark-insert-url)
  "l" '("insert link" .  consult-gh-embark-insert-link)
  "U" '("insert user info" . consult-gh-embark-user-insert-menu-map))

(fset 'consult-gh-embark-insert-menu-map consult-gh-embark-insert-menu-map)

;;;;; Links Menu Keymap
(defvar-keymap consult-gh-embark-links-menu-map
  :doc "Keymap for links menu"
  :parent nil
  "h" '("copy https url" . consult-gh-embark-copy-https-link-as-kill)
  "s" '("copy ssh url" . consult-gh-embark-copy-ssh-link-as-kill)
  "l" '("copy url" . consult-gh-embark-copy-url-as-kill)
  "o" '("copy org-mode link" . consult-gh-embark-copy-org-link-as-kill)
  "u" '("copy straight use-package link" . consult-gh-embark-copy-straight-usepackage-link-as-kill)
  "U" '("copy user page link" . consult-gh-embark-copy-user-link-as-kill))

(fset 'consult-gh-embark-links-menu-map consult-gh-embark-links-menu-map)

;;;;; Open Menu Keymap
(defvar-keymap consult-gh-embark-open-menu-map
  :doc "Keymap for open menu"
  :parent nil
  "O" '("system browser" . consult-gh-embark-open-in-system-browser)
  "o" '("consult-gh default browser" . consult-gh-embark-open-in-default-browser)
  "p" '("preview buffer" . consult-gh-embark-show-preview)
  "RET" '("open in emacs" . consult-gh-embark-default-action))

(fset 'consult-gh-embark-open-menu-map consult-gh-embark-open-menu-map)

;;;;; Repo Menu Keymap
(defvar-keymap consult-gh-embark-repo-menu-map
  :doc "Keymap for repo actions menu"
  :parent nil
  "c" '("clone repo" . consult-gh-embark-clone-repo)
  "f" '("fork repo" . consult-gh-embark-fork-repo)
  "r" '("other repos of user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("issues of repo" . consult-gh-embark-view-issues-of-repo)
  "p" '("prs of repo" . consult-gh-embark-view-prs-of-repo)
  "s" '("search for code in repo" . consult-gh-embark-search-code-in-repo)
  "b" '("browse files of repo" . consult-gh-embark-view-files-of-repo)
  "o" '("open repo page in default browser" .  consult-gh-embark-open-repo-in-default-browser)
  "O" '("open repo page in system browser" .  consult-gh-embark-open-repo-in-system-browser))


(fset 'consult-gh-embark-repo-menu-map consult-gh-embark-repo-menu-map)

;;;;; User Menu Keymap
(defvar-keymap consult-gh-embark-user-menu-map
  :doc "Keymap for user actions menu"
  :parent nil
  "e" '("email user" . consult-gh-embark-email-user)
  "r" '("repos of user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("issues involving user" . consult-gh-embark-view-issues-involves-user)
  "p" '("prs involving user" . consult-gh-embark-view-prs-involves-user)
  "a" '("user assignment" . consult-gh-embark-view-user-assignment)
  "u" '("insert user info" . consult-gh-embark-user-insert-menu-map)
  "w" '("copy user info as kill" . consult-gh-embark-user-copy-menu-map)
  "o" '("open user page in default browser" .  consult-gh-embark-user-open-in-default-browser)
  "O" '("open repo page in system browser" .  consult-gh-embark-user-open-in-system-browser))

(fset 'consult-gh-embark-user-menu-map consult-gh-embark-user-menu-map)

;;;;; View Menu Keymap
(defvar-keymap consult-gh-embark-view-menu-map
  :doc "Keymap for view actions menu"
  :parent nil)

(fset 'consult-gh-embark-view-menu-map consult-gh-embark-view-menu-map)

;;;;; Main Menu Keymap
(defvar-keymap consult-gh-embark-general-actions-map
  :doc "Keymap for consult-gh-embark"
  :parent embark-general-map
  "b" '("gh bookmarks" . consult-gh-embark-bookmarks-menu-map)
  "c" '("gh create" . consult-gh-embark-create-menu-map)
  "f" '("gh find" . consult-gh-embark-find-menu-map)
  "i" '("gh insert" . consult-gh-embark-insert-menu-map)
  "l" '("gh links" . consult-gh-embark-links-menu-map)
  "o" '("gh open" . consult-gh-embark-open-menu-map)
  "r" '("gh repo" . consult-gh-embark-repo-menu-map)
  "u" '("gh user" . consult-gh-embark-user-menu-map)
  "w" '("gh copy-as-kill" . consult-gh-embark-copy-menu-map)
  "v" '("gh view" . consult-gh-embark-view-menu-map))

;;;; Org Keymap
(defvar-keymap consult-gh-embark-orgs-actions-map
  :doc "Keymap for consult-gh-embark-orgs"
  :parent consult-gh-embark-general-actions-map)

;;;; Repo Keymap
;;;;;; Repo View Menu Keymap
(defvar-keymap consult-gh-embark-repo-view-menu-map
  :doc "Keymap for viewing Repo details"
  :parent nil
  "r" '("view readme" . consult-gh-embark-view-readme-of-repo)
  "i" '("view issues" . consult-gh-embark-view-issues-of-repo)
  "p" '("view pull requests" . consult-gh-embark-view-prs-of-repo))

(fset 'consult-gh-embark-repo-view-menu-map consult-gh-embark-repo-view-menu-map)

;;;;;; Repo Main Menu Keymap
(defvar-keymap consult-gh-embark-repos-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "v" '("gh view repo" . consult-gh-embark-repo-view-menu-map))

;;;;; Files Keymap
;;;;;; Files Insert Menu Keymap
(defvar-keymap consult-gh-embark-fiels-insert-menu-map
  :doc "Keymap for editing issues"
  :parent consult-gh-embark-user-insert-menu-map
  "f" '("file content" . consult-gh-embark-insert-file-contents))

(fset 'consult-gh-embark-fiels-insert-menu-map consult-gh-embark-fiels-insert-menu-map)

;;;;;; Files Main Menu Keymap
(defvar-keymap consult-gh-embark-files-actions-map
  :doc "Keymap for consult-gh-embark-files"
  :parent consult-gh-embark-general-actions-map
  "s" '("gh save file" . consult-gh-embark-save-file)
  "i" '("gh insert" . consult-gh-embark-fiels-insert-menu-map))

;;;;; Issue Keymap
;;;;;; Edit Issue Menu Keymap
(defvar-keymap consult-gh-embark-issues-edit-menu-map
  :doc "Keymap for editing issues"
  :parent consult-gh-embark-edit-menu-map
  "D" '("delete issue" . consult-gh-embark-delete-issue)
  "e" '("edit issue" . consult-gh-embark-edit-issue)
  "d" '("develop issue" . consult-gh-embark-develop-issue)
  "l" '("lock/ulock issue" . consult-gh-embark-toggle-issue-lock)
  "o" '("open/close issue" . consult-gh-embark-toggle-issue-open)
  "p" '("pin/unpin issue" . consult-gh-embark-toggle-issue-pin)
  "t" '("transfer issue" . consult-gh-embark-transfer-issue)
  "L" '("link to pr" . consult-gh-embark-link-pr-to-issue))

(fset 'consult-gh-embark-issues-edit-menu-map consult-gh-embark-issues-edit-menu-map)

;;;;;; Issue Main Menu Keymap
(defvar-keymap consult-gh-embark-issues-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "c" '("gh create" . consult-gh-embark-create-menu-map)
  "c C-c" '("comment on issue" . consult-gh-embark-comment-on-issue)
  "e" '("gh edit issue" . consult-gh-embark-issues-edit-menu-map))

;;;;; Pull Request Keymap
;;;;;; Edit PRs Menu Keymap
(defvar-keymap consult-gh-embark-prs-edit-menu-map
  :doc "Keymap for editing PRs"
  :parent consult-gh-embark-edit-menu-map
  "e" '("edit pr" . consult-gh-embark-edit-pr)
  "d" '("draft/undraft pr" . consult-gh-embark-toggle-pr-draft)
  "l" '("lock/unlock pr" . consult-gh-embark-toggle-pr-lock)
  "m" '("merge pr" . consult-gh-embark-merge-pr)
  "o" '("open/close pr" . consult-gh-embark-toggle-pr-open)
  "L" '("link to issue" . consult-gh-embark-link-pr-to-issue))

(fset 'consult-gh-embark-prs-edit-menu-map consult-gh-embark-prs-edit-menu-map)

;;;;;; PR View Menu Keymap
(defvar-keymap consult-gh-embark-prs-view-menu-map
  :doc "Keymap for viewing PR details"
  :parent nil
  "d" '("view diff" . consult-gh-embark-view-pr-diff)
  "r" '("view repo" . consult-gh-embark-repo-view-menu-map))

(fset 'consult-gh-embark-prs-view-menu-map consult-gh-embark-prs-view-menu-map)

;;;;; PR Main Menu Keymap
(defvar-keymap consult-gh-embark-prs-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "c" '("gh create" . consult-gh-embark-create-menu-map)
  "c C-c" '("comment on pr" . consult-gh-embark-comment-on-pr)
  "c C-r" '("review for pr" . consult-gh-embark-review-pr)
  "e" '("gh edit pr" . consult-gh-embark-prs-edit-menu-map)
  "v" '("gh view pr" . consult-gh-embark-prs-view-menu-map))

;;;;; Release Keymap
;;;;;; Edit Release Menu Keymap
(defvar-keymap consult-gh-embark-releases-edit-menu-map
  :doc "Keymap for editing issues"
  :parent nil
  "D" '("delete release" . consult-gh-embark-delete-release)
  "e" '("edit release" . consult-gh-embark-edit-release)
  "d" '("mark as draft" . consult-gh-embark-mark-release-draft)
  "P" '("publish release" . consult-gh-embark-publish-release)
  "l" '("mark as latest" . consult-gh-embark-mark-release-latest)
  "p" '("mark/unmark prerelease" . consult-gh-embark-toggle-release-prerelease))

(fset 'consult-gh-embark-releases-edit-menu-map consult-gh-embark-releases-edit-menu-map)

;;;;;; Release Main Menu Keymap
(defvar-keymap consult-gh-embark-releases-actions-map
  :doc "Keymap for consult-gh-embark-releases"
  :parent consult-gh-embark-general-actions-map
  "c" '("gh create" . consult-gh-embark-create-menu-map)
  "d" '("gh download release" . consult-gh-embark-download-release)
  "e" '("gh edit release" . consult-gh-embark-releases-edit-menu-map)
  "T" '("gh test" . consult-gh-embark-test))

;;;; Code Keymap
(defvar-keymap consult-gh-embark-codes-actions-map
  :doc "Keymap for consult-gh-embark-codes"
  :parent consult-gh-embark-general-actions-map)

;;;; Notifications Keymap
;;;;; Edit Notifications Menu Keymap
(defvar-keymap consult-gh-embark-notifications-edit-menu-map
  :doc "Keymap for editing notifications"
  :parent nil
  "r" '("mark read/unread" . consult-gh-embark-toggle-notification-read)
  "s" '("unsubscribe/resubscribe" . consult-gh-embark-notification-toggle-subscription))

(fset 'consult-gh-embark-notifications-edit-menu-map consult-gh-embark-notifications-edit-menu-map)

;;;;; Notifications Main Menu Keymap
(defvar-keymap consult-gh-embark-notifications-actions-map
  :doc "Keymap for consult-gh-embark-notifications"
  :parent consult-gh-embark-general-actions-map
  "e" '("gh edit notification" . consult-gh-embark-notifications-edit-menu-map))

(defvar-keymap consult-gh-embark-dashboard-actions-map
  :doc "Keymap for consult-gh-embark-dashboard"
  :parent consult-gh-embark-general-actions-map)

(defun consult-gh-embark--mode-on ()
  "Enable `consult-gh-embark-mode'."
  ;; add keymaps
  (setq embark-keymap-alist
        (append embark-keymap-alist
                '((consult-gh . consult-gh-embark-general-actions-map)
                  (consult-gh-orgs . consult-gh-embark-orgs-actions-map)
                  (consult-gh-repos . consult-gh-embark-repos-actions-map)
                  (consult-gh-files . consult-gh-embark-files-actions-map)
                  (consult-gh-issues . consult-gh-embark-issues-actions-map)
                  (consult-gh-prs . consult-gh-embark-prs-actions-map)
                  (consult-gh-notifications . consult-gh-embark-notifications-actions-map)
                  (consult-gh-dashboard . consult-gh-embark-dashboard-actions-map)
                  (consult-gh-releases . consult-gh-embark-releases-actions-map))))


  ;; override default actions
  (setq embark-default-action-overrides
        (append embark-default-action-overrides
                '((consult-gh-repos . consult-gh-embark-default-action)
                  (consult-gh-issues . consult-gh-embark-default-action)
                  (consult-gh-prs . consult-gh-embark-default-action)
                  (consult-gh-files . consult-gh-embark-default-action)
                  (consult-gh-codes . consult-gh-embark-default-action)
                  (consult-gh-notifications . consult-gh-embark-default-action)
                  (consult-gh-dashboard . consult-gh-embark-default-action)
                  (consult-gh-releases . consult-gh-embark-default-action))))


  ;; set post actions-hook
  (setq embark-post-action-hooks
        (append embark-post-action-hooks
                '((consult-gh-embark-mark-release-draft embark--restart)
                  (consult-gh-embark-toggle-release-prerelease embark--restart)
                  (consult-gh-embark-publish-release embark--restart)
                  (consult-gh-embark-mark-release-latest embark--restart)))))



(defun consult-gh-embark--mode-off ()
  "Disable `consult-gh-embark-mode'."
  ;;unset keymaps
  (setq  embark-keymap-alist
         (seq-difference embark-keymap-alist
                         '((consult-gh . consult-gh-embark-general-actions-map)
                           (consult-gh-orgs . consult-gh-embark-orgs-actions-map)
                           (consult-gh-repos . consult-gh-embark-repos-actions-map)
                           (consult-gh-files . consult-gh-embark-files-actions-map)
                           (consult-gh-issues . consult-gh-embark-issues-actions-map)
                           (consult-gh-prs . consult-gh-embark-prs-actions-map)
                           (consult-gh-notifications . consult-gh-embark-notifications-actions-map)
                           (consult-gh-dashboard . consult-gh-embark-dashboard-actions-map)
                           (consult-gh-releases . consult-gh-embark-releases-actions-map))))

;; unset default actions
  (setq embark-default-action-overrides
        (seq-difference embark-default-action-overrides
                        '((consult-gh-repos . consult-gh-embark-default-action)
                          (consult-gh-issues . consult-gh-embark-default-action)
                          (consult-gh-prs . consult-gh-embark-default-action)
                          (consult-gh-files . consult-gh-embark-default-action)
                          (consult-gh-codes . consult-gh-embark-default-action)
                          (consult-gh-notifications . consult-gh-embark-default-action)
                          (consult-gh-dashboard . consult-gh-embark-default-action)
                          (consult-gh-releases . consult-gh-embark-default-action))))

  ;; unset post action hooks
  (setq embark-post-action-hooks
        (seq-difference embark-post-action-hooks
                        '((consult-gh-embark-mark-release-draft embark--restart)
                          (consult-gh-embark-toggle-release-prerelease embark--restart)
                          (consult-gh-embark-publish-release embark--restart)
                          (consult-gh-embark-mark-release-latest embark--restart)))))

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
