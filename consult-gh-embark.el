;;; consult-gh-embark.el --- Embark Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.2
;; Package-Requires: ((emacs "30.0") (embark-consult "1.1") (consult-gh "2.2"))
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
          (funcall (consult-gh--notifications-state) 'preview cand))
         ("dashboard"
          (funcall (consult-gh--dashboard-state) 'preview cand))
         (_
          (funcall (consult-gh--repo-state) 'preview cand)))))))

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
          (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/blob/%s/%s" (or branch "HEAD") path))))
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
           (repo (get-text-property 0 :repo cand))
           (url (consult-gh-embark-get-url-link cand))
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

(defun consult-gh-embark-insert-url-link (cand)
  "Insert the url of CAND at point."
  (when (stringp cand)
    (let* ((class (get-text-property 0 :class cand))
           (repo (get-text-property 0 :repo cand))
           (url (consult-gh-embark-get-url-link cand))
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

(defun consult-gh-embark-search-code-in-repo (cand)
  "Search for code in CAND repo at point."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (or (get-text-property 0 :repo cand) (substring-no-properties cand))))
       (consult-gh-search-code nil repo)))))

(defun consult-gh-embark-search-code-in-repo (cand)
  "Search for code in CAND repo at point."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (or (get-text-property 0 :repo cand) (substring-no-properties cand))))
       (consult-gh-search-code nil repo)))))

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

(defun consult-gh-embark-create-repo (cand)
  "Create a new repo with CAND as name"
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-repo-create (and (stringp cand) (string-remove-prefix (consult-gh--get-split-style-character) cand)))))

(defun consult-gh-embark-create-repo (cand)
  "Create a new repo with CAND as name"
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-repo-create (and (stringp cand) (string-remove-prefix (plist-get (alist-get consult-async-split-style consult-async-split-styles-alist) :initial) (substring-no-properties cand))))))

(defun consult-gh-embark-create-issue (cand)
  "Create an issue in repo of CAND"
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
                   (consult-gh-embark-get-url-link cand))
                  (_ nil)))
           (body (when ref (not (string-empty-p ref)) (format "%s" ref))))
       (funcall #'consult-gh-issue-create repo title body)))))

(defun consult-gh-embark-create-pr (cand)
  "Create a pull request in repo of CAND"
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
                   (consult-gh-embark-get-url-link cand))
                  (_ nil)))
           (body (when ref (not (string-empty-p ref)) (format "%s" ref))))
       (funcall #'consult-gh-pr-create repo title body)))))

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
           (funcall #'consult-gh-pr-unlock cand)
         (funcall #'consult-gh-pr-lock cand))))))

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

;; General Actions
; Bookmarks Menu
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

; Links Menu
(defvar-keymap consult-gh-embark-links-menu-map
  :doc "Keymap for links menu"
  :parent nil
  "h" '("copy https url" . consult-gh-embark-copy-https-link-as-kill)
  "s" '("copy ssh url" . consult-gh-embark-copy-ssh-link-as-kill)
  "l" '("copy url" . consult-gh-embark-copy-url-link-as-kill)
  "o" '("copy org-mode link" . consult-gh-embark-copy-url-org-link-as-kill)
  "u" '("copy straight use-package link" . consult-gh-embark-copy-straight-usepackage-link-as-kill))

(fset 'consult-gh-embark-links-menu-map consult-gh-embark-links-menu-map)

; Create Menu
(defvar-keymap consult-gh-embark-create-menu-map
  :doc "Keymap for create menu"
  :parent nil
  "i" '("create issue" . consult-gh-embark-create-issue)
  "p" '("create pull request" . consult-gh-embark-create-pr)
  "r" '("create repo" . consult-gh-embark-create-repo)
  "c" '("create a clone of repo" . consult-gh-embark-clone-repo)
  "f" '("create a fork repo" . consult-gh-embark-fork-repo))

(fset 'consult-gh-embark-create-menu-map consult-gh-embark-create-menu-map)

; Find Menu
(defvar-keymap consult-gh-embark-find-menu-map
  :doc "Keymap for find menu"
  :parent nil
  "f" '("find file (browse files of repo)" . consult-gh-embark-view-files-of-repo)
  "r" '("find repos by user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("find issues of repo" . consult-gh-embark-view-issues-of-repo)
  "p" '("find prs of repo" . consult-gh-embark-view-prs-of-repo)
  "c" '("find code in repo" . consult-gh-embark-search-code-in-repo))

(fset 'consult-gh-embark-find-menu-map consult-gh-embark-find-menu-map)


; Insert Menu
(defvar-keymap consult-gh-embark-insert-menu-map
  :doc "Keymap for insert menu"
  :parent nil
  "t" '("insert title" . consult-gh-embark-insert-title)
  "u" '("insert url" . consult-gh-embark-insert-url-link))

(fset 'consult-gh-embark-insert-menu-map consult-gh-embark-insert-menu-map)

; Open Menu
(defvar-keymap consult-gh-embark-open-menu-map
  :doc "Keymap for open menu"
  :parent nil
  "o" '("system browser" . consult-gh-embark-open-in-system-browser)
  "O" '("consult-gh default browser" . consult-gh-embark-open-in-default-browser)
  "p" '("preview buffer" . consult-gh-embark-show-preview)
  "RET" '("open in emacs" . consult-gh-embark-default-action))

(fset 'consult-gh-embark-open-menu-map consult-gh-embark-open-menu-map)

; Repos Menu
(defvar-keymap consult-gh-embark-repo-menu-map
  :doc "Keymap for repo actions menu"
  :parent nil
  "c" '("clone repo" . consult-gh-embark-clone-repo)
  "f" '("fork repo" . consult-gh-embark-fork-repo)
  "r" '("other repos of user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("issues of repo" . consult-gh-embark-view-issues-of-repo)
  "p" '("prs of repo" . consult-gh-embark-view-prs-of-repo)
  "s" '("search for code in repo" . consult-gh-embark-search-code-in-repo)
  "b" '("browse files of repo" . consult-gh-embark-view-files-of-repo))

(fset 'consult-gh-embark-repo-menu-map consult-gh-embark-repo-menu-map)

; Copy Menu
(defvar-keymap consult-gh-embark-copy-menu-map
  :doc "Keymap for copy-as-kill menu"
  :parent nil
  "t" '("title" . consult-gh-embark-copy-title-as-kill)
  "u" '("url link" . consult-gh-embark-copy-url-link-as-kill)
  "h" '("https link" . consult-gh-embark-copy-https-link-as-kill)
  "s" '("ssh link" . consult-gh-embark-copy-ssh-link-as-kill)
  "o" '("org-mode link" . consult-gh-embark-copy-url-org-link-as-kill))

(fset 'consult-gh-embark-copy-menu-map consult-gh-embark-copy-menu-map)

; Main
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
  "w" '("gh copy-as-kill" . consult-gh-embark-copy-menu-map))

;; Org Actions
(defvar-keymap consult-gh-embark-orgs-actions-map
  :doc "Keymap for consult-gh-embark-orgs"
  :parent consult-gh-embark-general-actions-map)

;; Repo Actions
(defvar-keymap consult-gh-embark-repos-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map)


;; Files Actions
(defvar-keymap consult-gh-embark-files-actions-map
  :doc "Keymap for consult-gh-embark-files"
  :parent consult-gh-embark-general-actions-map
  "s" '("gh save file" . consult-gh-embark-save-file))

;; Issue Actions

; Edit Menu
(defvar-keymap consult-gh-embark-issues-edit-menu-map
  :doc "Keymap for editing issues"
  :parent nil
  "D" '("delete issue" . consult-gh-embark-delete-issue)
  "e" '("edit issue" . consult-gh-embark-edit-issue)
  "d" '("develop issue" . consult-gh-embark-develop-issue)
  "l" '("lock/ulock issue" . consult-gh-embark-toggle-issue-lock)
  "o" '("open/close issue" . consult-gh-embark-toggle-issue-open)
  "p" '("pin/unpin issue" . consult-gh-embark-toggle-issue-pin)
  "t" '("transfer issue" . consult-gh-embark-transfer-issue)
  "L" '("link to pr" . consult-gh-embark-link-pr-to-issue))

(fset 'consult-gh-embark-issues-edit-menu-map consult-gh-embark-issues-edit-menu-map)

(defvar-keymap consult-gh-embark-issues-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "c" '("gh create" . consult-gh-embark-create-menu-map)
  "c C-c" '("comment on issue" . consult-gh-embark-comment-on-issue)
  "e" '("gh edit issue" . consult-gh-embark-issues-edit-menu-map))


;; Pull Request Actions

;Edit PRs menu
(defvar-keymap consult-gh-embark-prs-edit-menu-map
  :doc "Keymap for editing PRs"
  :parent nil
  "e" '("edit pr" . consult-gh-embark-edit-pr)
  "d" '("draft/undraft pr" . consult-gh-embark-toggle-pr-draft)
  "l" '("lock/unlock pr" . consult-gh-embark-toggle-pr-lock)
  "m" '("merge pr" . consult-gh-embark-merge-pr)
  "o" '("open/close pr" . consult-gh-embark-toggle-pr-open)
  "L" '("link to issue" . consult-gh-embark-link-pr-to-issue))

(fset 'consult-gh-embark-prs-edit-menu-map consult-gh-embark-prs-edit-menu-map)

(defvar-keymap consult-gh-embark-prs-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "c" '("gh create" . consult-gh-embark-create-menu-map)
  "c C-c" '("comment on pr" . consult-gh-embark-comment-on-pr)
  "c C-r" '("review for pr" . consult-gh-embark-review-pr)
  "e" '("gh edit pr" . consult-gh-embark-prs-edit-menu-map))

;; Code Actions
(defvar-keymap consult-gh-embark-codes-actions-map
  :doc "Keymap for consult-gh-embark-codes"
  :parent consult-gh-embark-general-actions-map)

;; Notifications Actions
;Edit Notifications Menu
(defvar-keymap consult-gh-embark-notifications-edit-menu-map
  :doc "Keymap for editing notifications"
  :parent nil
  "r" '("mark read/unread" . consult-gh-embark-toggle-notification-read)
  "s" '("unsubscribe/resubscribe" . consult-gh-embark-notification-toggle-subscription))

(fset 'consult-gh-embark-notifications-edit-menu-map consult-gh-embark-notifications-edit-menu-map)

(defvar-keymap consult-gh-embark-notifications-actions-map
  :doc "Keymap for consult-gh-embark-notifications"
  :parent consult-gh-embark-general-actions-map
  "e" '("gh edit notification" . consult-gh-embark-notifications-edit-menu-map))

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
