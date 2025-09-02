;;; consult-gh-embark.el --- Embark Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 3.0
;; Package-Requires: ((emacs "29.4") (consult "2.0") (consult-gh "3.0") (embark-consult "1.1") (which-key "3.6.0"))
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
(require 'which-key)

(defun consult-gh--embark-restart (&rest _)
  "Restart current async search with current input.
Use this to refresh the list of candidates for commands that do
not handle that themselves."
  (when (active-minibuffer-window)
  (with-selected-window (minibuffer-window)
    (let ((text  (minibuffer-contents)))
      (when (commandp consult-gh--last-command)
          (embark--become-command consult-gh--last-command text))))))

;;; Define Embark Action Functions

;;;; Default Actions
(defun consult-gh-embark-default-action (cand)
  "Open CAND."
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

(defun consult-gh-embark-add-repo-to-workflow-template-list (cand)
  "Add CAND repo to `consult-gh-workflow-template-repo-sources'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand)))
      (add-to-list 'consult-gh-workflow-template-repo-sources repo))))

(defun consult-gh-embark-remove-repo-from-workflow-template-list (cand)
  "Remove CAND repo from `consult-gh-workflow-template-repo-sources'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand)))
      (setq consult-gh-workflow-template-repo-sources (delete repo consult-gh-workflow-template-repo-sources)))))

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
           (sha (get-text-property 0 :sha cand))
           (branch (get-text-property 0 :ref cand))
           (title (pcase class
                    ((or "file" "code")
                     (if current-prefix-arg
                         (format "%s/%s" repo path)
                       (format "%s" path)))
                    ((or "issue" "pr" "dashboard")
                     (if current-prefix-arg
                         (format "%s/#%s: %s" repo number title)
                       (concat "#" (format "%s: %s" number title))))
                    ("notification"
                     (cond
                      ((or (equal (get-text-property 0 :type cand) "issue")
                           (equal (get-text-property 0 :type cand) "pr"))
                       (if current-prefix-arg
                           (format "%s/#%s: %s" repo number title)
                         (concat "#" (format "%s: %s" number title))))
                      (t title)))
                    ("commit"
                     (if current-prefix-arg
                         (format "%s/%s" repo sha)
                       (concat (format "%s" sha))))
                    ("branch"
                     (if current-prefix-arg
                         (format "%s@%s" repo branch)
                       (concat (format "%s" branch))))
                    (_ (format "%s" repo)))))
      (string-trim title))))

(defun consult-gh-embark-get-url (cand)
  "Get url link of CAND.

The candidate can be a repo, issue, PR, file path, or a branch."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand))
            (class (get-text-property 0 :class cand))
            (number (get-text-property 0 :number cand))
            (path (get-text-property 0 :path cand))
            (id (get-text-property 0 :id cand))
            (sha (get-text-property 0 :sha cand))
            (url (get-text-property 0 :url cand))
            (ref (get-text-property 0 :ref cand)))
       (or url
           (pcase class
             ("issue"
              (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/issues/%s" number))))
             ((or "file" "code")
              (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/blob/%s/%s" (or ref "HEAD") path))))
             ("pr"
              (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" number))))
             ("notification"
              (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (cond
                                                                                                                                      ((equal (get-text-property 0 :type cand) "issue")
                                                                                                                                       (format "/issues/%s" number))
                                                                                                                                      ((equal (get-text-property 0 :type cand) "pr")
                                                                                                                                       (format "/pull/%s" number))
                                                                                                                                      (t
                                                                                                                                       (let* ((api-url (get-text-property 0 :api-url cand))
                                                                                                                                              (discussion
                                                                                                                                               (and (stringp api-url)
                                                                                                                                                    (cadr (member "discussions" (split-string api-url "/" t))))))

                                                                                                                                         (format "/discussions/%s" discussion)))))))
             ("dashboard"
              (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (cond
                                                                                                                                      ((equal (get-text-property 0 :type cand) "issue")
                                                                                                                                       (format "/issues/%s" number))
                                                                                                                                      ((equal (get-text-property 0 :type cand) "pr")
                                                                                                                                       (format "/pull/%s" number))))))
             ("commit"
              (and (stringp sha)
                   (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/commit/%s" sha)))))
             ("branch"
              (string-trim (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/tree/%s" ref))))

             ("release"
              (when-let ((tagname (get-text-property 0 :tagname cand)))
                  (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/releases/%s" tagname))))
             ("workflow"
              (and (stringp path)
                   (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser"))
                           (format "/actions/workflows/%s" (file-name-nondirectory path)))))
             ("run"
              (and id
                   (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/actions/runs/%s" id))))
             ("compare"
              (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/compare/%s" ref)))
             (_
              (string-trim (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser"))))))))))


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
  "Get the link of the user from CAND at point."
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
           (ref (or (get-text-property 0 :ref cand) nil)))
      (consult-gh-with-host
       (consult-gh--auth-account-host)
       (pcase class
         ("issue"
          (consult-gh--call-process "issue" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
         ("file"
          (funcall #'browse-url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) (format "/blob/%s/" (or ref "HEAD")) path)))
         ("pr"
          (consult-gh--call-process "pr" "view" "--web" "--repo" (substring-no-properties repo) (substring-no-properties number)))
         (_
          (consult-gh--call-process "repo" "view" "--web" (substring repo))))))))

(defun consult-gh-embark-open-in-default-browser (cand)
  "Open the url link of CAND with `consult-gh-browse-url-func'."
  (when (stringp cand)
    (let* ((url (consult-gh-embark-get-url cand)))
      (funcall (or consult-gh-browse-url-func #'browse-url) url))))

(defun consult-gh-embark-open-repo-in-system-browser (cand)
  "Open the url link for repo in CAND in the system's default browser."
  (when (stringp cand)
    (if-let* ((repo (get-text-property 0 :repo cand)))
        (consult-gh-with-host
         (consult-gh--auth-account-host)
         (consult-gh--call-process "repo" "view" "--web" (substring repo)))
      (message "No repo at point!"))))

(defun consult-gh-embark-open-repo-in-default-browser (cand)
  "Open the url link for repo in CAND in the system's default browser."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (if-let* ((repo (get-text-property 0 :repo cand))
               (url (string-trim (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))))
         (funcall (or consult-gh-browse-url-func #'browse-url) url)
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
        (funcall (or consult-gh-browse-url-func #'browse-url) url)
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

(defun consult-gh-embark-view-workflows-of-repo (cand)
  "Browse GitHub action workflows of CAND repo at point."
  (when (stringp cand)
    (let ((repo (or (get-text-property 0 :repo cand))))
      (consult-gh-workflow-list repo))))

(defun consult-gh-embark-view-runs-of-repo (cand)
  "Browse GitHub action run instances of CAND repo at point."
  (when (stringp cand)
    (let ((repo (or (get-text-property 0 :repo cand))))
      (consult-gh-run-list repo))))

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
  "Search pull requests involving the user in CAND."
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
    (let ((repo (or (get-text-property 0 :repo cand) (substring-no-properties cand))))
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
           (ref (or (get-text-property 0 :ref cand) nil))
           (str nil))

      (pcase class
        ((or "issue" "pr")
         (when (not current-prefix-arg) (setq title (concat repo "/" title))))
        ((or "file" "code")
         (when (not current-prefix-arg) (setq title (concat repo "/" ref "/" path))))
        ("commit"
         (when-let ((sha (get-text-property 0 :sha cand))
                    (sha-str (and (stringp sha)
                                  (substring sha 0 6))))
           (when (not current-prefix-arg) (setq title sha-str))))
        ("branch"
         (when ref (setq title ref))))

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
  "Copy ssh link of CAND to `kill-ring'."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand)))
       (kill-new (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" (string-trim repo) "--json" "sshUrl") :sshUrl))))))

(defun consult-gh-embark-copy-straight-usepackage-link-as-kill (cand)
  "Copy a `use-package' script for “straight.el” of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (package (car (last (split-string repo "\/")))))
      (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" repo "\")\n)")))))

(defun consult-gh-embark-copy-usepackage-link-as-kill (cand)
  "Copy a drop-in `use-package' script of CAND to `kill-ring'."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (package (car (last (split-string repo "\/")))))
      (kill-new (concat "(use-package " package)))))

(defun consult-gh-embark-copy-file-contents-as-kill (cand)
  "Copy the contents of CAND file to `kill-ring'."
  (when (stringp cand)
    (let* ((api-url (get-text-property 0 :api-url cand))
           (repo (get-text-property 0 :repo cand))
           (path (get-text-property 0 :path cand))
           (ref (get-text-property 0 :ref cand))
           (contents (if (and api-url (stringp api-url))
                         (consult-gh--files-get-content-by-api-url api-url)
                       (consult-gh--files-get-content-by-path repo path ref))))
      (when (and contents (stringp contents))
        (kill-new contents)))))

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
           (path (get-text-property 0 :path cand))
           (ref (get-text-property 0 :ref cand))
           (sha (get-text-property 0 :sha cand)))

      (pcase class
        ((or "issue" "pr")
         (when (not current-prefix-arg) (setq title (concat repo "/" title))))
        ((or "file" "code")
         (when (not current-prefix-arg) (setq title (concat repo "/" ref "/" path))))
        ("commit"
         (when (not current-prefix-arg) (setq title (concat repo "@" sha)))))

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
    (let* ((api-url (get-text-property 0 :api-url cand))
          (repo (get-text-property 0 :repo cand))
          (path (get-text-property 0 :path cand))
          (ref (get-text-property 0 :ref cand))
          (contents (if (and api-url (stringp api-url))
                        (consult-gh--files-get-content-by-api-url api-url)
                      (consult-gh--files-get-content-by-path repo path ref))))
      (when (and contents (stringp contents))
        (embark-insert contents)))))

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
  "Insert the org-format url link of CAND at point."
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

;;;; Save/Clone/Fork Actions

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
            (ref-link (pcase class
                   ((or "pr" "notification" "dashboard" "code" "file")
                    (consult-gh-embark-get-url cand))
                   (_ nil)))
            (body (when ref-link (not (string-empty-p ref-link)) (format "%s" ref-link))))
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
            (ref-link (pcase class
                   ((or "issue" "notification" "dashboard" "code" "file")
                    (consult-gh-embark-get-url cand))
                   (_ nil)))
            (body (when ref-link (not (string-empty-p ref-link)) (format "%s" ref-link))))
       (funcall #'consult-gh-pr-create repo title body)))))

(defun consult-gh-embark-create-release (cand)
  "Create a release in repo of CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand)))
       (funcall #'consult-gh-release-create repo)))))

;;;; Edit Repo Actions
(defun consult-gh-embark-delete-repo (cand)
  "Delete the repo in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-repo-delete cand))))

(defun consult-gh-embark-rename-repo (cand)
  "Rename the repo in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-repo-rename cand))))

(defun consult-gh-embark-edit-repo-settings (cand)
  "Edit the settings of repo in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-repo-edit-settings cand))))

(defun consult-gh-embark-edit-repo-readme (cand)
  "Edit the README of repo in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-repo-edit-readme cand))))

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
                          (consult-gh--json-to-hashtable  (consult-gh--api-get-command-string (format "/repos/%s/issues/%s" repo number)) :locked))))
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
  "Edit the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-issue-edit cand))))

(defun consult-gh-embark-comment-on-issue (cand)
  "Comment on the issue in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let ((repo (get-text-property 0 :repo cand))
           (number (get-text-property 0 :number cand)))
       (with-current-buffer (funcall #'consult-gh--issue-view repo number)
         (consult-gh-topics-comment-create))))))

;;;; Edit File Actions
(defun consult-gh-embark-edit-file (cand)
  "Edit the file in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-edit-file cand))))

(defun consult-gh-embark-delete-file (cand)
  "Delete the file in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (funcall #'consult-gh-delete-file cand))))

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
  "Comment on the pull request in CAND."
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
                          (consult-gh--json-to-hashtable (consult-gh--api-get-command-string (format "/repos/%s/pulls/%s" repo number)) :locked))))
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
  "Subscribe/Unsubscribe to the notification in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((thread (get-text-property 0 :thread cand))
            (subscription (consult-gh--json-to-hashtable (consult-gh--api-get-command-string (format "/notifications/threads/%s/subscription" thread)) :subscribed)))
       (if (eq subscription 't)
           (consult-gh--notifications-unsubscribe cand)
         (consult-gh--notifications-subscribe cand))))))

;;;; Edit Release Actions

(defun consult-gh-embark-mark-release-draft (cand)
  "Mark/Unmark the release in CAND as draft."
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

(defun consult-gh-embark-workflow-view (cand)
  "View workflow in CAND."
 (when (stringp cand)
    (consult-gh--workflow-view-action cand)))

(defun consult-gh-embark-workflow-runs-list (cand)
  "Browse run instances of workflow in CAND."
 (when (stringp cand)
    (let ((repo (get-text-property 0 :repo cand))
          (id (get-text-property 0 :id cand)))
(consult-gh-run-list repo nil nil nil id))))

(defun consult-gh-embark-workflow-run (cand)
  "Run the workflow in CAND."
 (when (stringp cand)
(consult-gh-workflow-run cand)))

(defun consult-gh-embark-workflow-enable (cand)
  "Enable the workflow in CAND."
 (when (stringp cand)
(consult-gh-workflow-enable cand)))

(defun consult-gh-embark-workflow-disable (cand)
  "Disable the workflow in CAND."
 (when (stringp cand)
(consult-gh-workflow-disable cand)))

(defun consult-gh-embark-workflow-edit-yaml (cand)
  "Edit the YAML file of the workflow in CAND."
 (when (stringp cand)
(consult-gh-workflow-edit-yaml cand)))

(defun consult-gh-embark-run-view-workflow (cand)
  "View the workflow for run in CAND."
  (when (stringp cand)
    (let* ((repo (get-text-property 0 :repo cand))
           (workflow-id (get-text-property 0 :workflow-id cand))
           (newcand (propertize (format "repo/actions/%s" workflow-id) :repo repo :id workflow-id)))
      (consult-gh--workflow-view-action newcand))))

(defun consult-gh-embark-run-view (cand)
  "View the run in CAND."
  (when (stringp cand)
    (consult-gh--run-view-action cand)))

(defun consult-gh-embark-delete-branch (cand)
  "Delete the branch in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand))
            (branch (get-text-property 0 :ref cand)))
     (funcall #'consult-gh-branch-delete repo branch)))))

(defun consult-gh-embark-commit-browse-files (cand)
  "Delete the branch in CAND."
  (when (stringp cand)
    (consult-gh-with-host
     (consult-gh--auth-account-host)
     (let* ((repo (get-text-property 0 :repo cand))
            (sha (get-text-property 0 :sha cand)))
     (consult-gh-commit-browse-files repo sha)))))

;;;; Other Actions

(defun consult-gh-embark-email-user (cand)
  "Email the user in CAND at point.

uses `compose-mail' for composing an email."
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
  "r" '("gh add repo to known repos" . consult-gh-embark-add-repo-to-known-repos)
  "k" '("gh remove repo from known repos" . consult-gh-embark-remove-repo-from-known-repos)
  "w" '("gh add repo to workflow template repos" . consult-gh-embark-add-repo-to-workflow-template-list)
  "W" '("gh remove repo from workflow template repos" . consult-gh-embark-remove-repo-from-workflow-template-list))

(fset 'consult-gh-embark-bookmark-repos-menu-map consult-gh-embark-bookmark-repos-menu-map)

(defvar-keymap consult-gh-embark-bookmark-orgs-menu-map
  :doc "Keymap for bookmarking orgs menu"
  :parent nil
  "o" '("gh add org to known orgs" . consult-gh-embark-add-org-to-known-orgs)
  "k" '("gh remove org from known orgs" . consult-gh-embark-remove-org-from-known-orgs)
  "d" '("gh add org to favorite orgs" . consult-gh-embark-add-org-to-favorite-list)
  "D" '("gh remove org from favorite orgs" . consult-gh-embark-remove-org-from-favorite-list))

(fset 'consult-gh-embark-bookmark-orgs-menu-map consult-gh-embark-bookmark-orgs-menu-map)

(defvar-keymap consult-gh-embark-bookmarks-menu-map
  :doc "Keymap for bookmarks menu"
  :parent nil
  "r" '("gh bookmark repos" . consult-gh-embark-bookmark-repos-menu-map)
  "o" '("gh bookmark orgs" . consult-gh-embark-bookmark-orgs-menu-map))

(fset 'consult-gh-embark-bookmarks-menu-map consult-gh-embark-bookmarks-menu-map)

;;;;; Copy Menu Keymap
;;;;;; Copy  User's Info Keymap
(defvar-keymap consult-gh-embark-user-copy-menu-map
  :doc "Keymap for copying user info as kill menu"
  :parent nil
  "w" '("gh copy username" . consult-gh-embark-copy-user-as-kill)
  "n" '("gh copy user full name" . consult-gh-embark-copy-user-name-as-kill)
  "e" '("gh copy user email" . consult-gh-embark-copy-user-email-as-kill)
  "l" '("gh copy user page" . consult-gh-embark-copy-user-link-as-kill))

(fset 'consult-gh-embark-user-copy-menu-map consult-gh-embark-user-copy-menu-map)
(defvar-keymap consult-gh-embark-copy-menu-map
  :doc "Keymap for copy-as-kill menu"
  :parent nil
  "t" '("gh copy title" . consult-gh-embark-copy-title-as-kill)
  "u" '("gh copy url link" . consult-gh-embark-copy-url-as-kill)
  "h" '("gh copy https link" . consult-gh-embark-copy-https-link-as-kill)
  "s" '("gh copy ssh link" . consult-gh-embark-copy-ssh-link-as-kill)
  "o" '("gh copy org-mode link" . consult-gh-embark-copy-org-link-as-kill)
  "U" '("gh copy user's info" . consult-gh-embark-user-copy-menu-map))

(fset 'consult-gh-embark-copy-menu-map consult-gh-embark-copy-menu-map)

;;;;; Create Menu Keymap
(defvar-keymap consult-gh-embark-create-menu-map
  :doc "Keymap for create menu"
  :parent nil
  "i" '("gh create an issue in repo" . consult-gh-embark-create-issue)
  "p" '("gh create a pull request in repo" . consult-gh-embark-create-pr)
  "r" '("gh create new repo" . consult-gh-embark-create-repo)
  "R" '("gh create a release in repo" . consult-gh-embark-create-release)
  "c" '("gh create a clone of repo" . consult-gh-embark-clone-repo)
  "f" '("gh create a fork of repo" . consult-gh-embark-fork-repo))

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
  "f" '("gh find file in repo" . consult-gh-embark-view-files-of-repo)
  "r" '("gh find repos by user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("gh find issue in repo" . consult-gh-embark-view-issues-of-repo)
  "p" '("gh find pr in repo" . consult-gh-embark-view-prs-of-repo)
  "c" '("gh find code in repo" . consult-gh-embark-search-code-in-repo)
  "a" '("gh find action workflows in repo" . consult-gh-embark-view-workflows-of-repo)
  "g" '("gh find action runs in repo" . consult-gh-embark-view-runs-of-repo))

(fset 'consult-gh-embark-find-menu-map consult-gh-embark-find-menu-map)

;;;;; Insert Menu Keymap
;;;;;; Insert  User's Info Keymap
(defvar-keymap consult-gh-embark-user-insert-menu-map
  :doc "Keymap for inserting user info menu"
  :parent nil
  "u" '("gh insert username" .  consult-gh-embark-insert-user)
  "n" '("gh insert user's full name" . consult-gh-embark-insert-user-name)
  "e" '("gh insert user's email" . consult-gh-embark-insert-user-email)
  "l" '("gh insert user's page url" . consult-gh-embark-insert-user-link))

(fset 'consult-gh-embark-user-insert-menu-map consult-gh-embark-user-insert-menu-map)

(defvar-keymap consult-gh-embark-insert-menu-map
  :doc "Keymap for insert menu"
  :parent nil
  "t" '("gh insert title" . consult-gh-embark-insert-title)
  "u" '("gh insert url" . consult-gh-embark-insert-url)
  "l" '("gh insert link" .  consult-gh-embark-insert-link)
  "U" '("gh insert user info" . consult-gh-embark-user-insert-menu-map))

(fset 'consult-gh-embark-insert-menu-map consult-gh-embark-insert-menu-map)

;;;;; Links Menu Keymap
(defvar-keymap consult-gh-embark-links-menu-map
  :doc "Keymap for links menu"
  :parent nil
  "h" '("gh copy https url" . consult-gh-embark-copy-https-link-as-kill)
  "s" '("gh copy ssh url" . consult-gh-embark-copy-ssh-link-as-kill)
  "l" '("gh copy url" . consult-gh-embark-copy-url-as-kill)
  "o" '("gh copy org-mode link" . consult-gh-embark-copy-org-link-as-kill)
  "u" '("gh copy straight use-package link" . consult-gh-embark-copy-straight-usepackage-link-as-kill)
  "U" '("gh copy user page link" . consult-gh-embark-copy-user-link-as-kill))

(fset 'consult-gh-embark-links-menu-map consult-gh-embark-links-menu-map)

;;;;; Open Menu Keymap
(defvar-keymap consult-gh-embark-open-menu-map
  :doc "Keymap for open menu"
  :parent nil
  "O" '("gh open in system browser" . consult-gh-embark-open-in-system-browser)
  "o" '("gh open in consult-gh's default browser" . consult-gh-embark-open-in-default-browser)
  "p" '("gh open preview buffer" . consult-gh-embark-show-preview)
  "RET" '("gh open in emacs" . consult-gh-embark-default-action))

(fset 'consult-gh-embark-open-menu-map consult-gh-embark-open-menu-map)

;;;;; Repo Menu Keymap
(defvar-keymap consult-gh-embark-repo-menu-map
  :doc "Keymap for repo actions menu"
  :parent nil
  "c" '("gh clone repo" . consult-gh-embark-clone-repo)
  "f" '("gh fork repo" . consult-gh-embark-fork-repo)
  "r" '("gh list repos of user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("gh list issues of repo" . consult-gh-embark-view-issues-of-repo)
  "p" '("gh list prs of repo" . consult-gh-embark-view-prs-of-repo)
  "a" '("gh list action workflows of repo" . consult-gh-embark-view-workflows-of-repo)
  "g" '("gh list action runs of repo" . consult-gh-embark-view-runs-of-repo)
  "s" '("gh search for code in repo" . consult-gh-embark-search-code-in-repo)
  "b" '("gh browse files of repo" . consult-gh-embark-view-files-of-repo)
  "o" '("gh open repo page in consult-gh's browser" .  consult-gh-embark-open-repo-in-default-browser)
  "O" '("gh open repo page in system browser" .  consult-gh-embark-open-repo-in-system-browser))


(fset 'consult-gh-embark-repo-menu-map consult-gh-embark-repo-menu-map)

;;;;; User Menu Keymap
(defvar-keymap consult-gh-embark-user-menu-map
  :doc "Keymap for user actions menu"
  :parent nil
  "e" '("gh email user" . consult-gh-embark-email-user)
  "r" '("gh list repos of user" . consult-gh-embark-get-other-repos-by-same-user)
  "i" '("gh list issues involving user" . consult-gh-embark-view-issues-involves-user)
  "p" '("gh list prs involving user" . consult-gh-embark-view-prs-involves-user)
  "a" '("gh list user assignment" . consult-gh-embark-view-user-assignment)
  "u" '("gh insert user info" . consult-gh-embark-user-insert-menu-map)
  "w" '("gh copy user info as kill" . consult-gh-embark-user-copy-menu-map)
  "o" '("gh open user page in consult-gh's default browser" .  consult-gh-embark-user-open-in-default-browser)
  "O" '("gh open user page in system browser" .  consult-gh-embark-user-open-in-system-browser))

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
  "b" 'consult-gh-embark-bookmarks-menu-map
  "c" 'consult-gh-embark-create-menu-map
  "f" 'consult-gh-embark-find-menu-map
  "i" 'consult-gh-embark-insert-menu-map
  "l" 'consult-gh-embark-links-menu-map
  "o"  'consult-gh-embark-open-menu-map
  "r" 'consult-gh-embark-repo-menu-map
  "u" 'consult-gh-embark-user-menu-map
  "w" 'consult-gh-embark-copy-menu-map
  "v" 'consult-gh-embark-view-menu-map)

;;;; Org Keymap
(defvar-keymap consult-gh-embark-orgs-actions-map
  :doc "Keymap for consult-gh-embark-orgs"
  :parent consult-gh-embark-general-actions-map)

;;;; Repo Keymap
;;;;;; Repo Edit Menu Keymap
(defvar-keymap consult-gh-embark-repo-edit-menu-map
  :doc "Keymap for editing repos"
  :parent consult-gh-embark-edit-menu-map
  "D" '("gh delete repo" . consult-gh-embark-delete-repo)
  "R" '("gh rename repo" . consult-gh-embark-rename-repo)
  "e" '("gh edit repo's settings" . consult-gh-embark-edit-repo-settings)
  "r" '("gh edit repo's README" . consult-gh-embark-edit-repo-readme))

(fset 'consult-gh-embark-repo-edit-menu-map consult-gh-embark-repo-edit-menu-map)

;;;;;; Repo View Menu Keymap
(defvar-keymap consult-gh-embark-repo-view-menu-map
  :doc "Keymap for viewing Repo details"
  :parent nil
  "r" '("gh view readme" . consult-gh-embark-view-readme-of-repo)
  "i" '("gh view issues" . consult-gh-embark-view-issues-of-repo)
  "p" '("gh view pull requests" . consult-gh-embark-view-prs-of-repo)
  "a" '("gh view action workflows" . consult-gh-embark-view-workflows-of-repo)
  "g" '("gh view action runs" . consult-gh-embark-view-runs-of-repo))

(fset 'consult-gh-embark-repo-view-menu-map consult-gh-embark-repo-view-menu-map)

;;;;;; Repo Main Menu Keymap
(defvar-keymap consult-gh-embark-repos-actions-map
  :doc "Keymap for consult-gh-embark-repos"
  :parent consult-gh-embark-general-actions-map
  "e" 'consult-gh-embark-repo-edit-menu-map
  "v" 'consult-gh-embark-repo-view-menu-map)

;;;;; Files Keymap
;;;;;; Files Insert Menu Keymap
(defvar-keymap consult-gh-embark-files-insert-menu-map
  :doc "Keymap for inserting files"
  :parent consult-gh-embark-user-insert-menu-map
  "f" '("gh file content" . consult-gh-embark-insert-file-contents))

(fset 'consult-gh-embark-files-insert-menu-map consult-gh-embark-files-insert-menu-map)

;;;;;; Files Edit Keymap
(defvar-keymap consult-gh-embark-files-edit-menu-map
  :doc "Keymap for editing files"
  :parent consult-gh-embark-edit-menu-map
  "e" '("gh edit file" . consult-gh-embark-edit-file)
  "D" '("gh delete file" . consult-gh-embark-delete-file))

(fset 'consult-gh-embark-files-edit-menu-map consult-gh-embark-files-edit-menu-map)

;;;;;; Files Main Menu Keymap
(defvar-keymap consult-gh-embark-files-actions-map
  :doc "Keymap for consult-gh-embark-files"
  :parent consult-gh-embark-general-actions-map
  "e" 'consult-gh-embark-files-edit-menu-map
  "i" 'consult-gh-embark-files-insert-menu-map
  "s" '("gh save file" . consult-gh-embark-save-file))

;;;;; Issue Keymap
;;;;;; Edit Issue Menu Keymap
(defvar-keymap consult-gh-embark-issues-edit-menu-map
  :doc "Keymap for editing issues"
  :parent consult-gh-embark-edit-menu-map
  "D" '("gh delete issue" . consult-gh-embark-delete-issue)
  "e" '("gh edit issue" . consult-gh-embark-edit-issue)
  "d" '("gh develop issue" . consult-gh-embark-develop-issue)
  "l" '("gh lock/ulock issue" . consult-gh-embark-toggle-issue-lock)
  "o" '("gh open/close issue" . consult-gh-embark-toggle-issue-open)
  "p" '("gh pin/unpin issue" . consult-gh-embark-toggle-issue-pin)
  "t" '("gh transfer issue" . consult-gh-embark-transfer-issue)
  "L" '("gh link to pr" . consult-gh-embark-link-pr-to-issue))

(fset 'consult-gh-embark-issues-edit-menu-map consult-gh-embark-issues-edit-menu-map)

;;;;;; Issue Main Menu Keymap
(defvar-keymap consult-gh-embark-issues-actions-map
  :doc "Keymap for consult-gh-embark-issues"
  :parent consult-gh-embark-general-actions-map
  "c" 'consult-gh-embark-create-menu-map
  "c C-c" '("gh comment on issue" . consult-gh-embark-comment-on-issue)
  "e" ' consult-gh-embark-issues-edit-menu-map)

;;;;; Pull Request Keymap
;;;;;; Edit PRs Menu Keymap
(defvar-keymap consult-gh-embark-prs-edit-menu-map
  :doc "Keymap for editing PRs"
  :parent consult-gh-embark-edit-menu-map
  "e" '("gh edit pr" . consult-gh-embark-edit-pr)
  "d" '("gh draft/undraft pr" . consult-gh-embark-toggle-pr-draft)
  "l" '("gh lock/unlock pr" . consult-gh-embark-toggle-pr-lock)
  "m" '("gh merge pr" . consult-gh-embark-merge-pr)
  "o" '("gh open/close pr" . consult-gh-embark-toggle-pr-open)
  "L" '("gh link to issue" . consult-gh-embark-link-pr-to-issue))

(fset 'consult-gh-embark-prs-edit-menu-map consult-gh-embark-prs-edit-menu-map)

;;;;;; PR View Menu Keymap
(defvar-keymap consult-gh-embark-prs-view-menu-map
  :doc "Keymap for viewing PR details"
  :parent nil
  "d" '("gh view diff" . consult-gh-embark-view-pr-diff)
  "r" 'consult-gh-embark-repo-view-menu-map)

(fset 'consult-gh-embark-prs-view-menu-map consult-gh-embark-prs-view-menu-map)

;;;;; PR Main Menu Keymap
(defvar-keymap consult-gh-embark-prs-actions-map
  :doc "Keymap for consult-gh-embark-prs"
  :parent consult-gh-embark-general-actions-map
  "c" 'consult-gh-embark-create-menu-map
  "c C-c" '("gh comment on pr" . consult-gh-embark-comment-on-pr)
  "c C-r" '("gh review pr" . consult-gh-embark-review-pr)
  "e" 'consult-gh-embark-prs-edit-menu-map
  "v" 'consult-gh-embark-prs-view-menu-map)

;;;;; Release Keymap
;;;;;; Edit Release Menu Keymap
(defvar-keymap consult-gh-embark-releases-edit-menu-map
  :doc "Keymap for editing releases"
  :parent nil
  "D" '("gh delete release" . consult-gh-embark-delete-release)
  "e" '("gh edit release" . consult-gh-embark-edit-release)
  "d" '("gh mark release as draft" . consult-gh-embark-mark-release-draft)
  "P" '("gh publish release" . consult-gh-embark-publish-release)
  "l" '("gh mark release as latest" . consult-gh-embark-mark-release-latest)
  "p" '("gh mark/unmark prerelease" . consult-gh-embark-toggle-release-prerelease))

(fset 'consult-gh-embark-releases-edit-menu-map consult-gh-embark-releases-edit-menu-map)

;;;;;; Release Main Menu Keymap
(defvar-keymap consult-gh-embark-releases-actions-map
  :doc "Keymap for consult-gh-embark-releases"
  :parent consult-gh-embark-general-actions-map
  "c" '("gh create release" . consult-gh-embark-create-menu-map)
  "d" '("gh download release" . consult-gh-embark-download-release)
  "e" 'consult-gh-embark-releases-edit-menu-map)

;;;;;; PR View Menu Keymap
(defvar-keymap consult-gh-embark-workflows-view-menu-map
  :doc "Keymap for viewing workflow details"
  :parent nil
  "r" 'consult-gh-embark-repo-view-menu-map
  "g" '("gh view runs" . consult-gh-embark-workflow-runs-list)
  "v" '("gh view workflow" . consult-gh-embark-workflow-view))

(fset 'consult-gh-embark-workflows-view-menu-map consult-gh-embark-workflows-view-menu-map)

;;;;;; Workflow Main Menu Keymap
(defvar-keymap consult-gh-embark-workflows-actions-map
  :doc "Keymap for consult-gh-embark-workflows"
  :parent consult-gh-embark-general-actions-map
  "r" '("gh run workflow" . consult-gh-embark-workflow-run)
  "e" '("gh enable workflow" . consult-gh-embark-workflow-enable)
  "d" '("gh disable workflow" . consult-gh-embark-workflow-disable)
  "g" '("gh list workflow runs" . consult-gh-embark-workflow-runs-list)
  "v" 'consult-gh-embark-workflows-view-menu-map
  "y" '("gh edit workflow yaml file" .  consult-gh-embark-workflow-edit-yaml))

;;;;;; PR View Menu Keymap
(defvar-keymap consult-gh-embark-runs-view-menu-map
  :doc "Keymap for viewing run details"
  :parent nil
  "r" 'consult-gh-embark-repo-view-menu-map
  "a" '("gh view run's workflow" . consult-gh-embark-run-view-workflow)
  "v" '("gh view run" . consult-gh-embark-run-view))

(fset 'consult-gh-embark-runs-view-menu-map consult-gh-embark-runs-view-menu-map)

;;;;;; Workflow Main Menu Keymap
(defvar-keymap consult-gh-embark-runs-actions-map
  :doc "Keymap for consult-gh-embark-runs"
  :parent consult-gh-embark-general-actions-map
  "a" '("gh view run's workflow" . consult-gh-embark-run-view-workflow)
  "v" 'consult-gh-embark-runs-view-menu-map)

;;;; Code Keymap
(defvar-keymap consult-gh-embark-codes-actions-map
  :doc "Keymap for consult-gh-embark-codes"
  :parent consult-gh-embark-general-actions-map)

;;;; Notifications Keymap
;;;;; Edit Notifications Menu Keymap
(defvar-keymap consult-gh-embark-notifications-edit-menu-map
  :doc "Keymap for editing notifications"
  :parent nil
  "r" '("gh mark notification read/unread" . consult-gh-embark-toggle-notification-read)
  "s" '("gh unsubscribe/resubscribe notification" . consult-gh-embark-notification-toggle-subscription))

(fset 'consult-gh-embark-notifications-edit-menu-map consult-gh-embark-notifications-edit-menu-map)

;;;;; Notifications Main Menu Keymap
(defvar-keymap consult-gh-embark-notifications-actions-map
  :doc "Keymap for consult-gh-embark-notifications"
  :parent consult-gh-embark-general-actions-map
  "e" 'consult-gh-embark-notifications-edit-menu-map)

;;;;; Dashboard Main Menu Keymap
(defvar-keymap consult-gh-embark-dashboard-actions-map
  :doc "Keymap for consult-gh-embark-dashboard"
  :parent consult-gh-embark-general-actions-map)

;;;;; Branches Main Menu Keymap
(defvar-keymap consult-gh-embark-branches-actions-map
  :doc "Keymap for consult-gh-embark-branches"
  :parent consult-gh-embark-general-actions-map
  "D" '("gh delete branch" . consult-gh-embark-delete-branch))

;;;;; Branches Main Menu Keymap
(defvar-keymap consult-gh-embark-commits-actions-map
  :doc "Keymap for consult-gh-embark-commits"
  :parent consult-gh-embark-general-actions-map
  "v f" '("gh view files @ commit" . consult-gh-embark-commit-browse-files))

;;; Replace key description in which-key

(setq which-key-replacement-alist (append
                                   (list
             (cons (cons nil "consult-gh-embark-bookmarks-menu-map") (cons nil "gh bookmarks menu"))
             (cons (cons nil "consult-gh-embark-create-menu-map") (cons nil "gh create menu"))
             (cons (cons nil "consult-gh-embark-find-menu-map") (cons nil "gh find menu"))
             (cons (cons nil "consult-gh-embark-insert-menu-map") (cons nil "gh insert menu"))
             (cons (cons nil "consult-gh-embark-links-menu-map") (cons nil "gh links menu"))
             (cons (cons nil "consult-gh-embark-open-menu-map") (cons nil "gh open menu"))
             (cons (cons nil "consult-gh-embark-repo-menu-map") (cons nil "gh repo menu"))
             (cons (cons nil "consult-gh-embark-user-menu-map") (cons nil "gh user menu"))
             (cons (cons nil "consult-gh-embark-copy-menu-map") (cons nil "gh copy as kill menu"))
             (cons (cons nil "consult-gh-embark-view-menu-map") (cons nil "gh view menu"))
             (cons (cons nil "consult-gh-embark-user-copy-menu-map") (cons nil "gh copy user info menu"))
(cons (cons nil "consult-gh-embark-user-insert-menu-map") (cons nil "gh insert user info menu"))
             (cons (cons nil "consult-gh-embark-repo-edit-menu-map") (cons nil "gh edit repo menu"))
             (cons (cons nil "consult-gh-embark-repo-view-menu-map") (cons nil "gh view repo menu"))
             (cons (cons nil "consult-gh-embark-files-edit-menu-map") (cons nil "gh edit file menu"))
             (cons (cons nil "consult-gh-embark-files-insert-menu-map") (cons nil "gh insert file menu"))
             (cons (cons nil "consult-gh-embark-issues-edit-menu-map") (cons nil "gh edit issue menu"))
             (cons (cons nil "consult-gh-embark-prs-edit-menu-map") (cons nil "gh edit pr menu"))
             (cons (cons nil "consult-gh-embark-prs-view-menu-map") (cons nil "gh view pr menu"))
             (cons (cons nil "consult-gh-embark-releases-edit-menu-map") (cons nil "gh edit release menu"))
             (cons (cons nil "consult-gh-embark-workflows-view-menu-map") (cons nil "gh view workflow menu"))
             (cons (cons nil "consult-gh-embark-runs-view-menu-map") (cons nil "gh view run menu"))
             (cons (cons nil "consult-gh-embark-notifications-edit-menu-map") (cons nil "gh view notification menu")))
                                   which-key-replacement-alist))

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
                  (consult-gh-releases . consult-gh-embark-releases-actions-map)
                  (consult-gh-workflows . consult-gh-embark-workflows-actions-map)
                  (consult-gh-runs . consult-gh-embark-runs-actions-map)
                  (consult-gh-branches . consult-gh-embark-branches-actions-map)
                  (consult-gh-commits . consult-gh-embark-commits-actions-map))))


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
                  (consult-gh-releases . consult-gh-embark-default-action)
                  (consult-gh-workflows . consult-gh-embark-default-action)
                  (consult-gh-runs . consult-gh-embark-default-action)
                  (consult-gh-branches . consult-gh-embark-default-action)
                  (consult-gh-commits . consult-gh-embark-default-action))))


  ;; set post actions-hook
  (setq embark-post-action-hooks
        (append embark-post-action-hooks
                '((consult-gh-embark-delete-issue consult-gh--embark-restart)
                  (consult-gh-embark-toggle-issue-open consult-gh--embark-restart)
                  (consult-gh-embark-transfer-issue consult-gh--embark-restart)
                  (consult-gh-embark-toggle-pr-draft consult-gh--embark-restart)
                  (consult-gh-embark-merge-pr consult-gh--embark-restart)
                  (consult-gh-embark-toggle-pr-open consult-gh--embark-restart)
                  (consult-gh-embark-toggle-notification-read consult-gh--embark-restart)
                  (consult-gh-embark-notification-toggle-subscription consult-gh--embark-restart)
                  (consult-gh-embark-mark-release-draft consult-gh--embark-restart)
                  (consult-gh-embark-toggle-release-prerelease consult-gh--embark-restart)
                  (consult-gh-embark-publish-release consult-gh--embark-restart)
                  (consult-gh-embark-mark-release-latest consult-gh--embark-restart)
                  (consult-gh-embark-workflow-enable consult-gh--embark-restart)
                  (consult-gh-embark-workflow-disable consult-gh--embark-restart)))))


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
                           (consult-gh-releases . consult-gh-embark-releases-actions-map)
                           (consult-gh-workflows . consult-gh-embark-workflows-actions-map)
                           (consult-gh-runs . consult-gh-embark-runs-actions-map)
                           (consult-gh-branches . consult-gh-embark-branches-actions-map)
                           (consult-gh-commits . consult-gh-embark-commits-actions-map))))

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
                          (consult-gh-releases . consult-gh-embark-default-action)
                          (consult-gh-workflows . consult-gh-embark-default-action)
                          (consult-gh-runs . consult-gh-embark-default-action)
                          (consult-gh-branches . consult-gh-embark-default-action)
                          (consult-gh-commits . consult-gh-embark-default-action))))

  ;; unset post action hooks
  (setq embark-post-action-hooks
        (seq-difference embark-post-action-hooks
                        '((consult-gh-embark-delete-issue consult-gh--embark-restart)
                          (consult-gh-embark-toggle-issue-open consult-gh--embark-restart)
                          (consult-gh-embark-transfer-issue consult-gh--embark-restart)
                          (consult-gh-embark-toggle-pr-draft consult-gh--embark-restart)
                          (consult-gh-embark-merge-pr consult-gh--embark-restart)
                          (consult-gh-embark-toggle-pr-open consult-gh--embark-restart)
                          (consult-gh-embark-toggle-notification-read consult-gh--embark-restart)
                          (consult-gh-embark-notification-toggle-subscription consult-gh--embark-restart)
                          (consult-gh-embark-mark-release-draft consult-gh--embark-restart)
                          (consult-gh-embark-toggle-release-prerelease consult-gh--embark-restart)
                          (consult-gh-embark-publish-release consult-gh--embark-restart)
                          (consult-gh-embark-mark-release-latest consult-gh--embark-restart)
                          (consult-gh-embark-workflow-enable consult-gh--embark-restart)
                          (consult-gh-embark-workflow-disable consult-gh--embark-restart)))))

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

;;; Provide `consult-gh-embark' module

(provide 'consult-gh-embark)

;;; consult-gh-embark.el ends here
