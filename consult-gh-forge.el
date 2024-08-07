;;; consult-gh-forge.el --- Magit/Forge Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.1
;; Package-Requires: ((emacs "29.1") (consult "1.0") (forge "0.3.3") (consult-gh "1.1"))
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
;; This package provides forge integration for consult-gh.
;; (see URL `https://github.com/armindarvish/consult-gh' for more info).

;;; Code:

;;; Requirements

(if (featurep 'forge)
    (require 'forge)
  (message "magit/forge is not loaded. Make sure both magit and forge are installed and loaded before loading consult-gh-forge."))
(require 'consult-gh)

;;; Customization Variables

(defcustom consult-gh-forge-timeout-seconds 10
  "How long in seconds to wait for forge-visit to load the issue?

If the topic \(i.e. issue or PR\) cannot be loaded by `forge-visit-topic' within
this time, `consult-gh--issue-view-action' or `consult-gh--pr-view-action'
is used to load the topic instead."
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-forge-confirm-account t
  "Ask for confirmation when account doesn't match git config?

Query the user to pick an account when the account from gh cli command
and git config do not match."
  :group 'consult-gh
  :type 'boolean)

;;; Other Variables

(defvar consult-gh-forge--added-repositories (list)
  "List of repositories added to forge's database.

Uses `consult-gh-forge--add-topic' to add topics.")

;;; Define Backend Functions for `consult-gh-forge'

(defun consult-gh-forge--make-tempdir (repo)
  "Make a temporary directory for REPO in `consult-gh-tempdir'.

This is to ensure magit and forge can operate as intended."
  (let* ((tempdir (expand-file-name (concat repo "/" "forge/") consult-gh-tempdir))
         (gitdir (expand-file-name ".git/" tempdir))
         (_ (make-directory (file-name-directory gitdir) t)))
    tempdir))

(defun consult-gh-forge--add-repository (url)
  "Add the repo at URL to the forge database.

Note that this only pull individual topics when the user
invokes `forge-pull-topic'.  See forge documentation
for `forge-add-repository'."
  (if (forge-get-repository url nil :tracked?)
      nil
    (when (forge-get-repository url nil :valid?)
      (let ((repo (forge-get-repository url nil :insert!)))
        (oset repo selective-p t)
        (forge--pull repo nil))
      "created")))

(defun consult-gh-forge--remove-repository (host owner name)
  "Remove the forge defined by OWNER/HOST/NAME.

Removes the forge from the list in variable `forge-database'."
  (closql-delete (forge-get-repository (list host owner name) nil :known?)))

(defun consult-gh-forge--remove-repository-by-url (url)
  "Remove the forge defined by URL.

Removes the forge from the list in variable `forge-database'."
  (let* ((forge-repo (forge-get-repository url :known))
         (owner (oref forge-repo owner))
         (name (oref forge-repo name))
         (host (oref forge-repo githost)))
    (closql-delete (forge-get-repository (list host owner name) :known?))
    (setq consult-gh-forge--added-repositories (delete url consult-gh-forge--added-repositories))))

(defun consult-gh-forge-remove-added-repositories (&optional urls)
  "Remove all the forge repos added by `consult-gh-forge--add-repository'.

If optional argument URLS is non-nil, remove forges of URLS.
The repos are stored in `conuslt-gh-forge--aded-repositories'."
  (interactive)
  (let ((urls (or urls consult-gh-forge--added-repositories)))
    (mapcar #'consult-gh-forge--remove-repository-by-url urls)))

(defun consult-gh-forge-remove-repository (&optional urls)
  "Ask the user to select forges to be removed.

Lists forges added by `consult-gh'
\(stored in `consult-gh-forge--added-repositories'\) and
removes them from the list in variable `forge-database'.

If optional argument URLS is non-nil, remove the forges of the URLS."
  (interactive)
  (let* ((list (mapcar (lambda (url) (let* ((url-parse (forge--split-forge-url url))
                                            (repo (string-join (cdr url-parse) "/"))
                                            (host (car url-parse)))
                                       (format "%s @%s" repo host)))
                       consult-gh-forge--added-repositories))
         (urls (or urls (completing-read-multiple "Remove Repository from forge db: " list))))
    (message "%s" urls)
    (mapcar (lambda (url-parts) (let* ((parts (string-split url-parts " "))
                                       (host (string-trim (car (cdr parts)) "@"))
                                       (owner (car (string-split (car parts) "/")))
                                       (name (cadr (string-split (car parts) "/")))
                                       (url (string-trim (consult-gh--command-to-string "browse" "--repo" (format "%s/%s" owner name) "--no-browser"))))
                                  (consult-gh-forge--remove-repository host owner name)
                                  (setq consult-gh-forge--added-repositories
                                        (delete url consult-gh-forge--added-repositories))))
            urls)))

(defun consult-gh-forge--pull-topic (url topic)
  "Pull the TOPIC from repository at URL using `forge-pull-topic'.

See forge documentation for `forge-pull-topic'."
  (let ((repo (forge-get-repository url :known?)))
    (forge--pull-topic repo topic)))

(defun consult-gh-forge--add-topic (url topic)
  "Add the TOPIC of URL to the forge database.

See forge documentation for `forge-add-repository'."
  (cl-letf (((symbol-function #'magit-toplevel)
             (lambda () (consult-gh--make-tempdir (string-join (cdr (forge--split-forge-url url)) "/")))))
    (let ((created (consult-gh-forge--add-repository url))
          (repo (forge-get-repository url)))
      (while (not repo)
        (sit-for 0.0001)
        (setq repo (forge-get-repository url)))
      (consult-gh-forge--pull-topic url topic)
      created)))

(defun consult-gh-forge--magit-setup-buffer-internal (mode locked bindings)
  "Reimplement `magit-setup-buffer-intenral'.

This is to avoid making changes to magit settings for the repository
in the current working directory.

MODE, LOCKED, and BINDINGS are as defined in `magit-setup-buffer-intenral'"
  (let* ((value (and locked
                     (with-temp-buffer
                       (pcase-dolist (`(,var ,val) bindings)
                         (set (make-local-variable var) val))
                       (let ((major-mode mode))
                         (magit-buffer-value)))))
         (buffer  (magit-get-mode-buffer mode value))
         (section (and buffer (magit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (magit-generate-new-buffer mode value)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (funcall mode)
      (magit-xref-setup #'magit-setup-buffer-internal bindings)
      (setq magit-buffer-gitdir (magit-gitdir))
      (setq magit-buffer-topdir (magit-toplevel))
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        (run-hooks 'magit-create-buffer-hook)))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'magit-setup-buffer-hook)
      (magit-refresh-buffer))
    buffer))

(defmacro consult-gh-forge--magit-setup-buffer (mode &optional locked &rest bindings)
  "Reimplement `magit-setup-buffer'.

This is  to avoid making changes to magit settings for the repository
in the current working directory.

MODE, LOCKED, and BINDINGS are as defined in `magit-setup-buffer'"
  (declare (indent 2))
  `(consult-gh-forge--magit-setup-buffer-internal
    ,mode ,locked
    ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                           `(list ',var ,form))
                         bindings))))

(defun consult-gh-forge--topic-setup-buffer (topic)
  "Reimplement `forge-topic-setup-buffer'.

This is to avoid making changes to magit settings for the repository
in the current working directory.

TOPIC is as defined in `forge-topic-setup-buffer'"
  (let* ((repo  (forge-get-repository topic))
         (ident (concat "#"
                        (number-to-string (oref topic number))))
         (owner (oref repo owner))
         (name (oref repo name))
         (buffname  (format "*forge: %s/%s %s*" owner name ident))
         (tempdir (consult-gh-forge--make-tempdir (format "%s/%s" owner name)))
         (magit-generate-buffer-name-function (lambda (_mode _value) buffname))
         (default-directory (or (oref repo worktree)
                                tempdir)))
    (consult-gh-forge--magit-setup-buffer (if (functionp 'forge-issue-mode) (if (forge-issue-p topic) #'forge-issue-mode #'forge-pullreq-mode) #'forge-topic-mode) t
      (forge-buffer-topic topic)
      (forge-buffer-topic-ident ident))))

(defun consult-gh-forge--visit-topic (topic)
  "Reimplement `forge-visit-topic'.

This is to avoid making changes to magit settings for the repository
in the current working directory.

TOPIC is as defined in `forge-visit-topic'."
  (consult-gh-forge--topic-setup-buffer topic))


;;; Redefine ghub authentication functions
(cl-defmethod ghub--username :around (host &optional forge)
"Get username for HOST and FORGE (consult-gh override).

Note that this is created by `consult-gh' and overrides the
default behavior of `ghub--username' to allow using
consult-gh user name instead if the user chooses to."
  (let ((ghub-user (cl-call-next-method))
        (consult-gh-user (car-safe (consult-gh--auth-current-active-account))))
  (cond
   ((equal ghub-user consult-gh-user) ghub-user)
   (t
    (let ((user (if consult-gh-forge-confirm-account
                    (consult--read (list (propertize consult-gh-user 'account "from consult-gh")
                                     (propertize ghub-user 'account "from ghub/forge (i.e. git config)"))
                  :prompt "Which account do you want to use?"
                  :sort nil
                  :annotate (lambda (cand) (let ((acc (get-text-property 0 'account cand)))
                                             (format "\t%s" (propertize acc 'face 'consult-gh-tags-face)))))
                  consult-gh-user)))
      (if (and user (not (string-empty-p user))) user
        (cl-call-next-method)))))))

(cl-defmethod ghub--host :around (&optional forge)
"Get host name for FORGE (consult-gh override).

Note that this is created by `consult-gh' and overrides the
default behavior of `ghub--host' to allow using
consult-gh host name instead if the user chooses to."
  (let ((ghub-host (cl-call-next-method))
        (consult-gh-host (and (consp consult-gh--auth-current) (cadr consult-gh--auth-current)))
  (cond
   ((equal ghub-host consult-gh-host) ghub-host)
   (t
    (let ((host (if consult-gh-forge-confirm-account
                    (consult--read (list (propertize consult-gh-host 'account "from consult-gh")
                                     (propertize ghub-host 'account "from ghub/forge (i.e. git config)"))
                  :prompt "Which account do you want to use?"
                  :sort nil
                  :annotate (lambda (cand) (let ((acc (get-text-property 0 'account cand)))
                                             (format "\t%s" (propertize acc 'face 'consult-gh-tags-face)))))
                  consult-gh-host)))
      (if (and host (not (string-empty-p host))) host
        (cl-call-next-method))))))))

(defun consult-gh-ghub--token (host username package &optional nocreate forge)
"Get GitHub token for HOST USERNAME and PACKAGE.

This is an override function for `ghub--token' to allow
using `consult-gh' for getting tokens when ghub--token fails.
This allows getting token from gh cli commands without saving tokens
in auth sources.

See `ghub--token' for more info."
  (let* ((user (ghub--ident username package))
         (host (if (equal host ghub-default-host) (string-trim-left ghub-default-host "api.") host))
         (cmd-args (append '("auth" "token")
                           (and username `("-u" ,username))
                           (and host `("-h" ,host))))
         (gh-token (apply #'consult-gh--command-to-string cmd-args))
         (token
          (or (car (ghub--auth-source-get (list :secret)
                     :host host :user user))
              (and (stringp gh-token) (string-trim gh-token))
              (progn
                ;; Auth-Source caches the information that there is no
                ;; value, but in our case that is a situation that needs
                ;; fixing so we want to keep trying by invalidating that
                ;; information.
                ;; The (:max 1) is needed and has to be placed at the
                ;; end for Emacs releases before 26.1.  #24 #64 #72
                (auth-source-forget (list :host host :user user :max 1))
                (and (not nocreate)
                     (error "\
Required %s token (\"%s\" for \"%s\") does not exist.
See https://magit.vc/manual/ghub/Getting-Started.html
or (info \"(ghub)Getting Started\") for instructions.
\(The setup wizard no longer exists.)"
                            (capitalize (symbol-name (or forge 'github)))
                            user host))))))
    (if (functionp token) (funcall token) token)))

(advice-add 'ghub--token :override #'consult-gh-ghub--token)


;;; Define Functions and Interactive Commands for `consult-gh-forge'

(defun consult-gh-forge--issue-view (repo issue &optional timeout)
  "Add the REPO and ISSUE to forge database.

Uses `consult-gh-forge--add-topic' and tries to load the issue in forge
by `consult-gh-forge--visit-topic' within the TIMEOUT limit \(in seconds\),
otherwise reverts to using `consult-gh--issue-view-action' to open the
ISSUE."
  (let* ((repo (string-trim repo))
         (tempdir (consult-gh-forge--make-tempdir repo))
         (default-directory (file-name-parent-directory tempdir))
         (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (id (string-to-number issue))
         (timeout (or timeout consult-gh-forge-timeout-seconds)))

    (cl-letf (((symbol-function #'magit-toplevel)
               `(lambda () ,tempdir))
              ((symbol-function #'magit-gitdir)
               `(lambda () (expand-file-name ".git/" ,tempdir))))
      (let* ((created (consult-gh-forge--add-topic url id))
             (topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
        (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (funcall (consult-gh--issue-view-action) (propertize (format "%s" issue) ':repo repo ':issue issue)))
          (while (not topic)
            (sit-for 0.001)
            (setq topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
          (if topic
              (consult-gh-forge--visit-topic topic)
            (consult-gh--issue-view repo issue)))
        (when created
          (add-to-list 'consult-gh-forge--added-repositories url))))))

(defun consult-gh-forge--issue-view-action (cand)
  "Open preview of an issue candidate, CAND, in `forge'.

This is a wrapper function arround `consult-gh-forge--issue-view'."
  (let* ((info (cdr cand))
         (repo (substring-no-properties (plist-get info :repo)))
         (issue (substring-no-properties (format "%s" (plist-get info :issue)))))
    (consult-gh-forge--issue-view repo issue)))

(defun consult-gh-forge--pr-view (repo pr &optional timeout)
  "Add the REPO and PR to forge database.

 Uses `consult-gh-forge--add-topic' and tries to load the issue in forge
by `consult-gh-forge--visit-topic' within the TIMEOUT limit \(in seconds\),
otherwise reverts to using `consult-gh--pr-view-action' to open the PR."
  (let* ((repo (string-trim repo))
         (tempdir (consult-gh-forge--make-tempdir repo))
         (default-directory tempdir)
         (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (id (string-to-number pr))
         (timeout (or timeout consult-gh-forge-timeout-seconds)))
    (cl-letf (((symbol-function #'magit-toplevel)
               `(lambda () ,tempdir))
              ((symbol-function #'magit-gitdir)
               `(lambda () (expand-file-name ".git/" ,tempdir))))
      (let* ((created (consult-gh-forge--add-topic url id))
             (topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
        (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (funcall (consult-gh--pr-view-action) (propertize (format "%s" issue) ':repo repo ':issue issue)))
          (while (not topic)
            (sit-for 0.001)
            (setq topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
          (if topic
              (consult-gh-forge--visit-topic topic)
            (consult-gh--pr-view repo pr)))
        (when created
          (add-to-list 'consult-gh-forge--added-repositories url))))))

(defun consult-gh-forge--pr-view-action (cand)
  "Open preview of a pr candidate, CAND, in `forge'.

This is a wrapper function arround `consult-gh-forge--pr-view'."
  (let* ((info (cdr cand))
         (repo (substring-no-properties (plist-get info :repo)))
         (pr (substring-no-properties (format "%s" (plist-get info :pr)))))
    (consult-gh-forge--pr-view repo pr)))

;;; Provide `consult-gh-forge' module

(provide 'consult-gh-forge)

;;; consult-gh-forge.el ends here
