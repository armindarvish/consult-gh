;;; consult-gh-forge.el --- Magit/Forge Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 3.0
;; Package-Requires: ((emacs "29.4") (consult "2.0") (forge "0.3.3") (consult-gh "3.0"))
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
(require 'forge)
(require 'ghub)
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

(defvar consult-gh-forge--default-issue-action consult-gh-issue-action
  "Default action for viewing issues without forge integration.")

(defvar consult-gh-forge--default-pr-action consult-gh-pr-action
  "Default action for viewing PRs without forge integration.")

(defun consult-gh-forge--mode-on ()
  "Enable `consult-gh-forge-mode'."
  (unless (equal consult-gh-issue-action #'consult-gh-forge--issue-view-action)
    (setq consult-gh-forge--default-issue-action consult-gh-issue-action))
  (unless (equal consult-gh-pr-action #'consult-gh-forge--pr-view-action)
    (setq consult-gh-forge--default-pr-action consult-gh-pr-action))
  (setq consult-gh-issue-action #'consult-gh-forge--issue-view-action)
  (setq consult-gh-pr-action #'consult-gh-forge--pr-view-action)
  (advice-add 'ghub--token :override #'consult-gh-forge--ghub-token))

(defun consult-gh-forge--mode-off ()
  "Disable `consult-gh-forge-mode'."
  (when (equal consult-gh-issue-action #'consult-gh-forge--issue-view-action)
    (setq consult-gh-issue-action consult-gh-forge--default-issue-action))
  (when (equal consult-gh-pr-action #'consult-gh-forge--pr-view-action)
    (setq consult-gh-pr-action consult-gh-forge--default-pr-action))
  (advice-remove 'ghub--token #'consult-gh-forge--ghub-token))

;;;###autoload
(define-minor-mode consult-gh-forge-mode
  "Use magit/forge with `consult-gh' for viewing issues/prs."
  :init-value nil
  :global t
  :group 'consult-gh
  :lighter " consult-gh-forge"
  (if consult-gh-forge-mode
      (consult-gh-forge--mode-on)
    (consult-gh-forge--mode-off)))

;;; Define Backend Functions for `consult-gh-forge'
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
The repos are stored in `consult-gh-forge--added-repositories'."
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
  (let ((created (consult-gh-forge--add-repository url))
        (repo (forge-get-repository url)))
    (while (not repo)
      (sit-for 0.0001)
      (setq repo (forge-get-repository url)))
    (consult-gh-forge--pull-topic url topic)
    created))

(defun consult-gh-forge--visit-topic (topic)
  "Reimplement `forge-visit-topic'.

This is to avoid making changes to magit settings for the repository
in the current working directory.

TOPIC is as defined in `forge-visit-topic'."
  (forge-topic-setup-buffer topic))

;;; Define Functions and Interactive Commands for `consult-gh-forge'

(defun consult-gh-forge--issue-view (repo number &optional timeout)
  "Add the REPO and ISSUE to forge database.

Uses `consult-gh-forge--add-topic' and tries to load the issue in forge
by `consult-gh-forge--visit-topic' within the TIMEOUT limit \(in seconds\),
otherwise reverts to using `consult-gh--issue-view-action' to open the
issue identified by NUMBER."
  (let* ((repo (string-trim repo))
         (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (id (string-to-number number))
         (timeout (or timeout consult-gh-forge-timeout-seconds))
         (created (consult-gh-forge--add-topic url id))
         (topic (ignore-errors (forge-get-topic (forge-get-repository url) id)))
         (cand (format "%s" number)))
    (add-text-properties 0 1 (list :repo repo :number number) cand)
    (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (funcall #'consult-gh--issue-view-action cand))
      (while (not topic)
        (sit-for 0.001)
        (setq topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
      (if topic
          (consult-gh-forge--visit-topic topic)
        (consult-gh--issue-view repo number)))
    (when created
      (add-to-list 'consult-gh-forge--added-repositories url))))

(defun consult-gh-forge--issue-view-action (cand)
  "Open preview of an issue candidate, CAND, in `forge'.

This is a wrapper function around `consult-gh-forge--issue-view'."
  (if consult-gh-forge-mode
      (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
             (number (substring-no-properties (format "%s" (get-text-property 0 :number cand)))))
        (consult-gh-forge--issue-view repo number))
    (message "consult-gh-forge-mode is disabled! You can either enable the mode or change view actions \(e.g. `consult-gh-issue-action'\).")))

(defun consult-gh-forge--pr-view (repo number &optional timeout)
  "Add the REPO and PR to forge database.

Uses `consult-gh-forge--add-topic' and tries to load the issue in forge by
`consult-gh-forge--visit-topic' within the TIMEOUT limit \(in seconds\),
otherwise reverts to using `consult-gh--pr-view-action' to open the pr
identified by NUMBER."
  (let* ((repo (string-trim repo))
         (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (id (string-to-number number))
         (timeout (or timeout consult-gh-forge-timeout-seconds))
         (created (consult-gh-forge--add-topic url id))
         (topic (ignore-errors (forge-get-topic (forge-get-repository url) id)))
         (cand (format "%s" number)))
    (add-text-properties 0 1 (list :repo repo :number number) cand)
    (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (funcall #'consult-gh--pr-view-action cand))
      (while (not topic)
        (sit-for 0.001)
        (setq topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
      (if topic
          (consult-gh-forge--visit-topic topic)
        (consult-gh--pr-view repo number)))
    (when created
      (add-to-list 'consult-gh-forge--added-repositories url))))

(defun consult-gh-forge--pr-view-action (cand)
  "Open preview of a pr candidate, CAND, in `forge'.

This is a wrapper function around `consult-gh-forge--pr-view'."
  (if consult-gh-forge-mode
      (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
             (number (substring-no-properties (format "%s" (get-text-property 0 :number cand)))))
        (consult-gh-forge--pr-view repo number))
    (message "consult-gh-forge-mode is disabled! You can either enable the mode or change view actions \(e.g. `consult-gh-pr-action'\).")))

(defun consult-gh-forge--ghub-token (host username package &optional nocreate forge)
  "Get GitHub token for HOST USERNAME and PACKAGE.

This is an override function for `ghub--token' to allow
using `consult-gh' for getting tokens when `ghub--token' fails.
This allows getting token from gh cli commands without saving tokens
in auth sources.

See `ghub--token' for definition of NOCREATE and FORGE as well as
more info."
  (let* ((user (ghub--ident username package))
         (ghub-default-host (alist-get 'github ghub-default-host-alist))
         (host (cond ((equal host ghub-default-host)
                      (string-trim-left ghub-default-host "api."))
                     ((string-suffix-p "/api/v3" host) (string-trim-right host "/api/v3"))
                     ((string-suffix-p "/api/v4" host) (string-trim-right host "/api/v4"))
                     ((string-suffix-p "/v3" host) (string-trim-right host "/v3"))
                     ((string-suffix-p "/v4" host) (string-trim-right host "/v4"))
                     ((string-suffix-p "/api" host) (string-trim-right host "/api"))
                     (t host)))
         (cmd-args (append '("auth" "token")
                           (and username `("-u" ,username))
                           (and host `("-h" ,host))))
         (gh-token (apply #'consult-gh--command-to-string cmd-args))
         (token
          (or (and (stringp gh-token) (string-trim gh-token))
              (car (ghub--auth-source-get (list :secret)
                     :host host :user user))
              (progn
                ;; Auth-Source caches the information that there is no
                ;; value, but in our case that is a situation that needs
                ;; fixing so we want to keep trying by invalidating that
                ;; information.
                ;; The (:max 1) is needed and has to be placed at the
                ;; end for Emacs releases before 26.1.
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



;;; Redefine ghub authentication functions
(cl-defmethod ghub--username :around (host &context (consult-gh-forge-mode (eql t)) &optional _forge)
  "Get username for HOST and FORGE (`consult-gh' override).

Note that this is created by `consult-gh' and overrides the
default behavior of `ghub--username' to allow using
`consult-gh' user name instead if the user chooses to."

  (let* ((ghub-user (cl-call-next-method))
         (ghub-default-host (alist-get 'github ghub-default-host-alist))
         (consult-gh-user (or (car-safe consult-gh--auth-current-account)
                              (car-safe (consult-gh--auth-current-active-account
                                         (cond ((equal host ghub-default-host)
                                                (string-trim-left ghub-default-host "api."))
                                               ((string-suffix-p "/api/v3" host)
                                                (string-trim-right host "/api/v3"))
                                               ((string-suffix-p "/api/v4" host)
                                                (string-trim-right host "/api/v4"))
                                               ((string-suffix-p "/v3" host)
                                                (string-trim-right host "/v3"))
                                               ((string-suffix-p "/v4" host)
                                                (string-trim-right host "/v4"))
                                               ((string-suffix-p "/api" host)
                                                (string-trim-right host "/api"))
                                               (t (or host consult-gh-default-host))))))))
    (cond
     ((equal ghub-user consult-gh-user) ghub-user)
     (t
      (let ((user (if (and consult-gh-forge-confirm-account
                           (stringp ghub-user)
                           (stringp consult-gh-user))
                      (consult--read (list (propertize consult-gh-user 'account "from consult-gh")
                                           (propertize ghub-user 'account "from ghub/forge (i.e. git config)"))
                                     :prompt "Which account do you want to use?"
                                     :sort nil
                                     :annotate (lambda (cand) (let ((acc (get-text-property 0 'account cand)))
                                                                (format "\t%s" (propertize acc 'face 'consult-gh-tags)))))
                    (or consult-gh-user ghub-user))))
        (if (and user (not (string-empty-p user))) user
          (cl-call-next-method)))))))

(cl-defmethod ghub--host :around (&context (consult-gh-forge-mode (eql t)) &optional _forge)
  "Get host name for FORGE (`consult-gh' override).

Note that this is created by `consult-gh' and overrides the
default behavior of `ghub--host' to allow using
`consult-gh' host name instead if the user chooses to."
  (let ((ghub-host (cl-call-next-method))
        (consult-gh-host (or (and (consp consult-gh--auth-current-account) (cadr consult-gh--auth-current-account))
                             (cadr (consult-gh--auth-current-active-account)))))
    (cond
     ((equal ghub-host consult-gh-host) ghub-host)
     (t
      (let ((host (if (and consult-gh-forge-confirm-account
                           (stringp ghub-host)
                           (stringp consult-gh-host))
                      (consult--read (list (propertize consult-gh-host 'account "from consult-gh")
                                           (propertize ghub-host 'account "from ghub/forge (i.e. git config)"))
                                     :prompt "Which account do you want to use?"
                                     :sort nil
                                     :annotate (lambda (cand) (let ((acc (get-text-property 0 'account cand)))
                                                                (format "\t%s" (propertize acc 'face 'consult-gh-tags)))))
                    (or consult-gh-host ghub-host))))
        (if (and host (not (string-empty-p host))) host
          (cl-call-next-method)))))))


;;;###autoload
(defun consult-gh-forge-open-topic-in-consult-gh ()
  "Open the  forge at point with `consult-gh--pr-view'."
  (interactive nil magit-mode forge-topic-mode)
  (let* ((topic (or (forge-pullreq-at-point) (forge-current-topic)))
         (_ (unless topic (error "Not on a forge issue or pullrequest!")))
         (repo    (forge-get-repository topic))
         (func (cond ((forge-pullreq-p topic) #'consult-gh--pr-view)
                     ((forge-issue-p topic) #'consult-gh--issue-view))))
    (if (forge-github-repository-p repo)
        (when-let* ((owner (oref repo owner))
                    (name (oref repo name))
                    (reponame (concat owner "/" name))
                    (number (oref topic number))
                    (number (cond ((numberp number) (number-to-string number))
                                  ((stringp number) number))))
          (funcall consult-gh-switch-to-buffer-func (funcall func reponame number)))
      (message "There is no Github Pullrequest at point!"))))

;;;###autoload
(defun consult-gh-topics-open-in-forge (&optional topic)
  "Open the consult-gh TOPIC in forge."
  (interactive nil consult-gh-pr-view-mode consult-gh-issue-view-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or topic consult-gh--topic))
          (type (and (stringp topic) (get-text-property 0 :type topic)))
          (repo (and (stringp topic) (get-text-property 0 :repo topic)))
          (number (and (stringp topic) (get-text-property 0 :number topic))))
     (pcase type
       ("issue"
        (consult-gh-forge--issue-view repo number))
       ("pr"
        (consult-gh-forge--pr-view repo number))))))

;;; Provide `consult-gh-forge' module

(provide 'consult-gh-forge)

;;; consult-gh-forge.el ends here
