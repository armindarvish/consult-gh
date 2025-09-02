;;; consult-gh-with-pr-review.el --- "pr-review" Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 3.0
;; Package-Requires: ((emacs "29.4") (consult "2.0") (pr-review "0.1") (consult-gh "3.0"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, completion

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
;; This package provides emacs-pr-review integration for consult-gh.
;; (see URL `https://github.com/armindarvish/consult-gh' for more info).

;;; Code:

;;; Requirements
(require 'pr-review)
(require 'consult-gh)

;;; Customization Variables

(defcustom consult-gh-with-pr-review-confirm-account t
  "Ask for confirmation when account doesn't match `pr-review' config?

Query the user to pick an account when the account from gh cli command
and pr review config do not match."
  :group 'consult-gh
  :type 'boolean)

;;; Other Variables

(defvar consult-gh-with-pr-review--default-pr-action consult-gh-pr-action
  "Default action for viewing PRs without `pr-review' integration.")

(defun consult-gh-with-pr-review--pr-view (repo number)
  "Open pullrequest NUMBER in REPO  with `pr-review'."
  (if consult-gh-with-pr-review-mode
      (let* ((repo-owner (consult-gh--get-username repo))
             (repo-name (consult-gh--get-package repo)))
        (pr-review-open repo-owner repo-name number))
    (message "consult-gh-with-pr-review-mode is disabled! You can either enable the mode or change view actions \(e.g. `consult-gh-pr-action'\).")))

(defun consult-gh-with-pr-review--pr-view-action (cand)
  "Open preview of a pr candidate, CAND, in `pr-review'.

This is a wrapper function around `consult-gh-pre-review--pr-view'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (format "%s" (get-text-property 0 :number cand)))))
    (consult-gh-with-pr-review--pr-view repo number)))

(defun consult-gh-with-pr-review--ghub-token (host username package &optional nocreate forge)
  "Get GitHub token for HOST USERNAME and PACKAGE.

This is an override function for `ghub--token' to allow
using `consult-gh' for getting tokens when `ghub--token' fails.
This allows getting token for `pr-review' from gh cli commands without
saving tokens in auth sources.

See `ghub--token' for definition of NOCREATE and FORGE as well as
more info."
  (let* ((user (ghub--ident username package))
         (ghub-default-host (alist-get 'github ghub-default-host-alist))
         (host (cond ((equal host ghub-default-host) (string-trim-left ghub-default-host "api."))
                     ((string-suffix-p "/api" host) (string-trim-right host "/api"))
                     ((string-suffix-p "/v3" host) (string-trim-right host "/v3"))
                     ((string-suffix-p "/api/v4" host) (string-trim-right host "/api/v4"))
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

(defun consult-gh-with-pr-review--mode-on ()
  "Enable `consult-gh-with-pr-review-mode'."
  (unless (equal consult-gh-pr-action #'consult-gh-with-pr-review--pr-view-action)
    (setq consult-gh-with-pr-review--default-pr-action consult-gh-pr-action))
  (setq consult-gh-pr-action #'consult-gh-with-pr-review--pr-view-action)
  (advice-add 'ghub--token :override #'consult-gh-with-pr-review--ghub-token))

(defun consult-gh-with-pr-review--mode-off ()
  "Disable `consult-gh-with-pr-review-mode'."
  (when (equal consult-gh-pr-action #'consult-gh-with-pr-review--pr-view-action)
    (setq consult-gh-pr-action consult-gh-with-pr-review--default-pr-action))
  (advice-remove 'ghub--token #'consult-gh-with-pr-review--ghub-token))


;;;###autoload
(define-minor-mode consult-gh-with-pr-review-mode
  "Use`pr-review' with `consult-gh' for viewing prs."
  :init-value nil
  :global t
  :group 'consult-gh
  :lighter " consult-gh-with-pr-review"
  (if consult-gh-with-pr-review-mode
      (consult-gh-with-pr-review--mode-on)
    (consult-gh-with-pr-review--mode-off)))

;;;###autoload
(defun consult-gh-with-pr-review-open-topic (&optional topic)
  "Open the `consult-gh' TOPIC in `pr-review'."
  (interactive nil consult-gh-pr-view-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or topic consult-gh--topic))
          (type (and (stringp topic) (get-text-property 0 :type topic)))
          (repo (and (stringp topic) (get-text-property 0 :repo topic)))
          (number (and (stringp topic) (get-text-property 0 :number topic))))
     (if (equal type "pr")
         (consult-gh-with-pr-review--pr-view repo number)
       (if (and repo number)
           (message "%s:%s is not a %s" (propertize repo 'face 'consult-gh-repo) (propertize number 'face 'consult-gh-repo) (propertize "pullrequest" 'face 'consult-gh-warning))
         (message "cannot find a GitHub pullrequest in this buffer to open with `pr-review'."))))))

;;; Redefine ghub authentication functions
(cl-defmethod ghub--username :around (host &context (consult-gh-with-pr-review-mode (eql t)) &optional _forge)
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
                                               ((string-suffix-p "/api" host)
                                                (string-trim-right host "/api"))
                                               ((string-suffix-p "/v3" host)
                                                (string-trim-right host "/v3"))
                                               ((string-suffix-p "/api/v4" host)
                                                (string-trim-right host "/api/v4"))
                                               (t (or host consult-gh-default-host))))))))
    (cond
     ((equal ghub-user consult-gh-user) ghub-user)
     (t
      (let ((user (if (and consult-gh-with-pr-review-confirm-account
                           (stringp ghub-user)
                           (stringp consult-gh-user))
                      (consult--read (list (propertize consult-gh-user 'account "from consult-gh")
                                           (propertize ghub-user 'account "from ghub/pr-review"))
                                     :prompt "Which account do you want to use?"
                                     :sort nil
                                     :annotate (lambda (cand) (let ((acc (get-text-property 0 'account cand)))
                                                                (format "\t%s" (propertize acc 'face 'consult-gh-tags)))))
                    (or consult-gh-user ghub-user))))
        (if (and user (not (string-empty-p user))) user
          (cl-call-next-method)))))))

(cl-defmethod ghub--host :around (&context (consult-gh-with-pr-review-mode (eql t)) &optional _forge)
  "Get host name for FORGE (`consult-gh' override).

Note that this is created by `consult-gh' and overrides the
default behavior of `ghub--host' to allow using
`consult-gh' host name instead, if the user chooses to."
  (let ((ghub-host (cl-call-next-method))
        (consult-gh-host (or (and (consp consult-gh--auth-current-account) (cadr consult-gh--auth-current-account))
                             (cadr (consult-gh--auth-current-active-account)))))
    (cond
     ((equal ghub-host consult-gh-host) ghub-host)
     (t
      (let ((host (if (and consult-gh-with-pr-review-confirm-account
                           (stringp ghub-host)
                           (stringp consult-gh-host))
                      (consult--read (list (propertize consult-gh-host 'account "from consult-gh")
                                           (propertize ghub-host 'account "from ghub/pr-review"))
                                     :prompt "Which account do you want to use?"
                                     :sort nil
                                     :annotate (lambda (cand) (let ((acc (get-text-property 0 'account cand)))
                                                                (format "\t%s" (propertize acc 'face 'consult-gh-tags)))))
                    (or consult-gh-host ghub-host))))
        (if (and host (not (string-empty-p host))) host
          (cl-call-next-method)))))))



;;; Provide `consult-gh-with-pr-review' module

(provide 'consult-gh-with-pr-review)

;;; consult-gh-with-pr-review.el ends here
