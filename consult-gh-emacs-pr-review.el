;;; consult-gh-emacs-pr-review.el --- Emacs pr-review Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.2
;; Package-Requires: ((emacs "30.0") (consult "1.9") (pr-review "0.1") (consult-gh "2.2"))
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


;;; Variables
(defvar consult-gh-emacs-pr-review--default-pr-action consult-gh-pr-action
  "Default action for viewing PRs without pr-review integration.")

(defun consult-gh-emacs-pr-review--pr-view (repo number)
  "Open pullrequest NUMBER in REPO  with `pr-review'."
  (let* ((repo-owner (consult-gh--get-username repo))
         (repo-name (consult-gh--get-package repo)))
    (pr-review-open repo-owner repo-name number)))

(defun consult-gh-emacs-pr-review--pr-view-action (cand)
  "Open preview of a pr candidate, CAND, in `pr-review'.

This is a wrapper function around `consult-gh-pre-review--pr-view'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (format "%s" (get-text-property 0 :number cand)))))
    (consult-gh-emacs-pr-review--pr-view repo number)))

(defun consult-gh-emacs-pr-review--mode-on ()
  "Enable `consult-gh-emacs-pr-review-mode'."
  (setq consult-gh-emacs-pr-review--default-pr-action consult-gh-pr-action)
  (setq consult-gh-pr-action #'consult-gh-emacs-pr-review--pr-view-action))

(defun consult-gh-emacs-pr-review--mode-off ()
  "Disable `consult-gh-emacs-pr-review-mode'."
  (setq consult-gh-pr-action consult-gh-emacs-pr-review--default-pr-action))

;;;###autoload
(define-minor-mode consult-gh-emacs-pr-review-mode
  "Use`pr-review' with `consult-gh' for viewing prs."
  :init-value nil
  :global t
  :group 'consult-gh
  :lighter " consult-gh-emacs-pr-review"
  (if consult-gh-emacs-pr-review-mode
      (consult-gh-emacs-pr-review--mode-on)
    (consult-gh-emacs-pr-review--mode-off)))

;;;###autoload
(defun consult-gh-topics-open-in-emacs-pr-review (&optional topic)
  "Open the consult-gh TOPIC in pr-review."
  (interactive nil consult-gh-pr-view-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or topic consult-gh--topic))
          (type (and (stringp topic) (get-text-property 0 :type topic)))
          (repo (and (stringp topic) (get-text-property 0 :repo topic)))
          (number (and (stringp topic) (get-text-property 0 :number topic))))
     (if (equal type "pr")
         (consult-gh-emacs-pr-review--pr-view repo number)
       (if (and repo number)
           (message (format "%s:%s is not a %s" (propertize repo 'face 'consult-gh-repo) (propertize number 'face 'consult-gh-repo) (propertize "pullrequest" 'face 'consult-gh-warning)))
         (message "cannot find a GitHub pullrequest in this buffer to open with `pr-review'."))))))


;;; Provide `consult-gh-emacs-pr-review' module

(provide 'consult-gh-emacs-pr-review)

;;; consult-gh-emacs-pr-review.el ends here
