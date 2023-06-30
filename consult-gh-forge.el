;;; consult-gh-forge.el --- Magit/Forge Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.11
;; Package-Requires: ((emacs "27.1") (consult "0.34") (forge "0.3.3") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

(if (featurep 'forge)
  (require 'forge)
  (message "forge is not loaded"))
(require 'consult-gh)
;;; Customization Variables

(defcustom consult-gh-forge-timeout-seconds 5
"Timeout in seconds for forge-visit to load the issue, otherwise revert back to normal viewing in consult-gh."
  :group 'consult-gh
  :type 'integer
)

(defun consult-gh-forge--add-topic (url topic)
  "Add a repository to the forge database and only
pull individual topics when the user invokes `forge-pull-topic'. see forge documentation for `forge-add-repository'."
  (if (forge-get-repository url nil 'full)
      ()
    (let ((repo (forge-get-repository url nil 'create)))
      (oset repo sparse-p nil)
      (oset repo selective-p t)
      (forge--pull repo nil)
      (forge--zap-repository-cache repo)
      (forge--pull-topic repo
                   (forge-issue :repository (oref repo id)
                                :number topic))
      )))

(defun consult-gh-forge--issue-view (repo issue &optional timeout)
"Try to load the issue in forge within the timeout limit, otherwise revert back to running `consult-gh--issue-view-action'."
(let* ((url (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser"))
      (timeout (or timeout consult-gh-forge-timeout-seconds))
      (_ (consult-gh-forge--add-topic url issue))
      (topic nil))
  (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (consult-gh--issue-view repo issue))
    (while (not topic)
      (sit-for 0.001)
      (setq topic (forge-get-topic (forge-get-repository url) id))
      )
    (if topic
        (forge-visit topic)
      (consult-gh--issue-view repo issue)))))

(defun consult-gh-forge--issue-view-action ()
   "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in `forge' using `consult-gh-forge--issue-view'."
  (lambda (cand)
    (let* ((repo (substring (get-text-property 0 :repo cand)))
          (issue (substring (get-text-property 0 :issue cand))))
      (consult-gh-forge--issue-view repo issue)
      )))

(provide 'consult-gh-forge)
