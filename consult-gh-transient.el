;;; consult-gh-transient.el --- Transient Menu for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 3.0
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
;; This package providesa transient menu for consult-gh.
;; (see URL `https://github.com/armindarvish/consult-gh' for more info).

;;; Code:

;; Requirements
(require 'consult-gh)
(require 'transient)

;; Prefixes
(transient-define-prefix consult-gh-transient ()
  "Main transient menu for `consult-gh'."
  [:description
   (lambda ()
     (let ((username (consult-gh--get-current-username))
           (repo (consult-gh--get-repo-from-directory)))
       (concat
        (propertize "*consult-gh: Consulting GitHub Client* \n" 'face 'transient-heading)
        (if username (concat (propertize "Current User: " 'face 'consult-gh-date) (propertize username 'face 'consult-gh-user) "\n"))
        (if repo (concat (propertize "Current Repo: " 'face 'consult-gh-date) (propertize repo 'face 'consult-gh-user) "\n")))))
   ""]

  [:description "--Quick Access--"
                (consult-gh-transient--suffix-switch-account)
                (consult-gh-transient--suffix-dashboard)
                (consult-gh-transient--suffix-notifications)]

  [:description "--Actions--"
                ["Search"
                 (consult-gh-transient--suffix-search-issues)
                 (consult-gh-transient--suffix-search-repos)
                 (consult-gh-transient--suffix-search-prs)
                 (consult-gh-transient--suffix-search-code)]

                ["Repos"
                 (consult-gh-transient--suffix-repo-list)
                 (consult-gh-transient--suffix-repo-create)
                 (consult-gh-transient--suffix-repo-clone)
                 (consult-gh-transient--suffix-repo-fork)]

                ["Issues"
                 (consult-gh-transient--suffix-issue-list)]

                ["Pull Requests"
                 (consult-gh-transient--suffix-pr-list)]]

  [:description
   "--Settings--"
   ["Limits"
    (consult-gh-transient--infix-repo-maxnum)
    (consult-gh-transient--infix-issue-maxnum)
    (consult-gh-transient--infix-pr-maxnum)
    (consult-gh-transient--infix-code-maxnum)]

   ["States"
    (consult-gh-transient--infix-issue-state)
    (consult-gh-transient--infix-prs-state)]])

(defun consult-gh-transient-read-variable (prompt initial-input history)
  "Read value from minibuffer for `consult-gh' transient menu.

PROMPT, INITIAL-INPUT, and HISTORY are passed to `read-from-minibffer'."
  (read-from-minibuffer prompt initial-input read-expression-map t history))


;; Infixes
(transient-define-infix consult-gh-transient--infix-repo-maxnum ()
  "Set `consult-gh-repo-maxnum' in `consult-gh' transient menu."
  :description "Max Number of Repos: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-repo-maxnum
  :key "r -L"
  :reader 'consult-gh-transient-read-variable)

(transient-define-infix consult-gh-transient--infix-issue-maxnum ()
  "Set `consult-gh-issue-maxnum' in `consult-gh' transient menu."
  :description "Max Number of Issues: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-issue-maxnum
  :key "i -L"
  :reader 'consult-gh-transient-read-variable)

(transient-define-infix consult-gh-transient--infix-pr-maxnum ()
  "Set `consult-gh-pr-maxnum' in `consult-gh' transient menu."
  :description "Max Number of PRs: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-pr-maxnum
  :key "p -L"
  :reader 'consult-gh-transient-read-variable)

(transient-define-infix consult-gh-transient--infix-code-maxnum ()
  "Set `consult-gh-code-maxnum' in `consult-gh' transient menu."
  :description "Max Number of Codes: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-code-maxnum
  :key "c -L"
  :reader 'consult-gh-transient-read-variable)

(transient-define-infix consult-gh-transient--infix-issue-state ()
  "Set `consult-gh-issues-state-to-show' in `consult-gh' transient menu."
  :description "State of Issues to Show "
  :class 'transient-lisp-variable
  :variable 'consult-gh-issues-state-to-show
  :key "i -s"
  :choices '("all" "open" "closed")
  :reader (lambda (prompt &rest _)
            (completing-read
             prompt
             '("all" "open" "closed"))))

(transient-define-infix consult-gh-transient--infix-prs-state ()
  "Set `consult-gh-prs-state-to-show' in `consult-gh' transient menu."
  :description "State of PRs to Show: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-prs-state-to-show
  :key "p -s"
  :choices '("all" "open" "closed" "merged")
  :reader (lambda (prompt &rest _)
            (completing-read
             prompt
             '("all" "open" "closed" "merged"))))

;; Suffixes
(transient-define-suffix consult-gh-transient--suffix-switch-account ()
  "Call `consult-gh-dashboard' in `consult-gh' transient menu."
  :transient nil
  :description "Switch Account"
  :key "a"
  (interactive)
  (consult-gh-auth-switch))


(transient-define-suffix consult-gh-transient--suffix-dashboard ()
  "Call `consult-gh-dashboard' in `consult-gh' transient menu."
  :transient nil
  :description "User's Dashboard"
  :key "d"
  (interactive)
  (consult-gh-dashboard))

(transient-define-suffix consult-gh-transient--suffix-notifications ()
  "Call `consult-gh-notifications' in `consult-gh' transient menu."
  :transient nil
  :description "User's Notifications"
  :key "n"
  (interactive)
  (consult-gh-notifications))

(transient-define-suffix consult-gh-transient--suffix-repo-create ()
  "Call `consult-gh-repo-list' in `consult-gh' transient menu."
  :transient nil
  :description "Create a New Repo"
  :key "r n"
  (interactive)
  (consult-gh-repo-create))

(transient-define-suffix consult-gh-transient--suffix-repo-list ()
  "Call `consult-gh-repo-list' in `consult-gh' transient menu."
  :transient nil
  :description "List Repos of a User"
  :key "r l"
  (interactive)
  (consult-gh-repo-list))

(transient-define-suffix consult-gh-transient--suffix-repo-clone ()
  "Call `consult-gh-repo-clone' in `consult-gh' transient menu."
  :transient nil
  :description "Clone a Repo"
  :key "r c"
  (interactive)
  (consult-gh-repo-clone))

(transient-define-suffix consult-gh-transient--suffix-repo-fork ()
  "Call `consult-gh-repo-fork' in `consult-gh' transient menu."
  :transient nil
  :description "Fork a Repo"
  :key "r f"
  (interactive)
  (consult-gh-repo-fork))

(transient-define-suffix consult-gh-transient--suffix-issue-list ()
  "Call `consult-gh-issue-list' in `consult-gh' transient menu."
  :transient nil
  :description "List Issues of a Repo"
  :key "i l"
  (interactive)
  (consult-gh-issue-list))

(transient-define-suffix consult-gh-transient--suffix-pr-list ()
  "Call `consult-gh-pr-list' in `consult-gh' transient menu."
  :transient nil
  :description "List PRs of a Repo"
  :key "p l"
  (interactive)
  (consult-gh-pr-list))

(transient-define-suffix consult-gh-transient--suffix-search-repos ()
  "Call `consult-gh-search-repos' in `consult-gh' transient menu."
  :transient nil
  :description "Search Repos"
  :key "s r"
  (interactive)
  (consult-gh-search-repos))

(transient-define-suffix consult-gh-transient--suffix-search-issues ()
  "Call `consult-gh-search-issues' in `consult-gh' transient menu."
  :transient nil
  :description "Search Issues"
  :key "s i"
  (interactive)
  (consult-gh-search-issues))

(transient-define-suffix consult-gh-transient--suffix-search-prs ()
  "Call `consult-gh-search-prs' in `consult-gh' transient menu."
  :transient nil
  :description "Search Pull Requests"
  :key "s p"
  (interactive)
  (consult-gh-search-prs))

(transient-define-suffix consult-gh-transient--suffix-search-code ()
  "Call `consult-gh-search-code' in `consult-gh' transient menu."
  :transient nil
  :description "Search Code"
  :key "s c"
  (interactive)
  (consult-gh-search-code))

;;; Provide `consult-gh-transient' module

(provide 'consult-gh-transient)

;;; consult-gh-transient.el ends here
