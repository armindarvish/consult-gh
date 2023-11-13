;;; consult-gh-embark.el --- Emabrk Actions for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (consult "0.34") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

;; Requirements
(require 'transient)

;; Prefixes
(transient-define-prefix consult-gh ()
  "Change parameters of prompt to send ChatGPT."
  ;; :incompatible '(("-m" "-n" "-k" "-e"))
  [:description
   (lambda ()
     (let ((username (consult-gh--get-current-username))
           (repo (consult-gh--get-repo-from-directory)))
       (concat
        (propertize "*CONSULT-GH* \n" 'face 'transient-heading)
        (if username (concat (propertize "user: " 'face 'consult-gh-date-face) (propertize username 'face 'consult-gh-user-face) "\t"))
        (if repo (concat (propertize "current repo: " 'face 'consult-gh-date-face) (propertize repo 'face 'consult-gh-user-face) "\t"))
        )))
   ""]

  [:description "--Actions--"
                ["Search"
                 (consult-gh-transient--suffix-search-issues)
                 (consult-gh-transient--suffix-search-repos)
                 (consult-gh-transient--suffix-search-prs)
                 (consult-gh-transient--suffix-search-code)
                 ]

                ["Repos"
                 (consult-gh-transient--suffix-repo-list)
                 (consult-gh-transient--suffix-repo-clone)
                 (consult-gh-transient--suffix-repo-fork)
                 ]

                ["Issues"
                 (consult-gh-transient--suffix-issue-list)
                 ]

                ["Pull Requests"
                 (consult-gh-transient--suffix-pr-list)
                 ]
                ]

  [:description
   "--Settings--"
   ["Limits"
    (consult-gh-transient--infix-repo-maxnum)
    (consult-gh-transient--infix-issue-maxnum)
    (consult-gh-transient--infix-pr-maxnum)
    (consult-gh-transient--infix-code-maxnum)]

   ["States"
    (consult-gh-transient--infix-issue-state)
    (consult-gh-transient--infix-prs-state)
    ]])


(defun consult-gh--transient-read-variable (prompt initial-input history)
  "Read value from minibuffer and interpret the result as a Lisp object."
  (read-from-minibuffer prompt initial-input read-expression-map t history))


;; Prefixes
(transient-define-prefix consult-gh--transient-repos ()
  [:description "Repos"]
  [["Actions"
    (consult-gh-transient--suffix-search-repos)
    (consult-gh-transient--suffix-repo-list)
    ]
   ["Parameters"
    (consult-gh-transient--infix-repo-maxnum)
    ]
   ]
  )

(transient-define-prefix consult-gh--transient-issues ()
  [:description "Issues"]
  [["Actions"
    (consult-gh-transient--suffix-search-issues)
    (consult-gh-transient--suffix-issue-list)
    ]
   ["Parameters"
    (consult-gh-transient--infix-issue-maxnum)
    ]
   ])

;; Infixes
(transient-define-infix consult-gh-transient--infix-repo-maxnum ()
  "AI Model for Chat."
  :description "Max Number of Repos: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-repo-maxnum
  :key "r -L"
  :reader 'consult-gh--transient-read-variable)

(transient-define-infix consult-gh-transient--infix-issue-maxnum ()
  "AI Model for Chat."
  :description "Max Number of Issues: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-issue-maxnum
  :key "i -L"
  :reader 'consult-gh--transient-read-variable)

(transient-define-infix consult-gh-transient--infix-pr-maxnum ()
  "AI Model for Chat."
  :description "Max Number of PRs: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-pr-maxnum
  :key "p -L"
  :reader 'consult-gh--transient-read-variable)

(transient-define-infix consult-gh-transient--infix-code-maxnum ()
  "AI Model for Chat."
  :description "Max Number of Codes: "
  :class 'transient-lisp-variable
  :variable 'consult-gh-code-maxnum
  :key "c -L"
  :reader 'consult-gh--transient-read-variable)

(transient-define-infix consult-gh-transient--infix-issue-state ()
  "AI Model for Chat."
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
  "AI Model for Chat."
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

(transient-define-suffix consult-gh-transient--suffix-repo-clone ()
  :transient nil
  :description "clone a repo"
  :key "r c"
  (interactive)
  (consult-gh-repo-clone))

(transient-define-suffix consult-gh-transient--suffix-repo-fork ()
  :transient nil
  :description "fork a repo"
  :key "r f"
  (interactive)
  (consult-gh-repo-fork))

(transient-define-suffix consult-gh-transient--suffix-repo-list ()
  :transient nil
  :description "list repos of a user"
  :key "r l"
  (interactive)
  (consult-gh-repo-list))

(transient-define-suffix consult-gh-transient--suffix-issue-list ()
  :transient nil
  :description "list issues of a repo"
  :key "i l"
  (interactive)
  (consult-gh-issue-list))

(transient-define-suffix consult-gh-transient--suffix-pr-list ()
  :transient nil
  :description "list prs of a repo"
  :key "p l"
  (interactive)
  (consult-gh-pr-list))

(transient-define-suffix consult-gh-transient--suffix-search-repos ()
  :transient nil
  :description "search repos"
  :key "s r"
  (interactive)
  (consult-gh-search-repos))

(transient-define-suffix consult-gh-transient--suffix-search-issues ()
  :transient nil
  :description "search issues"
  :key "s i"
  (interactive)
  (consult-gh-search-issues))

(transient-define-suffix consult-gh-transient--suffix-search-prs ()
  :transient nil
  :description "search pull requests"
  :key "s p"
  (interactive)
  (consult-gh-search-prs))

(transient-define-suffix consult-gh-transient--suffix-search-code ()
  :transient nil
  :description "search code"
  :key "s c"
  (interactive)
  (consult-gh-search-code))

(transient-define-suffix consult-gh-transient--suffix-test ()
  :transient nil
  :description "test"
  :key "t"
  (interactive)
  (message "%s" (transient-args transient-current-suffixes))
  )

;;; Provide `consul-gh-transient' module

(provide 'consult-gh-transient)


