;;; consult-gh.el --- Consulting GitHub Client -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.0
;; Package-Requires: ((emacs "30.0") (consult "1.9") (markdown-mode "2.6") (ox-gfm "1.0"))
;; Keywords: convenience, matching, tools, vc
;; Homepage: https://github.com/armindarvish/consult-gh

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

;; This package provides an interactive interface to GitHub command-line
;; client (see URL `https://cli.github.com/').  It uses a consult-based minibuffer
;; completion for searching and selecting GitHub repositories, issues,
;; pull requests, codes, and etc.

;;; Code:

;;; Requirements

(eval-when-compile
  (require 'json))

(require 'consult) ;; core dependency
(require 'markdown-mode) ;; markdown-mode for viewing issues,prs, ...
(require 'ox-gfm) ;; for exporting org-mode to github flavored markdown
(require 'org) ;; for its awesomeness!

;;; Group

(defgroup consult-gh nil
  "Consult-based interface for GitHub CLI."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'magit
  :prefix "consult-gh-"
  :link '(url-link :tag "GitHub" "https://github.com/armindarvish/consult-gh"))

;;; Customization Variables

(defcustom consult-gh-args '("gh")
  "Command line arguments to call GitHub CLI.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-repo-list-args '("repo" "list")
  "Additional arguments for `consult-gh-repo-list'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-issue-list-args '("issue" "list" "--repo")
  "Additional arguments for `consult-gh-issue-list'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))


(defcustom consult-gh-search-issues-args '("search" "issues")
  "Additional arguments for `consult-gh-search-issues'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-search-repos-args '("search" "repos" "--include-forks" "true")
  "Additional arguments for `consult-gh-search-repos'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-pr-list-args '("pr" "list" "--repo")
  "Additional arguments for `consult-gh-pr-list'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-search-prs-args '("search" "prs")
  "Additional arguments for `consult-gh-search-prs'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-search-code-args '("search" "code")
  "Additional arguments for `consult-gh-search-code'.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-notifications-show-unread-only t
  "Whether to hide read notifications?"
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-notifications-args-func #'consult-gh-notifications-make-args
  "Additional arguments for `consult-gh-notifications'.

Common options include:

 - `consult-gh-notifications-make-args' Make args to see unread notifications
 - A custom function                    A function that takes
                                        no input argument."
  :group 'consult-gh
  :type '(choice (const :tag "Default Function" consult-gh-notifications-make-args)
                 (function :tag "Custom Function")))

(defcustom consult-gh-browse-url-func #'browse-url
  "What function to call browsing a url?

The function should take at least one argument for url similar to
`browse-url'.

Common options include:

 - `browse-url'         Opens url in default browser
 - `eww-browse-url'     Open url in eww
 - `browse-url-firefox' Open url in firefox
 - `browse-url-chrome'  Open url in chrome"
  :group 'consult-gh
  :type '(choice (function :tag "Browse URL in default browser" browse-url)
                 (function :tag "Browse URL in EWW" eww-browse-url)
                 (function :tag "Browse URL in Firefox" browse-url-firefox)
                 (function :tag "Browse URL in Chrome" browse-url-chrome)
                 (function :tag "Custom Function")))

(defcustom consult-gh-switch-to-buffer-func #'switch-to-buffer
  "What function to call when switching buffers?

The function should take at least one argument for buffer similar to
`switch-to-buffer'.

Common options include:

 - `switch-to-buffer'              Switch to buffer in current window
 - `switch-to-buffer-other-window' Switch to buffer in other window
 - `switch-to-buffer-other-frame'  Switch to buffer in other frame
 - `switch-to-buffer-other-tab'    Switch to buffer in other tab"
  :group 'consult-gh
  :type '(choice (function :tag "(Default) Switch to buffer in current window" switch-to-buffer)
                 (function :tag "Switch to buffer in other window" switch-to-buffer-other-window)
                 (function :tag "Switch to buffer in other frame" switch-to-buffer-other-frame)
                 (function :tag "Switch to buffer in other tab" switch-to-buffer-other-tab)
                 (function :tag "Custom Function")))

(defcustom consult-gh-pop-to-buffer-func #'pop-to-buffer
  "What function to call when popping to buffers?

The function should take at least one argument for buffer similar to
`pop-to-buffer'.

Common options include:

 - `pop-to-buffer'                 Switch to buffer in current window
 - `switch-to-buffer-other-window' Switch to buffer in other window
 - `switch-to-buffer-other-frame'  Switch to buffer in other frame
 - `switch-to-buffer-other-tab'    Switch to buffer in other tab"
  :group 'consult-gh
  :type '(choice (function :tag "(Default) Pop to buffer in another" pop-to-buffer)
                 (function :tag "Switch to buffer in other window" switch-to-buffer-other-window)
                 (function :tag "Switch to buffer in other frame" switch-to-buffer-other-frame)
                 (function :tag "Switch to buffer in other tab" switch-to-buffer-other-tab)
                 (function :tag "Custom Function")))

(defcustom consult-gh-quit-window-func #'consult-gh-quit-window
  "What function to call when quitting windows?

The function should take two arguments similar to
`consult-gh-quit-window'.

Common options include:

 - `consult-gh-quit-window'  Quit or delete window
 - `quit-window'             Quit window"
  :group 'consult-gh
  :type '(choice (function :tag "(Default) Quite or delete window" consult-gh-quit-window)
                 (function :tag "Quit window" quit-window)
                 (function :tag "Custom Function")))

(defcustom consult-gh-dashboard-items-functions (list #'consult-gh--dashboard-collect-author #'consult-gh--dashboard-collect-assigned #'consult-gh--dashboard-collect-mentions #'consult-gh--dashboard-collect-involves)
  "A list of functions for collecting items in `consult-gh-dashboard'.

Each function in this list gets called in `consult-gh--dashboard-items'.
The function should accept an optional arg for user and
should return a list of candidates (relevant issues/pr for the user) to be
used in `consult-gh-dashboard'.  For an example see
`consult-gh--dashboard-collect-author'."
  :group 'consult-gh
  :type '(repeat function))

(defcustom consult-gh-tempdir (expand-file-name "consult-gh" temporary-file-directory)
  "Temporary file directory for the `consult-gh' package.

This directory is used for storing temporary files when
pulling files for viewing."
  :group 'consult-gh
  :type 'directory)

(make-obsolete-variable 'consult-gh-crm-separator nil "1.0")

(defcustom consult-gh-temp-tempdir-time-format "%Y%m%d%I%H%M"
  "Tme FORMAT-STRING for temporary directories.

This is passed as FORMAT-STRING to `format-time-string' for naming
temporary directories."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-temp-tempdir-cache 300
  "Time in seconds before making a new temp directory."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-repo-maxnum 30
  "Maximum number of repos to show for list and search operations.

This is the value passed to “--limit” in the command line.
The default is set to gh's default config, 30."
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-issue-maxnum 30
  "Maximum number of issues to show for list and search operations.

This is the value passed to “--limit” in the command line.
The default is set to gh's default config, 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-dashboard-maxnum 30
  "Maximum number of dashboard items to show for each search operations.

This is the value passed to “--limit” in the command line.
The default is set to 30."
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-pr-maxnum 30
  "Maximum number of PRs to show for list and search operations.

This is the value passed to “--limit” in the command line.
The default is set to gh's default config, 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-code-maxnum 30
  "Maximum number of codes to show for list and search operations.

This is the value passed to “--limit” in the command line.
The default is set to gh's default config, 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-comments-maxnum 30
  "Maximum number of comments to show when viewing issues or prs.

If there are more than this many comments, the user is queried about
whether to filer comments or not."
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-issues-state-to-show "open"
  "Which type of issues should be listed by `consult-gh-issue-list'?

This is what is passed to “--state” argument in the command line
when running `gh issue list`.

The possible options are “open”, “closed” or “all”."
  :group 'consult-gh
  :type '(choice (const :tag "Show open issues only" "open")
                 (const :tag "Show closed issues only" "closed")
                 (const :tag "Show all issues" "all")))

(defcustom consult-gh-prs-state-to-show "open"
  "Which type of PRs should be listed by `consult-gh-pr-list'?

This is what is passed to “--state” argument in the command line
when running `gh pr list`.

The possible options are “open”, “closed”, “merged”, or “all”."
  :group 'consult-gh
  :type '(choice (const :tag "Show open pull requests only" "open")
                 (const :tag "Show closed pull requests only" "closed")
                 (const :tag "Show all pull requests" "all")))

(defcustom consult-gh-prs-show-commits-in-view t
  "Whether to include all commits in `consult-gh--pr-view'?

Not including all commits make viewing long PRs faster."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-large-file-warning-threshold large-file-warning-threshold
  "Threshold for size of file to require confirmation for preview/open/save.

Files larger than this value in size will require user confirmation
before previewing, opening or saving the file.

Default value is set by `large-file-warning-threshold'.
If nil, no confirmation is required."
  :group 'consult-gh
  :type '(choice integer (const :tag "Never request confirmation" nil)))

(defcustom consult-gh-prioritize-local-folder 'suggest
  "How to use the local repository for completion?

There are three options, \='suggest, nil or t.

When set to \='suggest,  the git repository from the local folder
\(i.e. `default-directory')\ is added to the future history list
so it can quickly be accessed by `next-history-element' \(bound to
'\\[next-history-element]'\) when running commands such as
`consult-gh-issue-list' or `consult-gh-find-file'.

When set to t, the git repository from the local folder is used
as initial-input value for commands such as `consult-gh-issue-list'
or `consult-gh-find-file'.  The entry can still be changed by user input.
If there is no GitHub repository in the `default-directory',
it falls back to no initial input.

When set to nil, the git repository from the local folder is ignored and
no initial input is provided."

  :group 'consult-gh
  :type '(choice (const :tag "Current repository is in future history" suggest)
                 (const :tag "Current repository is default input" t)
                 (const :tag "Current repository is ignored" nil)))

(defcustom consult-gh-repo-preview-major-mode nil
  "Major mode to preview repository READMEs.

Choices are:
  - \='nil            Use major-mode associated with original file extension
  - \='gfm-mode       Use `gfm-mode'
  - \='markdown-mode  Use `markdown-mode'
  - \='org-mode       Use `org-mode'"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Guess major mode based on file format " nil)
                 (const :tag "Use GitHub flavor markdown mode" gfm-mode)
                 (const :tag "Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)))

(defcustom consult-gh-issue-preview-major-mode 'gfm-mode
  "Major mode to preview issues and pull requests.

Choices are:
  - \='nil            Use `fundamental-mode'
  - \='gfm-mode       Use `gfm-mode'
  - \='markdown-mode  Use `markdown-mode'
  - \='org-mode       Use `org-mode'"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use GitHub flavor markdown mode" gfm-mode)
                 (const :tag "Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)
                 (const :tag "Use fundamental-mode" nil)))

(defcustom consult-gh-topic-major-mode 'gfm-mode
  "Major mode for editing comments on issues or pull requests.

Choices are:
  - \='nil            Use `text-mode'
  - \='gfm-mode       Use `gfm-mode'
  - \='markdown-mode  Use `markdown-mode'
  - \='org-mode       Use `org-mode'"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use GitHub flavor markdown mode" gfm-mode)
                 (const :tag "Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)
                 (const :tag "Use text-mode" nil)))

(defcustom consult-gh-topic-use-capf t
  "Use `consult-gh--topics-edit-capf' for `completion-at-point'.

When non-nil, `consult-gh--topics-edit-capf' is used in
`consult-gh-topic-major-mode' buffer for autocompleting
issue/pr numbers or user names."
  :group 'consult-gh
  :type '(choice (const :tag "Use autocompletion" t)
                 (const :tag "Do not use autocompletion" nil)))


(make-obsolete-variable 'consult-gh-preview-buffer-mode "Use `consult-gh-repo-preview-major-mode', or `consult-gh-issue-preview-major-mode' instead." "1.1")

(defcustom consult-gh-favorite-orgs-list (list)
  "List of default GitHub orgs.

This can be a list of orgs or a function returning a list"
  :group 'consult-gh
  :type '(repeat (string :tag "GitHub Organization (i.e. Username)")))

(make-obsolete 'consult-gh-default-orgs-list 'consult-gh-favorite-orgs-list "2.0")

(defcustom consult-gh-preview-buffer-name "*consult-gh-preview*"
  "Default name for preview buffers."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-user-prefix "user "
  "Prefix label to use for users in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-issue-prefix "issue "
  "Prefix label to use for issues in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-pullrequest-prefix "pr "
  "Prefix label to use for pull requests in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-branch-prefix "branch "
  "Prefix label to use for milestones in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-label-prefix "label "
  "Prefix label to use for labels in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-project-prefix "project "
  "Prefix label to use for projects in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-milestone-prefix "milestone "
  "Prefix label to use for milestones in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-completion-max-items "2000"
  "Maximum number of items to load for autocomplete suggestions.

This is used in `consult-gh--topics-edit-capf'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-show-preview nil
  "Should `consult-gh' show previews?

It turns previews on/off globally for all categories
\(repos, issues, prs, codes, files,...\)"
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-preview-key consult-preview-key
  "What key to use to show preview for `consult-gh'?

This key is bound in minibuffer, and is similar to `consult-preview-key'
\(the default\) but explicitly for `consult-gh'.
This is used for all categories \(issues, prs, codes, files, etc.\)"
  :group 'consult-gh
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))

(defcustom consult-gh-group-by t
  "What field to use to group the results in the minibuffer?

By default it is set to t, but can be any of:

  t           Use headers for marginalia info
  nil         Do not group
  :user       Group by repository owner
  :type       Group by candidate's type (e.g. issue, pr, ....)
  :url        Group by URL
  :date       Group by the last updated date
  :visibility Group by visibility (e.g. public or private)
  symbol    Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "Repository's owner" :user)
                 (const :tag "Repository's package name" :package)
                 (const :tag "Type of Item" :type)))

(defcustom consult-gh-group-repos-by consult-gh-group-by
  "What field to use to group results in repo search?

This is used in `consult-gh-search-repos'.
By default it is set to t, but can be any of:

  t           Use headers for marginalia info
  nil         Do not group
  :user       Group by repository owner
  :package    Group by package name
  :date       Group by the last updated date
  :visibility Group by visibility (e.g. public or private)
  symbol      Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's owner" :user)
                 (const :tag "Repository's package name" :package)
                 (const :tag "Date the repo was last updated" :date)
                 (const :tag "Visibility (i.e. public, private,...)" :visibility)))

(defcustom consult-gh-group-issues-by consult-gh-group-by
  "What field to use to group results in issue search?

This is used in `consult-gh-search-issues'.
By default it is set to t, but can be any of:

  t         Use headers for marginalia info
  nil       Do not group
  :repo     Group by repository full name
  :state    Group by status og issue (i.e. open or closed)
  :user     Group by repository owner
  :package  Group by package name
  :date     Group by the last updated date
  symbol    Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "State of issue (e.g. open or closes)" :state)
                 (const :tag "Repository's owner" :user)
                 (const :tag "Repository's package name" :package)
                 (const :tag "Date the repo was last updated" :date)))

(defcustom consult-gh-group-prs-by consult-gh-group-by
  "What field to use to group results in pull request search?

This is used in `consult-gh-search-prs'.
By default it is set to t, but can be any of:

  t        Use headers for marginalia info
  nil      Do not group
  :repo    Group by repository full name
  :state   Group by status og issue (i.e. open or closed)
  :user    Group by repository owner
  :package Group by package name
  :date    Group by the last updated date
  symbol   Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "State of issue (e.g. open or closes)" :state)
                 (const :tag "Repository's owner" :user)
                 (const :tag "Repository's package name" :package)
                 (const :tag "Date the repo was last updated" :date)))

(defcustom consult-gh-group-files-by consult-gh-group-by
  "What field to use to group results in file search?

This is used in `consult-gh-search-codes'.
By default it is set to t, but can be any of:

  t        Use headers for marginalia info
  nil      Do not group
  :repo    Group by repository full name
  :user    Group by repository owner
  :package Group by package name
  :path    Group by the file path
  symbol   Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "Repository's owner" :user)
                 (const :tag "Repository's package name" :package)
                 (const :tag "File path relative to repo's root" :path)))

(defcustom consult-gh-group-code-by consult-gh-group-by
  "What field to use to group results in code search?

This is used in `consult-gh-search-codes'.
By default it is set to t, but can be any of:

  t        Use headers for marginalia info
  nil      Do not group
  :repo    Group by repository full name
  :user    Group by repository owner
  :package Group by package name
  :path    Group by the file path
  symbol   Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "Repository's owner" :user)
                 (const :tag "Repository's package name" :package)
                 (const :tag "File path relative to repo's root" :path)))

(defcustom consult-gh-group-dashboard-by consult-gh-group-by
  "What field to use to group results in code search?

This is used in `consult-gh-dashboard'.
By default it is set to t, but can be any of:

  t       Use headers for marginalia info
  nil     Do not group
  :repo   Group by repository full name
  :reason Group by the reason (e.g. mentions)
  :date   Group by the last updated date
  :type   Group by candidate's type (e.g. issue, pr, ....)
  symbol  Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "The reason (e.g. mentions)" :reason)
                 (const :tag "Date the repo was last updated" :date)
                 (const :tag "Type of Item" :type)))

(defcustom consult-gh-group-notifications-by consult-gh-group-by
  "What field to use to group results in notifications?

This is used in `consult-gh-notifications'.
By default it is set to t, but can be any of:

  t       Use headers for marginalia info
  nil     Do not group
  :repo   Group by repository full name
  :reason Group by the reason (e.g. mentions, comment, ...)
  :date   Group by the last updated date
  :type   Group by candidate's type (e.g. issue, pr, ....)
  :state  Group by status of issue (i.e. unread or read)
  symbol  Group by another property of the candidate"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use Headers of Marginalia Info" t)
                 (const :tag "Do Not Group" nil)
                 (const :tag "Repository's full name" :repo)
                 (const :tag "The reason (e.g. mentions)" :reason)
                 (const :tag "Date the repo was last updated" :date)
                 (const :tag "State of issue (e.g. unread or read)" :state)
                 (const :tag "Type of Item" :type)))

(defcustom consult-gh-default-clone-directory "~/"
  "Where should GitHub repos be cloned to by default?"
  :group 'consult-gh
  :type 'directory)

(defcustom consult-gh-default-save-directory "~/Downloads/"
  "Where should single files be saved by default?

Note that this is used for saving individual files
\(see `consult-gh--files-save-file-action'\),
and not cloning entire repositories."
  :group 'consult-gh
  :type 'directory)

(defcustom consult-gh-confirm-before-clone t
  "Should confirmation of path and name be requested before cloning?

When set to nil, the default directory
`consult-gh-default-clone-directory' and package name are used
without confirmation."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-confirm-name-before-fork nil
  "Should the new repository name be confirmed when forking a repository?

When set to nil \(default\), the original repo's name will be used,
otherwise request a name."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-ask-for-path-before-save t
  "Should file path be confirmed when saving files?

When set to nil, the default directory \(`consult-gh-default-save-directory'\),
and the buffer file name \(variable `buffer-file-name'\) are used,
otherwise a file path is requested."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-default-branch-to-load 'ask
  "Which branch of repository to load by default in `consult-gh-find-file'?

Possible values are:

  - \='confirm:  Ask for confirmation if “HEAD” branch should be loaded.
               If not, then the user can choose a different branch.
  - \='ask:      Asks the user to select a branch.
  - \='nil:      load the “HEAD” branch, no questions asked.
  - A symbol:  loads the branch named in this variable.

Note that when this is set to a specific branch,
it is used for any repository that is fetched and if the branch does not exist,
it will cause an error.  Therefore, using a specific branch is not recommended
as a general case but in temporary settings where one is sure the branch exists
on the repositories being fetched."

  :group 'consult-gh
  :type '(choice (const :tag "Ask for a branch name" ask)
                 (const :tag "Ask user to confirm loading HEAD, and if \"No\", ask for a branch name" confirm)
                 (const :tag "Loads the HEAD Branch, without confirmation"
                        nil)
                 (symbol :tag "Loads Specific Branch")))

(defcustom consult-gh-repo-action #'consult-gh--repo-browse-url-action
  "What function to call when a repo is selected?

Common options include:

 - `consult-gh--repo-browse-url-action'   Opens url in default browser

 - `consult-gh--repo-browse-files-action' Open files in Emacs

 - `consult-gh--repo-view-action'         Open repository's READMEe in Emacs

 - `consult-gh--repo-clone-action'        Clone the repository

 - `consult-gh--repo-fork-action'         Fork the repository

 - A custom function:                     A function that takes
                                          only 1 input argument,
                                          the repo candidate."
  :group 'consult-gh
  :type '(choice (function :tag "Browse the Repository URL in default browser" #'consult-gh--repo-browse-url-action)
                 (function :tag "Open the Repository's README in an Emacs buffer" #'consult-gh--repo-view-action)
                 (function :tag "Browse Branches and Files inside Emacs" #'consult-gh--repo-browse-files-action)
                 (function :tag "Clone Repository to local folder" #'consult-gh--repo-clone-action)
                 (function :tag "Fork Repository" #'consult-gh--repo-fork-action)
                 (function :tag "Custom Function")))

(defcustom consult-gh-issue-action #'consult-gh--issue-browse-url-action
  "What function to call when an issue is selected?

Common options include:

 - `consult-gh--issue-browse-url-action' Opens the issue url in default browser

 - `consult-gh--issue-view-action'       Opens issue in Emacs

 - `consult-gh-forge--issue-view-action' Opens issue in `magit-forge'.
                                         \(requires `consult-gh-forge' library\)

 - A custom function                     A function that takes
                                         only 1 input argument,
                                         the issue candidate."
  :group 'consult-gh
  :type (if (featurep 'consult-gh-forge) '(choice (const :tag "Browse the Issue URL in default browser" #'consult-gh--issue-browse-url-action)
                                                  (const :tag "Open the Issue in an Emacs buffer" #'consult-gh--issue-view-action)
                                                  (const :tag "Open the Issue in a Magit/Forge buffer" #'consult-gh-forge--issue-view-action)
                                                  (function :tag "Custom Function"))
          '(choice (const :tag "Open the Issue URL in default browser" #'consult-gh--issue-browse-url-action)
                   (const :tag "Open the Issue in an Emacs buffer" #'consult-gh--issue-view-action)
                   (const :tag "Open the Issue in a Magit/Forge buffer" #'consult-gh-forge--issue-view-action)
                   (function :tag "Custom Function"))))

(defcustom consult-gh-pr-action #'consult-gh--pr-browse-url-action
  "What function to call when a pull request is selected?

Common options include:

 - `consult-gh--pr-browse-url-action' opens the PR url in default browser

 - `consult-gh--pr-view-action'       opens PR in Emacs

 - `consult-gh-forge--pr-view-action' Open PR in a `magit-forge'
                                      \(requires `consult-gh-forge' library\)

 - A custom function                  A function that takes only
                                      1 input argument,
                                      the PR candidate."
  :group 'consult-gh
  :type (if (featurep 'consult-gh-forge) '(choice (const :tag "Browse the PR URL in default browser" #'consult-gh--pr-browse-url-action)
                                                  (const :tag "Open the PR in an Emacs buffer" #'consult-gh--pr-view-action)
                                                  (const :tag "Open the PR in a Magit/Forge buffer" #'consult-gh-forge--pr-view-action)
                                                  (function :tag "Custom Function"))
          '(choice (const :tag "Open the PR URL in default browser" #'consult-gh--pr-browse-url-action)
                   (const :tag "Open the PR in an Emacs buffer" #'consult-gh--pr-view-action)
                   (function :tag "Custom Function"))))

(defcustom consult-gh-code-action #'consult-gh--code-browse-url-action
  "What function to call when a code is selected?

Common options include:

 - `consult-gh--code-browse-url-action' Opens the code in default browser

 - `consult-gh--pr-view-action'         Opens the codein Emacs

 - A custom function                    A function that takes
                                        only 1 input argument,
                                        the code candidate."
  :group 'consult-gh
  :type '(choice (const :tag "Browse the Code (target file) URL in default browser" consult-gh--code-browse-url-action)
                 (const :tag "Open code (target file) in an Emacs buffer" consult-gh--code-view-action)
                 (function :tag "Custom Function")))

(defcustom consult-gh-file-action #'consult-gh--files-browse-url-action
  "What function to call when a file is selected?

Common options include:

 - `consult-gh--files-browse-url-action' Opens the file url  in default browser

 - `consult-gh--files-view-action'       Opens the file in Emacs

 - A custom function                     A function that takes
                                         only 1 input argument,
                                         the file candidate."
  :group 'consult-gh
  :type '(choice (const :tag "Browse the File URL" consult-gh--files-browse-url-action)
                 (const :tag "Save the File to local folder" consult-gh--files-view-action)
                 (function :tag "Custom Function")))

(defcustom consult-gh-discussion-action #'consult-gh--discussion-browse-url-action
  "What function to call when a discussion is selected?

Common options include:

 - `consult-gh--discussion-browse-url-action' Opens the notification url
                                              in default browser
 - A custom function                          A function that takes
                                              only 1 input argument,
                                              the notification candidate."
  :group 'consult-gh
  :type '(choice (const :tag "Browse the Discussion URL" consult-gh--discussion-browse-url-action)
                 (function :tag "Custom Function")))

(defcustom consult-gh-notifications-action #'consult-gh--notifications-action
  "What function to call when a notification is selected?

Common options include:

 - `consult-gh--notifications-action'            Uses default action of
                                                 item type (e.g. issue,
                                                 pr, discussion,...)
 - `consult-gh--notifications-browse-url-action' Open relevant
                                                 notifications in external
                                                 browser
 - A custom function                             A function that takes
                                                 only 1 input argument,
                                                 the notification
                                                 candidate."
  :group 'consult-gh
  :type '(choice (const :tag "Use Default Action of Item Type (e.g. issue, pr, ...)" consult-gh--notifications-action)
                 (const :tag "Open relevant notifications in the browser)" consult-gh--notifications-browse-url-action)
                 (function :tag "Custom Function")))

(defcustom consult-gh-dashboard-action #'consult-gh--dashboard-action
  "What function to call when a dashboard item is selected?

Common options include:

 - `consult-gh--dashboard-action'            Uses default action of item type
                                             (e.g. issue or pr)
 - `consult-gh--dashboard-browse-url-action' Opens the link in an external
                                             browser
 - A custom function                         A function that takes
                                             only 1 input argument,
                                             the dashboard candidate."
  :group 'consult-gh
  :type '(choice (const :tag "Use Default Action of Item Type (e.g. issue, pr, ...)" consult-gh--dashboard-action)
                 (const :tag "Open Issue/PR in external browser" consult-gh--dashboard-browse-url-action)
                 (function :tag "Custom Function")))

(defcustom consult-gh-highlight-matches t
  "Should queries or code snippets be highlighted in preview buffers?"
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-default-interactive-command #'consult-gh-search-repos
  "Which command should `consult-gh' call?"
  :group 'consult-gh
  :type '(choice (function :tag "(Default) Search Repositories"  consult-gh-search-repos)
                 (function :tag "List default repos of user" consult-gh-favorite-repos)
                 (function :tag "Open transient menu" consult-gh-transient)
                 (function :tag "Other custom interactive command")))


(defcustom consult-gh-use-search-to-find-name nil
  "Whether to use `consult-gh-search-repos' to find repo name.

If this is set to non-nil, consult-gh calls `consult-gh-search-repos'
to get the repo name before running `consult-gh-issue-list',
`consult-gh-pr-list', etc.

This is useful if you do not remember package names and want to do a
search first."
  :group 'consult-gh
  :type 'boolean)

;;; Other Variables
(defvar consult-gh-category 'consult-gh
  "Category symbol for the `consult-gh' package.")

(defvar consult-gh-repos-category 'consult-gh-repos
  "Category symbol for repos in `consult-gh' package.")

(defvar consult-gh-issues-category 'consult-gh-issues
  "Category symbol for issues in `consult-gh' package.")

(defvar consult-gh-prs-category 'consult-gh-prs
  "Category symbol for pull requests in `consult-gh' package.")

(defvar consult-gh-codes-category 'consult-gh-codes
  "Category symbol for codes in `consult-gh' package.")

(defvar consult-gh-notifications-category 'consult-gh-notifications
  "Category symbol for notifications in `consult-gh' package.")

(defvar consult-gh-orgs-category 'consult-gh-orgs
  "Category symbol for orgs in `consult-gh' package.")

(defvar consult-gh-files-category 'consult-gh-files
  "Category symbol for files in `consult-gh' package.")

(defvar consult-gh--preview-buffers-list (list)
  "List of currently open preview buffers.")

(defvar consult-gh--orgs-history nil
  "History variable for orgs used in `consult-gh-repo-list'.")

(defvar consult-gh--repos-history nil
  "History variable for repos.

This is used in `consult-gh-issue-list' and `consult-gh-pr-list'.")

(defvar consult-gh--notifications-history nil
  "History variable for notifications.

This is used in `consult-gh-notifications'.")

(defvar consult-gh--dashboard-history nil
  "History variable for dashboard.

This is used in `consult-gh-dashboard'.")

(defvar consult-gh--search-repos-history nil
  "History variable for searching repos in `consult-gh-search-repos'.")

(defvar consult-gh--search-issues-history nil
  "History variable for issues used in `consult-gh-search-issues'.")

(defvar consult-gh--search-prs-history nil
  "History variable for pull requests used in `consult-gh-search-prs'.")

(defvar consult-gh--search-code-history nil
  "History variable for codes used in `consult-gh-search-code'.")

(defvar consult-gh--files-history nil
  "History variable for files used in `consult-gh-find-file'.")

(defvar consult-gh--current-user-orgs nil
  "List of repos of current user.")

(defvar consult-gh--known-orgs-list nil
  "List of previously visited orgs.")

(defvar consult-gh--known-repos-list nil
  "List of previously visited repos.")

(defvar consult-gh--open-files-list nil
  "List of currently open files.")

(defvar consult-gh--current-tempdir nil
  "Current temporary directory.")

(defvar consult-gh--async-process-buffer-name " *consult-gh-async*"
  "Name of buffer for async processes.")

(defvar consult-gh--async-log-buffer " *consult-gh-async-log*"
  "Name of buffer for logging async process errors.")

(defvar consult-gh--current-input nil
  "Current input of user query.")

(defvar consult-gh--auth-current-account nil
  "Current logged-in and active account.

This is a list of \='(USERNAME HOST IF-ACTIVE)")

(defvar consult-gh-default-host "github.com"
  "Default host of GitHub.")

(defvar-local consult-gh--topic nil
  "Topic in consult-gh preview buffers.")

(defvar consult-gh--override-group-by nil
  "Override grouping based on user input.

This is used to change grouping dynamically.")

(defvar consult-gh--issue-view-json-fields "assignees,author,body,closedAt,createdAt,labels,milestone,number,projectItems,state,title,updatedAt,url"
  "String of comma separated json fields to retrieve for viewing issues.")

(defvar consult-gh--pr-view-json-fields "additions,assignees,author,baseRefName,body,closedAt,commits,createdAt,deletions,files,headRefName,headRepository,headRepositoryOwner,headRefOid,labels,mergeable,milestone,number,projectItems,reviewDecision,reviewRequests,state,statusCheckRollup,title,updatedAt,url"
  "String of comma separated json fields to retrieve for viewing prs.")

(defvar consult-gh--issue-view-mode-keybinding-alist '(("C-c C-c" . consult-gh-topics-comment-create)
                                                       ("C-c C-e" . consult-gh-issue-edit)
                                                       ("C-c C-<return>" . consult-gh-topics-open-in-browser))

  "Keymap alist for `consult-gh-issue-view-mode'.")

(defvar consult-gh--pr-view-mode-keybinding-alist '(("C-c C-c" . consult-gh-topics-comment-create)
                                                    ("C-c C-e" . consult-gh-pr-edit)
                                                    ("C-c C-m" . consult-gh-pr-merge)
                                                    ("C-c C-r" . consult-gh-pr-review)
                                                    ("C-c C-<return>" . consult-gh-topics-open-in-browser))

  "Keymap alist for `consult-gh-pr-view-mode'.")

(defvar consult-gh--repo-view-mode-keybinding-alist '(("C-c C-<return>" . consult-gh-topics-open-in-browser))

  "Keymap alist for `consult-gh-repo-view-mode'.")

(defvar consult-gh--topics-edit-mode-keybinding-alist '(("C-c C-c" . consult-gh-topics-submit)
                                                        ("C-c C-k" . consult-gh-topics-cancel))

  "Keymap alist for `consult-gh-topics-edit-mode'.")

;;; Faces
(defface consult-gh-success
  `((t :inherit 'success))
  "The face used to show issues or PRS that are successfully dealt with.

\(e.g. “closed” issues or “merged” PRS)\ when listing or searching
issues and PRS with `consult-gh'.

By default inherits from `success'.")

(defface consult-gh-warning
  `((t :inherit 'warning))
  "The face to show currently open issues or PRS.

By default inherits from `warning'.")

(defface consult-gh-error
  `((t :inherit 'error))
  "The face to show closed PRS.

By default inherits from `error'.")

(defface consult-gh-highlight-match
  `((t :inherit 'consult-highlight-match))
  "Highlight match face in preview buffers.

By default, inherits from `consult-highlight-match'.")

(defface consult-gh-preview-match
  `((t :inherit 'consult-preview-match))
  "Highlight match face in preview buffers.

 By default, inherits from `consult-preview-match'.
This face is for example used to highlight the matches to the user's
search queries \(e.g. when using `consult-gh-search-repos')\ or
code snippets \(e.g. when using `consult-gh-search-code')\ in preview buffer.")

(defface consult-gh-default
  `((t :inherit 'default))
  "Default face in minibuffer annotations.

By default, inherits from `default'.")

(defface consult-gh-user
  `((t :inherit 'font-lock-constant-face))
  "User face in minibuffer annotations.

By default, inherits from `font-lock-constant-face'.")

(defface consult-gh-package
  `((t :inherit 'font-lock-type-face))
  "Packageface in minibuffer annotations.

By default, inherits from `font-lock-type-face'.")

(defface consult-gh-repo
  `((t :inherit 'font-lock-type-face))
  "Repository face in minibuffer annotations.

By default, inherits from `font-lock-type-face'.")

(defface consult-gh-issue
  `((t :inherit 'warning))
  "Issue number face in minibuffer annotations.

By default, inherits from `warning'.")

(defface consult-gh-pr
  `((t :inherit 'warning))
  "Pull request number face in minibuffer annotations.

By default, inherits from `warning'.")


(defface consult-gh-branch
  `((t :inherit 'font-lock-string-face))
  "Branch face in minibuffer annotations.

By default, inherits from `font-lock-string-face'.")

(defface consult-gh-visibility
  `((t :inherit 'font-lock-warning-face))
  "Visibility face in minibuffer annotations.

By default, inherits from `font-lock-warning-face'.")

(defface consult-gh-date
  `((t :inherit 'font-lock-keyword-face))
  "Date face in minibuffer annotations.

By default, inherits from `font-lock-keyword-face'.")

(defface consult-gh-tags
  `((t :inherit 'font-lock-comment-face))
  "Tags/Comments face in minibuffer annotations.

By default, inherits from `font-lock-comment-face'.")

(defface consult-gh-description
  `((t :inherit 'font-lock-builtin-face))
  "Repository description face in minibuffer annotations.

By default, inherits from `font-lock-builtin-face'.")

(defface consult-gh-code
  `((t :inherit 'font-lock-variable-use-face))
  "Code snippets face in minibuffer annotations.

By default, inherits from `font-lock-vairable-use-face'.")

(defface consult-gh-url
  `((t :inherit 'link))
  "URL face in minibuffer annotations.

By default, inherits from `link'.")

;;; Utility functions

(defun consult-gh--nonutf-cleanup (string)
  "Remove non UTF-8 characters if any in the STRING."
  (string-join
   (delq nil (mapcar (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                     string))))

(defun consult-gh--set-string-width (string width &optional prepend char)
  "Set the STRING width to a fixed value, WIDTH.

If the String is longer than WIDTH, it truncates
the string and adds an ellipsis, “...”.
If the string is shorter it adds whitespace to the string.
If PREPEND is non-nil, it truncates or adds whitespace from
the beginning of string, instead of the end.
if CHAR is non-nil, uses char instead of whitespace."
  (let* ((string (format "%s" string))
         (w (length string)))
    (when (< w width)
      (if prepend
          (setq string (format "%s%s" (make-string (- width w) (or char ?\s)) (substring string)))
        (setq string (format "%s%s" (substring string) (make-string (- width w) (or char ?\s))))))
    (when (> w width)
      (if prepend
          (setq string (format "%s%s" (propertize (substring string 0 (- w (- width 3))) 'display "...") (substring string (- w (- width 3)) w)))
        (setq string (format "%s%s" (substring string 0 (- width (+ w 3))) (propertize (substring string (- width (+ w 3)) w) 'display "...")))))
    string))

(defun consult-gh--justify-left (string prefix maxwidth &optional char)
  "Set the width of STRING+PREFIX justified from left.

It uses `consult-gh--set-string-width' and sets the width
of the concatenated of STRING+PREFIX \(e.g. “\(concat prefix string\)”\)
within MAXWIDTH or a fraction of MAXWIDTH.  This is used for aligning
 marginalia info in minibuffer when using `consult-gh'.

If optional argument CHAR is non-nil uses it insted of whitespace."
  (let ((s (length string))
        (w (length prefix)))
    (cond ((< (+ s w) (floor (/ maxwidth 2)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 2))  w) t char))
          ((< (+ s w) (floor (/ maxwidth 1.8)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.8))  w) t char))
          ((< (+ s w) (floor (/ maxwidth 1.6)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.6))  w) t char))
          ((< (+ s w) (floor (/ maxwidth 1.4)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.4)) w) t char))
          ((< (+ s w) (floor (/ maxwidth 1.2)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.2)) w) t char))
          ((< (+ s w) maxwidth)
           (consult-gh--set-string-width string (- maxwidth w) t char))
          (t string))))

(defun consult-gh--highlight-match (regexp str ignore-case)
  "Highlight REGEXP in STR.

If a regular expression contains capturing groups, only these are highlighted.
If no capturing groups are used highlight the whole match.  Case is ignored
if IGNORE-CASE is non-nil.
\(This is adapted from `consult--highlight-regexps'.\)"
  (let ((i 0))
    (while (and (let ((case-fold-search ignore-case))
                  (string-match regexp str i))
                (> (match-end 0) i))
      (let ((m (match-data)))
        (setq i (cadr m)
              m (or (cddr m) m))
        (while m
          (when (car m)
            (add-face-text-property (car m) (cadr m)
                                    'consult-gh-highlight-match nil str))
          (setq m (cddr m))))))
  str)

(defun consult-gh--whole-buffer-string (&optional buffer)
  "Get whole content of the BUFFER or current buffer.

it widens the buffer to get whole content not just narrowed region."
  (with-current-buffer (or (and (buffer-live-p buffer) buffer)  (current-buffer))
    (save-restriction
      (widen)
      (buffer-string))))

(defun consult-gh--markdown-to-org-footnotes (&optional buffer)
  "Convert Markdown style footnotes to \='org-mode style footnotes in BUFFER.

Uses simple regexp replacements."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-max))
          (insert "\n")
          (while (re-search-backward "^\\[\\^\\(?1:.*\\)\\]:\s" nil t)
            (replace-match "[fn:\\1]")))))
    nil))

(defun consult-gh--markdown-to-org-emphasis (&optional buffer)
  "Convert markdown style markings to \='org-mode style emphasis in BUFFER.

Uses simple regexp replacements."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-min))
          (while (re-search-forward "#\\|^\\*\s\\|\\*\\{1,2\\}\\(?1:[^\s].+?\\)\\*\\{1,2\\}\\|\\(?1:^\\*+?\\)\s\\|_\\{1,2\\}\\(?2:[^\s].+?\\)_\\{1,2\\}\\|~\\{1,2\\}\\(?2:[^\s].+?\\)~\\{1,2\\}\\|`\\(?3:[^`].+?\\)`\\|```\\(?4:.*\n\\)\\(?5:[[:ascii:][:nonascii:]]*?\\)```" nil t)
            (pcase (match-string-no-properties 0)
              ("#" (cond
                    ((looking-at "#\\|[[:blank:]]")
                     (delete-char -1)
                     (insert (propertize "*" :consult-gh (get-text-property 0 :consult-gh (match-string 0)))))
                    ((looking-at "\\+begin.+\\|\\+end.+")
                     (delete-char -1)
                     (insert (propertize ",#":consult-gh (get-text-property 0 :consult-gh (match-string 0)))))))

              ("* "
               (delete-char -2)
               (insert (propertize "- " :consult-gh (get-text-property 0 :consult-gh (match-string 0)))))

              ((pred (lambda (el) (string-match-p "```.*\n[[:ascii:][:nonascii:]]*```" el)))
               (replace-match (concat
                               (propertize (concat  "#+begin_src " (match-string 4) "\n") :consult-gh (get-text-property 0 :consult-gh (match-string 4)))
                               (concat (match-string 5) "\n")
                               (propertize "#+end_src\n" :consult-gh (get-text-property 0 :consult-gh (match-string 4))))
                              nil t))

              ((pred (lambda (el) (string-match-p "^\\*+\s" el)))
               (replace-match (propertize (concat (make-string (length (match-string 1)) ?-) " ")
:consult-gh (get-text-property 0 :consult-gh (match-string 1)))
                              nil t))

              ((pred (lambda (el) (string-match-p "#\\+begin.+" el)))
               (replace-match (propertize (concat "," (match-string 1)) :consult-gh (get-text-property 0 :consult-gh (match-string 1))) nil t))

              ((pred (lambda (el) (string-match-p "#\\+end.+" el)))
               (replace-match (propertize (concat "," (match-string 1)) :consult-gh (get-text-property 0 :consult-gh (match-string 1))) nil t))

              ((pred (lambda (el) (string-match-p "\\*\\{2\\}[^\s].+?\\*\\{2\\}" el)))
               (replace-match (propertize (concat "*" (match-string 1) "*") :consult-gh (get-text-property 0 :consult-gh (match-string 1))) nil t))

              ((pred (lambda (el) (string-match-p "\\*\\{1\\}[^[\\*\s]].+?\\*\\{1\\}" el)))
               (replace-match (propertize (concat "/" (match-string 1) "/") :consult-gh (get-text-property 0 :consult-gh (match-string 1))) nil t))

              ((pred (lambda (el) (string-match-p "_\\{2\\}.+?_\\{2\\}" el)))
               (replace-match (propertize (concat "*" (match-string 2) "*") :consult-gh (get-text-property 0 :consult-gh (match-string 2))) nil t))

              ((pred (lambda (el) (string-match-p "_\\{1\\}[^_]*?_\\{1\\}" el)))
               (replace-match (propertize (concat "/" (match-string 2) "/") :consult-gh (get-text-property 0 :consult-gh (match-string 0))) nil t))

              ((pred (lambda (el) (string-match-p "~\\{1,2\\}.+?~\\{1,2\\}" el)))
               (replace-match (propertize (concat "+" (match-string 2) "+") :consult-gh (get-text-property 0 :consult-gh (match-string 2))) nil t))

              ((pred (lambda (el) (string-match-p "`[^`].+?`" el)))
               (replace-match (propertize (concat "=" (match-string 3) "=") :consult-gh (get-text-property 0 :consult-gh (match-string 0))) nil t)))))))
    nil))

(defun consult-gh--markdown-to-org-links (&optional buffer)
  "Convert markdown style links to \='org-mode links in BUFFER.

Uses simple regexp replacements."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-min))
          (while (re-search-forward "\\[\\^\\(?1:[^\]\[]+?\\)\\]:\s\\(?2:.*\\)$\\|\\[\\^\\(?3:[^\]\[]+?\\)\\]\\{1\\}\\|\\[\\(?4:[^\]\[]+?\\)\\]\(#\\(?5:.+?\\)\)\\{1\\}\\|.\\[\\(?6:[^\]\[]+?\\)\\]\(\\(?7:[^#].+?\\)\)\\{1\\}\\|\\[\\(?8:[^\]\[]+?\\)\\]\(\\(?9:.+?\\)\)\\{1\\}" nil t)
            (pcase (match-string-no-properties 0)
              ((pred (lambda (el) (string-match-p "^\\[\\^.+?\\]:\s.*$" el)))
               (replace-match "[fn:\\1] \\2"))

              ((pred (lambda (el) (string-match-p "\\[\\^.+?\\]\\{1\\}" el)))
               (replace-match "[fn:\\3]"))

              ((pred (lambda (el) (string-match-p "\\[.+?\\]\(#.+?\)\\{1\\}" el)))
               (replace-match "[[*\\5][\\4]]"))

              ((pred (lambda (el) (string-match-p "!\\[.*\\]\([^#].*\)" el)))
               (replace-match "[[\\7][\\6]]"))

              ((pred (lambda (el) (string-match-p "[[:blank:]]\\[.*\\]\([^#].*\)" el)))
               (replace-match " [[\\7][\\6]]"))

              ((pred (lambda (el) (string-match-p "\\[.+?\\]\(.+?\)\\{1\\}" el)))
               (replace-match "[[\\9][\\8]]"))))

          (goto-char (point-min))
          (while
              (re-search-forward
               "\\[fn:\\(.+?\\)\\]\\{1\\}" nil t)
            (pcase (match-string 0)
              ((pred (lambda (el) (string-match-p "\\[fn:.+?[[:blank:]].+?\\]\\{1\\}" (substring-no-properties el))))
               (progn
                 (replace-regexp-in-region "[[:blank:]]" "_" (match-beginning 1) (match-end 1)))))))))
    nil))

(defun consult-gh--github-header-to-org (&optional buffer)
"Convert GitHub's default markdown header to \='org-mode in BUFFER."
(let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^-\\{2\\}$" nil t)
            (delete-char -2)
            (insert "-----\n")
            (while (re-search-backward "\\(^[a-zA-Z]+:[[:blank:]]\\)" nil t)
              (replace-match "#+\\1" nil nil))))))))

(defun consult-gh--markdown-to-org (&optional buffer)
  "Convert from markdown format to \='org-mode format in BUFFER.

This is used for viewing repos \(a.k.a. fetching README file of repos\)
or issue, when `consult-gh-repo-preview-major-mode' or
`consult-gh-issue-preview-major-mode'  is set to \='org-mode."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (consult-gh--markdown-to-org-footnotes buffer)
      (consult-gh--markdown-to-org-emphasis buffer)
      (consult-gh--markdown-to-org-links buffer)
      (org-mode)
      (org-table-map-tables 'org-table-align t)
      (org-fold-show-all)
      (goto-char (point-min))))
  nil)

(defun consult-gh-recenter (&optional pos)
  "Recenter the text in a window so that the cursor is at POS.

POS a symbol and can be \='top, \='bottom or \='middle.
The default is \='middle so if POS is nil or anything else,
the text will be centered in the middle of the window."
  (let ((this-scroll-margin
	 (min (max 0 scroll-margin)
	      (truncate (/ (window-body-height) 4.0))))
        (pos (or pos 'middle)))
    (pcase pos
      ('middle
       (recenter nil t))
      ('top
       (recenter this-scroll-margin t))
      ('bottom
       (recenter (- -1 this-scroll-margin) t))
      (_
       (recenter nil t)))))

(defun consult-gh--org-to-markdown (&optional buffer)
  "Convert content of BUFFER from org format to markdown.

This is used for creating or editing comments, issues, pull requests,
etc. in org format.  It Uses `ox-gfm' for the conversion."
  (when (derived-mode-p 'org-mode)
    (let* ((org-export-with-toc nil)
           (org-export-preserve-breaks t)
           (text (consult-gh--whole-buffer-string buffer)))
      (save-mark-and-excursion
        (with-temp-buffer
          (and (stringp text) (insert text))
          (save-window-excursion (ignore-errors
                                   (org-export-to-buffer 'gfm (current-buffer)))
                                 (buffer-string)))))))

(defun consult-gh--time-ago (datetime)
  "Convert DATETIME to human-radable time difference.

DATETIME must be a time string in the past.
It returns strings like “1 year ago”, “30 minutes ago”."
  (when (stringp datetime) (setq datetime (date-to-time datetime)))
  (let* ((delta (float-time (time-subtract (current-time) datetime)))
         (years (format-seconds "%y" delta))
         (days (and (<= (string-to-number years) 0) (format-seconds "%d" delta)))
         (months (and days (>= (string-to-number days) 30) (number-to-string (/ (string-to-number days) 30))))
         (hours (and days (<= (string-to-number days) 0) (format-seconds "%h" delta)))
         (minutes (and hours (<= (string-to-number hours) 0) (format-seconds "%m" delta)))
         (seconds (and minutes (<= (string-to-number minutes) 0) (format-seconds "%s" delta))))
    (or  (and seconds (concat seconds " second(s) ago"))
         (and minutes (concat minutes " minute(s) ago"))
         (and hours (concat hours " hour(s) ago"))
         (and months (concat months " month(s) ago"))
         (and days (concat days " day(s) ago"))
         (and years (concat years " year(s) ago"))
         "now")))

(defun consult-gh--get-region-with-prop (prop &optional buffer beg end)
  "Get region with property PROP from BUFFER.

When optional arguments BEG and END are no-nil, limit the search between
BEG and END positions."
  (with-current-buffer (or buffer (current-buffer))
    (unless  (= (buffer-size buffer) 0)
    (save-excursion
      (goto-char (or beg (point-min)))
      (let* ((regions nil)
             (begin (point))
             (isProp (get-text-property (point) prop)))
        (while-let ((next (and (< begin (or end (point-max))) (next-single-property-change begin prop nil end))))
          (goto-char next)
          (when (and (get-text-property (- (point) 1) prop) isProp)
            (push (cons (set-marker (make-marker) begin) (point-marker)) regions))
          (setq begin (point))
          (setq isProp (get-text-property (point) prop)))
        (goto-char (or end (point-max)))
        (when (and (get-text-property (- (point) 1) prop) isProp)
          (push (cons (set-marker (make-marker) begin) (point-marker)) regions))
        (nreverse regions))))))

(defun consult-gh--delete-region-with-prop (prop &optional buffer beg end)
  "Remove any text with property PROP from BUFFER.

When optional arguments BEG and END are non-nil, limit the search between
BEG and END positions."

  (let ((regions (consult-gh--get-region-with-prop prop buffer beg end)))
    (when (and regions (listp regions))
      (cl-loop for region in regions
               do
               (let ((p1 (car region))
                     (p2 (min (cdr region) (point-max))))
               (delete-region p1 p2))))))

(defun consult-gh--get-region-with-overlay (symbol &optional buffer beg end)
  "Get regions with SYMBOL overlay from BUFFER.

When BEG and END are non-nil, look in the region between
BEG and END positions."
  (with-current-buffer (or buffer (current-buffer))
    (let ((points nil))
    (save-excursion
      (dolist (o (overlays-in (or beg (point-min)) (or end (point-max))))
        (when (overlay-get o symbol)
          (push (cons (overlay-start o) (overlay-end o)) points))))
    points)))

(defun consult-gh--delete-region-with-overlay (symbol &optional buffer beg end)
  "Remove regions with SYMBOL overlay from BUFFER.

When BEG or END are non-nil, limit the search in the region between
BEG and END positions."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (dolist (o (overlays-in (or beg (point-min)) (or end (point-max))))
        (when (overlay-get o symbol)
          (delete-region (overlay-start o) (overlay-end o)))))))

(defun consult-gh--separate-add-and-remove (new old)
  "Compare the lists NEW and OLD and return a list of differences.

Splits the difference and returns a list where:
 The first element is a list of items to add to OLD
 The second element is a list of items to remove form OLD."
  (cond
   ((and (listp new) (listp old) (not (equal new old)))
   (list
    (seq-uniq (seq-difference new old))
    (seq-uniq (seq-difference old new))))
   (t
    (list nil nil))))

(defun consult-gh--list-to-string (list)
  "Convert a LIST of strings to a single comma separated string."
  (mapconcat #'substring-no-properties list ","))

;;; Backend functions for call to `gh` program

(defun consult-gh--auth-account-host (&optional account)
  "Get the host of current ACCOUNT."
  (let* ((account (or account consult-gh--auth-current-account)))
    (when (consp account)
      (cadr account))))

;;;###autoload
(defmacro consult-gh-with-host (host &rest body)
"Run BODY after setting environment var “GH_HOST” to HOST."
  `(progn
     (if ,host
         (with-environment-variables
             (("GH_HOST" ,host))
           ,@body)
       ,@body)))

(defun consult-gh--async-log (formatted &rest args)
  "Log FORMATTED ARGS to variable `consult-gh--async-log-buffer'.

FORMATTED and ARGS are passed to `format' with \=(format FORMATTED ARGS)"
  (with-current-buffer (get-buffer-create consult-gh--async-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format formatted args))))

(cl-defun consult-gh--make-process (name &rest args &key filter when-done cmd-args)
  "Make asynchronous process with NAME and pass ARGS to “gh” program.

This command runs gh program asynchronously.

Description of Arguments:
  NAME      a string; is passed as \=:name t `make-process'
  FILTER    a function: iss passed as \=:filter to `make-process'
  WHEN-DONE a funciton; is applied to the the output of process when it is done
            This function should take two input arguments STATUS and STRING
            STATUS is the status of the process and STRING is the output
  CMD-ARGS  a list of strings; is passed as \=:command to `make-process'"
  (if (executable-find "gh")
      (consult-gh-with-host
       (consult-gh--auth-account-host)
        (when-let ((proc (get-process name)))
          (delete-process proc))
        (let* ((cmd-args (append (list "gh") cmd-args))
               (proc-buf (generate-new-buffer (concat consult-gh--async-process-buffer-name "-" name)))
               (when-done (if (functionp when-done)
                                when-done
                           (lambda (_ str) str)))
               (proc-sentinel
                `(lambda (proc event)
                   (cond
                    ((string-prefix-p "finished" event)
                       (with-current-buffer ,proc-buf
                         (widen)
                         (funcall ,when-done nil (buffer-string))
                           (erase-buffer)))
                     ((string-prefix-p "killed" event)
                      (message "consult-gh--async-process was %s" (propertize "killed" 'face 'warning)))
                     (t (message "consult-gh--async-process %s" (propertize "failed" 'face 'error))))
                   (when (> (buffer-size ,proc-buf) 0)
                     (with-current-buffer (get-buffer-create consult-gh--async-log-buffer)
                       (goto-char (point-max))
                       (insert ">>>>> stderr >>>>>\n")
                       (let ((beg (point)))
                         (insert-buffer-substring ,proc-buf)
                         (save-excursion
                           (goto-char beg)
                           (message #("%s" 0 2 (face error))
                                    (buffer-substring-no-properties (pos-bol) (pos-eol)))))
                       (insert "<<<<< stderr <<<<<\n")))))
               (process-adaptive-read-buffering t))
          (with-current-buffer proc-buf
            (set-buffer-file-coding-system 'unix))
          (consult-gh--async-log "consult-gh--make-process started %s\n" cmd-args)
          (make-process :name name
                        :buffer proc-buf
                        :noquery t
                        :command cmd-args
                        :connection-type 'pipe
                        :filter filter
                        :sentinel proc-sentinel)))
    (progn
      (message (propertize "\"gh\" is not found on this system" 'face 'warning))
      nil)))

(defun consult-gh--call-process (&rest args)
  "Run “gh” program and pass ARGS as arguments.

Returns a list where the CAR is exit status
\(e.g. 0 means success and non-zero means error\) and CADR is the output's text.
If gh is not found it returns \(127 “”\)
and a message saying “gh” is not found."
  (if (executable-find "gh")
      (with-temp-buffer
        (set-buffer-file-coding-system 'unix)
        (consult-gh-with-host (consult-gh--auth-account-host)
                              (list (apply #'call-process "gh" nil (current-buffer) nil args)
                                    (buffer-string))))
    (progn
      (message (propertize "\"gh\" is not found on this system" 'face 'warning))
      '(127 ""))))

(defun consult-gh--command-to-string (&rest args)
  "Run `consult-gh--call-process' and return a string if no error.

If there are errors passes them to `message'.
ARGS are passed to `consult-gh-call-process'"
  (let ((out (apply #'consult-gh--call-process args)))
    (if (= (car out) 0)
        (cadr out)
      (progn
        (message (cadr out))
        nil))))

(defun consult-gh--api-get-json (arg)
  "Make a GitHub API call to get response in JSON format.

Passes the ARG \(e.g. a GitHub API URL\) to
“gh api -H Accept:application/vnd.github+json” command."
  (consult-gh--call-process "api" "-H" "Accept: application/vnd.github+json" "--paginate" arg))

(defun consult-gh--api-command-string (url &rest args)
  "Return the output of an api call to URL with ARGS.

Passes the ARGS to a GitHub API URL using
“gh api -H Accept:application/vnd.github+json URL ARGS” command."
  (let ((args (append `("api" "-H" "Accept: application/vnd.github+json" "--paginate" ,url) (if (listp args) args (list args)))))
  (apply #'consult-gh--command-to-string args)))

(defun consult-gh--json-to-hashtable (json &optional keys)
  "Convert a JSON object to a hash table.

Uses lists for arrays and symbols for keys.
If optional argument KEYS is non-nil, returns only the value of KEYS."
  (if (stringp json)
      (let* ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'keyword)
            (json-false :false)
            (results (json-read-from-string json)))
        (cond
         ((hash-table-p results)
          (cond
           ((and keys (listp keys))
            (let* ((table (make-hash-table :test 'equal)))
              (cl-loop for key in keys
                     do
                     (puthash key (gethash key results) table))
            table))
          ((and keys (symbolp keys))
          (gethash keys results))
          (t results)))
         ((listp results)
          (cond
           ((and keys (listp keys))
              (cl-loop for result in results
                     collect
                     (cl-loop for key in keys
                              do (let* ((table (make-hash-table :test 'equal)))
                              (puthash key (gethash key result) table)
                              table))))
          ((and keys (symbolp keys))
           (cl-loop for result in results
                    collect
                    (gethash keys result)))
          (t results)))))
    nil))

(defun consult-gh--get-current-username ()
  "Get the currently logged in user.

Runs “gh api user” and returns the login field of json data."
  (consult-gh--json-to-hashtable (cadr (consult-gh--api-get-json "user")) :login))

(defun consult-gh--get-current-user-orgs (&optional user include-user)
  "Get the organizations for USER.

USER defaults to currently logged in user.
When INCLUDE-USER is non-nil, add the name of the user to the list."
  (let* ((data (if user (consult-gh--api-get-json (format "users/%s/orgs" user)) (consult-gh--api-get-json "user/orgs")))
         (table (when (eq (car data) 0)
                  (consult-gh--json-to-hashtable (cadr data) :login)))
         (user (or user (consult-gh--get-current-username))))
    (cond
     ((listp table)
      (append table (if include-user (list user))))
     ((stringp table)
      (append (list table)
              (if include-user (list user))))
     (t (if include-user (list user))))))

(defun consult-gh--get-gitignore-template-list ()
  "List name of .gitignore templates."
  (string-split (string-trim
                 (cadr
                  (consult-gh--api-get-json "gitignore/templates"))
                 "\\[" "\\]") "," t "\""))

(defun consult-gh--get-license-template-list ()
  "List name of open source license templates.

Each item is a cons of (name . key) for a license."
  (mapcar (lambda (item) (cons (gethash :name item) (gethash :key item)))
          (consult-gh--json-to-hashtable
           (cadr
            (consult-gh--api-get-json "licenses")))))

(defun consult-gh--get-user-template-repos (&optional user)
  "List template repository for USER.

When USER is nil, the curent authenticated user is used instead."
  (let ((endpoint (if user (format "users/%s/repos" user) "user/repos")))
    (delq nil (mapcar (lambda (item) (when (eq (gethash :is_template item) t)
                                       (gethash :full_name item)))
                      (consult-gh--json-to-hashtable
                       (cadr
                        (consult-gh--api-get-json endpoint)))))))

(defun consult-gh--get-repo-from-directory (&optional dir)
  "Return the full name of the GitHub repository in current directory.

If optional arg DIR is non-nil, use DIR instead of the current directory.
Formats the output as “[HOST/]OWNER/REPO” if any, otherwise returns nil."
  (let* ((default-directory (or dir default-directory))
         (response (consult-gh--call-process "repo" "view" "--json" "nameWithOwner" "--jq" ".nameWithOwner")))
    (if (eq (car response) 0)
        (if (not (string-empty-p (cadr response)))
            (string-trim (cadr response))
          nil)
      nil)))

(defun consult-gh--get-repo-from-topic (&optional topic)
  "Return the full name of the GitHub repository in topic.

TOPIC should be a string with property field :repo, and defaults to
`consult-gh--topic'."
  (when-let* ((topic (or topic consult-gh--topic)))
    (if (stringp topic)
        (get-text-property 0 :repo topic))))

(defun consult-gh--split-repo (repo &optional separators)
  "Split REPO string by SEPARATORS to get user and package name.

Returns a list where CAR is the user's name and CADR is the package name."
  (let ((separators (or separators "\/")))
    (and (stringp repo) (split-string repo separators))))

(defun consult-gh--get-username (repo)
  "Return the username of REPO.

\(e.g. “armindarvish” if REPO is “armindarvish/consult-gh”\)"
  (car (consult-gh--split-repo repo)))

(defun consult-gh--get-package (repo)
  "Return the package name of REPO.

\(e.g. “consult-gh” if REPO is “armindarvish/consult-gh”\)"
  (cadr (consult-gh--split-repo repo)))

(defun consult-gh--tempdir ()
 "Make a new temporary directory with timestamp."
 (if (and consult-gh--current-tempdir (< (time-convert (time-subtract (current-time) (nth 5 (file-attributes (substring (file-name-as-directory consult-gh--current-tempdir) 0 -1)))) 'integer) consult-gh-temp-tempdir-cache))
         consult-gh--current-tempdir
(expand-file-name (make-temp-name (concat (format-time-string consult-gh-temp-tempdir-time-format  (current-time)) "-")) consult-gh-tempdir)))

(defun consult-gh--parse-diff (diff)
  "Parse DIFF to extract diff hunks per file.

Returns an alist with key value pairs of (file . diff)"
  (let ((chunks nil)
        (p nil))
    (with-temp-buffer
      (save-match-data
        (insert diff)
        (goto-char (point-max))
        (setq p (point))
        (while (re-search-backward "^--- \\(?1:.*\\)\n\\+\\+\\+ \\(?2:.*\\)\n\\|similarity index.*\nrename from \\(?1:.*\\)\nrename to \\(?2:.*\\)\n" nil t)
          (let ((filea (match-string 1))
                (fileb (match-string 2))
                (start (or (min (+ (match-end 2) 1) (point-max)) (point)))
                (file nil))
            (when filea
              (if (equal filea "/dev/null") (setq filea nil) (setq filea (string-trim-left filea "a/"))))
            (when fileb
              (if (equal fileb "/dev/null") (setq fileb nil) (setq fileb (string-trim-left fileb "b/"))))
            (cond
             ((looking-at "similarity index.*") (setq file (propertize (concat "renamed\t" filea "->" fileb) :path fileb)))
             ((and filea fileb (setq file (propertize (concat "modified\t" filea) :path filea))))
             (fileb (setq file (propertize (concat "new file\t" fileb) :path fileb)))
             (filea (setq file (propertize (concat "deleted\s\t" filea) :path filea))))
            (when (looking-at "---") (push (cons file (buffer-substring start p)) chunks))
            (when (looking-at "similarity index.*") (push (cons file nil) chunks))
            (re-search-backward "diff --git" nil t)
            (setq p (max (- (point) 1) (point-min)))))))
    chunks))

(defun consult-gh--get-line-and-side-at-pos-inside-diff (&optional pos)
  "Get line and side at POS inside a diff code block."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (when (plist-get (get-text-property (point) :consult-gh) :code)
      (pcase-let* ((`(,_end ,endline) (list (point) (line-number-at-pos)))
                   (`(,begin ,beginline) (save-excursion
                                           (re-search-backward "@@ [-\\+]\\(?1:[0-9]+\\).* \\+\\(?2:[0-9]+\\).*" nil t)
                                           (list (point) (line-number-at-pos))))
                   (cord (list (if (and (match-string 1) (> (string-to-number (match-string 1)) 0))
                                   (string-to-number (match-string 1))
                                 0)
                               (if (and (match-string 2) (> (string-to-number (match-string 2)) 0))
                                   (string-to-number (match-string 2))
                                 0)))

                   (difference (- endline beginline 1))
                   (count 0))
        (goto-char (pos-bol))
        (cond
         ((= endline beginline) (list nil nil))
         ((looking-at "-.*")
          (while (re-search-backward "^+.*" begin t) (cl-incf count))
          (list "LEFT" (- (+ (car cord) difference) count)))

         ((looking-at "+.*")
          (while (re-search-backward "^-.*" begin t) (cl-incf count))
          (list "RIGHT" (- (+ (cadr cord) difference) count)))

         (t
          (while (re-search-backward "^-.*" begin t) (cl-incf count))
          (list "RIGHT" (- (+ (cadr cord) difference) count))))))))

(defun consult-gh--get-line-and-side-inside-diff ()
  "Get lines and side from region or point."
  (pcase-let* ((`(,start ,end) (if (region-active-p)
                                   (list (region-beginning)
                                         (region-end))
                                 (list nil (point)))))
    (cond
     ((and start end)
      (let ((l1 (consult-gh--get-line-and-side-at-pos-inside-diff start))
            (l2  (consult-gh--get-line-and-side-at-pos-inside-diff end)))
        (cond
         ((> (cadr l2) (cadr l1))
            (append l2 l1))
         ((> (cadr l1) (cadr l2))
            (append l1 l2))
         (t l2))))
     (end (consult-gh--get-line-and-side-at-pos-inside-diff end))
     (t
      (consult-gh--get-line-and-side-at-pos-inside-diff)))))

(defun consult-gh--group-function (cand transform &optional group-by)
  "Group CAND by GROUP-BY keyword.

This is passed as GROUP to `consult--read' on candidates
and is used to define the grouping for CAND.

If TRANSFORM is non-nil, the CAND itself is returned."
  (if transform (substring cand)
    (let* ((group-by (or consult-gh--override-group-by group-by consult-gh-group-by))
           (group-by (if (stringp group-by) (if (not (keywordp (intern group-by))) (intern (concat ":" (format "%s" group-by))) (intern group-by)) group-by)))
      (cond
       ((member group-by '(nil :nil :none :no :not))
        nil)
       ((not (member group-by '(:t t)))
        (if-let ((group (get-text-property 0 group-by cand)))
            (format "%s" group)
          "N/A"))
       (t t)))))

(defun consult-gh--split-command (input)
  "Return command argument and options list given INPUT string.

It sets `consult-gh--override-group-by' if and argument
for grouping is provided in options.

See `consult--command-split' for more info."
  (pcase-let* ((`(,query . ,opts) (consult--command-split input)))
    (if (and opts (listp opts) (> (length opts) 0))
        (progn
          (setq opts (cl-substitute ":group" ":g" opts :test 'equal))
          (if (member ":group" opts)
              (progn
                (setq consult-gh--override-group-by (cadr (member ":group" opts)))
                (setq opts (seq-difference opts (list ":group" (cadr (member ":group" opts))))))
            (setq consult-gh--override-group-by nil)))
      (setq consult-gh--override-group-by nil))
    (append (list (or query input)) opts)))

(defun consult-gh--get-assignable-users (repo)
  "Get a table of assignbale users of REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "assignableUsers"))
         (table (and (stringp json)  (consult-gh--json-to-hashtable json :assignableUsers))))
    (and table (listp table) (mapcar (lambda (item) (gethash :login item)) table))))

(defun consult-gh--get-mentionable-users (repo)
  "Get a table of mentionable users of REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "mentionableUsers"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :mentionableUsers))))
    (and table (listp table) (mapcar (lambda (item) (gethash :login item)) table))))

(defun consult-gh--get-labels (repo)
  "Get a list of labels in REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "labels"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :labels))))
    (and table (listp table) (mapcar (lambda (item) (gethash :name item)) table))))

(defun consult-gh--get-milestones (repo)
  "Get a list of milestones in REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "milestones"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :milestones))))
    (and table (listp table) (mapcar (lambda (item) (gethash :title item)) table))))

(defun consult-gh--get-projects (repo)
  "Get a list of projects of REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "projectsV2"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :projectsV2)))
         (nodes (and (hash-table-p table) (gethash :Nodes table))))
    (and nodes (listp nodes) (mapcar (lambda (item) (gethash :title item)) nodes))))

(defun consult-gh--get-issue-templates (repo)
  "Get issue templates of REPO."
  (let* ((table (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "issueTemplates") :issueTemplates))
         (templates (and table (listp table)
                         (mapcar (lambda (item) (cons (gethash :name item)
                                                      (list
                                                       :title (gethash :title item)
                                                       :body (gethash :body item))))
                                 table))))
    (and templates (append templates (list (cons "Blank" (list :title "" :body "")))))))

(defun consult-gh--user-canadmin (repo)
  "Determine if the current user can administer REPO."
  (let ((json (consult-gh--command-to-string "repo" "view" repo "--json" "viewerCanAdminister")))
    (and (stringp json)
         (eq (consult-gh--json-to-hashtable json :viewerCanAdminister) 't))))

(defun consult-gh--user-isauthor (topic &optional user)
  "Determine if the USER is the author of TOPIC.

USER defaults to `consult-gh--auth-current-active-account'."
  (let* ((user (or user (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
         (type (get-text-property 0 :type topic))
         (repo (get-text-property 0 :repo topic))
         (number (get-text-property 0 :number topic))
         (author (get-text-property 0 :author topic))
         (json (when (and (not author) number (member type '("issue" "pr")))
                 (consult-gh--command-to-string type "view" number "--repo" repo "--json" "author")))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :author)))
         (author (or author (and (hash-table-p table) (gethash :login table)))))
         (equal user author)))

(defun consult-gh--enable-keybindings-alist (map alist)
  "Enable keymap ALIST in MAP."
  (cl-loop for k in alist
           do
           (keymap-set map (car k) (cdr k))))

(defun consult-gh--disable-keybindings-alist (map alist)
  "Disable keymap ALIST in MAP."
  (cl-loop for k in alist
           do
           (keymap-unset map (car k) t)))

(defun consult-gh--get-split-style-character (&optional style)
"Get the character for consult async split STYLE.

STYLE defaults to `consult-async-split-style'."
(let ((style (or style consult-async-split-style 'none)))
  (or (plist-get (alist-get style consult-async-split-styles-alist) :initial)
      (char-to-string (plist-get (alist-get style consult-async-split-styles-alist) :separator))
      "")))

;;; Backend functions for `consult-gh'.

;; Buffers and Windows
(defun consult-gh-quit-window (&optional kill window)
  "Quit WINDOW and bury its buffer or delete WINDOW.

WINDOW must be a live window and defaults to the selected one.
This calls `quit-window' when there are more than one windows
and `delete-window' when there is only one window.

When KILL is non-nil, it kills the current buffer as well."
  (if (one-window-p)
      (quit-window kill window)
    (progn
      (when kill (kill-buffer (current-buffer)))
      (delete-window window))))

(defun consult-gh-kill-preview-buffers ()
  "Kill all open preview buffers stored in `consult-gh--preview-buffers-list'.

It asks for confirmation if the buffer is modified
and removes the buffers that are killed from the list."
  (interactive)
  (when consult-gh--preview-buffers-list
    (mapcar (lambda (buff) (if (buffer-live-p buff)
                               (kill-buffer buff))
              (unless (buffer-live-p buff)
                (setq consult-gh--preview-buffers-list
                      (delete buff consult-gh--preview-buffers-list))))
            consult-gh--preview-buffers-list)))

(defun consult-gh--completion-get-issue-list (string)
  "Filter function to parse STRING, json output of “gh issue list”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (mapcar (lambda (item) (cons (format "#%s" (gethash :number item)) (gethash :title item))) (consult-gh--json-to-hashtable string)))

(defun consult-gh--completion-set-issues (&optional topic repo)
  "Make async process to get list of issues of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic)))
         (issueEnabled (gethash :hasIssuesEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasIssuesEnabled")))))
    (if (eq issueEnabled 't)
        (consult-gh--make-process "consult-gh-issue-list"
                                  :when-done `(lambda (_ out)
                                     (add-text-properties 0 1 (list :issues (consult-gh--completion-get-issue-list out)) ,topic))
                                  :cmd-args (list "issue" "list" "--repo" repo "--state" "all" "--limit" consult-gh-completion-max-items "--search" "sort:updated" "--json" "number,title")))))

(defun consult-gh--completion-set-prs (&optional topic repo)
  "Make async process to get list of pullrequests of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-pr-list"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list :prs (consult-gh--completion-get-issue-list out)) ,topic))
                              :cmd-args (list "pr" "list" "--repo" repo "--state" "all" "--search" "sort:updated" "--limit" consult-gh-completion-max-items "--json" "number,title"))))

(defun consult-gh--completion-get-mentionable-users-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (mapcar (lambda (item)
            (let* ((user (and (hash-table-p item) (gethash :login item))))
              (and (stringp user) (concat "@" user))))
          (consult-gh--json-to-hashtable string :mentionableUsers)))

(defun consult-gh--completion-set-mentionable-users (&optional topic repo)
  "Make async process to get list of mentionable users of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-mentionable-users"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list :mentionable-users (consult-gh--completion-get-mentionable-users-list out))
                                                      ,topic))
                              :cmd-args (list "repo" "view" repo "--json" "mentionableUsers"))))

(defun consult-gh--completion-get-assignable-users-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (mapcar (lambda (item) (and (hash-table-p item) (gethash :login item))) (consult-gh--json-to-hashtable string :assignableUsers)))

(defun consult-gh--completion-set-assignable-users (&optional topic repo)
  "Make async process to get list of assignable users of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-valid-assignees"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :assignable-users (consult-gh--completion-get-assignable-users-list out))
                                                      ,topic))
                              :cmd-args (list "repo" "view" repo "--json" "assignableUsers"))))

(defun consult-gh--completion-get-labels-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (mapcar (lambda (item) (and (hash-table-p item) (gethash :name item))) (consult-gh--json-to-hashtable string :labels)))

(defun consult-gh--completion-set-valid-labels (&optional topic repo)
  "Make async process to get list of labels of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-valid-labels"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-labels (consult-gh--completion-get-labels-list out))
                                                      ,topic))
                              :cmd-args (list "repo" "view" repo "--json" "labels"))))

(defun consult-gh--completion-get-milestones-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (mapcar (lambda (item) (and (hash-table-p item) (gethash :title item))) (consult-gh--json-to-hashtable string :milestones)))

(defun consult-gh--completion-set-valid-milestones (&optional topic repo)
  "Make async process to get list of milestones of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-valid-milestones"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-milestones (consult-gh--completion-get-milestones-list out))
                                                      ,topic))
                              :cmd-args (list "repo" "view" repo "--json" "milestones"))))

(defun consult-gh--completion-get-projects-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (when (eq (consult-gh--json-to-hashtable string :hasProjectsEnabled) 't)
  (mapcar (lambda (item) (and (hash-table-p item) (gethash :title item))) (gethash :Nodes (consult-gh--json-to-hashtable string :projectsV2)))))

(defun consult-gh--completion-set-valid-projects (&optional topic repo)
  "Make async process to get list of milestones of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."

  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic)))
         (token-scopes (consult-gh--auth-get-token-scopes)))
    (when (or (member "read:project" token-scopes)
              (member "project" token-scopes))
        (consult-gh--make-process "consult-gh-valid-projects"
                                  :when-done `(lambda (_ out)
                                     (add-text-properties 0 1 (list
                                                               :valid-projects (ignore-errors (consult-gh--completion-get-projects-list out)))
                                                          ,topic))
                                  :cmd-args (list "repo" "view" repo "--json" "hasProjectsEnabled,projectsV2")))))

(defun consult-gh--completion-get-branches-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (let ((branches (consult-gh--json-to-hashtable string)))
    (when (listp branches)
      (cl-loop for branch in branches
               collect
               (when (hash-table-p branch) (gethash :name branch))))))

(defun consult-gh--completion-set-branches (&optional topic repo)
  "Make async process to get list of branches of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-valid-branches"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-branches (consult-gh--completion-get-branches-list out))
                                                      ,topic))
                              :cmd-args (list "api" (format "/repos/%s/branches" repo)))))

(defun consult-gh--completion-get-pr-refs-list (string repo refonly)
  "Filter function to parse STRING and get branches of REPO.

STRING is the json output of “gh api repos/repo/branches”.
When optional argument REFONLY is non-nil returns a list of branch naes only,
otherwise returns “OWNER:BRANCH”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (let ((branches (consult-gh--json-to-hashtable string)))
    (when (listp branches)
      (cl-loop for branch in branches
               collect
               (when (hash-table-p branch) (if refonly (gethash :name branch)
                                             (concat repo ":" (gethash :name branch))))))))

(defun consult-gh--completion-set-pr-refs (&optional topic baserepo headrepo refonly)
  "Make async process add branches of BASEREPO and HEADREPO in TOPIC.

When optional argument REFONLY is non-nil returns a list of branch naes only,
otherwise returns “OWNER:BRANCH”.
When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (baserepo (or baserepo (get-text-property 0 :baserepo topic)))
         (headrepo (or headrepo (get-text-property 0 :headrepo topic))))
    (when (stringp baserepo)
    (consult-gh--make-process "consult-gh-valid-basebranches"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-baserefs (consult-gh--completion-get-pr-refs-list out (substring-no-properties ,baserepo) ,refonly))
                                                      ,topic))
                              :cmd-args (list "api" (format "/repos/%s/branches" baserepo))))
    (when (stringp headrepo)
    (consult-gh--make-process "consult-gh-valid-headbranches"
                              :when-done `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-headrefs (consult-gh--completion-get-pr-refs-list out (substring-no-properties ,headrepo) ,refonly))
                                                      ,topic))
                              :cmd-args (list "api" (format "/repos/%s/branches" headrepo))))))

(defun consult-gh--topics-edit-capf ()
  "Complettion at point for editing comments.

Completes for issue/pr numbers or user names."
  (save-match-data
    (when consult-gh-topics-edit-mode
    (cond
     ((or (looking-back "@[^[:space:]]*?" (pos-bol)) (looking-back "#[^[:space:]]*?\\|#[^\\+][^[:digit:]]+.*?" (pos-bol)))
      (let* ((begin (save-excursion (if (looking-back "@[^[:space:]]*?\\|#.*?" (pos-bol))
                                        (match-beginning 0)
                                      (progn
                                        (backward-word)
                                        (point)))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (append (get-text-property 0 :mentionable-users consult-gh--topic)
                                                                 (get-text-property 0 :commenters consult-gh--topic)
                                                                 (mapcar (lambda (item) (and (consp item)
                                                                                             (concat (car item)
                                                                                                     "\t"
                                                                                                     (propertize (cdr item) 'face 'completions-annotations))))
                                                                         (get-text-property 0 :issues consult-gh--topic))
                                                                 (mapcar (lambda (item) (and (consp item)
                                                                                             (concat (car item)
                                                                                                     "\t"
                                                                                                     (propertize (cdr item) 'face 'completions-annotations))))
                                                                         (get-text-property 0 :prs consult-gh--topic)))))))
        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item
                                                     (cond ((string-prefix-p "@" item) consult-gh-completion-user-prefix)
                                                           ((assoc (car (split-string item "\t" t))
                                                                   (get-text-property 0 :issues consult-gh--topic))
                                                            consult-gh-completion-issue-prefix)
                                                           ((assoc (car (split-string item "\t" t))
                                                                   (get-text-property 0 :prs consult-gh--topic))
                                                            consult-gh-completion-pullrequest-prefix)
                                                           (t ""))
                                                     ""))
                                             list))
              :exit-function (lambda (str _status) ""
                               (delete-char (- (length str)))
                               (when (looking-back "#\\|@" (- (point) 1)) (delete-char -1))
                               (insert (or (car (split-string str "\t" t)) str)))

              :exclusive 'no
              :category 'string)))
     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}base: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :valid-baserefs consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-branch-prefix ""))
                                             list))
              :exclusive 'yes
              :category 'string)))

     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}head: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :valid-headrefs consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-branch-prefix ""))
                                             list))
              :exclusive 'yes
              :category 'string)))

     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}assignees: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :assignable-users consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-user-prefix ""))
                                             list))
              :exit-function (lambda (_str _status) ""
                               (insert ", "))
              :exclusive 'yes
              :category 'string)))
     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}labels: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :valid-labels consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-label-prefix ""))
                                             list))
              :exit-function (lambda (_str _status) ""
                               (insert ", "))

              :exclusive 'yes
              :category 'string)))
     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}milestone: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :valid-milestones consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-milestone-prefix ""))
                                             list))
              :exclusive 'yes
              :category 'string)))
     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}projects: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :valid-projects consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-project-prefix ""))
                                             list))
              :exit-function (lambda (_str _status) ""
                               (insert ", "))
              :exclusive 'yes
              :category 'string)))
     ((and (get-text-property (pos-bol) 'read-only) (looking-back "^.\\{1,3\\}reviewers: .*" (pos-bol)))
      (let* ((begin (if (looking-back " " (- (point) 1))
                                        (point)
                                      (save-excursion
                                        (backward-word)
                                        (point))))
             (end (point))
             (candidates (cl-remove-duplicates (delq nil (get-text-property 0 :assignable-users consult-gh--topic)))))

        (list begin end candidates
              :affixation-function (lambda (list)
                                     (mapcar (lambda (item)
                                               (list item consult-gh-completion-user-prefix ""))
                                             list))
              :exit-function (lambda (_str _status) ""
                               (insert ", "))
              :exclusive 'yes
              :category 'string)))))))

(defun consult-gh--auth-accounts ()
  "Return a list of currently autheticated accounts.

Each account is in the form \='(USERNAME HOST IF-ACTIVE)."
  (let* ((str (consult-gh--command-to-string "auth" "status"))
         (i 0)
         (accounts nil))
    (while (and (stringp str) (string-match "Logged in to \\(.+\\)? account \\(.+\\)? \(.*\)\n.*Active account: \\(.+\\)?" str i)
                (> (match-end 0) i))
      (let ((m (match-data))
            (host (match-string 1 str))
            (name (match-string 2 str))
            (active (equal (match-string 3 str) "true")))
        (push `(,name ,host ,active) accounts)
        (setq i (cadr m))))
    accounts))

(defun consult-gh--auth-current-active-account (&optional host)
  "Return currently logged-in active account.

This is a list of \='(USERNAME HOST IF-ACTIVE)."
  (let* ((accounts (consult-gh--auth-accounts))
         (host (or host consult-gh-default-host))
         (current-account (car-safe (seq-filter (lambda (acc) (and (equal (cadr acc) host) (caddr acc) acc)) accounts))))
    (when current-account
      (setq consult-gh--auth-current-account current-account))))

(consult-gh--auth-current-active-account)

(defvar consult-gh-auth-post-switch-hook nil
  "Functions called after `consult-auth--switch'.

host and username are passed to these functions.")

(defun consult-gh--auth-switch (host user)
"Authentication the account for USER on HOST.

This is an internal function for non-interactive use.
For interactive use see `consult-gh-auth-switch'."
(if (and (stringp host) (stringp user))
  (let ((str (consult-gh--command-to-string "auth" "switch" "-h" host "-u" user)))
    (when (stringp str)
      (setq consult-gh--auth-current-account `(,user ,host t))
      (run-hook-with-args 'consult-gh-auth-post-switch-hook user host)
      (message str)))
  (message "%s" (concat (propertize "HOST" 'face 'warning) "and " (propertize "USER" 'face 'warning) "need to be provided as strings."))))

(defun consult-gh--auth-get-token-scopes (&optional username host)
  "Return a list of token scopes for USERNAME on HOST.

USERNAME and HOST default to `consult-gh--auth-current-account'."
  (let* ((username (or username (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account)) ".*?"))
         (host (or host (cadr consult-gh--auth-current-account) ".*?"))
         (str (consult-gh--command-to-string "auth" "status")))
    (when
        (string-match (format "Logged in to %s account %s \(.*\)\n.*Active account: true[[:ascii:][:nonascii:]]*?Token scopes: \\(?1:.+\\)?" host username) str)
      (let ((m (match-string 1 str)))
        (and (stringp m) (split-string m ", " t "['\s\n\t]"))))))

(defun consult-gh--set-current-user-orgs (&rest _args)
  "Set `consult-gh--user-repos' to list of repos for current user."
  (setq consult-gh--current-user-orgs (consult-gh--get-current-user-orgs nil t)))

(consult-gh--set-current-user-orgs)

;; add hook to set user orgs after switching accounts
(add-hook 'consult-gh-auth-post-switch-hook #'consult-gh--set-current-user-orgs)

(defun consult-gh--files-get-branches (repo)
  "List branches of REPO, in json format.

uses `consult-gh--api-get-json' to get branches from GitHub API."
  (consult-gh--api-get-json (concat "repos/" repo "/branches")))

(defun consult-gh--files-branches-hashtable-to-list (table repo)
  "Convert TABLE with branches of REPO to a list of propertized text.

TABLE can for example be obtained by converting the json object from
`consult-gh--files-get-branches' to a hash table
by using `consult-gh--json-to-hashtable'."
  (mapcar (lambda (item) (cons (gethash :name item)
                               `(:repo ,repo
                                       :branch ,(gethash :name item)
                                       :url ,(gethash :url item))))
          table))

(defun consult-gh--files-branches-list-items (repo)
  "Return REPO's information in propertized text format.

Uses `consult-gh--files-get-branches',
`consult-gh--files-branches-hashtable-to-list',
and `consult-gh--json-to-hashtable'."
  (let ((response (consult-gh--files-get-branches repo)))
    (if (eq (car response) 0)
        (consult-gh--files-branches-hashtable-to-list
         (consult-gh--json-to-hashtable (cadr response)) repo)
      (message (cadr response)))))

(defun consult-gh--read-branch (repo &optional prompt)
  "Query the user to select a branch of REPO.

REPO must be  a Github repository full name
for example “armindarvish/consult-gh”.
If PROMPT is non-nil, use it as the query prompt"
  (pcase consult-gh-default-branch-to-load
    ('confirm
     (if (y-or-n-p "Load Default HEAD branch?")
         (cons repo "HEAD")
       (cons repo (completing-read
                    (or prompt (concat "Select Branch for "
                           (propertize (format "\"%s\"" repo) 'face 'consult-gh-default)
                           ": "))
                   (consult-gh--files-branches-list-items repo)))))
    ('ask
     (cons repo (completing-read
                 (or prompt (concat "Select Branch for "
                         (propertize (format "\"%s\"" repo) 'face 'consult-gh-default)
                         ": "))
                 (consult-gh--files-branches-list-items repo))))
    ('nil
     (cons repo "HEAD"))
    (_
     (cons repo (format "%s" consult-gh-default-branch-to-load)))))

;; Files
(defun consult-gh--files-get-trees (repo &optional branch)
  "Get a recursive git “tree” of REPO and BRANCH in json object format.

Uses `consult-gh--api-get-json'."
  (let ((branch (or branch "HEAD")))
    (consult-gh--api-get-json
     (concat "repos/" repo
             "/git/trees/" branch
             ":?recursive=1"))))

(defun consult-gh--files-table-to-list (table repo &optional branch)
  "Convert a TABLE containing git tree information of REPO and BRANCH.

Returns a list of propertized texts
formatted properly to be sent to `consult-gh-find-file'."
  (let ((branch (or branch "HEAD")))
    (mapcar (lambda (item) (cons (gethash :path item)
                                 `(:repo ,repo
                                         :branch ,branch
                                         :url ,(gethash :url item)
                                         :path ,(gethash :path item)
                                         :size ,(gethash :size item))))
            table)))

(defun consult-gh--files-list-items (repo &optional branch)
  "Fetch a list of files and directories in REPO and BRANCH.

Returns text with properties containing information about the file
generated by `consult-gh--files-table-to-list'.  This list can be passed to
`consult-gh-find-file'.

See `consult-gh--files-nodirectory-items' for getting a list of file
but not directories."
  (let* ((branch (or branch "HEAD"))
         (response (consult-gh--files-get-trees repo branch)))
    (if (eq (car response) 0)
        (delete-dups (consult-gh--files-table-to-list
                      (consult-gh--json-to-hashtable (cadr response) :tree) repo branch))
      (message (cadr response)))))

(defun consult-gh--files-nodirectory-items (repo &optional branch)
  "Fetch a list of non-directory files in REPO and BRANCH from GitHub.

The format is propertized text that include information about the file
generated by `consult-gh--files-table-to-list'.
This list can be passed to `consult-gh-find-file'.

This list does not have directories.  See `consult-gh--files-list-items'
for getting a list of file and directories."
  (let* ((branch (or branch "HEAD"))
         (items (consult-gh--files-list-items repo branch)))
    (cl-remove-if-not (lambda (item) (plist-get (cdr item) :size))
                      items)))

(defun consult-gh--files-get-content (url)
  "Fetch the contents of file at URL retrieved from GitHub api.

Uses `consult-gh--api-get-json' and decodes it into raw text."
  (let* ((response (consult-gh--api-get-json url))
         (content (if (eq (car response) 0) (consult-gh--json-to-hashtable (cadr response) :content)
                    nil)))
    (if content
        (base64-decode-string content)
      "")))

(defun consult-gh--file-format (cons)
  "Format minibuffer candidates for files.
\(e.g. in`consult-gh-find-file'\).

CONS is a list of files for example returned by
`consult-gh--files-nodirectory-items'."
  (when-let* ((class "file")
              (type "file")
              (path (car cons))
              (path (string-join (mapcar #'identity (string-split path "/")) (propertize "/" 'face 'consult-gh-default)))
              (info (cdr cons))
              (repo (plist-get info :repo))
              (user (consult-gh--get-username repo))
              (package (consult-gh--get-package repo))
              (size (plist-get info :size))
              (branch (plist-get info :branch))
              (url (plist-get info :url))
              (str path))
    (add-text-properties 0 1 (list :repo repo
                                   :user user
                                   :package package
                                   :path (substring-no-properties path)
                                   :url url
                                   :size size
                                   :branch branch
                                   :class class
                                   :type type) str)
    str))

(defun consult-gh--file-state ()
  "State function for file candidates in `consult-gh-find-file'.

This is passed as STATE to `consult--read' on file candidates
and is used to preview files or do other actions on the file."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if (and consult-gh-show-preview cand)
             (let* ((repo (get-text-property 0 :repo cand))
                    (path (get-text-property 0 :path cand))
                    (branch (or (get-text-property 0 :branch cand) "HEAD"))
                    (url (get-text-property 0 :url cand))
                    (tempdir (expand-file-name (concat repo "/" branch "/")
                                               (or consult-gh--current-tempdir
                                                   (consult-gh--tempdir))))
                    (file-p (or (file-name-extension path) (get-text-property 0 :size cand)))
                    (file-size (and file-p (get-text-property 0 :size cand)))
                    (confirm (if (and file-size (>= file-size
                                                    consult-gh-large-file-warning-threshold))
                                 (yes-or-no-p (format "File is %s Bytes.  Do you really want to load it?" file-size))
                               t))
                    (temp-file (or (cdr (assoc (substring-no-properties (concat repo "/" "path")) consult-gh--open-files-list)) (expand-file-name path tempdir)))
                    (_ (and file-p confirm (progn
                                             (unless (file-exists-p temp-file)
                                               (make-directory (file-name-directory temp-file) t)
                                               (with-temp-file temp-file
                                                 (insert (consult-gh--files-get-content url))
                                                 (set-buffer-file-coding-system 'raw-text)
                                                 (write-file temp-file)))
                                             (add-to-list 'consult-gh--open-files-list `(,(substring-no-properties (concat repo "/" path)) . ,temp-file)))))
                    (buffer (or (and file-p confirm (find-file-noselect temp-file t)) nil)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (funcall preview action
                        buffer))))))))

(defun consult-gh--file-annotate ()
  "Annotate each file candidate for `consult-gh-find-file'.

For more info on annotation refer to the manual, particularly
`consult--read' and `consult--read-annotate' documentation."
  (lambda (cands cand)
    (if-let* ((obj (assoc cand cands))
              (size (format "%s Bytes" (get-text-property 0 :size obj)))
              (repo (format "%s" (get-text-property 0 :repo obj)))
              (user (car (string-split repo "\/")))
              (package (cadr (string-split repo "\/")))
              (branch (format "%s" (get-text-property 0 :branch obj)))
              (url (format "%s" (get-text-property 0 :url obj)))
              (str (format "\s%s\s\s%s -- "
                           (propertize size 'face 'consult-gh-visibility)
                           (concat (propertize user 'face 'consult-gh-user ) "/" (propertize package 'face 'consult-gh-package) "@" (propertize branch 'face 'consult-gh-branch))))
              (cand (substring-no-properties cand)))
        (concat
         (consult-gh--justify-left str cand  (* 1.5 (frame-width)))
         (propertize url 'face 'consult-gh-url))
      nil)))

(defun consult-gh--file-group (cand transform)
  "Group function for file candidate, CAND.

This is passed as GROUP to `consult--read' on file candidates
and is used to group files by repository names.

If TRANSFORM is non-nil, return CAND."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-files-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "File " 98 nil ?-)
       (consult-gh--set-string-width " Path " 8 nil ?-)
       (consult-gh--set-string-width " > Repo " 40 nil ?-))))))

(defun consult-gh--files-browse-url-action (cand)
  "Browse the url for a file candidate, CAND.

This is an internal action function that gets a candidate, CAND,
from `consult-gh-find-file' and opens the url of the file in a browser.

To use this as the default action in `consult-gh-find-file',
set `consult-gh-file-action' to `consult-gh--files-browse-url-action'."
  (let* ((repo (get-text-property 0 :repo cand))
         (path (get-text-property 0 :path cand))
         (branch (get-text-property 0 :branch cand))
         (url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/" branch "/" path)))
    (funcall (or consult-gh-browse-url-func #'browse-url) url)))

(defun consult-gh--files-view (repo path url &optional no-select tempdir jump-to-str branch)
  "Open file in an Emacs buffer.

This is an internal function that gets the PATH to a file within a REPO
and the URL of the file on GitHub API, then fetches the content from GitHub
by `consult-gh--files-get-content' and inserts it into a temporary file
stored under `consult-gh-tempdir' in appropriate subdirectories for REPO.

If the optional input NO-SELECT is nil, it switches to the buffer
by `find-file', otherwise it does not swith-to-buffer
and only returns the name of the buffer.

To use this as the default action in `consult-gh-find-file',
see `consult-gh--files-view-action'.

Description of Arguments:
  REPO      full name of the repo e.g. “arimindarvish/consult-gh”
  PATH      the relative path of the file to the root of repo
            e.g “./README.org”
  URL       the url of the file as retrieved from GitHub API
  NO-SELECT a boolean for whether to swith-to-buffer or not
  TEMPDIR   the directory where the temporary file is saved
  BRANCH    is the branch of the repository

Output is the buffer visiting the file."
  (let* ((tempdir (or tempdir consult-gh--current-tempdir (consult-gh--tempdir)))
         (temp-file (or (cdr (assoc (substring-no-properties (concat repo "/" path)) consult-gh--open-files-list)) (expand-file-name path tempdir)))
         (topic (format "%s/%s" repo path)))
    (add-text-properties 0 1 (list :repo repo :type "file" :path path :branch branch :title nil) topic)
    (unless (file-exists-p temp-file)
      (make-directory (file-name-directory temp-file) t)
      (with-temp-file temp-file
        (insert (consult-gh--files-get-content url))
        (set-buffer-file-coding-system 'raw-text)
        (write-file temp-file))
      (add-to-list 'consult-gh--open-files-list `(,(substring-no-properties (concat repo "/" path)) . ,temp-file)))
    (if no-select
        (find-file-noselect temp-file)
      (with-current-buffer (find-file temp-file)
        (if jump-to-str
            (progn
              (goto-char (point-min))
              (search-forward jump-to-str nil t)
              (consult-gh-recenter 'middle))
          nil)
        (add-to-list 'consult-gh--preview-buffers-list (current-buffer))
        (setq-local consult-gh--topic topic)
        (current-buffer)))))

(defun consult-gh--files-view-action (cand)
  "Open file candidate, CAND, in an Emacs buffer.

This is a wrapper function around `consult-gh--files-view'.

It parses CAND to extract relevant values
\(e.g. repository, file path, url, ...\) and passes them to
`consult-gh--files-view'.

To use this as the default action on consult-gh's files,
set `consult-gh-file-action' to `consult-gh--files-view-action'."
  (let* ((repo (get-text-property 0 :repo cand))
         (path (get-text-property 0 :path cand))
         (url (get-text-property 0 :url cand))
         (branch (or (get-text-property 0 :branch cand) "HEAD"))
         (tempdir (expand-file-name (concat repo "/" branch "/")
                                    (or consult-gh--current-tempdir (consult-gh--tempdir))))
         (file-p (or (file-name-extension path) (get-text-property 0 :size cand)))
         (file-size (and file-p (get-text-property 0 :size cand)))
         (confirm t))
    (when (and file-size (>= file-size consult-gh-large-file-warning-threshold))
      (if (yes-or-no-p (format "File is %s Bytes.  Do you really want to load it?" file-size))
          (setq confirm t)
        (setq confirm nil)))
    (if (and file-p confirm)
        (consult-gh--files-view repo path url nil tempdir nil branch))))

(defun consult-gh--files-save-file-action (cand)
  "Save file candidate, CAND, to a file.

Its parses CAND to extract relevant information
\(e.g. repository name, file path, url, ...\)
and passes them to `consult-gh--files-view',
then saves the buffer to file.

If `consult-gh-ask-for-path-before-save' is non-nil,
it queries the user for a file path, otherwise it saves the file under
`consult-gh-default-save-directory' with the variable `buffer-file-name' as
the name of the file.

To use this as the default action on consult-gh's files,
set `consult-gh-file-action' to `consult-gh--files-save-file-action'."
  (let* ((repo (get-text-property 0 :repo cand))
         (path (get-text-property 0 :path cand))
         (url (get-text-property 0 :url cand))
         (file-p (or (file-name-extension path) (get-text-property 0 :size cand)))
         (file-size (and file-p (get-text-property 0 :size cand)))
         (filename (and file-p (file-name-nondirectory path)))
         (targetpath (if consult-gh-ask-for-path-before-save
                         (file-truename (read-file-name "Save As: " consult-gh-default-save-directory nil nil filename))
                       (expand-file-name filename consult-gh-default-save-directory)))
         (confirm t))
    (when (and file-size (>= file-size consult-gh-large-file-warning-threshold))
      (if (yes-or-no-p (format "File is %s Bytes.  Do you really want to load it?" file-size))
          (setq confirm t)
        (setq confirm nil)))
    (let ((buffer (and file-p (consult-gh--files-view repo path url t))))
      (if (and file-p confirm)
          (save-mark-and-excursion
            (save-restriction
              (with-current-buffer buffer
                (write-file targetpath t))))))))

(defun consult-gh--repo-format (string input highlight)
  "Format minibuffer candidates for repos in `consult-gh-search-repos'.

Description of Arguments:

  STRING    output of a “gh” call \(e.g. “gh search repos ...”\).
  INPUT     a query from the user
            \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted with
            `consult-gh-highlight-match' in the minibuffer."
  (let* ((class "repo")
         (type "repo")
         (parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (description (cadr parts))
         (visibility (cadr (cdr parts)))
         (date (cadr (cdr (cdr parts))))
         (date (if (length> date 9) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s"
                      (concat
                       (and user (propertize user 'face 'consult-gh-user))
                       (and package "/")
                       (and package (propertize package 'face 'consult-gh-package)))
                      (consult-gh--justify-left (propertize visibility 'face 'consult-gh-visibility) repo (frame-width))
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date)
                      (propertize description 'face 'consult-gh-description))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :description description :visibility visibility :date date :query query :class class :type type) str)
    str))

(defun consult-gh--repo-state ()
  "State function for repo candidates.

This is passed as STATE to `consult--read'
in `consult-gh-search-repos' and is used
to preview or do other actions on the repo."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if (and consult-gh-show-preview cand)
             (when-let ((repo (get-text-property 0 :repo cand))
                        (query (get-text-property 0 :query cand))
                        (match-str (consult--build-args query))
                        (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (consult-gh--repo-view (format "%s" repo) buffer)
               (with-current-buffer buffer
                 (if consult-gh-highlight-matches
                     (cond
                      ((listp match-str)
                       (mapc (lambda (item)
                                 (highlight-regexp item 'consult-gh-preview-match)) match-str))
                      ((stringp match-str)
                       (highlight-regexp match-str 'consult-gh-preview-match)))))
               (funcall preview action
                        buffer))))
        ('return
         cand)))))

(defun consult-gh--repo-group (cand transform)
  "Group function for repo candidate, CAND.

This is passed as GROUP to `consult--read'
in `consult-gh-search-repos' and is used
to group repos by user\owner's names.

If TRANSFORM is non-nil, return the CAND itself."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-repos-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat "Repository  "
              (consult-gh--justify-left " Visibility " "Repository  " (frame-width) ?-)
              (consult-gh--set-string-width " Date " 12 nil ?-)
              " Description")))))

(defun consult-gh--repo-browse-url-action (cand)
  "Browse the url for a repo candidate, CAND.

This is an internal action function that gets a candidate, CAND,
for example from `consult-gh-search-repos' and opens the url of the repo
in an external browser.

To use this as the default action for repos,
set `consult-gh-repo-action' to `consult-gh--repo-browse-url-action'."
  (let* ((repo (get-text-property 0 :repo cand))
         (response (consult-gh--call-process "browse" "--repo" (substring-no-properties repo) "--no-browser"))
         (url (string-trim (cadr response))))
    (if (eq (car response) 0)
        (funcall (or consult-gh-browse-url-func #'browse-url) url)
      (message url))))

(defun consult-gh--repo-insert-readme-gfm (repo)
  "Insert REPO's Readme in GitHub flavor markdown format at point."
  (when-let ((readme (consult-gh--command-to-string "repo" "view" repo)))
  (save-mark-and-excursion
    (insert readme)
    (set-buffer-modified-p nil)
    (gfm-mode)
    (markdown-display-inline-images)
    (set-buffer-modified-p nil))
    nil))

(defun consult-gh--repo-insert-readme-markdown (repo)
  "Insert REPO's Readme in markdown format at point."
  (when-let ((readme (consult-gh--command-to-string "repo" "view" repo)))
  (save-mark-and-excursion
    (insert readme)
    (set-buffer-modified-p nil)
    (markdown-mode)
    (when (display-images-p)
      (markdown-display-inline-images))
    (set-buffer-modified-p nil))
  nil))

(defun consult-gh--repo-insert-readme-org (repo)
  "Insert REPO's Readme in org format at point."
  (let* ((org-display-remote-inline-images 'download)
         (info (cadr (consult-gh--api-get-json (format "repos/%s" repo))))
         (name (consult-gh--json-to-hashtable info :name))
         (desc (consult-gh--json-to-hashtable info :description))
         (readme (cadr (consult-gh--api-get-json (format "repos/%s/readme" repo))))
         (path (consult-gh--json-to-hashtable readme :path))
         (extension (and (stringp path) (file-name-extension path)))
         (content (consult-gh--json-to-hashtable readme :content)))
    (save-mark-and-excursion
      (insert ""
              "#+name:\t" (or name "") "\n"
              "#+description:\t" (or desc "") "\n"
              (make-string 5 ?\-)
              "\n\n")
      (when content
        (insert (base64-decode-string content))
        (set-buffer-file-coding-system 'raw-text))
      (cond
       ((and (stringp extension) (equal (downcase extension) "org"))
        (org-mode)
        (org-table-map-tables 'org-table-align t)
        (org-fold-show-all))
       ((and (stringp extension) (member (downcase extension) '("md" "mkdn" "mdown" "markdown")))
        (consult-gh--github-header-to-org)
        (consult-gh--markdown-to-org))
       (t
        (org-mode)))
      (set-buffer-modified-p nil)))
  nil)

(defun consult-gh--repo-insert-readme (repo)
  "Insert REPO's Readme at point."
  (let* ((info (cadr (consult-gh--api-get-json (format "repos/%s" repo))))
         (name (consult-gh--json-to-hashtable info :name))
         (desc (consult-gh--json-to-hashtable info :description))
         (readme (cadr (consult-gh--api-get-json (format "repos/%s/readme" repo))))
         (path (consult-gh--json-to-hashtable readme :path))
         (content (consult-gh--json-to-hashtable readme :content)))
  (save-mark-and-excursion
      (setq-local buffer-file-name path)
      (insert ""
              (or comment-start "") "name:\t" (or name "") (or comment-end "") "\n"
              (or comment-start "") "description:\t" (or desc "") (or comment-end "") "\n"
              (make-string 5 ?\-)
              "\n\n")
      (when content
        (insert (base64-decode-string content))
        (set-buffer-file-coding-system 'raw-text))
      (normal-mode)
      (set-buffer-modified-p nil)))
  nil)

(defun consult-gh--repo-view (repo &optional buffer preview)
  "Open REPO's Readme in an Emacs buffer, BUFFER.

This is an internal function that takes REPO, the full name of
a GitHub repository \(e.g. “armindarvish/consult-gh”\) and
shows the README of that repo in an Emacs buffer.

It fetches the preview from GitHub by “gh repo view REPO”
and puts the response as raw text in the buffer defined by
the optional input, BUFFER.  If BUFFER is nil, a buffer named by
`consult-gh-preview-buffer-name' is used instead.

If `consult-gh-repo-preview-major-mode' is non-nil, uses it to set the
major-mode, otherwise uses the major mode associated with the README's
file extension (e.g. .md, .org, .rst).

Description of Arguments:

REPO    a string; the name of the repository to be previewed.
BUFFER  a string; an optional buffer the preview should be shown in.
PREVIEW a boolean; when non-nil loads the preview without details."
  (with-current-buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name))
    (let ((inhibit-read-only t)
          (topic (format "%s" repo)))

      (unless preview
        ;; collect list of repo branches for completion at point
        (consult-gh--completion-set-branches topic repo))

      (add-text-properties 0 1 (list :repo repo :type "repo" :title repo) topic)
      (erase-buffer)
      (pcase consult-gh-repo-preview-major-mode
        ('gfm-mode
         (consult-gh--repo-insert-readme-gfm repo))
        ('markdown-mode
         (consult-gh--repo-insert-readme-markdown repo))
        ('org-mode
         (consult-gh--repo-insert-readme-org repo))
        (_ (consult-gh--repo-insert-readme repo)))
      (goto-char (point-min))
      (consult-gh-repo-view-mode +1)
      (setq-local consult-gh--topic topic)
      (current-buffer))))

(defun consult-gh--repo-view-action (cand)
  "Open the preview of a repo candidate, CAND.

This is a wrapper function around `consult-gh--repo-view'.
It parses CAND to extract relevant values \(e.g. repository's name\) and
passes them to `consult-gh--repo-view'.

To use this as the default action for repos, set
`consult-gh-repo-action' to function `consult-gh--repo-view-action'."

  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "*"))
         (existing (get-buffer buffername))
         (confirm (if (and existing (not (= (buffer-size existing) 0)))
                      (consult--read
                       (list (cons "Switch to existing buffer." :resume)
                             (cons "Reload the README in the existing buffer." :replace)
                             (cons "Make a new buffer and load the README in it (without killing the old buffer)." :new))
                       :prompt "You already have this repo open in another buffer.  Would you like to switch to that buffer or make a new one? "
                       :lookup #'consult--lookup-cdr
                       :sort nil
                       :require-match t))))
    (if existing
        (cond
         ((eq confirm :resume) (funcall consult-gh-switch-to-buffer-func existing))
         ((eq confirm :replace)
          (message "Reloading README in the existing buffer...")
          (funcall consult-gh-switch-to-buffer-func (consult-gh--repo-view repo existing)))
         ((eq confirm :new)
          (message "Opening README in a new buffer...")
          (funcall consult-gh-switch-to-buffer-func (consult-gh--repo-view repo (generate-new-buffer buffername nil)))))
      (progn
        (funcall consult-gh-switch-to-buffer-func (consult-gh--repo-view repo))
        (rename-buffer buffername t)
        (set-buffer-modified-p nil)
        (buffer-name (current-buffer))))))

(defun consult-gh--repo-browse-files-action (cand)
  "Browse file tree of a repo candidate, CAND.

Opens the preview of a repo candidate, CAND, in an Emacs buffer.

This is a wrapper function around `consult-gh-find-file'.
It parses CAND to extract relevant values \(e.g. repository name\)
and passes them to `consult-gh-find-file'.

To use this as the default action for repos,
set `consult-gh-repo-action' to `consult-gh--repo-browse-files-action'."
  (let* ((repo (get-text-property 0 :repo cand)))
    (consult-gh-find-file repo)))

(defvar consult-gh-repo-post-clone-hook nil
  "Functions called after `consult-gh--repo-clone'.

Full path of the cloned repo is passed to these functions.")

(defun consult-gh--repo-clone (repo name targetdir &rest _)
  "Clones REPO to the path TARGETDIR/NAME.

This is an internal function for non-interactive use.
For interactive use see `consult-gh-repo-clone'.

It runs the command “gh clone REPO TARGETDIR/NAME”
using `consult-gh--command-to-string'.

ARGS are ignored."
  (let ((buffer (current-buffer)))
    (consult-gh--make-process (format "consult-gh-clone-%s" repo)
                            :when-done `(lambda (_proc _str)
                                          (with-current-buffer ,buffer
                                            (progn
                                            (run-hook-with-args 'consult-gh-repo-post-clone-hook (expand-file-name ,name ,targetdir))
                                            (message "repo %s was cloned to %s" (propertize ,repo 'face 'font-lock-keyword-face) (propertize (expand-file-name ,name ,targetdir) 'face 'font-lock-type-face)))))
                            :cmd-args (list "repo" "clone" (format "%s" repo) (expand-file-name name targetdir))))
    (let ((inhibit-message t))
       (expand-file-name name targetdir)))

(defun consult-gh--repo-clone-action (cand)
  "Clones a repo candidate, CAND.

This is a wrapper function around `consult-gh--repo-clone'.
It parses CAND to extract relevant values \(e.g. repository's name\)
and passes them to `consult-gh--repo-clone'.

To use this as the default action for repos,
set `consult-gh-repo-action' to `consult-gh--repo-clone-action'.

If `consult-gh-confirm-before-clone' is nil it clones the repo
under `consult-gh-default-clone-directory' and uses the package name
from REPO as the default name for the cloned folder."

  (let* ((reponame (get-text-property 0 :repo cand))
         (package (consult-gh--get-package reponame)))
    (if consult-gh-confirm-before-clone
        (let* ((targetdir (read-directory-name (concat "Select Directory for " (propertize (format "%s: " reponame) 'face 'font-lock-keyword-face)) (or (file-name-as-directory consult-gh-default-clone-directory) default-directory) default-directory))
               (name (read-string "name: " package)))
          (consult-gh--repo-clone reponame name targetdir))
      (consult-gh--repo-clone reponame package consult-gh-default-clone-directory))))

(defvar consult-gh-repo-post-fork-hook nil
  "Functions called after `consult-gh--repo-fork'.

Full name of the forked repo e.g. “armindarvish/consult-gh”
is passed to these functions as input arg.")

(defun consult-gh--repo-fork (repo &optional name)
  "Fork REPO as NAME.

This is an internal function for non-interactive use.
For interactive use see `consult-gh-repo-fork'.

It runs the command “gh fork REPO --fork-name NAME”
using `consult-gh--command-to-string'."
  (let* ((package (consult-gh--get-package repo))
         (name (or name package))
         (forkrepo (concat (consult-gh--get-current-username) "/" name)))
    (consult-gh--make-process (format "consult-gh-fork-%s" repo)
                              :when-done `(lambda (_proc _str)
                                            (run-hook-with-args 'consult-gh-repo-post-fork-hook ,forkrepo)
                                            (message "repo %s was forked to %s" (propertize ,repo 'face 'font-lock-keyword-face) (propertize ,forkrepo 'face 'font-lock-warning-face)))
                              :cmd-args (list "repo" "fork" (format "%s" repo) "--fork-name" name))
    (let ((inhibit-message t))
      forkrepo)))

(defun consult-gh--repo-fork-action (cand)
  "Fork a repo candidate, CAND.

This is a wrapper function around `consult-gh--repo-fork'.
It parses CAND to extract relevant values \(e.g. repository name\)
and passes them to `consult-gh--repo-fork'.

To use this as the default action for repos,
set `consult-gh-repo-action' to `consult-gh--repo-fork-action'."
  (let* ((reponame (get-text-property 0 :repo cand)))
    (consult-gh--repo-fork reponame)))

(defun consult-gh--repo-create-scratch (&optional name directory owner description visibility make-readme gitignore-template license-key)
  "Create a new repository on github from scratch.

Description of Arguments:

 NAME               name of repository
 DIRECTORY          path to local directory of git repository
 OWNER              user/organization owning the repo
 DESCRIPTION        description for the repo
 VISIBILITY         private|public|internal
 MAKE-README        boolean, whether to make a readme file or not
 GITIGNORE-TEMPLATE name of gitignore template
 LICENSE-KEY        key for license template"
  (let* ((name (or name (read-string "Repository name: ")))
         (owner (or owner (consult--read (consult-gh--get-current-user-orgs nil t)
                                         :prompt "Repository owner: "
                                         :initial nil
                                         :sort nil
                                         :require-match t)))
         (targetrepo (concat (and owner (unless (string-empty-p owner) (concat owner "/"))) name))
         (description (or description (read-string "Description: ")))
         (description (and (stringp description)
                           (not (string-empty-p description))
                           description))
         (visibility (or visibility (downcase (consult--read (list "Public" "Private" "Internal")
                                                             :prompt "Visibility: "
                                                             :sort nil
                                                             :require-match t))))
         (readme (or make-readme (y-or-n-p "Would you like to add a README file?")))
         (gitignore (if (not gitignore-template) (y-or-n-p "Would you like to add a .gitignore?") t))
         (gitignore-template (or gitignore-template (and gitignore (string-trim (consult--read (consult-gh--get-gitignore-template-list)
                                                                                               :prompt (format "Select template for %s" (propertize ".gitignore " 'face 'error))
                                                                                               :sort nil
                                                                                               :require-match t)))))
         (license (if (not license-key) (y-or-n-p "Would you like to add a license?") t))
         (license-key (or license-key (and license (consult--read (consult-gh--get-license-template-list)
                                                                  :prompt (format "Select %s template" (propertize "license " 'face 'consult-gh-warning))
                                                                  :lookup #'consult--lookup-cdr
                                                                  :require-match t
                                                                  :sort nil))))
         (confirm (y-or-n-p (format "This will create %s as a %s repository on GitHub.  Continue?" (propertize name 'face 'consult-gh-repo) (propertize visibility 'face 'warning))))
         (clone (if confirm (y-or-n-p "Clone the new repository locally?")))
         (clonedir (if clone (read-directory-name (format "Select Directory to clone %s in " (propertize name 'face 'font-lock-keyword-face)) (or directory (file-name-as-directory consult-gh-default-clone-directory) default-directory))))
         (default-directory (or clonedir default-directory))
         (targetdir (expand-file-name name default-directory))
         (args '("repo" "create"))
         (out))

    (setq args (if (and targetrepo confirm visibility)
                   (delq nil (append args
                                     (list targetrepo)
                                     (list (concat "--" visibility))
                                     (and description (list "--description" description))
                                     (and readme (list "--add-readme"))
                                     (and gitignore-template (list "--gitignore" gitignore-template))
                                     (and license (list "--license" license-key))
                                     (and clone (list "--clone"))))))

    (setq out (apply #'consult-gh--call-process args))
    (if (eq (car out) 0)
        (when (and clone (file-exists-p targetdir))
          (message "repo %s was cloned to %s" (propertize name 'face 'font-lock-keyword-face) (propertize targetdir 'face 'font-lock-type-face))
          (run-hook-with-args 'consult-gh-repo-post-clone-hook targetdir)
          targetdir)
      (progn (message (cadr out))))))

(defun consult-gh--repo-create-template (&optional name owner description visibility template)
  "Create a new repository on github from TEMPLATE repo.

Description of Arguments:

 NAME        name of repository
 OWNER       user/organization owning the repo
 DESCRIPTION description for the repo
 VISIBILITY  private|public|internal
 TEMPLATE    Full name of template repo \(e.g. user/repo\)"
  (let* ((name (or name (read-string "Repository name: ")))
         (owner (or owner (consult--read (consult-gh--get-current-user-orgs nil t)
                                         :prompt "Repository owner: "
                                         :initial nil
                                         :sort nil
                                         :require-match t)))
         (targetrepo (concat (and owner (unless (string-empty-p owner) (concat owner "/"))) name))
         (description (or description (read-string "Description: ")))
         (description (and (stringp description)
                           (not (string-empty-p description))
                           description))
         (visibility (or visibility (downcase (consult--read (list "Public" "Private" "Internal")
                                                             :prompt "Visibility: "
                                                             :sort nil
                                                             :require-match t))))
         (templates (consult-gh--get-user-template-repos))
         (template (or template (and templates (consult--read templates
                                                              :prompt "Select template repository: "
                                                              :sort nil
                                                              :require-match t))))
         (template (and (stringp template) (not (string-empty-p template)) template))
         (use-scratch (if (not template) (y-or-n-p "No template selected.  Would you like to make the repo without template?"))))
    (cond
     (template
      (let* ((confirm (y-or-n-p (format "This will create %s as a %s repository on GitHub.  Continue?" (propertize name 'face 'consult-gh-repo) (propertize visibility 'face 'warning))))
             (clone (if confirm (y-or-n-p "Clone the new repository locally?")))
             (clonedir (if clone (read-directory-name (format "Select Directory to clone %s in " (propertize name 'face 'font-lock-keyword-face)) (or (file-name-as-directory consult-gh-default-clone-directory) default-directory))))
             (default-directory (or clonedir default-directory))
             (targetdir (expand-file-name name default-directory))
             (args '("repo" "create"))
             (out))

        (setq args (if (and targetrepo confirm visibility)
                       (delq nil (append args
                                         (list targetrepo)
                                         (list (concat "--" visibility))
                                         (list "--template" template)
                                         (and description (list "--description" description))
                                         (and clone (list "--clone"))))))

        (setq out (apply #'consult-gh--call-process args))
        (if (eq (car out) 0)
            (when (and clone (file-exists-p targetdir))
              (message "repo %s was cloned to %s" (propertize name 'face 'font-lock-keyword-face) (propertize targetdir 'face 'font-lock-type-face))
              (run-hook-with-args 'consult-gh-repo-post-clone-hook targetdir)
              targetdir)
          (progn (message (cadr out))))))
     (use-scratch
      (consult-gh--repo-create-scratch name owner description visibility))
     (t
      (message "aborted without making repository")))))

(defun consult-gh--repo-create-push-existing (&optional name directory owner description visibility)
  "Create a new repository on github from local repo in DIRECTORY.

Description of arguments:
 DIRECTORY   path to local directory of git repository
 NAME        name of repository
 OWNER       user/organization owning the repo
 DESCRIPTION description for the repo
 VISIBILITY  private|public|internal"
  (let* ((directory (or directory (read-directory-name "Path to local repository: " default-directory)))
         (directory (and (stringp directory) (file-directory-p directory) (file-expand-wildcards (expand-file-name ".git" directory)) directory))
         (use-scratch (if (not directory) (y-or-n-p "No git directory selected.  Would you like to make the repo from scratch instead?"))))
    (cond
     (directory
      (let* ((name (or name (read-string "Repository name: " (file-name-nondirectory (string-trim-right directory "/")))))
             (owner (or owner (consult--read (consult-gh--get-current-user-orgs nil t)
                                             :prompt "Repository owner: "
                                             :initial nil
                                             :sort nil
                                             :require-match t)))
             (targetrepo (concat (and owner (unless (string-empty-p owner) (concat owner "/"))) name))
             (description (or description (read-string "Description: ")))
             (description (and (stringp description)
                               (not (string-empty-p description))
                               description))
             (visibility (or visibility (downcase (consult--read (list "Public" "Private" "Internal")
                                                                 :prompt "Visibility: "
                                                                 :sort nil
                                                                 :require-match t))))
             (confirm (y-or-n-p (format "This will create %s as a %s repository on GitHub.  Continue?" (propertize name 'face 'consult-gh-repo) (propertize visibility 'face 'warning))))
             (remote (and confirm (y-or-n-p "Add a remote?") (read-string "What should the new remote be called? " "origin")))
             (remote (and (stringp remote) (not (string-empty-p remote)) remote))
             (args '("repo" "create"))
             (out))

        (setq args (if (and targetrepo confirm visibility)
                       (delq nil (append args
                                         (list targetrepo)
                                         (list (concat "--" visibility))
                                         (list "--source" (file-truename directory))
                                         (and description (list "--description" description))
                                         (and remote (list "--remote" remote))))))
        (setq out (apply #'consult-gh--call-process args))
        (message (cadr out))))
     (use-scratch
      (consult-gh--repo-create-scratch name owner description visibility))
     (t
      (message "Aborted without making repository!")))))

(defun consult-gh--issue-list-format (string input highlight)
  "Format minibuffer candidates for issues.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh issue list ...”\).
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match' in the minibuffer."
  (let* ((class "issue")
         (type "issue")
         (parts (string-split string "\t"))
         (repo (car (consult--command-split input)))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (car parts))
         (state (upcase (cadr parts)))
         (face (pcase state
                 ("CLOSED" 'consult-gh-success)
                 ("OPEN" 'consult-gh-warning)
                 (_ 'consult-gh-issue)))
         (title (cadr (cdr parts)))
         (tags (cadr (cdr (cdr parts))))
         (date (cadr (cdr (cdr (cdr parts)))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face face) ":" (propertize (format "%s" title) 'face 'consult-gh-default)) 70)
                      (propertize (consult-gh--set-string-width state 8) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date)
                      (propertize (consult-gh--set-string-width tags 18) 'face 'consult-gh-tags)
                      (consult-gh--set-string-width (concat (and user (propertize user 'face 'consult-gh-user)) (and package "/") (and package (propertize package 'face 'consult-gh-package))) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :number number :state state :title title :tags tags :date date :query query :class class :type type) str)
    str))

(defun consult-gh--search-issues-format (string input highlight)
  "Format candidates for issues.

Description of Arguments:

  STRING the output of a “gh” call
         \(e.g. “gh search issues ...”\).
  INPUT  the query from the user
         \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
           with `consult-gh-highlight-match' in the minibuffer."
  (let* ((class "issue")
         (type "issue")
         (parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (cadr parts))
         (state (upcase (cadr (cdr parts))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-success)
                 ("OPEN" 'consult-gh-warning)
                 (_ 'consult-gh-issue)))
         (title (cadr (cdr (cdr parts))))
         (tags (cadr (cdr (cdr (cdr parts)))))
         (date (cadr (cdr (cdr (cdr (cdr parts))))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face face) ":" (propertize (format "%s" title) 'face 'consult-gh-default)) 70)
                      (propertize (consult-gh--set-string-width state 8) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date)
                      (propertize (consult-gh--set-string-width tags 18) 'face 'consult-gh-tags)
                      (consult-gh--set-string-width (concat (and user (propertize user 'face 'consult-gh-user )) (and package "/") (and package (propertize package 'face 'consult-gh-package))) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo
                     :user user
                     :package package
                     :number number
                     :state state
                     :title title
                     :tags tags
                     :date date
                     :query query
                     :class class
                     :type type)
                         str)
    str))

(defun consult-gh--issue-state ()
  "State function for issue candidates.

This is passed as STATE to `consult--read' in `consult-gh-search-issues'
and is used to preview or do other actions on the issue."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if (and consult-gh-show-preview cand)
             (when-let ((repo (get-text-property 0 :repo cand))
                        (query (get-text-property 0 :query cand))
                        (number (get-text-property 0 :number cand))
                        (match-str (consult--build-args query))
                        (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (consult-gh--issue-view (format "%s" repo) (format "%s" number) buffer)
               (with-current-buffer buffer
                 (if consult-gh-highlight-matches
                     (cond
                      ((listp match-str)
                       (mapc (lambda (item)
                                 (highlight-regexp item 'consult-gh-preview-match)) match-str))
                      ((stringp match-str)
                       (highlight-regexp match-str 'consult-gh-preview-match)))))
               (funcall preview action
                        buffer))))
        ('return
         cand)))))

(defun consult-gh--issue-group (cand transform)
  "Group function for issue.

This is passed as GROUP to `consult--read' in `consult-gh-issue-list'
or `consult-gh-search-issues', and is used to group issues.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-issues-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "Number:Title " 68 nil ?-)
       (consult-gh--set-string-width " State " 10 nil ?-)
       (consult-gh--set-string-width " Date " 12 nil ?-)
       (consult-gh--set-string-width " Tags " 20 nil ?-)
       (consult-gh--set-string-width " Repo " 40 nil ?-))))))

(defun consult-gh--issue-browse-url-action (cand)
  "Browse the url for an issue candidate, CAND.

This is an internal action function that gets an issue candidate, CAND,
from `consult-gh-search-issues' and opens the url of the issue
in an external browser.

To use this as the default action for issues,
set `consult-gh-issue-action' to `consult-gh--issue-browse-url-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (get-text-property 0 :number cand)))
         (repo-url (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")))
         (url (and repo-url (concat repo-url "/issues/" number))))
    (funcall (or consult-gh-browse-url-func #'browse-url) url)))

(defun consult-gh--issue-read-json (repo number &optional json)
  "Get json response of issue of NUMBER in REPO.

Runs an async shell command with the command:
gh issue view NUMBER --repo REPO --json JSON,
and returns the output as a hash-table.

Optional argument JSON defaults to `consult-gh--issue-view-json-fields'."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false :false))
    (json-read-from-string (consult-gh--command-to-string "issue" "view" number "--repo" repo "--json" (or json consult-gh--issue-view-json-fields)))))

(defun consult-gh--issue-get-comments (repo number)
  "Get comments of issue NUMBER in REPO.

Retrieves a list of comments issue with id NUMBER in REPO.
Optional argument maxnum limits the number of comments retrieved."
  (consult-gh--json-to-hashtable (consult-gh--command-to-string "issue" "view" number "--repo" repo "--json" "comments") :comments))

(defun consult-gh--issue-get-commenters (table &optional comments)
  "Get a list of related users to an issue.

Retrieves a list of all related commenter users for the issue
stored in TABLE, a hash-table output
from `consult-gh--issue-read-json'.

Optional argument COMMENTS is a list o comments, for example
from running “gh issue view” with argument “--json comments”"
  (let* ((author (gethash :login (gethash :author table)))
         (assignees (gethash :assignees table))
         (assignees (and (listp assignees) (mapcar (lambda (item) (and (hash-table-p item) (gethash :login item))) assignees)))
         (comments (or comments (gethash :comments table)))
         (commenters (when (and comments (listp comments)) (cl-loop for comment in comments
                                                                    collect
                                                                    (when (hash-table-p comment)
                                                                      (gethash :login (gethash :author comment)))))))
         (cl-remove-duplicates (delq nil (append (list author) assignees commenters)) :test #'equal)))

(defun consult-gh--issue-format-header (repo number table &optional topic)
  "Format a header for an issue of NUMBER in REPO.

TABLE is a hash-table output containing issue information
from `consult-gh--issue-read-json'.  Returns a formatted string containing
the header section for `consult-gh--issue-view'.

The optional argument TOPIC is a propertized text where the related info
from the header will get appended to the properties.  For an example, see
the buffer-local variable `consult-gh--topic' in the buffer created by
`consult-gh--issue-view'."
  (let* ((title (gethash :title table))
         (author (gethash :login (gethash :author table)))
         (state (gethash :state table))
         (createdAt (gethash :createdAt table))
         (updatedAt (gethash :updatedAt table))
         (updatedAt (and updatedAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time updatedAt))))
         (url (gethash :url table))
         (closedAt (gethash :closedAt table))
         (closedAt (and closedAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time closedAt))))
         (labels (gethash :labels table))
         (labels (and labels (listp labels) (mapcar (lambda (item) (and (hash-table-p item) (gethash :name item))) labels)))
         (labels-text (and labels (listp labels) (mapconcat #'identity labels "\s\s")))
         (milestone (gethash :milestone table))
         (milestone-title (and (hash-table-p milestone) (gethash :title milestone)))
         (milestone-text (and (hash-table-p milestone) (concat (format "%s-%s\s\s(%s)"
                                                                       (or (gethash :number milestone) "")
                                                                       (or (gethash :title milestone) "N/A")
                                                                       (or (gethash :description milestone) "no description")))))
         (assignees (gethash :assignees table))
         (assignees (and assignees (listp assignees) (mapcar (lambda (item) (when (hash-table-p item) (gethash :login item))) assignees)))
         (assignees-text (and assignees (listp assignees) (mapconcat #'identity assignees ",\s")))
         (projects (gethash :projectItems table))
         (projects (and projects (listp projects) (mapcar (lambda (item) (when (hash-table-p item) (gethash :title item))) projects)))
         (projects-text (and projects (listp projects) (mapconcat #'identity projects ",\s"))))

    (when (stringp topic)
      (add-text-properties 0 1 (list :author author :state state :lastUpdated updatedAt :labels labels :milestone milestone-title :assignees assignees :projects projects) topic))

    (concat "title: " title "\n"
            "author: " author "\n"
            "repository: " repo "\n"
            "number: " number "\n"
            "state: " state "\n"
            (and createdAt (concat "created: " createdAt "\n"))
            (and updatedAt (concat "lastUpdated: " updatedAt "\n"))
            (and closedAt (concat "closed: " closedAt "\n"))
            (and url (concat "url: " url "\n"))
            (and assignees-text (concat "assignees: " assignees-text "\n"))
            (and labels-text (concat "labels: " "[ " labels-text " ]""\n"))
            (and milestone-text (concat "milestone: " milestone-text "\n"))

            (and projects-text (concat "projects: " projects-text "\n"))
            "\n--\n")))

(defun consult-gh--issue-format-body (table &optional topic)
  "Format a body section for an issue stored in TABLE.

This function returns a formatted string containing the body section for
`consult-gh--issue-view'.

TABLE is a hash-table output from `consult-gh--issue-read-json'
containing issue's body under the key :body.

The optional argument TOPIC is a propertized text where the related info
from the header will get appended to the properties.  For an example, see
the buffer-local variable `consult-gh--topic' in the buffer created by
`consult-gh--issue-view'."
  (let* ((author (gethash :login (gethash :author table)))
         (body (gethash :body table))
         (createdAt (gethash :createdAt table)))

    (when topic (add-text-properties 0 1 (list :body body) topic))

    (concat author " " (consult-gh--time-ago createdAt)
            " " (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)) "\n"
            "---\n" body "\n" "\n")))

(defun consult-gh--issue-filter-comments (comments &optional maxnum)
  "Filter COMMENTS when there are more than MAXNUM.

Queries the user for how to filter the comments."

  (let ((maxnum (or maxnum consult-gh-comments-maxnum)))
    (when (and (listp comments) (> (length comments) maxnum))
      (pcase (consult--read (list (cons "Load Everything" :nil)
                                  (cons "Load comments in the last week." :last-week)
                                  (cons "Load comments in the last month." :last-month)
                                  (cons (format "Load up to %s latest comments." maxnum) :last-maxnum)
                                  (cons "Load comments since a particular date" :date)
                                  (cons "Load comments in a date range" :daterange))
                            :prompt (format "There are more than %s comments on that pull request. Do you want to load them all?" maxnum)
                            :lookup #'consult--lookup-cdr
                            :sort nil)
        (':last-week
         (setq comments (cl-remove-if-not (lambda (k)
                                            (time-less-p (encode-time (decoded-time-add (decode-time (current-time) t) (make-decoded-time :day -7))) (date-to-time (gethash :createdAt k))))
                                          comments)))
        (':last-month
         (setq comments (cl-remove-if-not (lambda (k)
                                            (time-less-p (encode-time (decoded-time-add (decode-time (current-time) t) (make-decoded-time :day -30))) (date-to-time (gethash :createdAt k))))
                                          comments)))
        (':last-maxnum
         (setq comments (cl-subseq comments 0 (min (length comments) maxnum))))
        (':date
         (let ((d (org-read-date nil t)))
           (setq comments (cl-remove-if-not (lambda (k)
                                              (time-less-p d (date-to-time (gethash :createdAt k))))
                                            comments))))
        (':daterange (let ((begin-date (org-read-date nil t nil "Select Beginning"))
                           (end-date (org-read-date nil t nil "Select End")))
                       (setq comments (cl-remove-if-not (lambda (k)
                                                          (let ((date (date-to-time (gethash :createdAt k))))
                                                            (or (time-less-p begin-date date)
                                                                (time-less-p date end-date))))

                                                        comments))))))
    comments))

(defun consult-gh--issue-format-comments (comments)
  "Format the COMMENTS.

COMMENTS must be a list of hash-tables containing comment for exmplae from
`consult-gh--issue-get-comments'.

This function returns a formatted string containing the comments section
for `consult-gh--issue-view'."
  (let* ((header-marker "#")
         (out nil))
    (when (listp comments)
      (cl-loop for comment in comments
               do
               (when (hash-table-p comment)
                 (let* ((author (gethash :author comment))
                        (author (and author (gethash :login author)))
                        (authorAssociation (gethash :authorAssociation comment))
                        (authorAssociation (unless (equal authorAssociation "NONE")
                                             authorAssociation))
                        (createdAt (gethash :createdAt comment))
                        (createdAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)))
                        (comment-url (gethash :url comment))
                        (body (gethash :body comment)))
                   (save-match-data
                     (when (and body (string-match (concat "^" header-marker " .*$")  body ))
                       (setq body (with-temp-buffer
                                    (insert body)
                                    (goto-char (point-min))
                                    (while (re-search-forward (concat "^" header-marker " +?.*$") nil t)
                                      (replace-match (concat header-marker "\\&")))
                                    (buffer-string)))))
                   (setq out (concat out
                                     (propertize
                                      (concat (and author (concat header-marker " " author " "))
                                              (and authorAssociation (concat "(" authorAssociation ")"))
                                              (and createdAt (concat (consult-gh--time-ago createdAt) " " createdAt))
                                              "\n"
                                              (and body (concat body "\n")))
                                      :consult-gh (list :author author :comment-url comment-url))))))))
    out))

(defun consult-gh--issue-view (repo number &optional buffer preview title)
  "Open ISSUE of REPO in an Emacs buffer, BUFFER.

This is an internal function that takes REPO, the full name of a
repository \(e.g. “armindarvish/consult-gh”\) and ISSUE,
a issue number of that repository, and shows
the contents of the issue in an Emacs buffer.

It fetches the preview of the ISSUE by running the command
“gh issue view ISSUE --repo REPO” using `consult-gh--call-process'
and put it as raw text in either BUFFER or if BUFFER is nil,
in a buffer named by `consult-gh-preview-buffer-name'.
If `consult-gh-issue-preview-major-mode' is non-nil, uses it as
major-mode, otherwise shows the raw text in \='fundamental-mode.

Description of Arguments:

  REPO    a string; the full name of the repository
  NUMBER  a string; issue id number
  BUFFER  a string; optional buffer name
  PREVIEW a boolean; whether to load reduced preview
  TITLE   a string; an optional title string

To use this as the default action for repos,
see `consult-gh--issue-view-action'."
  (let* ((topic (format "%s/#%s" repo number))
         (canAdmin (consult-gh--user-canadmin repo))
         (buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
         (table (consult-gh--issue-read-json repo number))
         (state (gethash :state table))
         (comments (unless preview (consult-gh--issue-filter-comments (consult-gh--issue-get-comments repo number))))
         (commenters (and table (not preview) (consult-gh--issue-get-commenters table comments)))
         (header-text (and table (consult-gh--issue-format-header repo number table topic)))
         (title (or title (car (split-string header-text "\n" t))))
         (title (string-trim-left title "title: "))
         (body-text (consult-gh--issue-format-body table topic))
         (comments-text (unless preview (consult-gh--issue-format-comments comments))))
    (unless preview
      ;; collect issues of repo for completion at point
      (consult-gh--completion-set-issues topic repo)

      ;; collect prs of repo for completion at point
      (consult-gh--completion-set-prs topic repo)

      ;; collect mentionable users for completion at point
      (consult-gh--completion-set-mentionable-users topic repo)

      ;; collect list of repo branches for completion at point
      (consult-gh--completion-set-branches topic repo)

       (if canAdmin
        (progn
          ;; collect labels for completion at point
          (consult-gh--completion-set-valid-labels topic repo)
          ;; collect valid assignees for completion at point
          (consult-gh--completion-set-assignable-users topic repo)
          ;; collect valid milestones for completion at point
          (consult-gh--completion-set-valid-milestones topic repo))

      (add-text-properties 0 1 (list :valid-labels nil :assignable-users nil :valid-milestones nil :valid-projects nil) topic)))

    (add-text-properties 0 1 (list :repo repo :type "issue" :commenters (mapcar (lambda (item) (concat "@" item)) commenters) :number number :title title :state state) topic)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when header-text (insert header-text))
        (save-excursion
          (when (eq consult-gh-issue-preview-major-mode 'org-mode)
           (consult-gh--github-header-to-org buffer)))
        (when body-text (insert body-text))
        (when comments-text (insert comments-text))
        (pcase consult-gh-issue-preview-major-mode
          ('gfm-mode
           (gfm-mode)
           (when (display-images-p)
             (markdown-display-inline-images)))
          ('markdown-mode
           (markdown-mode)
           (when (display-images-p)
             (markdown-display-inline-images)))
          ('org-mode
           (let ((org-display-remote-inline-images 'download))
             (consult-gh--markdown-to-org)))
          (_
           (consult-gh--markdown-to-org-emphasis)
           (outline-mode)))
        (set-buffer-file-coding-system 'unix)
        (goto-char (point-min))
        (save-excursion
          (while (re-search-forward "\r\n" nil t)
            (replace-match "\n")))
        (outline-hide-sublevels 1)
        (consult-gh-issue-view-mode +1)
        (setq-local consult-gh--topic topic)
        (current-buffer)))))

(defun consult-gh--issue-view-action (cand)
  "Open the preview of an issue candidate, CAND.

This is a wrapper function around `consult-gh--issue-view'.
It parses CAND to extract relevant values
\(e.g. repository's name and issue number\)
and passes them to `consult-gh--issue-view'.

To use this as the default action for issues,
set `consult-gh-issue-action' to `consult-gh--issue-view-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (format "%s" (get-text-property 0 :number cand))))
         (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/issues/" number "*"))
         (existing (get-buffer buffername))
         (confirm (if (and existing (not (= (buffer-size existing) 0)))
                      (consult--read
                       (list (cons "Switch to existing buffer." :resume)
                             (cons "Reload the issue in the existing buffer." :replace)
                             (cons "Make a new buffer and load the issue in it (without killing the old buffer)." :new))
                       :prompt "You already have this issue open in another buffer.  Would you like to switch to that buffer or make a new one? "
                       :lookup #'consult--lookup-cdr
                       :sort nil
                       :require-match t))))

(if existing
      (cond
       ((eq confirm :resume) (funcall consult-gh-switch-to-buffer-func existing))
       ((eq confirm :replace)
        (message "Reloading issue in the existing buffer...")
        (funcall consult-gh-switch-to-buffer-func (consult-gh--issue-view repo number existing))
        (set-buffer-modified-p nil)
        (buffer-name (current-buffer)))
       ((eq confirm :new)
        (message "Opening issue in a new buffer...")
        (funcall consult-gh-switch-to-buffer-func (consult-gh--issue-view repo number (generate-new-buffer buffername nil)))
        (set-buffer-modified-p nil)
        (buffer-name (current-buffer))))
      (progn
        (funcall consult-gh-switch-to-buffer-func (consult-gh--issue-view repo number))
        (rename-buffer buffername t)
        (set-buffer-modified-p nil)
        (buffer-name (current-buffer))))))

(defun consult-gh-topics--issue-parse-metadata ()
  "Parse issue topic metadata."
  (let* ((region (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
         (header (when region (buffer-substring-no-properties (car region) (cdr region))))
         (assignees nil)
         (labels nil)
         (milestone nil)
         (projects nil))
(when (and header (string-match "^.*\\(?:assignees:\\)\\(?1:.*\\)\\(?:\n.*labels:\\)?\\(?2:.*\\)?\\(?:\n.*milestone:\\)?\\(?3:.*\\)?\\(?:\n.*projects:\\)?\\(?4:.*\\)?\\(?:\n-+\\)" header))

  (setq assignees (match-string 1 header)
        labels (match-string 2 header)
        milestone (match-string 3 header)
        projects (match-string 4 header)))
  (list (and (stringp assignees)
             (string-trim assignees))
        (and (stringp labels)
             (string-trim labels))
        (and (stringp milestone)
             (string-trim milestone))
        (and (stringp projects)
             (string-trim projects)))))

(defun consult-gh-topics--issue-get-metadata (&optional issue)
  "Get metadata of ISSUE.

ISSUE defaults to `consult-gh--topic'."

  (let* ((issue (or issue consult-gh--topic))
         (repo (get-text-property 0 :repo issue))
         (canAdmin (consult-gh--user-canadmin repo))
         (assignees (get-text-property 0 :assignees issue))
         (labels (get-text-property 0 :labels issue))
         (milestone (get-text-property 0 :milestone issue))
         (projects  (get-text-property 0 :projects issue))
         (valid-assignees (append (get-text-property 0 :assignable-users issue) (list "@me")))
         (valid-labels (get-text-property 0 :valid-labels issue))
         (valid-projects (get-text-property 0 :valid-projects issue))
         (valid-milestones (get-text-property 0 :valid-milestones issue)))

    (when canAdmin
      (pcase-let* ((`(,text-assignees ,text-labels ,text-milestone ,text-projects) (consult-gh-topics--issue-parse-metadata)))

        (when (derived-mode-p 'org-mode)
          (setq text-assignees (or (cadar (org-collect-keywords '("assignees"))) text-assignees)
                text-labels (or (cadar (org-collect-keywords '("labels"))) text-labels)
                text-milestone (or (cadar (org-collect-keywords '("milestone"))) text-milestone)
                text-projects (or (cadar (org-collect-keywords '("projects"))) text-projects)))


        (when (stringp text-assignees)
          (setq assignees (cl-remove-duplicates
                           (cl-remove-if-not
                            (lambda (item) (member item valid-assignees))
                            (split-string text-assignees "," t "[ \t]+"))
                           :test #'equal)))

        (when (stringp text-labels)
          (setq labels (cl-remove-duplicates
                        (cl-remove-if-not
                         (lambda (item) (member item valid-labels))
                         (split-string text-labels "," t "[ \t]+"))
                        :test #'equal)))

        (when (stringp text-milestone)
          (cond
           ((member text-milestone valid-milestones)
            (setq milestone text-milestone))
           (t (setq milestone nil))))

        (when (stringp text-projects)
          (setq projects (cl-remove-duplicates
                          (cl-remove-if-not
                           (lambda (item) (member item valid-projects))
                           (split-string text-projects "," t "[ \t]+"))
                          :test #'equal)))))

    (list (cons "assignees" assignees)
          (cons "labels" labels)
          (cons "milestone" milestone)
          (cons "projects" projects))))

(defun consult-gh-topics--issue-create-add-metadata (&optional repo issue)
  "Add metadata to ISSUE of REPO.

This is used when creating new issues for REPO."
  (let* ((issue (or issue consult-gh--topic))
         (meta (consult-gh-topics--issue-get-metadata issue))
         (repo (or repo (get-text-property 0 :repo issue)))
         (canAdmin (consult-gh--user-canadmin repo))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))
    (when canAdmin
      ;; add assignees
      (and (y-or-n-p "Would you like to add assignees?")
           (let* ((current (cdr (assoc "assignees" meta)))
                  (table (consult-gh--get-assignable-users repo))
                  (users (append (list "@me") table))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Users: " users)) :test #'equal)))
             (add-text-properties 0 1 (list :assignees (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) issue)

             (save-excursion (goto-char (car header))
                             (when (re-search-forward "^.*assignees: \\(?1:.*\\)?" (cdr header) t)
                               (replace-match (mapconcat #'identity (get-text-property 0 :assignees issue) ", ") nil nil nil 1)))))

      ;; add labels
      (and (y-or-n-p "Would you like to add lables?")
           (let* ((current (cdr (assoc "labels" meta)))
                  (labels (consult-gh--get-labels repo))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Labels: " labels)) :test #'equal)))
             (add-text-properties 0 1 (list :labels (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) issue)


             (save-excursion (goto-char (car header))
                             (when (re-search-forward "^.*labels: \\(?1:.*\\)?" (cdr header) t)
                               (replace-match (mapconcat #'identity (get-text-property 0 :labels issue) ", ") nil nil nil 1)))))

      ;; add projects
      (and (y-or-n-p "Would you like to add projects?")
           (let* ((current (cdr (assoc "projects" meta)))
                  (table (consult-gh--get-projects repo))
                  (table (and (hash-table-p table) (gethash :Nodes table)))
                  (projects (and (listp table) (mapcar (lambda (item) (gethash :title item)) table)))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Projects: " projects)) :test #'equal)))
             (add-text-properties 0 1 (list :projects (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) issue)

             (save-excursion (goto-char (car header))
                             (when (re-search-forward "^.*projects: \\(?1:.*\\)?" (cdr header) t)
                               (replace-match (mapconcat #'identity (get-text-property 0 :projects issue) ", ") nil nil nil 1)))))

      ;; add a milestone
      (and (y-or-n-p "Would you like to add a milestone?")
           (let* ((milestones (consult-gh--get-milestones repo))
                  (selection (if milestones
                                 (consult--read milestones
                                                :prompt "Select a Milestone: "
                                                :require-match t))))
             (if (string-empty-p selection) (setq selection nil))
             (add-text-properties 0 1 (list :milestone selection) issue)
             (save-excursion (goto-char (car header))
                             (when (re-search-forward "^.*milestone: \\(?1:.*\\)?" (cdr header) t)
                               (replace-match (get-text-property 0 :milestone issue) nil nil nil 1))))))
    (setq consult-gh--topic issue)))

(defun consult-gh-topics--issue-create-submit (repo title body &optional assignees labels milestone projects web)
  "Create a new issue in REPO with TITLE and BODY.

Description of Arguments:
  REPO      a string; full name of the repository
  TITLE     a string; title of the issue
  BODY      a string; body of the issue
  ASSIGNEES a list of strings; list of assignees
  LABELS    a list of strings; list of labels
  MILESTONE a string; a milestone
  PROJECTS  a list of strings; list of projects
  WEB       a boolean; whether to continuing editing on the web?"
  (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t))))
         (title (or title (consult--read nil :prompt "Title: ")))
         (body (or body (consult--read nil :prompt "Body: ")))
         (assignees (if (stringp assignees)
                        assignees
                      (and assignees (listp assignees)
                           (mapconcat #'identity assignees ","))))
         (labels (if (stringp labels)
                     labels
                   (and labels (listp labels)
                        (mapconcat #'identity labels ","))))
         (projects (if (stringp projects)
                       projects
                     (and projects (listp projects)
                          (mapconcat #'identity projects ","))))
         (milestone (if (and (stringp milestone) (not (string-empty-p milestone)))
                        milestone
                      (and milestone (listp milestone)
                           (car milestone))))
         (args nil))
    (when (and title body)
      (setq args (delq nil (append args
                                   (list "--repo" repo)
                                   (list "--title" title)
                                   (list "--body" body)
                                   (and assignees (list "--assignee" assignees))
                                   (and labels (list "--label" labels))
                                   (and milestone (list "--milestone" milestone))
                                   (and projects (list "--project" projects))
                                   (and web (list "--web")))))
      (apply #'consult-gh--command-to-string "issue" "create" args))))

(defun consult-gh-topics--issue-create-presubmit (issue)
  "Prepare ISSUE to submit for creating a new issue.

ISSUE is a string with properties that identify a github issue.
For an example see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh-issue-create'."
  (if consult-gh-topics-edit-mode
      (let* ((repo (get-text-property 0 :repo issue))
             (canAdmin (consult-gh--user-canadmin repo))
             (nextsteps (append (list (cons "Submit" :submit))
                                (list (cons "Continue in the Browser" :browser))
                                (and canAdmin (list (cons "Add Metadata" :metadata)))
                                (list (cons "Cancel" :cancel))))
             (next (consult--read nextsteps
                                  :prompt "Choose what to do next? "
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))
        (while (eq next ':metadata)
          (consult-gh-topics--issue-create-add-metadata)
          (setq next (consult--read nextsteps
                                    :prompt "Choose what to do next? "
                                    :lookup #'consult--lookup-cdr
                                    :sort nil)))
        (pcase-let* ((`(,title . ,body) (consult-gh-topics--get-title-and-body))
                     (title (or title
                                (and (derived-mode-p 'org-mode)
                                     (cadar (org-collect-keywords
                                             '("title"))))
                                ""))
                     (body (or body ""))
                     (metadata (consult-gh-topics--issue-get-metadata))
                     (assignees (cdr (assoc "assignees" metadata)))
                     (labels (cdr (assoc "labels" metadata)))
                     (milestone (cdr (assoc "milestone" metadata)))
                     (projects (cdr (assoc "projects" metadata))))

          (pcase next
            (':browser (and (consult-gh-topics--issue-create-submit repo title body assignees labels milestone projects t)))
            (':submit (and (consult-gh-topics--issue-create-submit repo title body assignees labels milestone projects nil)
                           (message "Issue Submitted!")
                           (funcall consult-gh-quit-window-func t))))))
    (message "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-restore-default (&optional issue)
  "Restore default values when editing an ISSUE."
  (if consult-gh-topics-edit-mode
  (let* ((issue (or issue consult-gh--topic))
         (repo (get-text-property 0 :repo issue))
         (canAdmin (consult-gh--user-canadmin repo))
         (title (get-text-property 0 :original-title issue))
         (body (get-text-property 0 :original-body issue))
         (assignees (get-text-property 0 :original-assignees issue))
         (labels (get-text-property 0 :original-labels issue))
         (milestone (get-text-property 0 :original-milestone issue))
         (projects (get-text-property 0 :original-projects issue))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))

    (add-text-properties 0 1 (list :title title :assignees assignees :labels labels :milestone milestone :projects projects) issue)

    (save-excursion
      ;; change title
      (goto-char (point-min))
      (when (re-search-forward "^.*title: \\(?1:.*\\)?" nil t)
        (replace-match (get-text-property 0 :title issue) nil nil nil 1))

      (when canAdmin
        ;;change assignees
        (goto-char (car header))
        (when (re-search-forward "^.*assignees: \\(?1:.*\\)?" (cdr header) t)
          (replace-match (mapconcat #'identity (get-text-property 0 :assignees issue) ", ") nil nil nil 1))

        ;; change labels
        (goto-char (car header))
        (when (re-search-forward "^.*labels: \\(?1:.*\\)?" (cdr header) t)
          (replace-match (mapconcat #'identity (get-text-property 0 :labels issue) ", ") nil nil nil 1))

        ;; change projects
        (goto-char (car header))
        (when (re-search-forward "^.*projects: \\(?1:.*\\)?" (cdr header) t)
          (replace-match (mapconcat #'identity (get-text-property 0 :projects issue) ", ") nil nil nil 1)))

      ;; change milestone
      (if (equal milestone nil) (setq milestone ""))
      (goto-char (car header))
      (when (re-search-forward "^.*milestone: \\(?1:.*\\)?" (cdr header) t)
        (replace-match (get-text-property 0 :milestone issue) nil nil nil 1))

      ;; change body
      (goto-char (cdr header))
      (delete-region (point) (point-max))
      (insert body)))
(error "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-change-title (&optional new old issue)
  "Change title of ISSUE from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((issue (or issue consult-gh--topic))
         (new (or new (consult--read nil
                                     :initial old
                                     :prompt "Title: ")))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))
    (add-text-properties 0 1 (list :title new) issue)

    (when (stringp new)
      (save-excursion (goto-char (point-min))
                      (when (re-search-forward "^.*title: \\(?1:.*\\)?" (and header (consp header) (cdr header)) t)
                        (replace-match (get-text-property 0 :title issue) nil nil nil 1)))))
    (error "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-change-body (&optional new old issue)
  "Change body of ISSUE from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((issue (or issue consult-gh--topic))
             (new (or new (consult--read nil
                                     :initial old
                                     :prompt "Body: ")))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))

    (when (and (stringp new) (not (string-empty-p new)))
      (add-text-properties 0 1 (list :body new) issue)

      (save-excursion
        (goto-char (cdr header))
        (delete-region (point) (point-max))
        (insert new))))
    (error "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-change-assignees (&optional new old issue)
  "Change assignees of ISSUE from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((issue (or issue consult-gh--topic))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (valid-assignees (get-text-property 0 :assignable-users issue))
             (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
             (old (cond ((stringp old) old)
                        ((and (listp old) (length> old 1)) (mapconcat #'identity old sep))
                        ((and (listp old) (length< old 2)) (car old))))
             (old (if (and (stringp old) (not (string-suffix-p sep old)))
                      (concat old sep)
                    old))
             (new (or new
                      (if (and valid-assignees (listp valid-assignees))
                          (completing-read-multiple "Select Asignees: " valid-assignees nil t old)
                        (error "No assignable users found")))))

        (when (listp new)
          (setq new (cl-remove-duplicates
                     (cl-remove-if-not (lambda (item) (member item valid-assignees)) new) :test #'equal))
          (add-text-properties 0 1 (list :assignees new) issue)

          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*assignees: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (mapconcat #'identity (get-text-property 0 :assignees issue) ", ") nil nil nil 1)))))
    (error "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-change-labels (&optional new old issue)
  "Change labels of ISSUE from OLD to NEW."
(if consult-gh-topics-edit-mode
  (let* ((issue (or issue consult-gh--topic))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
         (valid-labels (get-text-property 0 :valid-labels issue))
         (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
         (old (cond ((stringp old) old)
                    ((and (listp old) (length> old 1)) (mapconcat #'identity old sep))
                    ((and (listp old) (length< old 2)) (car old))))
         (old (if (and (stringp old) (not (string-suffix-p sep old)))
                  (concat old sep)
                old))
         (new (or new
                  (if (and valid-labels (listp valid-labels))
                      (completing-read-multiple "Select Labels: " valid-labels nil t old)
                    (error "No labels found!")))))

    (when (listp new)
      (setq new (cl-remove-duplicates
                 (cl-remove-if-not (lambda (item) (member item valid-labels)) new) :test #'equal))
      (add-text-properties 0 1 (list :labels new) issue)

      (save-excursion (goto-char (car header))
                      (when (re-search-forward "^.*labels: \\(?1:.*\\)?" (cdr header) t)
                        (replace-match (mapconcat #'identity (get-text-property 0 :labels issue) ", ") nil nil nil 1)))))
(error "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-change-projects (&optional new old issue)
  "Change projects of ISSUE from OLD to NEW."
  (if consult-gh-topics-edit-mode
  (let* ((issue (or issue consult-gh--topic))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
         (valid-projects (get-text-property 0 :valid-projects issue))
         (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
         (old (cond ((stringp old) old)
                    ((and (listp old) (length> old 1)) (mapconcat #'identity old sep))
                    ((and (listp old) (length< old 2)) (car old))))
         (old (if (and (stringp old) (not (string-suffix-p sep old)))
                  (concat old sep)
                old))
         (new (or new
                  (if (and valid-projects (listp valid-projects))
                      (completing-read-multiple "Select Projects: " valid-projects nil t old)
                    (error "No projects found!")))))

    (when (listp new)
      (setq new (cl-remove-duplicates
                 (cl-remove-if-not (lambda (item) (member item valid-projects)) new) :test #'equal))
      (add-text-properties 0 1 (list :projects new) issue)

      (save-excursion (goto-char (car header))
                      (when (re-search-forward "^.*projects: \\(?1:.*\\)?" (cdr header) t)
                        (replace-match (mapconcat #'identity (get-text-property 0 :projects issue) ", ") nil nil nil 1)))))
(error "Not in an issue editing buffer!")))

(defun consult-gh-issue--edit-change-milestone (&optional new old issue)
  "Change milestone of ISSUE from OLD to NEW."
  (if consult-gh-topics-edit-mode
  (let* ((issue (or issue consult-gh--topic))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
         (valid-milestones (get-text-property 0 :valid-milestones issue))
         (new (or new
                  (if (and valid-milestones (listp valid-milestones))
                      (consult--read valid-milestones
                                     :initial old
                                     :prompt "Milestone: "
                                     :require-match t)
                    (error "No milestones found!")))))

    (when (stringp new)
      (if (string-empty-p new) (setq new nil))
      (add-text-properties 0 1 (list :milestone new) issue)
      (save-excursion (goto-char (car header))
                      (when (re-search-forward "^.*milestone: \\(?1:.*\\)?" (cdr header) t)
                        (replace-match (get-text-property 0 :milestone issue) nil nil nil 1)))))
(error "Not in an issue editing buffer!")))

(defun consult-gh-topics--edit-issue-submit (issue &optional title body assignees labels projects milestone)
  "Edit ISSUE with new metadata.

Description of Arguments:
  ISSUE     a plis: list of key value pairs for issue
  TITLE     a string; new title
  BODY      a string; new body
  ASSIGNEES a list of strings; new list of assignees
  LABELS    a list of strings; new list of labels
  PROJECTS  a list of strings; new list of projects
  MILESTONE a string; new milestone"
  (pcase-let* ((repo (or (get-text-property 0 :repo issue) (get-text-property 0 :repo (consult-gh-search-repos nil t))))
               (canAdmin (consult-gh--user-canadmin repo))
               (token-scopes (consult-gh--auth-get-token-scopes))
               (number (or (get-text-property 0 :number issue)  (get-text-property 0 :number (consult-gh-issue-list repo t))))
               (original-title (get-text-property 0 :original-title issue))
               (original-body (get-text-property 0 :original-body issue))
               (original-assignees (get-text-property 0 :original-assignees issue))
               (original-labels (get-text-property 0 :original-labels issue))
               (original-projects (get-text-property 0 :original-projects issue))
               (original-milestone (get-text-property 0 :original-milestone issue))
               (`(,add-assignees ,remove-assignees)                          (consult-gh--separate-add-and-remove assignees original-assignees))
               (`(,add-labels ,remove-labels)                          (consult-gh--separate-add-and-remove labels original-labels))
               (`(,add-projects ,remove-projects)                          (consult-gh--separate-add-and-remove projects original-projects))
               (add-milestone (and (not (equal milestone original-milestone)) (stringp milestone) milestone))
               (remove-milestone (and (not (equal milestone original-milestone)) (or (equal milestone nil) (string-empty-p milestone))))
               (title (and (not (equal title original-title)) title))
               (body (and (not (equal body original-body)) body))
               (args (list "--repo" repo)))

    (when (and add-assignees (listp add-assignees)) (setq add-assignees (consult-gh--list-to-string add-assignees)))

    (when (and remove-assignees (listp remove-assignees)) (setq remove-assignees (consult-gh--list-to-string remove-assignees)))

    (when (and add-labels (listp add-labels))
      (setq add-labels  (consult-gh--list-to-string add-labels)))

    (when (and remove-labels (listp remove-labels))
      (setq remove-labels (consult-gh--list-to-string remove-labels)))

    (when (and add-projects (listp add-projects))
      (setq add-projects (consult-gh--list-to-string add-projects)))

    (when (and remove-projects (listp remove-projects))
      (setq remove-projects (consult-gh--list-to-string remove-projects)))

    (when (or (and canAdmin (or title body add-assignees remove-assignees add-labels remove-labels add-milestone remove-milestone add-projects remove-projects))
              (or title body))
      (setq args (delq nil (append args
                                   (and title (list "--title" (concat (substring-no-properties title))))
                                   (and body (list "--body" (concat (substring-no-properties body) )))
                                   (and canAdmin add-assignees (list "--add-assignee" add-assignees))
                                   (and canAdmin remove-assignees (list "--remove-assignee" remove-assignees))
                                   (and canAdmin add-labels (list "--add-label" add-labels))
                                   (and canAdmin remove-labels (list "--remove-label" remove-labels))
                                   (and canAdmin (member "project" token-scopes) add-projects (list "--add-project" add-projects))
                                   (and canAdmin (member "project" token-scopes) remove-projects (list "--remove-project" remove-projects))
                                   (and canAdmin add-milestone (list "--milestone" (concat (substring-no-properties add-milestone))))
                                   (and canAdmin remove-milestone (list "--remove-milestone")))))

      (apply #'consult-gh--command-to-string "issue" "edit" number args))))

(defun consult-gh-topics--edit-issue-presubmit (issue)
  "Prepare edits on ISSUE to submit.

ISSUE is a string with properties that identify a github issue.
For an example see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh--issue-view'."
  (if consult-gh-topics-edit-mode
      (let* ((repo (get-text-property 0 :repo issue))
             (canAdmin (consult-gh--user-canadmin repo))
             (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
             (isAuthor (consult-gh--user-isauthor issue))
             (nextsteps (if (or canAdmin isAuthor)
                            (append (list (cons "Submit" :submit))
                                    (and canAdmin
                                         (list (cons "Add/Remove Assignees" :assignees) (cons "Add/Remove Labels" :labels) (cons "Change Milestone" :milestone) (cons "Add/Remove Projects" :projects)))
                                    (list (cons "Change Title" :title))
                                    (list (cons "Change Body" :body))
                                    (list (cons "Discard edits and restore original values" :default))
                                    (list (cons "Cancel" :cancel)))
                          (user-error "Current user, %s, does not have permissions to edit this pull request" user)))
             (next (consult--read nextsteps
                                  :prompt "Choose what do you want to do? "
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))

        (pcase-let* ((`(,title . ,body) (consult-gh-topics--get-title-and-body))
                     (title (or title
                                (and (derived-mode-p 'org-mode)
                                     (cadar (org-collect-keywords
                                             '("title"))))
                                ""))
                     (body (or body ""))
                     (metadata (when canAdmin (consult-gh-topics--issue-get-metadata)))
                     (assignees (when metadata (cdr (assoc "assignees" metadata))))
                     (labels (when metadata (cdr (assoc "labels" metadata))))
                     (milestone (when canAdmin (cdr (assoc "milestone" metadata))))
                     (projects (when canAdmin (cdr (assoc "projects" metadata)))))

          (pcase next
            (':default (consult-gh-issue--edit-restore-default))
            (':title (consult-gh-issue--edit-change-title nil title))
            (':body  (consult-gh-issue--edit-change-body nil nil))
            (':assignees (consult-gh-issue--edit-change-assignees nil assignees))
            (':labels (consult-gh-issue--edit-change-labels nil labels))
            (':milestone (consult-gh-issue--edit-change-milestone nil milestone))
            (':projects (consult-gh-issue--edit-change-projects nil projects))
            (':submit
             (and (consult-gh-topics--edit-issue-submit issue title body assignees labels milestone projects)
                  (message "Edits Submitted!")
                  (funcall consult-gh-quit-window-func t))))))
    (message "Not in an issue editing buffer!")))

(defun consult-gh--pr-list-format (string input highlight)
  "Format minibuffer candidates for listing pull requests.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh pr list ...”\)
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\)
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match' in the minibuffer."
  (let* ((class "pr")
         (type "pr")
         (parts (string-split string "\t"))
         (repo (car (consult--command-split input)))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (car parts))
         (state (upcase (cadr (cdr (cdr parts)))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-error)
                 ("MERGED" 'consult-gh-success)
                 ("OPEN" 'consult-gh-repo)
                 (_ 'consult-gh-pr)))
         (branch (cadr (cdr parts)))
         (title (cadr parts))
         (date (cadr (cdr (cdr (cdr parts)))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face  face) ":" (propertize (format "%s" title) 'face 'consult-gh-default)) 70)
                      (propertize (consult-gh--set-string-width state 6) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date)
                      (propertize (consult-gh--set-string-width branch 24) 'face 'consult-gh-branch)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user ) "/" (propertize package 'face 'consult-gh-package)) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :number number :state state :title title :branch branch :date date :query query :class class :type type) str)
    str))

(defun consult-gh--search-prs-format (string input highlight)
  "Format minibuffer candidates for searching pull requests.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh search prs ...”\)
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match' in the minibuffer."

  (let* ((class "pr")
         (type "pr")
         (parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (cadr parts))
         (state (upcase (cadr (cdr parts))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-error)
                 ("MERGED" 'consult-gh-success)
                 ("OPEN" 'consult-gh-repo)
                 (_ 'consult-gh-pr)))
         (title (cadr (cdr (cdr parts))))
         (tags (cadr (cdr (cdr (cdr parts)))))
         (date (cadr (cdr (cdr (cdr (cdr parts))))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face  face) ":" (propertize (format "%s" title) 'face 'consult-gh-default)) 70)
                      (propertize (consult-gh--set-string-width state 6) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date)
                      (propertize (consult-gh--set-string-width tags 18) 'face 'consult-gh-tags)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user ) "/" (propertize package 'face 'consult-gh-package)) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :number number :state state :title title :tags tags :date date :query query :class class :type type) str)
    str))

(defun consult-gh--pr-state ()
  "State function for pull request candidates.

This is passed as STATE to `consult--read' in `consult-gh-search-prs'
and is used to preview or do other actions on the pr."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (if cand
          (pcase action
            ('preview
             (if (and consult-gh-show-preview cand)
                 (when-let ((repo (get-text-property 0 :repo cand))
                            (number (get-text-property 0 :number cand))
                            (query (get-text-property 0 :query cand))
                            (match-str (consult--build-args query))
                            (buffer (get-buffer-create consult-gh-preview-buffer-name)))
                   (add-to-list 'consult-gh--preview-buffers-list buffer)
                   (consult-gh--pr-view repo number buffer t)
                   (with-current-buffer buffer
                     (if consult-gh-highlight-matches
                         (cond
                          ((listp match-str)
                           (mapc (lambda (item)
                                     (highlight-regexp item 'consult-gh-preview-match)) match-str))
                          ((stringp match-str)
                           (highlight-regexp match-str 'consult-gh-preview-match)))))
                   (funcall preview action
                            buffer))))
            ('return
             cand))))))

(defun consult-gh--pr-list-group (cand transform)
  "Group function for pull requests.

This is passed as GROUP to `consult--read' in `consult-gh-pr-list'
or `consult-gh-search-prs', and is used to group prs.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-prs-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "Number:Title " 68 nil ?-)
       (consult-gh--set-string-width " State " 8 nil ?-)
       (consult-gh--set-string-width " Date " 12 nil ?-)
       (consult-gh--set-string-width " Branch " 26 nil ?-)
       (consult-gh--set-string-width " Repo " 40 nil ?-))))))

(defun consult-gh--pr-search-group (cand transform)
  "Group function for pull requests.

This is passed as GROUP to `consult--read' in `consult-gh-pr-list'
or `consult-gh-search-prs', and is used to group prs.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-prs-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "Number:Title " 68 nil ?-)
       (consult-gh--set-string-width " State " 8 nil ?-)
       (consult-gh--set-string-width " Date " 12 nil ?-)
       (consult-gh--set-string-width " Tags " 20 nil ?-)
       (consult-gh--set-string-width " Repo " 40 nil ?-))))))

(defun consult-gh--pr-browse-url-action (cand)
  "Browse the url for a pull request candidate, CAND.

This is an internal action function that gets a candidate, CAND,
from `consult-gh-search-prs' and opens the url of the pr
in an external browser.

To use this as the default action for prs,
set `consult-gh-pr-action' to `consult-gh--pr-browse-url-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (get-text-property 0 :number cand)))
         (repo-url (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")))
         (url (and repo-url (concat repo-url "/pull/" number))))
    (funcall (or consult-gh-browse-url-func #'browse-url) url)))

(defun consult-gh--pr-read-json (repo number &optional json)
  "Get json response of pull request of NUMBER in REPO.

Runs an async shell command with the command:
gh pr view NUMBER --repo REPO --json JSON
and returns the output as a hash-table.

If optional argument PREVIEW is non-nil, do not load full details.
Optional argument JSON, defaults to `consult-gh--pr-view-json-fields'."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false :false))
    (json-read-from-string (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" (or json consult-gh--pr-view-json-fields)))))

(defun consult-gh--pr-get-comments (repo number)
  "Get comments and reviews of pull request NUMBER in REPO.

Retrieves a list of comments and reviews for pull request stored in TABLE,
a hash-table output from `consult-gh--pr-read-json'."
  (let* ((comments (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/repos/%s/issues/%s/comments" repo number))))
         (reviews (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/repos/%s/pulls/%s/reviews" repo number))))
         (review-comments (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/repos/%s/pulls/%s/comments" repo number))))
         (all-comments (append comments reviews review-comments)))
    (sort all-comments :key (lambda (k)
                              (date-to-time (or (gethash :updated_at k) (gethash :created_at k) (gethash :submitted_at k) (format-time-string "%Y-%m-%dT%T%Z" (encode-time (decode-time (current-time) t)))))))))

(defun consult-gh--pr-get-commenters (table &optional comments)
  "Get list of relevant users on a pull request.

Retrieves a list of all relevant users (commenters, reviewers, etc.) for
a pull request stored in TABLE, a hash-table output from
`consult-gh--pr-read-json'.

If the optinal argument COMMENTS is non-nil,
use COMMENTS instead of the comments in the table."
  (let* ((author (gethash :login (gethash :author table)))
         (assignees (gethash :assignees table))
         (assignees (and (listp assignees) (mapcar (lambda (item) (and (hash-table-p item) (gethash :login item))) assignees)))
         (comments (or comments (gethash :comments table)))
         (commenters (when (and comments (listp comments)) (cl-loop for comment in comments
                                                    collect
                                                    (when (hash-table-p comment)
                                                       (gethash :login (gethash :user comment)))))))
    (cl-remove-duplicates (delq nil (append (list author) assignees commenters)) :test #'equal)))

(defun consult-gh--pr-format-header (repo number table &optional topic)
  "Format a header for a pull reqeust of NUMBER in REPO.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing
the header section for `consult-gh--pr-view'.

The optional argument TOPIC is a propertized text where the related info
from the header will get appended to the properties.  For an example, see
the buffer-local variable `consult-gh--topic' in the buffer created by
`consult-gh--pr-view'."
  (let* ((title (gethash :title table))
         (author (gethash :login (gethash :author table)))
         (baseRef (gethash :baseRefName table))
         (head (gethash :headRepository table))
         (headRepo (and head (gethash :name head)))
         (headRepoOwner (gethash :headRepositoryOwner table))
         (headRepoOwner (and headRepoOwner (gethash :login headRepoOwner)))
         (headRef (gethash :headRefName table))
         (state (gethash :state table))
         (createdAt (gethash :createdAt table))
         (updatedAt (gethash :updatedAt table))
         (updatedAt (and updatedAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time updatedAt))))
         (url (gethash :url table))
         (closedAt (gethash :closedAt table))
         (closedAt (and closedAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time closedAt))))

         (labels (gethash :labels table))
         (labels (and labels (listp labels) (mapcar (lambda (item) (and (hash-table-p item) (gethash :name item))) labels)))
         (labels-text (and (listp labels) (mapconcat #'identity labels ", ")))
         (milestone (gethash :milestone table))
         (milestone-title (and (hash-table-p milestone) (gethash :title milestone)))
         (milestone-description (and (hash-table-p milestone) (gethash :description milestone)))
         (milestone-text (concat (and milestone-title (not (string-empty-p milestone-title)) milestone-title) (and milestone-description (not (string-empty-p milestone-description)) (concat "(" milestone-description ")"))))
         (reviewers (gethash :reviewRequests table))
         (reviewers (and reviewers (listp reviewers) (mapcar (lambda (item) (when (hash-table-p item) (gethash :login item))) reviewers)))
         (reviewers-text (and reviewers (listp reviewers) (mapconcat #'identity reviewers ",\s")))
         (assignees (gethash :assignees table))
         (assignees (and assignees (listp assignees) (mapcar (lambda (item) (when (hash-table-p item) (gethash :login item))) assignees)))
         (assignees-text (and assignees (listp assignees) (mapconcat #'identity assignees ",\s")))
         (projects (gethash :projectItems table))
         (projects (and projects (listp projects) (mapcar (lambda (item) (when (hash-table-p item) (gethash :title item))) projects)))
         (projects-text (and projects (listp projects) (mapconcat #'identity projects ",\s"))))

    (when (stringp topic)
      (add-text-properties 0 1 (list :author author :state state :lastUpdated updatedAt :labels labels :milestone milestone-title :reviewers reviewers :assignees assignees :projects projects :headrepo (concat headRepoOwner "/" headRepo) :headbranch headRef :baserepo repo :basebranch baseRef) topic))

    (concat "title: " title "\n"
            "author: " author "\n"
            "repository: " (format "%s:%s" repo baseRef) "\n"
            "number: " number "\n"
            "ref: " repo ":" baseRef " <- " (and headRepoOwner (concat headRepoOwner "/")) (and headRepo (format "%s:" headRepo)) headRef "\n"
            "state: " state "\n"
            (and createdAt (concat "created: " createdAt "\n"))
            (and updatedAt (concat "lastUpdated: " updatedAt "\n"))
            (and closedAt (concat "closed: " closedAt "\n"))
            (and url (concat "url: " url "\n"))
            (and labels-text (concat "labels: " "[ " labels-text " ]""\n"))
            (and projects-text (concat "projects: " projects-text "\n"))
            (and milestone-text (concat "milestone: " milestone-text "\n"))
            (and assignees-text (concat "assignees: " assignees-text "\n"))
            (and reviewers-text (concat "reviewers: " reviewers-text "\n"))
            "\n--\n")))

(defun consult-gh--pr-format-body (table &optional topic)
  "Format the body section for a pull request.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing
the comments section for `consult-gh--pr-view'.

If optional argument PREVIEW is non-nil, do not load all comments.

The optional argument TOPIC is a propertized text where the related info
from the header will get appended to the properties.  For an example, see
the buffer-local variable `consult-gh--topic' in the buffer created by
`consult-gh--pr-view'."
  (let* ((author (gethash :login (gethash :author table)))
         (body (gethash :body table))
         (createdAt (gethash :createdAt table))
         (url (gethash :url table)))

    (when topic (add-text-properties 0 1 (list :body body) topic))

    (propertize (concat "\n" author " " (consult-gh--time-ago createdAt)
                        " " (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)) "\n"
                        "---\n" body "\n" "\n")
                :consult-gh (list :author author
                                  :url url))))

(defun consult-gh--pr-format-files-changed (table)
  "Format a changed files section for a pull request.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing the
files changed section for `consult-gh--pr-view'."
  (let ((files (gethash :files table))
        (additions (gethash :additions table))
        (deletions (gethash :deletions table)))
    (and (listp files) (concat "# Files Changed - " (format "%s file(s)" (length files)) (and additions (format ", %s additions(+)" additions)) (and deletions (format ", %s deletions(-)" deletions)) "\n"))))

(defun consult-gh--pr-format-diffs (diffs repo number &optional commit-id header-level url preview)
  "Format DIFFS from pull request NUMBER in REPO.

This is used for preparing the text section of diffs in `consult-gh--pr-view'.

Description of Arguments:
  DIFFS         a string; diff text for pull request
  REPO          a string; full name of repository
  NUMBER        a string; number id of the pull request
  COMMIT-ID     a string; commit id of pull request
  HEADER-LEVEL  a number; outline-level for adding the diff section
  URL           a string; url of the commit
  PREVIEW       a boolean; whether this is for preview"
  (when (listp diffs)
    (with-temp-buffer
      (cl-loop for chunk in diffs
           do (when (consp chunk)
                   (insert (propertize (concat (make-string (+ header-level 1) ?#) " " (car-safe chunk)) :consult-gh (list :repo repo :number number :path (get-text-property 0 :path (car-safe chunk)) :commit-id commit-id :commit-url url :file t :code nil)))
                   (if preview
                       (insert "\n")
                     (when (cdr chunk)
                     (insert (concat (propertize  "\n```diff\n" :consult-gh (list :repo repo :number number :path (get-text-property 0 :path (car-safe chunk)) :commit-id commit-id :commit-url url :file t :code nil)) (propertize (cdr chunk) :consult-gh (list :repo repo :number number :path (get-text-property 0 :path (car-safe chunk)) :commit-id commit-id :commit-url url :file t :code t)) (propertize  "\n```\n" :consult-gh (list :repo repo :number number :path (get-text-property 0 :path (car-safe chunk)) :commit-id commit-id :commit-url url :file t :code nil))))))))
      (consult-gh--whole-buffer-string))))

(defun consult-gh--pr-filter-comments (comments &optional maxnum)
  "Filter COMMENTS when there are more than MAXNUM.

Queries the user for how to filter the comments."

  (let ((maxnum (or maxnum consult-gh-comments-maxnum)))
    (when (and (listp comments) (> (length comments) maxnum))
      (pcase (consult--read (list (cons "Load Everything" :nil)
                                  (cons "Load comments in the last week." :last-week)
                                  (cons "Load comments in the last month." :last-month)
                                  (cons (format "Load up to %s latest comments." maxnum) :last-maxnum)
                                  (cons "Load comments since a particular date" :date)
                                  (cons "Load comments in a date range" :daterange))
                            :prompt (format "There are more than %s comments on that pull request. Do you want to load them all?" maxnum)
                            :lookup #'consult--lookup-cdr
                            :sort nil)
        (':last-week
         (setq comments (cl-remove-if-not (lambda (k)
                                            (time-less-p (encode-time (decoded-time-add (decode-time (current-time) t) (make-decoded-time :day -7))) (date-to-time (or (gethash :updated_at k) (gethash :created_at k) (gethash :submitted_at k) (format-time-string "%Y-%m-%dT%T%Z" (encode-time (decode-time (current-time) t)))))))
                                          comments)))
        (':last-month
         (setq comments (cl-remove-if-not (lambda (k)
                                            (time-less-p (encode-time (decoded-time-add (decode-time (current-time) t) (make-decoded-time :day -30))) (date-to-time (or (gethash :updated_at k) (gethash :created_at k) (gethash :submitted_at k) (format-time-string "%Y-%m-%dT%T%Z" (encode-time (decode-time (current-time) t)))))))
                                          comments)))
        (':last-maxnum
         (setq comments (cl-subseq comments (max 0 (- (length comments) maxnum)))))
        (':date
         (let ((d (org-read-date nil t)))
           (setq comments (cl-remove-if-not (lambda (k)
                                              (time-less-p d (date-to-time (or (gethash :updated_at k) (gethash :created_at k) (gethash :submitted_at k) (format-time-string "%Y-%m-%dT%T%Z" (encode-time (decode-time (current-time) t)))))))
                                            comments))))
        (':daterange
         (let ((begin-date (org-read-date nil t nil "Select Beginning"))
               (end-date (org-read-date nil t nil "Select End")))
           (setq comments (cl-remove-if-not (lambda (k)
                                              (let ((date (date-to-time (or (gethash :updated_at k) (gethash :created_at k) (gethash :submitted_at k) (format-time-string "%Y-%m-%dT%T%Z" (encode-time (decode-time (current-time) t)))))))
                                                (and (time-less-p begin-date date) (time-less-p date end-date))))
                                            comments))))))
    comments))

(defun consult-gh--pr-format-comments (comments repo number &optional url)
  "Format the COMMENTS for the pull request NUMBER in REPO.

The optional argument URL, is the web url for the pull request on GitHub."
  (let* ((header-marker "#")
         (out nil))
    (when (listp comments)
      (cl-loop for comment in comments
               do
               (when (hash-table-p comment)
                 (let* ((author (gethash :user comment))
                        (author (and author (gethash :login author)))
                        (comment-url (gethash :html_url comment))
                        (pull-url (gethash :pull_request_url comment))
                        (issue-url (gethash :issue_url comment))
                        (pull-id (gethash :pull_request_review_id comment))
                        (reply (gethash :in_reply_to_id comment))
                        (id (gethash :id comment))
                        (authorAssociation (gethash :authorAssociation comment))
                        (authorAssociation (unless (equal authorAssociation "NONE")
                                             authorAssociation))
                        (createdAt (or (gethash :updated_at comment)
                                       (gethash :created_at comment)
                                       (gethash :submitted_at comment)
                                       (format-time-string "%Y-%m-%dT%T%Z" (encode-time (decode-time (current-time) t)))))
                        (createdAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)))
                        (state (gethash :state comment))
                        (state (cond
                                ((equal state "COMMENTED") (propertize "COMMENTED" 'face 'default))
                                ((equal state "CHANGES_REQUESTED") (propertize "REQUESTED CHANGES" 'face 'consult-gh-warning))
                                ((equal state "APPROVED") (propertize "APPROVED" 'face 'consult-gh-success))
                                (t state)))
                        (oid (gethash :commit_id comment))
                        (diff (gethash :diff_hunk comment))
                        (path (gethash :path comment))
                        (diff-chunks (when (and (stringp diff) (stringp path))
                                       (list (cons path (if (string-empty-p diff) nil diff)))))
                        (body (gethash :body comment)))

                   (save-match-data
                     (when (and body (string-match (concat "^" header-marker "+?\s.*$")  body))
                       (setq body (with-temp-buffer
                                    (insert body)
                                    (goto-char (point-min))
                                    (while (re-search-forward (concat "^" header-marker "+?\s.*$") nil t)
                                      (replace-match (concat header-marker header-marker "\\&")))
                                    (buffer-string)))))
                   (setq out (concat out (propertize (concat header-marker header-marker (when diff header-marker) " "
                                                             (and author (concat (cond
                                                                   (reply "[reply] ")
                                                                   ((and pull-url pull-id)
                                                                      "[review-comment] ")
                                                                   (issue-url "[comment] ")
                                                                   (t "[review] "))
                                                                                 author (and state (concat " " state))  " "))
                                                             (and authorAssociation (concat "(" authorAssociation ")"))
                                                             (and createdAt (concat (consult-gh--time-ago createdAt) " " createdAt))
                                                             "\n"

(and oid pull-id (concat "\ncommit: " "[" (substring oid 0 6) "]" (format "(%s/commits/%s)" url oid) "\n---"))
(and body (concat "\n" body "\n"))
                                                             (when diff-chunks
                                                               (consult-gh--pr-format-diffs diff-chunks repo number oid 3  (format "%s/commits" url)))
                                                             "\n----------\n")
                                                     :consult-gh (list :author author :comment-url comment-url :comment-id (when (and pull-id (not (equal id pull-id))) id) :reply-url (when (and pull-id pull-url
                  (not (equal pull-id id)))
         (concat pull-url "/comments"))
                                                                       :commit-url (when (and pull-url oid) (format "%s/commits/%s" url oid))))))))))
    out))

(defun consult-gh--pr-format-commits (commits repo number url &optional preview)
  "Format COMMITS for the pull request NUMBER in REPO.

The optional argument URL, is the web url for the pull request on GitHub.

If optional argument PREVIEW is non-nil, do not load diff of commits."
  (when (listp commits)
    (let ((out nil))
      (cl-loop for commit in commits
               do
               (when (hash-table-p commit)
                 (let* ((oid (gethash :oid commit))
                        (authors (gethash :authors commit))
                        (authors (and (listp authors) (mapconcat (lambda (author) (gethash :login author)) authors ",\s")))
                        (date (gethash :committedDate commit))
                        (date (and (stringp date) (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time date))))
                        (diff (unless preview (and oid
                                   (consult-gh--command-to-string "api" "-H" "Accept:application/vnd.github.diff" (format "repos/%s/commits/%s" repo oid)))))
                        (diff-chunks (and (stringp diff) (consult-gh--parse-diff diff)))
                        (title (propertize (concat "## " (concat "[" (substring oid 0 6) "]" (format "(%s/commits/%s)" url oid) " by " authors " at " date "\n")) :consult-gh (list :commit-id oid)))
                        (body
                              (when (and diff-chunks (listp diff-chunks))
                                  (consult-gh--pr-format-diffs diff-chunks repo number oid 2 (format "%s/commits/%s" url oid) preview))))
                   (setq out (concat out title body)))))
      out)))

(defun consult-gh--pr-view (repo number &optional buffer preview title)
  "Open the pull request with id NUMBER in REPO in BUFFER.

This is an internal function that takes REPO, the full name of a repository
\(e.g. “armindarvish/consult-gh”\) and NUMBER, a pr number of REPO,
and shows the contents of the pull request in an Emacs buffer.

It fetches the details of the pull request by calling
`consult-gh--pr-read-json' and parses and formats it in markdown syntax,
and puts it in either BUFFER, or if BUFFER is nil, in a buffer named by
`consult-gh-preview-buffer-name'.  If `consult-gh-issue-preview-major-mode'
is non-nil, uses it as major-mode for BUFFER, otherwise shows the raw text
in \='text-mode.

Description of Arguments:

  REPO    a string; the full name of the repository
  NUMBER  a string; pull request id number
  BUFFER  a string; optional buffer name
  PREVIEW a boolean; whether to load reduced preview
  TITLE   a string; an optional title string

To use this as the default action for PRs, see
`consult-gh--pr-view-action'."
  (consult--with-increased-gc
   (let* ((topic (format "%s/#%s" repo number))
          (canAdmin (consult-gh--user-canadmin repo))
          (buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
          (_ (message "Collecting info from %s..." (propertize "GitHub" 'face 'consult-gh-date)))
          (table (consult-gh--pr-read-json repo number))
          (comments (unless preview (consult-gh--pr-filter-comments (consult-gh--pr-get-comments repo number))))
          (commenters (unless preview (and table (consult-gh--pr-get-commenters table comments))))
          (state (gethash :state table))
          (url (gethash :url table))
          (_ (message "Formating the %s..." (propertize "content" 'face 'consult-gh-issue)))
          (header-text (consult-gh--pr-format-header repo number table topic))
          (title (or title (car (split-string header-text "\n" t))))
          (title (string-trim-left title "title: "))
          (latest-commit (gethash :headRefOid table))
          (body-text (consult-gh--pr-format-body table topic))
          (comments-text (when (and comments (listp comments)) (consult-gh--pr-format-comments comments repo number url)))
          (file-change-text (consult-gh--pr-format-files-changed table))
          (_ (message "Working on some %s details..." (propertize "more" 'face 'consult-gh-issue)))
          (diff (consult-gh--command-to-string "pr" "diff" number "--repo" repo))
          (chunks (when (and diff (stringp diff)) (consult-gh--parse-diff diff)))
          (diff-text (when (and chunks (listp chunks)) (consult-gh--pr-format-diffs chunks repo number latest-commit 1 (format "%s/commits/%s" url latest-commit) preview)))
          (commits (when consult-gh-prs-show-commits-in-view
                     (gethash :commits table)))
          (commits-text (when (and commits (listp commits)) (consult-gh--pr-format-commits commits repo number url preview))))

     (unless preview
       ;; collect issues of repo for completion at point
       (consult-gh--completion-set-issues topic repo)

       ;; collect prs of repo for completion at point
       (consult-gh--completion-set-prs topic repo)

       ;; collect mentionable users for completion at point
       (consult-gh--completion-set-mentionable-users topic repo)

       ;; collect list of repo branches for completion at point
       (consult-gh--completion-set-branches topic repo)

       (if canAdmin
           (progn
             ;; collect labels for completion at point
             (consult-gh--completion-set-valid-labels topic repo)
             ;; collect valid assignees for completion at point
             (consult-gh--completion-set-assignable-users topic repo)
             ;; collect valid milestones for completion at point
             (consult-gh--completion-set-valid-milestones topic repo))

         (add-text-properties 0 1 (list :valid-labels nil :assignable-users nil :valid-milestones nil :valid-projects nil) topic)))

     (add-text-properties 0 1 (list :repo repo :type "pr" :number number :title title :state state :commenters (mapcar (lambda (item) (concat "@" item)) commenters)) topic)

     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (when header-text (insert header-text))
         (save-excursion
           (when (eq consult-gh-issue-preview-major-mode 'org-mode)
             (consult-gh--github-header-to-org buffer)))
         (insert (concat "# Conversation\n"))
         (when body-text (insert body-text))
         (if preview
             (insert "## comments not available in preview!\n")
           (when comments-text (insert comments-text)))
         (when file-change-text (insert file-change-text))
         (when diff-text (insert diff-text))
         (insert "\n")
         (insert "# Commits\n")
         (when commits-text (insert commits-text))
         (insert "\n")
         (message "Putting %s together..." (propertize "everything" 'face 'consult-gh-repo))
         (save-excursion
           (pcase consult-gh-issue-preview-major-mode
             ('gfm-mode
              (gfm-mode)
              (when (display-images-p)
                (markdown-display-inline-images)))
             ('markdown-mode
              (markdown-mode)
              (when (display-images-p)
                (markdown-display-inline-images)))
             ('org-mode
              (let ((org-display-remote-inline-images 'download))
                (consult-gh--markdown-to-org buffer)))
             (_
              (consult-gh--markdown-to-org-emphasis buffer)
              (outline-mode))))
         (goto-char (point-min))
         (save-excursion
           (while (re-search-forward "\r\n" nil t)
             (replace-match "\n")))
         (set-buffer-file-coding-system 'utf-8-unix)
         (outline-hide-sublevels 1)
         (consult-gh-pr-view-mode +1)
         (setq-local consult-gh--topic topic)
         (current-buffer))))))

(defun consult-gh--pr-view-action (cand)
  "Opens the preview of a pull request candidate, CAND.

This is a wrapper function around `consult-gh--pr-view'.  It parses CAND
to extract relevant values \(e.g. repository's name and pull request
number\) and passes them to `consult-gh--pr-view'.

To use this as the default action for prs,
set `consult-gh-pr-action' to `consult-gh--pr-view-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (format "%s" (get-text-property 0 :number cand))))
         (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/pull/" number "*"))
         (existing (get-buffer buffername))
         (confirm (if (and existing (not (= (buffer-size existing) 0)))
                      (consult--read
                       (list (cons "Switch to existing buffer." :resume)
                             (cons "Reload the pull request in the existing buffer." :replace)
                             (cons "Make a new buffer and load the pull request in it (without killing the old buffer)." :new))
                       :prompt "You already have this pull request open in another buffer.  Would you like to switch to that buffer or make a new one? "
                       :lookup #'consult--lookup-cdr
                       :sort nil
                       :require-match t))))
    (if existing
        (cond
         ((eq confirm :resume) (funcall consult-gh-switch-to-buffer-func existing))
         ((eq confirm :replace)
          (message "Reloading pull request in the existing buffer...")
          (funcall consult-gh-switch-to-buffer-func (consult-gh--pr-view repo number existing))
          (set-buffer-modified-p nil)
          (buffer-name (current-buffer)))
         ((eq confirm :new)
          (message "Opening pull request in a new buffer...")
          (funcall consult-gh-switch-to-buffer-func (consult-gh--pr-view repo number (generate-new-buffer buffername nil)))
          (set-buffer-modified-p nil)
          (buffer-name (current-buffer))))
      (progn
        (funcall consult-gh-switch-to-buffer-func (consult-gh--pr-view repo number))
        (rename-buffer buffername t)
        (set-buffer-modified-p nil)
        (buffer-name (current-buffer))))))

(defun consult-gh--get-pr-templates (repo)
  "Get pull request templates of REPO."
  (let* ((table (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "pullRequestTemplates") :pullRequestTemplates))
         (templates (and table (listp table)
                         (mapcar (lambda (item) (cons (gethash :filename item)
                                                      (gethash :body item)))
                                 table))))
    (when templates
      (cond
       ((length= templates 1) templates)
       (t (append templates (list (cons "Blank" ""))))))))

(defun consult-gh-topics--pr-get-branches (repo)
  "Get a list of branches of REPO."
  (mapcar (lambda (item) (gethash :name item)) (consult-gh--json-to-hashtable (cadr (consult-gh--api-get-json (concat "repos/" repo "/branches"))))))

(defun consult-gh-topics--pr-get-forks (repo)
  "Get list of forks of REPO."
  (let* ((table (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "repos/%s/forks" repo)))))
    (and (listp table)  (mapcar (lambda (item) (gethash :full_name item)) table))))

(defun consult-gh-topics--pr-get-parent (repo)
  "Get the upstream parent of REPO."
  (let* ((table (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "parent")))
         (parent (and (hash-table-p table) (gethash :parent table))))
    (and (hash-table-p parent) (concat (gethash :login (gethash :owner parent)) "/" (gethash :name parent)))))

(defun consult-gh-topics--pr-get-siblings (repo)
  "Get the siblings of REPO.

Sinblings here means forks of the upstream repositories."
  (let* ((current repo)
         (forks (list)))
    (while-let ((parent (consult-gh-topics--pr-get-parent repo)))
      (setq forks (append forks (consult-gh-topics--pr-get-forks parent)))
      (setq repo parent))
    (cl-remove-duplicates (delq nil (remove current (delq nil forks))) :test #'equal)))

(defun consult-gh-topics--pr-get-similar (repo)
  "Get all the similar repositories to REPO.

Similar here means forks, paretns from `consult-gh-topics--pr-get-parent'
and siblings from `consult-gh-topics--pr-get-siblings'."
  (let* ((parent (consult-gh-topics--pr-get-parent repo))
         (forks (consult-gh-topics--pr-get-forks repo))
         (siblings (consult-gh-topics--pr-get-siblings repo)))
    (cl-remove-duplicates (delq nil (append (list parent) forks siblings)) :test #'equal)))

(defun consult-gh-topics--pr-parse-metadata ()
  "Parse pull requests' metadata."
  (let* ((region (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
         (header (when region (buffer-substring-no-properties (car region) (cdr region))))
         (base nil)
         (head nil)
         (reviewers nil)
         (assignees nil)
         (labels nil)
         (milestone nil)
         (projects nil))
(when (and header (string-match "^\\(?:.*base:\\)\\(?1:.*\\)\\(?:\n.*head:\\)?\\(?2:.*\\)?\\(?:\n.*reviewers:\\)?\\(?3:.*\\)?\\(?:\n.*assignees:\\)?\\(?4:.*\\)?\\(?:\n.*labels:\\)?\\(?5:.*\\)?\\(?:\n.*milestone:\\)?\\(?6:.*\\)?\\(?:\n.*projects:\\)?\\(?7:.*\\)?\\(?:\n-+\\)" header))

  (setq base (match-string 1 header)
        head (match-string 2 header)
        reviewers (match-string 3 header)
        assignees (match-string 4 header)
        labels (match-string 5 header)
        milestone (match-string 6 header)
        projects (match-string 7 header)))
  (list (and (stringp base)
             (string-trim base))
        (and (stringp head)
             (string-trim head))
        (and (stringp reviewers)
             (string-trim reviewers))
        (and (stringp assignees)
             (string-trim assignees))
        (and (stringp labels)
             (string-trim labels))
        (and (stringp milestone)
             (string-trim milestone))
        (and (stringp projects)
             (string-trim projects)))))

(defun consult-gh-topics--pr-get-metadata (&optional pr)
  "Get metadata of the PR.

PR defaults to `consult-gh--topic'."
  (let* ((pr (or pr consult-gh--topic))
         (baserepo (get-text-property 0 :baserepo pr))
         (basebranch (get-text-property 0 :basebranch pr))
         (headrepo (get-text-property 0 :headrepo pr))
         (headbranch (get-text-property 0 :headbranch pr))
         (canAdmin (consult-gh--user-canadmin baserepo))
         (isAuthor (consult-gh--user-isauthor pr))
         (author (get-text-property 0 :author pr))
         (reviewers (get-text-property 0 :reviewers pr))
         (assignees (get-text-property 0 :assignees pr))
         (labels (get-text-property 0 :labels pr))
         (milestone (get-text-property 0 :milestone pr))
         (projects  (get-text-property 0 :projects pr))
         (valid-assignees (append (get-text-property 0 :assignable-users pr) (list "@me")))
         (valid-reviewers (delq author (append (get-text-property 0 :assignable-users pr) (list "@me"))))
         (valid-labels (get-text-property 0 :valid-labels pr))
         (valid-projects (get-text-property 0 :valid-projects pr))
         (valid-milestones (get-text-property 0 :valid-milestones pr)))


    (pcase-let* ((`(,text-base ,text-head ,text-reviewers ,text-assignees ,text-labels ,text-milestone ,text-projects) (consult-gh-topics--pr-parse-metadata))
                 (text-baserepo nil)
                 (text-basebranch nil)
                 (text-headrepo nil)
                 (text-headbranch nil))

      (when (derived-mode-p 'org-mode)
        (setq text-base (or (cadar (org-collect-keywords '("base"))) text-base)
              text-head (or (cadar (org-collect-keywords '("head"))) text-head)
              text-reviewers (or (cadar (org-collect-keywords '("reviewers"))) text-reviewers)
              text-assignees (or (cadar (org-collect-keywords '("assignees"))) text-assignees)
              text-labels (or (cadar (org-collect-keywords '("labels"))) text-labels)
              text-milestone (or (cadar (org-collect-keywords '("milestone"))) text-milestone)
              text-projects (or (cadar (org-collect-keywords '("projects"))) text-projects)))

      (when (stringp text-base)
        (cond
         ((string-match "\\(?1:.*\\):\\(?2:.*\\)" text-base)
          (setq text-baserepo (string-trim (match-string 1 text-base)))
          (setq text-basebranch (string-trim (match-string 2 text-base))))
         (t
          (setq text-baserepo baserepo)
          (setq text-basebranch text-base)))

        (when (and (stringp text-baserepo) (not (string-empty-p text-baserepo)))
          (setq baserepo text-baserepo))

        (when (and (stringp text-basebranch) (not (string-empty-p text-basebranch)))
          (cond
           ((member text-basebranch (consult-gh-topics--pr-get-branches baserepo))
            (setq basebranch text-basebranch))
           (t (setq basebranch nil)))))

      (when (stringp text-head)
        (cond
         ((string-match "\\(?1:.*\\):\\(?2:.*\\)" text-head)
          (setq text-headrepo (string-trim (match-string 1 text-head)))
          (setq text-headbranch (string-trim (match-string 2 text-head))))
         (t
          (setq text-headrepo headrepo)
          (setq text-headbranch text-head)))

        (when (and (stringp text-headrepo) (not (string-empty-p text-headrepo)))
          (setq headrepo text-headrepo))

        (when (and (stringp text-headbranch) (not (string-empty-p text-headbranch)))
          (cond
           ((member text-headbranch (consult-gh-topics--pr-get-branches headrepo))
            (setq headbranch text-headbranch))
           (t (setq headbranch nil)))))

      (when (or canAdmin isAuthor)
        (when (stringp text-reviewers)
          (setq reviewers (cl-remove-duplicates
                           (cl-remove-if-not
                            (lambda (item) (member item valid-reviewers))
                            (split-string text-reviewers "," t "[ \t]+"))
                           :test #'equal)))

        (when (stringp text-assignees)
          (setq assignees (cl-remove-duplicates
                           (cl-remove-if-not
                            (lambda (item) (member item valid-assignees))
                            (split-string text-assignees "," t "[ \t]+"))
                           :test #'equal)))

        (when (stringp text-labels)
          (setq labels (cl-remove-duplicates
                        (cl-remove-if-not
                         (lambda (item) (member item valid-labels))
                         (split-string text-labels "," t "[ \t]+"))
                        :test #'equal)))

        (when (stringp text-projects)
          (setq projects (cl-remove-duplicates
                          (cl-remove-if-not
                           (lambda (item) (member item valid-projects))
                           (split-string text-projects "," t "[ \t]+"))
                          :test #'equal)))

        (when (stringp text-milestone)
          (cond
           ((member text-milestone valid-milestones)
            (setq milestone text-milestone))
           (t (setq milestone nil))))))

    (list (cons "baserepo" baserepo)
          (cons "basebranch" basebranch)
          (cons "headrepo" headrepo)
          (cons "headbranch" headbranch)
          (cons "reviewers" reviewers)
          (cons "assignees" assignees)
          (cons "labels" labels)
          (cons "milestone" milestone)
          (cons "projects" projects))))

(defun consult-gh-topics--pr-create-add-metadata (&optional repo pr)
  "Add metadata to PR topic for REPO.

This is used for creating new pull requests."
  (let* ((pr (or pr consult-gh--topic))
         (meta (consult-gh-topics--pr-get-metadata pr))
         (repo (or repo (get-text-property 0 :repo pr)))
         (baserepo (get-text-property 0 :baserepo pr))
         (canAdmin (consult-gh--user-canadmin baserepo))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))
    (when canAdmin
      ;; add reviewers
      (and (y-or-n-p "Would you like to add reviewers?")
           (let* ((current (cdr (assoc "reviewers" meta)))
                  (users (or (get-text-property 0 :assignable-users pr) (consult-gh--get-assignable-users repo)))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Users: " users)) :test #'equal)))
             (add-text-properties 0 1 (list :reviewers (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

             (save-excursion (goto-char (car header))
                               (when (re-search-forward "^.*reviewers: \\(?1:.*\\)?" (cdr header) t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :reviewers pr) ", ") nil nil nil 1)))))

      ;; add assignees
      (and (y-or-n-p "Would you like to add assignees?")
           (let* ((current (cdr (assoc "assignees" meta)))
                  (table (or (get-text-property 0 :assignable-users pr) (consult-gh--get-assignable-users repo)))
                  (users (append (list "@me") table))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Users: " users)) :test #'equal)))
             (add-text-properties 0 1 (list :assignees (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

               (save-excursion (goto-char (car header))
                               (when (re-search-forward "^.*assignees: \\(?1:.*\\)?" (cdr header) t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :assignees pr) ", ") nil nil nil 1)))))

      ;; add labels
      (and (y-or-n-p "Would you like to add lables?")
           (let* ((current (cdr (assoc "labels" meta)))
                  (labels (or (get-text-property 0 :valid-labels pr) (and (not (member :valid-labels (text-properties-at 0 pr))) (consult-gh--get-labels repo))))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Labels: " labels)) :test #'equal)))
             (add-text-properties 0 1 (list :labels (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)


               (save-excursion (goto-char (car header))
                               (when (re-search-forward "^.*labels: \\(?1:.*\\)?" (cdr header) t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :labels pr) ", ") nil nil nil 1)))))

      ;; add projects
      (and (y-or-n-p "Would you like to add projects?")
           (let* ((current (cdr (assoc "projects" meta)))
                  (projects (or (get-text-property 0 :valid-projects pr) (and (not (member :valid-projects (text-properties-at 0 pr))) (mapcar (lambda (item) (gethash :title item)) (gethash :Nodes (consult-gh--get-projects repo))))))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Projects: " projects)) :test #'equal)))
             (add-text-properties 0 1 (list :projects (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

               (save-excursion (goto-char (car header))
                               (when (re-search-forward "^.*projects: \\(?1:.*\\)?" (cdr header) t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :projects pr) ", ") nil nil nil 1)))))

      ;; add a milestone
      (and (y-or-n-p "Would you like to add a milestone?")
           (let* ((milestones (consult-gh--get-milestones repo))
                  (selection (if milestones (consult--read milestones
                                           :prompt "Select a Milestone: "
                                           :require-match t))))
             (if (string-empty-p selection) (setq selection nil))
             (add-text-properties 0 1 (list :milestone selection) pr)
             (save-excursion (goto-char (car header))
                             (when (re-search-forward "^.*milestone: \\(?1:.*\\)?" (cdr header) t)
                                 (replace-match (get-text-property 0 :milestone pr) nil nil nil 1))))))
      (setq consult-gh--topic pr)))

(defun consult-gh-topics--pr-create-change-refs (&optional pr)
  "Change refs in PR topic.

This is used for changing ref branches in a pull requests."
  (let* ((pr (or pr consult-gh--topic))
         (meta (consult-gh-topics--pr-get-metadata pr))
         (baserepo (cdr (assoc "baserepo" meta)))
         (canAdmin (consult-gh--user-canadmin baserepo))
         (isAuthor (consult-gh--user-isauthor pr))
         (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))

    (when (or canAdmin isAuthor)
      (let* ((baserepo (get-text-property 0 :repo (consult-gh-search-repos (consult-gh--get-package baserepo) t "Search for the target base repo you want to merge to: ")))
             (basebranch (consult--read (consult-gh-topics--pr-get-branches baserepo)
                                        :prompt "Select the branch you want to merge to: "
                                        :require-match t
                                        :sort t))
             (headrepo (get-text-property 0 :repo (consult-gh-search-repos (consult-gh--get-package baserepo) t "Search for the source head repo you want to merge from: ")))
             (headbranch (consult--read (cond
                                         ((equal baserepo headrepo)
                                          (remove basebranch (consult-gh-topics--pr-get-branches baserepo)))
                                         (t (consult-gh-topics--pr-get-branches headrepo)))
                                        :prompt "Select the head branch: "
                                        :require-match t
                                        :sort t)))

        (while (equal (consult-gh--json-to-hashtable (consult-gh--api-command-string (format "/repos/%s/compare/%s...%s" baserepo (concat (consult-gh--get-username baserepo) ":" basebranch) (concat (consult-gh--get-username headrepo) ":" headbranch))) :status) "identical")
          (when (y-or-n-p "Do you want to select a different head branch?")
                          (setq headbranch (consult--read (cond
                                                           ((equal baserepo headrepo)
                                                            (remove basebranch (consult-gh-topics--pr-get-branches baserepo)))
                                                           (t (consult-gh-topics--pr-get-branches headrepo)))
                                                          :prompt "Select the head branch: "
                                                          :require-match t
                                                          :sort t))))

        (add-text-properties 0 1 (list :baserepo baserepo :basebranch basebranch :headrepo headrepo :headbranch headbranch) pr)

        ;;collect valid refs for completion at point
        (consult-gh--completion-set-pr-refs pr baserepo headrepo)

        (save-excursion (goto-char (car header))
                        (when (re-search-forward "^.*base: \\(?1:.*\\)?" (cdr header) t)
                          (replace-match (concat (get-text-property 0 :baserepo pr) ":" (get-text-property 0 :basebranch pr)) nil nil nil 1)))
        (save-excursion (goto-char (car header))
                        (when (re-search-forward "^.*head: \\(?1:.*\\)?" (cdr header) t)
                          (replace-match (concat (get-text-property 0 :headrepo pr) ":" (get-text-property 0 :headbranch pr)) nil nil nil 1)))
        (setq consult-gh--topic pr)))))

(defun consult-gh-topics--pr-create-submit (baserepo basebranch headrepo headbranch title body &optional reviewers assignees labels milestone projects draft fill web)
  "Create a new pull request in REPO with metadata.

Description of Arguments:
  BASEREPO    a string; full name of the base (target) repository
  BASEBRANCH  a string; name of the base ref branch
  HEADREPO    a string; name of the head (source) repository
  HEADBRANCH  a string; name of the head ref branch
  TITLE     a string; title of the pr
  BODY      a string; body of the pr
  REVIEWERS a list of strings; list of reviewers
  ASSIGNEES a list of strings; list of assignees
  LABELS    a list of strings; list of labels
  MILESTONE a string; a milestone
  PROJECTS  a list of strings; list of projects
  DRAFT     a boolean; whether to submit pull request as draft?
  FILL      a string; whether to add commit details?
              this can either be t, first, or verbose
  WEB       a boolean; whether to continuing editing on the web?"
  (let* ((baserepo (or baserepo (get-text-property 0 :repo (consult-gh-search-repos nil t "Select the target base repo you want to merge to: "))))
         (basebranch (or basebranch (consult--read (consult-gh-topics--pr-get-branches baserepo)
                                       :prompt "Select the base branch you want to merge into: "
                                       :require-match t
                                       :sort t)))
         (headrepo (or headrepo (get-text-property 0 :repo (consult-gh-search-repos (consult-gh--get-package baserepo) t "Select the target base repo you want to merge from: "))))
         (headbranch (or headbranch (consult--read (cond
                                      ((equal baserepo headrepo)
                                       (remove basebranch (consult-gh-topics--pr-get-branches baserepo)))
                                      (t (consult-gh-topics--pr-get-branches headrepo)))
                                     :prompt "Select the head branch: "
                                     :sort t)))
         (title (or title (consult--read nil :prompt "Title: ")))
         (body (or body (consult--read nil :prompt "Body: ")))
         (head headbranch)
         (base basebranch)
         (reviewers (if (stringp reviewers)
                        reviewers
                      (and reviewers (listp reviewers)
                           (mapconcat #'identity reviewers ","))))
         (assignees (if (stringp assignees)
                        assignees
                      (and assignees (listp assignees)
                           (mapconcat #'identity assignees ","))))
         (labels (if (stringp labels)
                     labels
                   (and labels (listp labels)
                        (mapconcat #'identity labels ","))))
         (projects (if (stringp projects)
                       projects
                     (and projects (listp projects)
                          (mapconcat #'identity projects ","))))
         (milestone (if (and (stringp milestone) (not (string-empty-p milestone)))
                        milestone
                      (and milestone (listp milestone)
                           (car milestone))))
         (args nil))

    (when (not (equal baserepo headrepo))
      (setq head (concat (consult-gh--get-username headrepo) ":" headbranch)))

    (when (and baserepo head base title body)
      (setq args (delq nil (append args
                                   (list "--repo" baserepo)
                                   (list "--head" head)
                                   (list "--base" base)
                                   (list "--title" (substring-no-properties title))
                                   (list "--body" (substring-no-properties body))
                                   (and reviewers (not web) (list "--reviewer" reviewers))
                                   (and assignees (list "--assignee" assignees))
                                   (and labels (list "--label" labels))
                                   (and milestone (list "--milestone" milestone))
                                   (and projects (list "--project" projects))
                                   (and draft (and (not web) (list "--draft")))
                                   (and web (and (not draft) (list "--web")))
                                   (and fill (pcase fill
                                               ('t (list "--fill"))
                                               ("first" (list "--fill-first"))
                                               ("verbose" (list "--fill-verbose")))))))
      (apply #'consult-gh--command-to-string "pr" "create" args))))

(defun consult-gh-topics--pr-create-presubmit (pr)
  "Prepare PR to submit for creating a new pull request.

PR is a string with properties that identify a github pull requests.
For an example, see  the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh-pr-create'."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (baserepo (get-text-property 0 :baserepo pr))
             (canAdmin (consult-gh--user-canadmin baserepo))
             (isAuthor (consult-gh--user-isauthor pr))
             (nextsteps (append (list (cons "Submit" :submit))
                                (list (cons "Submit as Draft" :draft))
                                (list (cons "Continue in the Browser" :browser))
                                (and canAdmin (list (cons "Add Metadata" :metadata)))
                                (and (or canAdmin isAuthor) (list (cons "Change Refs" :refs)))
                                (list (cons "Cancel" :cancel))))
             (next (consult--read nextsteps
                                  :prompt "Choose what to do next? "
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))
        (while (eq next ':metadata)
          (consult-gh-topics--pr-create-add-metadata)
          (setq next (consult--read nextsteps
                                    :prompt "Choose what to do next? "
                                    :lookup #'consult--lookup-cdr
                                    :sort nil)))

        (while (eq next ':refs)
          (consult-gh-topics--pr-create-change-refs)
          (setq next (consult--read nextsteps
                                    :prompt "Choose what to do next? "
                                    :lookup #'consult--lookup-cdr
                                    :sort nil)))

        (pcase-let* ((`(,title . ,body) (consult-gh-topics--get-title-and-body))
                     (title (or title
                                (and (derived-mode-p 'org-mode)
                                     (cadar (org-collect-keywords
                                             '("title"))))
                                ""))
                     (body (or body ""))
                     (metadata (consult-gh-topics--pr-get-metadata))
                     (baserepo (cdr (assoc "baserepo" metadata)))
                     (basebranch (cdr (assoc "basebranch" metadata)))
                     (headrepo (cdr (assoc "headrepo" metadata)))
                     (headbranch (cdr (assoc "headbranch" metadata)))
                     (reviewers (cdr (assoc "reviewers" metadata)))
                     (assignees (cdr (assoc "assignees" metadata)))
                     (labels (cdr (assoc "labels" metadata)))
                     (milestone (cdr (assoc "milestone" metadata)))
                     (projects (cdr (assoc "projects" metadata))))

          (pcase next
            (':browser (and (consult-gh-topics--pr-create-submit baserepo basebranch headrepo headbranch title body reviewers assignees labels milestone projects nil nil t)))
            (':submit (and (consult-gh-topics--pr-create-submit baserepo basebranch headrepo headbranch title body reviewers assignees labels milestone projects nil nil nil)
                           (message "Pull Request Submitted!")
                           (funcall consult-gh-quit-window-func t)))
            (':draft (and (consult-gh-topics--pr-create-submit baserepo basebranch headrepo headbranch title body reviewers assignees labels milestone projects t nil nil)
                          (message "Draft Submitted!")
                          (funcall consult-gh-quit-window-func t))))))
    (message "Not in a pull requests editing buffer!")))

(defun consult-gh-pr--edit-restore-default (&optional pr)
  "Restore default values when editing PR."

  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (repo (get-text-property 0 :repo pr))
             (canAdmin (consult-gh--user-canadmin repo))
             (basebranch (get-text-property 0 :original-basebranch pr))
             (title (get-text-property 0 :original-title pr))
             (body (get-text-property 0 :original-body pr))
             (reviewers (get-text-property 0 :original-reviewers pr))
             (assignees (get-text-property 0 :original-assignees pr))
             (labels (get-text-property 0 :original-labels pr))
             (milestone (get-text-property 0 :original-milestone pr))
             (projects (get-text-property 0 :original-projects pr))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))

        (add-text-properties 0 1 (list :title title :basebranch basebranch :reviewers reviewers :assignees assignees :labels labels :milestone milestone :projects projects) pr)

        (save-excursion
          ;; change title
          (goto-char (point-min))
          (when (re-search-forward "^.*title: \\(?1:.*\\)?" nil t)
            (replace-match (get-text-property 0 :title pr) nil nil nil 1))

          ;; change base branch
          (goto-char (point-min))
          (when (re-search-forward "^.*base: \\(?1:.*\\)?" nil t)
            (replace-match (get-text-property 0 :basebranch pr) nil nil nil 1))

          (when canAdmin
            ;;change reviewers
            (goto-char (car header))
            (when (re-search-forward "^.*reviewers: \\(?1:.*\\)?" (cdr header) t)
              (replace-match (mapconcat #'identity (get-text-property 0 :reviewers pr) ", ") nil nil nil 1))

            ;;change assignees
            (goto-char (car header))
            (when (re-search-forward "^.*assignees: \\(?1:.*\\)?" (cdr header) t)
              (replace-match (mapconcat #'identity (get-text-property 0 :assignees pr) ", ") nil nil nil 1))

            ;; change labels
            (goto-char (car header))
            (when (re-search-forward "^.*labels: \\(?1:.*\\)?" (cdr header) t)
              (replace-match (mapconcat #'identity (get-text-property 0 :labels pr) ", ") nil nil nil 1))

            ;; change milestone
            (if (equal milestone nil) (setq milestone ""))
            (goto-char (car header))
            (when (re-search-forward "^.*milestone: \\(?1:.*\\)?" (cdr header) t)
              (replace-match (get-text-property 0 :milestone pr) nil nil nil 1))

            ;; change projects
            (goto-char (car header))
            (when (re-search-forward "^.*projects: \\(?1:.*\\)?" (cdr header) t)
              (replace-match (mapconcat #'identity (get-text-property 0 :projects pr) ", ") nil nil nil 1)))

          ;; change body
          (goto-char (cdr header))
          (delete-region (point) (point-max))
          (insert body)))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-title (&optional new old pr)
  "Change title of PR from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (new (or new (consult--read nil
                                         :initial old
                                         :prompt "Title: ")))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))
        (add-text-properties 0 1 (list :title new) pr)
        (when (stringp new)
          (save-excursion (goto-char (point-min))
                          (when (re-search-forward "^.*title: \\(?1:.*\\)?" (and header (consp header) (cdr header)) t)
                            (replace-match (get-text-property 0 :title pr) nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-body (&optional new old pr)
  "Change body of PR from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (new (or new (consult--read nil
                                         :initial old
                                         :prompt "Body: ")))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header))))

        (when (and (stringp new) (not (string-empty-p new)))
          (add-text-properties 0 1 (list :body new) pr)
          (save-excursion
            (goto-char (cdr header))
            (delete-region (point) (point-max))
            (insert new))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-base (&optional new old pr)
  "Change the base branch of PR from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (baserepo (get-text-property 0 :baserepo pr))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (branches (remove old (consult-gh-topics--pr-get-branches baserepo)))
             (new (or new (and branches (listp branches)
                               (consult--read branches
                                              :prompt "Select the new base branch: "
                                              :sort t)))))

        (when (and (stringp new) (not (string-empty-p new)))
          (add-text-properties 0 1 (list :basebranch new) pr)
          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*base: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (get-text-property 0 :basebranch pr) nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-reviewers (&optional new old pr)
  "Change reviewers of PR from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (author (get-text-property 0 :author pr))
             (valid-reviewers (get-text-property 0 :assignable-users pr))
             (valid-reviewers (and (listp valid-reviewers) (delq author valid-reviewers)))
             (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
             (old (cond ((stringp old) old)
                        ((and old (listp old) (length> old 1)) (mapconcat #'identity old sep))
                        ((and old (listp old) (length< old 2)) (car old))))
             (old (if (and (stringp old) (not (string-suffix-p sep old)))
                      (concat old sep)
                    old))
             (new (or new
                      (if (and valid-reviewers (listp valid-reviewers))
                          (completing-read-multiple "Select Reviewers: " valid-reviewers nil t old)
                        (error "No assignable reviewers found")))))

        (when (listp new)
          (setq new (cl-remove-duplicates
                     (cl-remove-if-not (lambda (item) (member item valid-reviewers)) new) :test #'equal))
          (add-text-properties 0 1 (list :reviewers new) pr)

          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*reviewers: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (mapconcat #'identity (get-text-property 0 :reviewers pr) ", ") nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-assignees (&optional new old pr)
  "Change assignees of PR from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (valid-assignees (get-text-property 0 :assignable-users pr))
             (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
             (old (cond ((stringp old) old)
                        ((and (listp old) (length> old 1)) (mapconcat #'identity old sep))
                        ((and (listp old) (length< old 2)) (car old))))
             (old (if (and (stringp old) (not (string-suffix-p sep old)))
                      (concat old sep)
                    old))
             (new (or new
                      (if (and valid-assignees (listp valid-assignees))
                          (completing-read-multiple "Select Asignees: " valid-assignees nil t old)
                        (error "No assignable users found")))))

        (when (listp new)
          (setq new (cl-remove-duplicates
                     (cl-remove-if-not (lambda (item) (member item valid-assignees)) new) :test #'equal))
          (add-text-properties 0 1 (list :assignees new) pr)

          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*assignees: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (mapconcat #'identity (get-text-property 0 :assignees pr) ", ") nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-labels (&optional new old pr)
  "Change PR's labels from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (valid-labels (get-text-property 0 :valid-labels pr))
             (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
             (old (cond ((stringp old) old)
                        ((and (listp old) (length> old 1)) (mapconcat #'identity old sep))
                        ((and (listp old) (length< old 2)) (car old))))
             (old (if (and (stringp old) (not (string-suffix-p sep old)))
                      (concat old sep)
                    old))
             (new (or new
                      (if (and valid-labels (listp valid-labels))
                          (completing-read-multiple "Select Labels: " valid-labels nil t old)
                        (error "No labels found!")))))

        (when (listp new)
          (setq new (cl-remove-duplicates
                     (cl-remove-if-not (lambda (item) (member item valid-labels)) new) :test #'equal))
          (add-text-properties 0 1 (list :labels new) pr)

          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*labels: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (mapconcat #'identity (get-text-property 0 :labels pr) ", ") nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-projects (&optional new old pr)
  "Change PR's labels from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (valid-projects (get-text-property 0 :valid-projects pr))
             (sep (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator))
             (old (cond ((stringp old) old)
                        ((and (listp old) (length> old 1)) (mapconcat #'identity old sep))
                        ((and (listp old) (length< old 2)) (car old))))
             (old (if (and (stringp old) (not (string-suffix-p sep old)))
                      (concat old sep)
                    old))
             (new (or new
                      (if (and valid-projects (listp valid-projects))
                          (completing-read-multiple "Select Projects: " valid-projects nil t old)
                        (error "No projects found!")))))

        (when (listp new)
          (setq new (cl-remove-duplicates
                     (cl-remove-if-not (lambda (item) (member item valid-projects)) new) :test #'equal))
          (add-text-properties 0 1 (list :projects new) pr)

          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*projects: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (mapconcat #'identity (get-text-property 0 :projects pr) ", ") nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-pr--edit-change-milestone (&optional new old pr)
  "Change PR's milestone from OLD to NEW."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr consult-gh--topic))
             (header (car (consult-gh--get-region-with-overlay ':consult-gh-header)))
             (valid-milestones (get-text-property 0 :valid-milestones pr))
             (new (or new
                      (if (and valid-milestones (listp valid-milestones))
                          (consult--read valid-milestones
                                         :initial old
                                         :prompt "Milestone: "
                                         :require-match t)
                        (error "No milestones found!")))))

        (when (stringp new)
          (if (string-empty-p new) (setq new nil))
          (add-text-properties 0 1 (list :milestone new) pr)
          (save-excursion (goto-char (car header))
                          (when (re-search-forward "^.*milestone: \\(?1:.*\\)?" (cdr header) t)
                            (replace-match (get-text-property 0 :milestone pr) nil nil nil 1)))))
    (error "Not in a pr editing buffer!")))

(defun consult-gh-topics--edit-pr-submit (pr &optional title body base reviewers assignees labels projects milestone)
  "Edit PR with new metadata.

Description of Arguments:
  PR        a plis: list of key value pairs for pull request
  TITLE     a string; new title
  BODY      a string; new body
  BASE      a string; new base branch
  REVIEWERS a list of strings; new list of reviewers
  ASSIGNEES a list of strings; new list of assignees
  LABELS    a list of strings; new list of labels
  PROJECTS  a list of strings; new list of projects
  MILESTONE a string; new milestone"
  (pcase-let* ((baserepo (or (get-text-property 0 :baserepo pr) (get-text-property 0 :repo (consult-gh-search-repos nil t))))
               (canAdmin (consult-gh--user-canadmin baserepo))
               (isAuthor (consult-gh--user-isauthor pr))
               (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
               (_ (unless (or canAdmin isAuthor)
                    (user-error "Current user, %s, does not have permissions to edit this pull request" user)))
               (token-scopes (consult-gh--auth-get-token-scopes))
               (number (or (get-text-property 0 :number pr)  (get-text-property 0 :number (consult-gh-pr-list baserepo t))))
               (original-title (get-text-property 0 :original-title pr))
               (original-body (get-text-property 0 :original-body pr))
               (original-basebranch (get-text-property 0 :original-basebranch pr))
               (original-reviewers (get-text-property 0 :original-reviewers pr))
               (original-assignees (get-text-property 0 :original-assignees pr))
               (original-labels (get-text-property 0 :original-labels pr))
               (original-projects (get-text-property 0 :original-projects pr))
               (original-milestone (get-text-property 0 :original-milestone pr))
               (`(,add-reviewers ,remove-reviewers)                          (consult-gh--separate-add-and-remove reviewers original-reviewers))
               (`(,add-assignees ,remove-assignees)                          (consult-gh--separate-add-and-remove assignees original-assignees))
               (`(,add-labels ,remove-labels)                          (consult-gh--separate-add-and-remove labels original-labels))
               (`(,add-projects ,remove-projects)                          (consult-gh--separate-add-and-remove projects original-projects))
               (add-milestone (and (not (equal milestone original-milestone)) (stringp milestone) milestone))
               (remove-milestone (and (not (equal milestone original-milestone)) (or (equal milestone nil) (string-empty-p milestone))))
               (title (and (not (equal title original-title)) title))
               (body (and (not (equal body original-body)) body))
               (base (when (and (not (equal base original-basebranch)) (stringp base)) base))
               (args (list "--repo" baserepo)))

    (when (and add-reviewers (listp add-reviewers)) (setq add-reviewers (consult-gh--list-to-string add-reviewers)))

    (when (and remove-reviewers (listp remove-reviewers)) (setq remove-reviewers (consult-gh--list-to-string remove-reviewers)))

    (when (and add-assignees (listp add-assignees)) (setq add-assignees (consult-gh--list-to-string add-assignees)))

    (when (and remove-assignees (listp remove-assignees)) (setq remove-assignees (consult-gh--list-to-string remove-assignees)))

    (when (and add-labels (listp add-labels))
      (setq add-labels  (consult-gh--list-to-string add-labels)))

    (when (and remove-labels (listp remove-labels))
      (setq remove-labels (consult-gh--list-to-string remove-labels)))

    (when (and add-projects (listp add-projects))
      (setq add-projects (consult-gh--list-to-string add-projects)))

    (when (and remove-projects (listp remove-projects))
      (setq remove-projects (consult-gh--list-to-string remove-projects)))

    (when (or (and canAdmin (or title body base add-assignees remove-assignees add-labels remove-labels add-milestone remove-milestone add-projects remove-projects))
              (or title body base))
      (setq args (delq nil (append args
                                   (and title (list "--title" (substring-no-properties title)))
                                   (and body (list "--body" (substring-no-properties body)))
                                   (and base (list "--base" (substring-no-properties base)))
                                   (and canAdmin add-reviewers (list "--add-reviewer" add-reviewers))
                                   (and canAdmin remove-reviewers (list "--remove-reviewer" remove-reviewers))
                                   (and canAdmin add-assignees (list "--add-assignee" add-assignees))
                                   (and canAdmin remove-assignees (list "--remove-assignee" remove-assignees))
                                   (and canAdmin add-labels (list "--add-label" add-labels))
                                   (and canAdmin remove-labels (list "--remove-label" remove-labels))
                                   (and canAdmin add-milestone (list "--milestone" (concat (substring-no-properties add-milestone))))
                                   (and canAdmin remove-milestone (list "--remove-milestone"))
                                   (and canAdmin (member "project" token-scopes) add-projects (list "--add-project" add-projects))
                                   (and canAdmin (member "project" token-scopes) remove-projects (list "--remove-project" remove-projects)))))
      (apply #'consult-gh--command-to-string "pr" "edit" number args))))

(defun consult-gh-topics--edit-pr-presubmit (pr)
  "Prepare edits on PR to submit.

PR is a string with properties that identify a github pull request.
For an example see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh--pr-view'."
  (if consult-gh-topics-edit-mode
      (let* ((repo (get-text-property 0 :repo pr))
             (canAdmin (consult-gh--user-canadmin repo))
             (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
             (isAuthor (consult-gh--user-isauthor pr))
             (nextsteps (if (or canAdmin isAuthor)
                            (append (list (cons "Submit" :submit))
                                    (and canAdmin
                                         (list (cons "Add/Remove Assignees" :assignees) (cons "Add/Remove Labels" :labels) (cons "Add/Remove Reviewers" :reviewers) (cons "Add/Remove Labels" :labels) (cons "Add/Remove Projects" :projects) (cons "Change Milestone" :milestone)))
                                    (list (cons "Change Base Branch" :base))
                                    (list (cons "Change Title" :title))
                                    (list (cons "Change Body" :body))
                                    (list (cons "Discard edits and restore original values" :default))
                                    (list (cons "Cancel" :cancel)))
                          (user-error "Current user, %s, does not have permissions to edit this pull request" user)))
             (next (when nextsteps (consult--read nextsteps
                                                  :prompt "Choose what do you want to do? "
                                                  :lookup #'consult--lookup-cdr
                                                  :sort nil))))
        (when next
          (pcase-let* ((`(,title . ,body) (consult-gh-topics--get-title-and-body))
                       (title (or title
                                  (and (derived-mode-p 'org-mode)
                                       (cadar (org-collect-keywords
                                               '("title"))))
                                  ""))
                       (body (or body ""))
                       (metadata (when (or isAuthor canAdmin) (consult-gh-topics--pr-get-metadata)))
                       (basebranch (when metadata (cdr (assoc "basebranch" metadata))))
                       (reviewers (when (and canAdmin metadata) (cdr (assoc "reviewers" metadata))))
                       (assignees (when (and canAdmin metadata) (cdr (assoc "assignees" metadata))))
                       (labels (when (and canAdmin metadata) (cdr (assoc "labels" metadata))))
                       (milestone (when (and canAdmin metadata) (cdr (assoc "milestone" metadata))))
                       (projects (when (and canAdmin metadata) (cdr (assoc "projects" metadata)))))

            (pcase next
              (':default (consult-gh-pr--edit-restore-default))
              (':title (consult-gh-pr--edit-change-title nil title))
              (':body  (consult-gh-pr--edit-change-body nil nil))
              (':assignees (consult-gh-pr--edit-change-assignees nil assignees))
              (':reviewers (consult-gh-pr--edit-change-reviewers nil reviewers))
              (':labels (consult-gh-pr--edit-change-labels nil labels))
              (':milestone (consult-gh-pr--edit-change-milestone nil milestone))
              (':projects (consult-gh-pr--edit-change-projects nil projects))
              (':base (consult-gh-pr--edit-change-base nil basebranch))
              (':submit
               (and (consult-gh-topics--edit-pr-submit pr title body basebranch reviewers assignees labels projects milestone)
                    (message "Edits %s" (propertize "Submitted!" 'face 'consult-gh-success))
                    (funcall consult-gh-quit-window-func t)))))))
    (message "% in a %s buffer!" (propertize "Not" 'face 'consult-gh-error) (propertize "pull request editing" 'face 'consult-gh-error))))

(defun consult-gh-pr--merge-create-commit (pr type &optional auto admin subject body)
  "Create a merge commit message of TYPE for PR.

TYPE can be either “merge”, “rebase”, or “squash”.

Description of Arguments:
  PR      a string; string with proerties that describes pull request
  TYPE    a string; can be  “merge”, “rebase”, or “squash”
  AUTO    a boolean; whether this is for auto-merge or not
  ADMIN   a boolean; whether this merge is overriding requirements with
          admin persmissions
  SUBJECT a string; header subject of merge commit message
  BODY    a string; body of merge commit message"
  (let* ((pr (or pr consult-gh--topic))
         (repo (get-text-property 0 :repo pr))
         (number (get-text-property 0 :number pr))
         (title (get-text-property 0 :title pr))
         (headrepo (get-text-property 0 :headrepo pr))
         (baserepo (get-text-property 0 :baserepo pr))
         (head (if (equal headrepo baserepo)
                   (get-text-property 0 :headbranch pr)
                 (concat (consult-gh--get-username headrepo) "/" (get-text-property 0 :headbranch pr))))
         (subject (or subject
                      (cond
                       ((equal type "merge") (format "Merge pull request #%s from %s" number head))
                       ((equal type "squash") title)
                       (t nil))))
         (body (or body
                   (cond
                    ((equal type "merge") title)
                    ((equal type "squash") (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" "commits" "--template" "{{range .commits}}-\s**{{.messageHeadline}}**\n{{.messageBody}}\n\n{{end}}"))
                    (t nil))))
         (newtopic (format "%s/%s merge commit" repo number))
         (buffer (format "*consult-gh-pr-%s-commit: %s #%s" type repo number)))

    (add-text-properties 0 1 (text-properties-at 0 pr) newtopic)
    (add-text-properties 0 1 (list :type "merge commit" :isComment nil :new t :target type :auto auto :admin admin) newtopic)
    (with-current-buffer  (consult-gh-topics--get-buffer-create buffer "commit" newtopic)
      (unless (not (= (buffer-size) 0))
        (pcase-let* ((inhibit-read-only t)
                     (`(,title-marker _header-marker) (consult-gh-topics--markers-for-metadata)))

          (insert (consult-gh-topics--format-field-header-string (concat title-marker "title: ")))

          (save-mark-and-excursion
            (when subject (insert subject))
            (insert "\n\n")
            (when body
              (pcase major-mode
                ('gfm-mode (insert body))
                ('markdown-mode (insert body))
                ('org-mode (insert (with-temp-buffer
                                     (insert body)
                                     (consult-gh--markdown-to-org)                                        (consult-gh--whole-buffer-string))))
                ('text-mode (insert body))))))))

    (funcall consult-gh-pop-to-buffer-func buffer)))

(cl-defun consult-gh-pr--merge-submit (pr &key merge rebase squash admin auto disable-auto delete-branch subject body)
  "Submit merge command for PR.

This runs “gh pr merge” and passes the arguments to command line arguments.
Refer to gh's manual for detailed description of each argument.

Description of Arguments:

  PR            a string; propertized text that describes a pull request
  MERGE         a boolean; whether to add the “--merge” switch
  REBASE        a boolean; whether to add the “--rebase” switch
  SQUASH        a boolean; whether to add the “--squash” switch
  ADMIN         a boolean; whether to add the “--admin” switch
  AUTO          a boolean; whether t add the “--auto” switch
  DISABLE-AUTO  a boolean; whether to add the “--disable-auto” switch
  DELETE-BRANCH a boolean; whether to add the “--delete-branch” switch
  SUBJECT       a string; subject of the merge commit message
  BODY          a string; body of the merge commit message"
  (let* ((pr (or pr consult-gh--topic))
         (repo (get-text-property 0 :repo pr))
         (number (get-text-property 0 :number pr))
         (args (list "pr" "merge" number "--repo" repo)))
    (setq args (delq nil (append args
                                 (and admin (list "--admin"))
                                 (and auto (list "--auto"))
                                 (and disable-auto (list "--disable-auto"))
                                 (and merge (list "--merge"))
                                 (and rebase (list "--rebase"))
                                 (and squash (list "--squash"))
                                 (and subject (list "--subject" subject))
                                 (and body (list "--body" body))
                                 (and delete-branch (list "--delete-branch")))))
    (apply #'consult-gh--command-to-string args)))

(defun consult-gh-pr--merge-read-commit-type (pr &optional auto admin)
  "Query the user to pick type of commit message for merging PR.

AUTO is a boolean determining whether the commit message is for enabling
auto-merge
ADMIN is a boolean, determining whether the commit message is for a merge
overriding requirements with admin permissions."
  (if auto
      (pcase (consult--read (list (cons "Yes, create an auto merge commit" :merge)
                                  (cons "Cancel" :cancel))
                            :prompt "Confirm enabling auto merge?"
                            :lookup #'consult--lookup-cdr
                            :require-match t
                            :sort nil)

        (':merge
         (pcase (consult--read (list (cons "Yes, edit the commit message" :commit)
                                     (cons "No, use default commit message" :submit)
                                     (cons "Cancel" :cancel))
                               :prompt "Do you want to edit the commit message?"
                               :lookup #'consult--lookup-cdr
                               :require-match t
                               :sort nil)
           (':cancel)
           (':commit
            (consult-gh-pr--merge-create-commit pr "merge" auto admin))
           (':submit
            (and (y-or-n-p "This will enable automerge for the pull reqeust on GitHub.  Do you want to continue?")
                 (consult-gh-pr--merge-submit pr :merge t :auto auto :admin admin))))))
    (pcase (consult--read (list (cons "Merge commit" :merge)
                                (cons "Rebase and merge" :rebase)
                                (cons "Squash and merge" :squash)
                                (cons "Cancel" :cancel))
                          :prompt "What is next?"
                          :lookup #'consult--lookup-cdr
                          :require-match t
                          :sort nil)

      (':merge
       (pcase (consult--read (list (cons "Edit the commit subject and message" :commit)
                                   (cons "Submit" :submit))
                             :prompt "What is next?"
                             :lookup #'consult--lookup-cdr
                             :require-match t
                             :sort nil)
         (':commit
          (consult-gh-pr--merge-create-commit pr "merge" auto admin))
         (':submit
          (and (y-or-n-p "This will merge the pull reqeust on GitHub.  Do you want to continue?")
               (consult-gh-pr--merge-submit pr :merge t :auto auto :admin admin)))))
      (':rebase
       (and (y-or-n-p "This will merge the pull reqeust with a rebase commit on GitHub.  Do you want to continue?")
            (consult-gh-pr--merge-submit pr :rebase t :auto auto :admin admin)))
      (':squash
       (pcase (consult--read (list (cons "Edit the commit subject and message" :commit)
                                   (cons "Submit" :submit))
                             :prompt "What is next?"
                             :lookup #'consult--lookup-cdr
                             :require-match t
                             :sort nil)
         (':commit
          (consult-gh-pr--merge-create-commit pr "squash" auto admin))
         (':submit
          (and (y-or-n-p "This will merge the pull reqeust with a squash commit on GitHub.  Do you want to continue?")
               (consult-gh-pr--merge-submit pr :squash t :auto auto :admin admin))))))))

(defun consult-gh-pr--merge-merge (pr &optional auto admin)
  "Merge PR.

PR is a propertized string describing a pull request.  For example,PR can be
   the text stored in the buffer-local variable `consult-gh--topic' in a
   buffer created by `consult-gh--pr-view'.

If AUTO is non-nil enables auto-merge.

If ADMIN is non-nil overrides requirements with admin premissions."
  (pcase-let* ((pr (or pr consult-gh--topic))
               (repo (get-text-property 0 :repo pr))
               (number (get-text-property 0 :number pr))
               (state (get-text-property 0 :state pr))
               (mergeable  (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" "mergeable" "--template" "{{.mergeable}}"))
               (merge-state  (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" "mergeStateStatus" "--template" "{{.mergeStateStatus}}")))

    (cond
     ((not (equal state "OPEN"))
      (message "Pull request is already %s!" (downcase state)))
     ((and (equal mergeable "CONFLICTING") (not auto))
      (pcase (consult--read (list (cons "Cancel merge and go resolve the conflicts" :cancel)
                                  (cons "Enable automerge (automatically merge when conflicts are resolved and all other requirements are met)." :auto))
                            :prompt "This branch has conflicts that must be resolved before merging. What do you want to do?"
                            :lookup #'consult--lookup-cdr
                            :require-match t
                            :sort nil)
        (':cancel)
        (':auto (consult-gh-pr--merge-read-commit-type pr t))))
     ((and (equal mergeable "MERGEABLE") (equal merge-state "BLOCKED") (not auto) (not admin))
      (pcase (consult--read (list (cons "Merge without waiting for requirements to be met (bypass branch protections)." :admin)
                                  (cons "Enable automerge (automatically merge when all requirements are met)." :auto))
                            :prompt "Merging is blocked. What would you like to do?"
                            :lookup #'consult--lookup-cdr
                            :require-match t
                            :sort nil)
        (':admin (setq admin t))
        (':auto (setq auto t)))
      (consult-gh-pr--merge-read-commit-type pr auto admin))
     ((and (equal mergeable "MERGEABLE") (equal merge-state "BLOCKED") (or auto admin))
      (consult-gh-pr--merge-read-commit-type pr auto admin))
     ((and (equal mergeable "MERGEABLE") (equal merge-state "CLEAN"))
      (consult-gh-pr--merge-read-commit-type pr auto admin))
     (t (and (y-or-n-p "Something went wrong.  Do you want to open the pull request in the browser?")
             (funcall #'consult-gh-topics-open-in-browser pr))))))

(defun consult-gh-pr--merge-enable-automerge (pr)
  "Enable auto-merge for PR.

PR is a propertized string describing a pull request.  For example,PR can be
the text stored in the buffer-local variable `consult-gh--topic' in a
buffer created by `consult-gh--pr-view'."
  (consult-gh-pr--merge-merge pr t))

(defun consult-gh-pr--merge-disable-automerge (pr)
  "Disable auto-merge for PR.

PR is a propertized string describing a pull request.  For example,PR can be
the text stored in the buffer-local variable `consult-gh--topic' in a
buffer created by `consult-gh--pr-view'."
  (consult-gh-pr--merge-submit pr :disable-auto t))

(defun consult-gh-topics--pr-merge-presubmit (pr)
  "Prepapre PR for merging.

This runs some interactive queries to determine how to merge the pull
request, PR, which is a propertized string describing the pull request.
For example, PR can be the text stored in the buffer-local variable
`consult-gh--topic' in a buffer created by `consult-gh--pr-view'."
  (if consult-gh-topics-edit-mode
      (pcase-let* ((auto (get-text-property 0 :auto pr))
                   (admin (get-text-property 0 :admin pr))
                   (`(,subject ,body) (consult-gh-topics--get-title-and-body))
                   (subject (or subject
                                (and (derived-mode-p 'org-mode)
                                     (cadar (org-collect-keywords
                                             '("title"))))
                                ""))
                   (subject (and (stringp subject) (substring-no-properties subject)))
                   (body (and (stringp body) (substring-no-properties body)))
                   (action (get-text-property 0 :target pr)))
        (pcase action
          ("merge" (and
                    (y-or-n-p "This will merge the pull reqeust on GitHub.  Do you want to continue?")
                    (consult-gh-pr--merge-submit pr :merge t :auto auto :admin admin :subject subject :body body)
                    (message "%s Commit Submitted!" (propertize "Merge" 'face 'consult-gh-success))
                    (funcall consult-gh-quit-window-func t)))
          ("rebase" (and
                     (y-or-n-p "This will merge the pull reqeust with a rebase commit on GitHub.  Do you want to continue?")
                     (consult-gh-pr--merge-submit pr :rebase t :auto auto :admin admin :subject subject :body body)
                     (message "%s Commit Submitted!" (propertize "Rebase" 'face 'consult-gh-success))
                     (funcall consult-gh-quit-window-func t)))
          ("squash"  (and
                      (y-or-n-p "This will merge the pull reqeust with a squash commit on GitHub.  Do you want to continue?")
                      (consult-gh-pr--merge-submit pr :squash t :auto auto :admin admin :subject subject :body body)
                      (message "%s Commit Submitted!"(propertize "Squash" 'face 'consult-gh-success))
                      (funcall consult-gh-quit-window-func t)))))
    (message "%s in a %s buffer!" (propertize "Not" 'face 'consult-gh-error) (propertize "pull request editing" 'face 'consult-gh-error))))

(defun consult-gh-topics--pr-review-add-comment (&optional review)
  "Add a comment to a REVIEW.

REVIEW is a string with properties that identify a review.
For an example, see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh-pr-review'."
  (let* ((topic (or review consult-gh--topic))
         (view-buffer (get-text-property 0 :view-buffer topic))
         (repo (get-text-property 0 :repo topic))
         (number (get-text-property 0 :number topic))
         (target (get-text-property 0 :target topic)))
    (cond
     ((equal target "review")
      (if (and view-buffer (buffer-live-p (get-buffer view-buffer)))
          (funcall consult-gh-pop-to-buffer-func (get-buffer view-buffer))
        (funcall #'consult-gh--pr-view repo number))
      (widen)
      (outline-hide-sublevels 2)
      (goto-char (point-min))
      (re-search-forward ".*Files Changed.*\n" nil t)))))

(defun consult-gh-topics--pr-review-insert-comment-in-buffer (body info &optional buffer)
"Insert comment with BODY and metadata INFO at the end of BUFFER.

BODY is a string for body of comment
INFO is a property list with key value pairs for :path, :line, :side,
and other info that identifies comment locatoin.  This is passed to comments
field on github api.
For more informaiton see:
URL `https://docs.github.com/en/rest/pulls/reviews?apiVersion=2022-11-28#create-a-review-for-a-pull-request'."
(let* ((comment-info (append info (list :body body)))
       (path (plist-get comment-info :path))
       (line (plist-get comment-info :line))
       (startline (plist-get comment-info :startline))
       (snippet (plist-get comment-info :snippet))
       (header-marker nil)
       (block-begin nil)
       (block-end nil)
       (text nil))

  (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
         (let* ((inhibit-read-only t)
                (comments (cl-remove-duplicates (append (get-text-property 0 :comments consult-gh--topic) (list comment-info)) :test #'equal))
                (mode-func nil))
           (cond
            ((derived-mode-p 'gfm-mode)
             (setq header-marker "#"
                   block-begin "```"
                   block-end "```"
                   mode-func #'gfm-mode))
            ((derived-mode-p 'markdown-mode)
             (setq header-marker "#"
                   block-begin "```"
                   block-end "```"
                   mode-func #'markdown-mode))
            ((derived-mode-p 'org-mode)
             (setq header-marker "*"
                   block-begin "#+begin_src "
                   block-end "#+end_src "
                   mode-func #'org-mode))
            (t
             (setq header-marker "#"
                   block-begin "```"
                   block-end "```"
                   mode-func #'outline-mode)))

           (setq text (with-temp-buffer
                   (insert (concat header-marker header-marker " "
                            (propertize (concat
                                   "Comment"
                                   (and path (concat " on file " path))
                                   (if startline
                                        (concat " line " (number-to-string startline) (and line (concat " to " (number-to-string line))))
                                     (and line (concat " line " (number-to-string line))))
                                   "\n"
                                   (and snippet (concat block-begin "\n"
                                                        snippet
                                                        "\n" block-end "\n")))
                                       'cursor-intangible t)
                            (if (derived-mode-p 'markdown-mode) (concat (consult-gh-topics--format-field-cursor-intangible "\n---") "\n")
                              (consult-gh-topics--format-field-cursor-intangible "\n---\n"))
                            (and body (propertize body :consult-gh-comments-body t))
                            (if (derived-mode-p 'markdown-mode) (concat (consult-gh-topics--format-field-cursor-intangible "\n---") "\n")
                              (consult-gh-topics--format-field-cursor-intangible "\n---\n"))))
           (funcall mode-func)
           (concat (propertize (consult-gh--whole-buffer-string) :consult-gh-comments comment-info :consult-gh-markings t))))
      (goto-char (point-max))
      (if (not (re-search-backward (concat header-marker " " "Comments\n") nil t))
          (insert (propertize (concat header-marker " " "Comments\n") :consult-gh-markings t 'cursor-intangible t))
        (goto-char (point-max)))
      (let ((beg (point))
             (end nil))
      (when (stringp text)
        (insert text)
        (setq end (point))
        (add-text-properties beg end (list :consult-gh-markings t)))
      (cursor-intangible-mode +1)
      (goto-char (point-max))
      (add-text-properties 0 1 (list :comments comments :isComment nil) consult-gh--topic)))))))

(defun consult-gh-topics--pr-review-append-comment (&optional topic review-buffer)
  "Add the comment in TOPIC to REVIEW-BUFFER.

TOPIC is a string with properties that identify a comment.
For an example, see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh-topics-comment-create'
TOPIC defaults to `consult-gh--topic'.

REVIEW-BUFFER defaults to value stored in text-properties of TOPIC."
  (let* ((topic (or topic consult-gh--topic))
         (body (consult-gh--whole-buffer-string))
         (info (get-text-property 0 :comment-info topic))
         (review-buffer (or review-buffer (get-text-property 0 :review-buffer topic))))
    (if (and review-buffer (buffer-live-p (get-buffer review-buffer)))
        (progn (and (consult-gh-topics--pr-review-insert-comment-in-buffer body info (get-buffer review-buffer))
                    (message "Comment Added!")
                    (funcall consult-gh-quit-window-func t))
               (funcall consult-gh-pop-to-buffer-func (get-buffer review-buffer)))
      (error "The review buffer, %s, does not exist" review-buffer))))

(defun consult-gh-topics--pr-review-get-comments (&optional buffer)
  "Get comments on review in BUFFER.

BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
                       (let* ((regions (consult-gh--get-region-with-prop :consult-gh-comments))
                              (text-comments nil)
                              (mode-func (cond
                                     ((derived-mode-p 'gfm-mode) #'gfm-mode)
                                     ((derived-mode-p 'markdown-mode) #'markdown-mode)
                                     ((derived-mode-p 'org-mode)  #'org-mode)
                                     (t #'outline-mode))))
                         (cl-loop for region in regions
                                  do
                                  (save-excursion
                                    (let* ((body-region
                                            (car-safe (consult-gh--get-region-with-prop :consult-gh-comments-body nil (car region) (cdr region))))
                                           (body (when (and body-region (listp body-region)) (string-trim (buffer-substring (car body-region) (cdr body-region)))))
                                           (info (get-text-property (car region) :consult-gh-comments)))
                                      (when (and (stringp body) (not (string-empty-p body)))
                                        (setq text-comments (append text-comments (list (plist-put info :body (with-temp-buffer (insert (substring-no-properties body)) (funcall mode-func) (consult-gh-topics--buffer-string))))))))))
                         text-comments)))

(defun consult-gh-topics--pr-review-comment-submit (&optional topic body)
  "Create a new review comment with BODY for TOPIC.

TOPIC defaults to `consult-gh--topic' and should be a string with
properties that identify a comment.  The properties should contain :path,
:line, :side, and other info that can be passed GitHub API.  For more
informaiton see:
URL `https://docs.github.com/en/rest/pulls/comments?apiVersion=2022-11-28#create-a-review-comment-for-a-pull-request'.

BODY is a string for comment text.  When BODY is nil, the value of \=:body
key stored in TOPIC properties is stored, and if that is nil as well, the
content of the current buffer is used."
  (let* ((topic (or topic consult-gh--topic))
         (review-buffer (get-text-property 0 :review-buffer topic))
         (view-buffer (get-text-property 0 :view-buffer topic))
         (target-buffer (if (or review-buffer view-buffer)
                            (get-buffer  (or review-buffer view-buffer))))
         (comment (get-text-property 0 :comment-info topic))
         (body (or body
                   (plist-get comment :body)
                   (consult-gh-topics--buffer-string)))
         (repo (plist-get comment :repo))
         (number (plist-get comment :number))
         (commit-id (plist-get comment :commit-id))
         (path (plist-get comment :path))
         (subject-type (plist-get comment :subject-type))
         (line (plist-get comment :line))
         (side (plist-get comment :side))
         (startside (plist-get comment :startside))
         (startline (plist-get comment :startline))
         (in-reply-to (plist-get comment :in-reply-to))
         (args (list "-X" "POST" "-H" "Accept: application/vnd.github+json")))

    (setq args (append args
                       (list (format "repos/%s/pulls/%s/comments" repo number))
                       (and body (list "-f" (concat "body=" body)))
                       (and commit-id (list "-f" (concat "commit_id=" commit-id)))
                       (and path (list "-f" (concat "path=" path)))
                       (and startline (list "-F" (concat "start_line=" (number-to-string startline))))
                       (and startside (list "-f" (concat "start_side=" startside)))
                       (and line (list "-F" (concat "line=" (number-to-string line))))
                       (and side (list "-f" (concat "side=" side)))
                       (and in-reply-to (list "-f" (concat "in_reply_to=" in-reply-to)))
                       (and subject-type (list "-f" (concat "subject_type=" subject-type)))))
    (let ((response (apply #'consult-gh--call-process "api" args)))
      (cond
       ((= (car response) 0)
        (let* ((table (consult-gh--json-to-hashtable (cadr response)))
               (id (and (hash-table-p table) (gethash :id table))))
          (prog2
              (message "Review Submitted!")
              (or id nil)
            (funcall consult-gh-quit-window-func t)
            (if (and target-buffer (buffer-live-p target-buffer))
                (funcall consult-gh-pop-to-buffer-func target-buffer)))))
       ((= (car response) 422)
        (let* ((table (consult-gh--json-to-hashtable (cadr response)))
               (id (and (hash-table-p table) (gethash :id table))))
          (prog2
              (message (cadr response))
              (or id nil)
            nil)))
       (t
        (message (cadr response))
        nil)))))

(defun consult-gh-topics--pr-review-submit (repo number body &optional commit-id event comments)
  "Create a new review for pull request NMBER in REPO with BODY.

Description of Arguments:
  REPO        a string; full name of the repository
  NUMBER      a string; pull request id nunber
  BODY        a string; body text of the review
  COMMIT-ID   a string; id of the commit being reviewed
  EVENT       a string;
  COMMENTS     a list of plists; each with detials of a single comment

COMMENTS should be a list of property lists, where each plist contains
:path, :startline, :startside, :line, :side, :body as needed for a comment.
For description of these parametrs refer to:
URL `https://docs.github.com/en/rest/pulls/reviews?apiVersion=2022-11-28#create-a-review-for-a-pull-request'."
  (let ((args (list "-X" "POST" "-H" "Accept: application/vnd.github+json")))
    (setq args (append args
                       (list (format "repos/%s/pulls/%s/reviews" repo number))
                       (and body (list "-F" (concat "body=" body)))
                       (and event (list "-F" (concat "event=" event)))
                       (and commit-id (list "-F" (concat "commit_id=" commit-id)))
                       (and comments (apply #'append (cl-loop for comment in comments
                                                              collect (let* ((path (plist-get comment :path))
                                                                             (line (plist-get comment :line))
                                                                             (side (plist-get comment :side))
                                                                             (startside (plist-get comment :startside))
                                                                             (startline (plist-get comment :startline))
                                                                             (text (plist-get comment :body)))
                                                                        (append (list)
                                                                                (and path (list "-F" (concat "comments[][path]=" path)))
                                                                                (and startline (list "-F" (concat "comments[][start_line]=" (number-to-string startline))))
                                                                                (and startside (list "-F" (concat "comments[][start_side]=" startside)))
                                                                                (and line (list "-F" (concat "comments[][line]=" (number-to-string line))))
                                                                                (and side (list "-F" (concat "comments[][side]=" side)))
                                                                                (and text (list "-F" (concat "comments[][body]=" text))))))))))

    (let ((response (apply #'consult-gh--call-process "api" args)))
      (cond
       ((= (car response) 0)
        (let* ((table (consult-gh--json-to-hashtable (cadr response)))
               (id (and (hash-table-p table) (gethash :id table))))
          (prog2
              (message (concat (if event "Review " "Draft Pending Review ") "Submitted!"))
              (or id nil)
            (funcall consult-gh-quit-window-func t))))
       ((= (car response) 422)
        (let* ((table (consult-gh--json-to-hashtable (cadr response)))
               (id (and (hash-table-p table) (gethash :id table))))
          (prog2
              (message (cadr response))
              (or id nil)
            nil)))
       (t
        (message (cadr response))
        nil)))))

(defun consult-gh-topics--pr-review-presubmit (review)
  "Prepare pull request REVIEW to submit.

REVIEW is a string with properties that identify a github pull request
review.  For an example, see buffer-local variable `consult-gh--topic' in
the buffer generated by `consult-gh-pr-review'."
  (if consult-gh-topics-edit-mode
      (let* ((review (or review consult-gh--topic))
             (repo (get-text-property 0 :repo review))
             (number (get-text-property 0 :number review))
             (nextsteps (append (list (cons "Submit" :submit))
                                (list (cons "Add Comment on the Code" :code-comment))
                                (list (cons "Continue in the Browser" :browser))
                                (list (cons "Cancel" :cancel))))
             (next (consult--read nextsteps
                                  :prompt "Choose what to do next? "
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))
        (cond
         ((eq next :code-comment)
          (consult-gh-topics--pr-review-add-comment))
         ((eq next :submit)
          (let* ((body (consult-gh-topics--buffer-string))
                 (comments (consult-gh-topics--pr-review-get-comments))
                 (commit-id (plist-get (car (get-text-property 0 :comments review)) :commit-id))
                 (event (consult--read '(("Comment" . "COMMENT")
                                         ("Request Changes" . "REQUEST_CHANGES")
                                         ("Approve" . "APPROVE")
                                         ("Submit Review as Pending Draft" . nil))
                                       :prompt "Select Action: "
                                       :lookup #'consult--lookup-cdr
                                       :require-match t
                                       :sort nil)))
            (consult-gh-topics--pr-review-submit repo number body commit-id event comments)))
         ((eq next :browser)
          (let* ((body (consult-gh-topics--buffer-string))
                 (comments (get-text-property 0 :comments review))
                 (commit-id (plist-get (get-text-property 0 :comment-info review) :commit-id))
                 (event nil)
                 (id (consult-gh-topics--pr-review-submit repo number body commit-id event comments)))
            (when id
              (consult-gh--make-process "consult-gh-draft-review"
                                        :when-done (lambda (_ out)
                                                     (let* ((table (consult-gh--json-to-hashtable out))
                                                            (url (and (hash-table-p table) (gethash :html_url table))))
                                                       (when url
                                                         (funcall consult-gh-browse-url-func url))))
                                        :cmd-args (list "api" "-H" "Accept: application/vnd.github.json" (format "repos/%s/pulls/%s/reviews/%s" repo number id))))))))
    (message "%s in a %s buffer!" (propertize "Not" 'face 'consult-gh-error) (propertize "pull request editing" 'face 'consult-gh-error))))

(defun consult-gh-topics--get-buffer-create (name subject topic)
  "Get or create a buffer with NAME for SUBJECT and TOPIC.

Description of Arguments:
  NAME    a strig; name of the buffer
  SUBJECT a string; the subject of the content
          (e.g. repo, comment, issue, pull request, etc.)
  TOPIC   a string; string with properties that identify the topic
          (see `consult-gh--topic' for example)"
  (let* ((buffer (get-buffer-create name))
         (existing (not (= (buffer-size buffer) 0)))
         (confirm (if existing
                      (consult--read
                       (list (cons "Resume editing the existing draft." :resume)
                             (cons (format "Create a new %s from scratch, but do not discard the old one." subject) :new)
                             (cons "Discard the old draft and create a new one from scratch." :replace))
                       :prompt (format "You already have an existing draft for this %s.  Would you like to resume editing that one or start a new one? " subject)
                       :lookup #'consult--lookup-cdr
                       :sort nil
                       :require-match t))))

    (when existing
      (cond
       ((eq confirm :resume) (setq existing t))
       ((eq confirm :replace) (setq existing nil))
       ((eq confirm :new)
        (setq existing nil)
        (setq buffer (generate-new-buffer name nil)))))

    (with-current-buffer buffer
      (unless existing
        (let ((inhibit-read-only t))
          (erase-buffer)
          (pcase consult-gh-topic-major-mode
            ('gfm-mode (gfm-mode) (markdown-display-inline-images))
            ('markdown-mode (markdown-mode) (markdown-display-inline-images))
            ('org-mode (org-mode))
            (_ (outline-mode)))))
      (setq-local consult-gh--topic topic)
      (consult-gh-topics-edit-mode +1)
      (goto-char (point-max))
      (with-no-warnings (outline-show-all))
      (current-buffer))))

(defun consult-gh-topics--buffer-string (&optional buffer)
  "Get BUFFER string for consult-gh-topics.

Extracts the buffer string after removing consult-gh specific regions.
This inclused regions with the text property \=:consult-gh-comments or
regions with an overlay of \=:consult-gh-header."
  (let* ((text (consult-gh--whole-buffer-string buffer))
         (header-regions (consult-gh--get-region-with-overlay ':consult-gh-header))
         (mode (cond
                ((derived-mode-p 'gfm-mode) 'gfm-mode)
                ((derived-mode-p 'markdown-mode) 'markdown-mode)
                ((derived-mode-p 'org-mode) 'org-mode)
                (t 'text-mode))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (insert text)
        (when header-regions
          (cl-loop for region in header-regions
                   do (delete-region (car region) (cdr region))))
        (consult-gh--delete-region-with-prop :consult-gh-comments)
        (consult-gh--delete-region-with-prop :consult-gh-markings)
        (cond
         ((eq mode 'gfm-mode)
          (gfm-mode)
          (consult-gh--whole-buffer-string))
         ((eq mode 'markdown-mode)
          (markdown-mode)
          (consult-gh--whole-buffer-string))
         ((eq mode 'org-mode)
          (org-mode)
          (consult-gh--org-to-markdown))
         (t (text-mode)
            (consult-gh--whole-buffer-string)))))))

(defun consult-gh-topics--get-title-and-body (&optional buffer)
  "Parse the BUFFER to get title and body of comment.

BUFFER defaults to the `current-buffer'."
  (let* ((text (consult-gh-topics--buffer-string buffer)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((title nil)
            (body nil))
        (goto-char (point-min))
        (when (looking-at "\\`# title: *\\|\\`#\\+title: *")
          (goto-char (match-end 0))
          (setq title (string-trim
                       (buffer-substring (point) (line-end-position))))
          (forward-line))
        (setq body (string-trim
                    (buffer-substring (point) (point-max))))
        (cons title body)))))

(defun consult-gh-topics--format-field-header-string (string &optional prefix suffix)
  "Make a read-only field header from STRING.

When optional arguments PREFIX, or SUFFIX are non-nil, add them
as normal text without propeties before or after STRING.

This is useful to create non-editable fields for forms such as
“Title: ” or “Date: ” in the line."
  (concat prefix
          (propertize (substring string 0 -1) 'read-only t 'cursor-intangible t)
          (propertize (substring string -1) 'read-only t 'cursor-intangible t 'rear-nonsticky t)
          suffix))

(defun consult-gh-topics--format-field-cursor-intangible (string &optional prefix suffix)
  "Make a read-only field header from STRING.

When optional arguments PREFIX, or SUFFIX are non-nil, add them
as normal text without propeties before or after STRING.

This is useful to create non-editable fields for forms such as
“Title: ” or “Date: ” in the line."
  (concat prefix
          (propertize (substring string 0 -1) 'cursor-intangible t)
          (propertize (substring string -1) 'cursor-intangible t 'rear-nonsticky t)
          suffix))

(defun consult-gh-topics--markers-for-metadata ()
  "Get markers depending on `consult-gh-topic-major-mode'."
  (pcase consult-gh-topic-major-mode
    ('gfm-mode
     (list "# " "> "))
    ('markdown-mode
     (list "# " "> "))
    ('org-mode
     (list "#+" "#+"))
    (_
     (list "# " "> "))))

(defun consult-gh-topics--code-comment-submit (comment repo number &optional commit-id path line side startline startside)
  "Submit a comment on LINE at PATH for pull request NUMBER in REPO.

Description of Arguments

  COMMENT    a string; comment on code in LINE at PATH in REPO
  REPO       a string; full name of repository
  NUMBER     a string; number of pull request
  COMMIT-ID  a string; sha of the commit to comment on
  PATH       a string; path to a file in repo
  LINE       a number; number of line at path to comment on
  SIDE       a string; side off diff to comment on
  STARTLINE  a number; start line for multi-line comments
  STARTSIDE  a string; start side for multi-line comments"
  (let* ((args (list "api" "-X" "POST")))
    (if (string-empty-p comment)
        (message "Comment cannot be empty!")
      (progn
        (setq args (append args
                           (and comment (list "-f" (concat "body="  comment)))
                           (and path (list "-f" (concat "path="  path )))
                           (and startline (list "-F" (concat "start_line=" (number-to-string startline))))
                           (and startside (list "-f" (concat "start_side=" startside)))
                           (and line (list "-F" (concat "line=" (number-to-string line))))
                           (and side (list "-f" (concat "side=" side)))
                           (and commit-id (list "-f" (concat "commit_id=" commit-id)))
                           (list (format "repos/%s/pulls/%s/comments" repo number))))
        (apply #'consult-gh--command-to-string args)))))

(defun consult-gh-topics--file-comment-submit (comment repo number &optional commit-id path)
  "Add a COMMENT on file in pull request NUMBER in REPO.

Description of Arguments

  COMMENT    a string; text of comment on file at PATH in REPO
  REPO       a string; full name of repository
  NUMBER     a string; id number of pull request
  COMMIT-ID  a string; sha of commit to comment on
  PATH       a string; path to a file in repo"
  (let* ((args (list "api" "-X" "POST")))
    (if (string-empty-p comment)
        (message "Comment cannot be empty!")
      (progn
        (setq args (append args
                           (and comment (list "-f" (concat "body="  comment)))
                           (and commit-id (list "-f" (concat "commit_id=" commit-id)))
                           (and path (list "-f" (concat "path="  path )))
                           (list "-f" "subject_type=file")
                           (list (format "repos/%s/pulls/%s/comments" repo number))))
        (apply #'consult-gh--command-to-string args)))))

(defun consult-gh-topics--reply-comment-submit (comment comment-id reply-url)
  "Add a COMMENT in reply to COMMENT-ID pull request.

Description of Arguments

  COMMENT    a string; body text of comment
  COMMENT-ID a string; id of the comment replying to
  REPLY-URL  a string; the GitHub api url to send the respond to"
  (let* ((args (list "api" "-X" "POST")))
    (if (string-empty-p comment)
        (message "Comment cannot be empty!")
      (progn
        (setq args (append args
                           (and comment (list "-f" (concat "body="  comment)))
                           (and comment-id (list "-F" (concat "in_reply_to=" (number-to-string comment-id))))
                           (list reply-url)))
        (apply #'consult-gh--command-to-string args)))))

(defun consult-gh-topics--topic-comment-submit (&optional comment repo target number)
  "Submit the COMMENT on topic of TYPE and NUMBER in REPO.

This command submits the content of the COMMENT string github api for topic
of TYPE (e.g. issue, pr, ...) and id NUMBER.

Description of Arguments:
  REPO   a string; full name of target epository
  TARGET a string; TARGET topic (e.g. issue, pr, review ...)
  NUMBER a string; id number for issue, pr, or ..."
  (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t))))
         (target (or target (consult--read  (list (cons "Issues" "issue") (cons "Pull Requests" "pr"))
                                        :prompt "What topic are you looking for? "
                                        :lookup #'consult--lookup-cdr
                                        :require-match t
                                        :sort nil)))
         (number (or number (pcase target
                              ("issue" (get-text-property 0 :number (consult-gh-issue-list repo t)))
                              ("pr" (get-text-property 0 :number (consult-gh-pr-list repo t))))))
         (comment (or comment (consult--read nil :prompt "Comment: "))))

    (if (string-empty-p comment)
        (progn
          (message "Comment cannot be empty!")
          nil)
      (pcase target
        ((or "issue" "pr")
          (consult-gh--api-command-string (format "repos/%s/issues/%s/comments" repo number) "-f" (format "body=%s" comment)))
        ("discussion"
         (message "Commenting on discussions is not supported, yet!")
         nil)))))

(defun consult-gh--search-code-format (string input highlight)
  "Format minibuffer candidates for code.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh search code ...”\).
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match' in the minibuffer."
  (let* ((class "code")
         (type "code")
         (parts (string-split string ":"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (path (format "%s" (cadr parts)))
         (url (format "repos/%s/contents/%s" repo path))
         (branch "HEAD")
         (path (concat "./" path))
         (code (mapcar (lambda (x) (replace-regexp-in-string "\t" "\s\s" (replace-regexp-in-string "\n" "\\n" (format "%s" x)))) (cdr (cdr parts))))
         (code (string-join code ":"))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\t%s\t%s"
                      (consult-gh--set-string-width (propertize code 'face  'consult-gh-code) 100)
                      (propertize path 'face 'consult-gh-url)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user ) "/" (propertize package 'face 'consult-gh-package)) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :code code :path path :url url :query query :class class :type type :branch branch) str)
    str))

(defun consult-gh--code-state ()
  "State function for code candidates.

This is passed as STATE to `consult--read' in `consult-gh-search-code'
and is used to preview or do other actions on the code."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (if cand
          (pcase action
            ('preview
             (if (and consult-gh-show-preview cand)
                 (let* ((repo (get-text-property 0 :repo cand))
                        (path (get-text-property 0 :path cand))
                        (branch (or (get-text-property 0 :branch cand) "HEAD"))
                        (code (get-text-property 0 :code cand))
                        (url (get-text-property 0 :url cand))
                        (tempdir (expand-file-name (concat repo "/" branch "/")
                                    (or consult-gh--current-tempdir (consult-gh--tempdir))))
                        (temp-file (or (cdr (assoc (substring-no-properties (concat repo "/" "path")) consult-gh--open-files-list)) (expand-file-name path tempdir)))
                        (_ (make-directory (file-name-directory temp-file) t))
                        (text (consult-gh--files-get-content url))
                        (_ (with-temp-file temp-file (insert text) (set-buffer-file-coding-system 'raw-text)))
                        (_ (progn
                             (unless (file-exists-p temp-file)
                               (make-directory (file-name-directory temp-file) t)
                               (with-temp-file temp-file
                                 (insert (consult-gh--files-get-content url))
                                 (set-buffer-file-coding-system 'raw-text)
                                 (write-file temp-file)))
                           (add-to-list 'consult-gh--open-files-list `(,(substring-no-properties (concat repo "/" path)) . ,temp-file))))
                        (buffer (or (find-file-noselect temp-file t) nil)))
                   (when buffer
                     (with-current-buffer buffer
                       (if consult-gh-highlight-matches
                           (highlight-regexp (string-trim code) 'consult-gh-preview-match))
                       (goto-char (point-min))
                       (search-forward code nil t)
                       (add-to-list 'consult-gh--preview-buffers-list buffer)
                       (funcall preview action
                                buffer)
                       (consult-gh-recenter 'middle))))))
            ('return
             cand))))))

(defun consult-gh--code-group (cand transform)
  "Group function for code candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-search-code'
and is used to group code results.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-code-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "Code " 98 nil ?-)
       (consult-gh--set-string-width " Path " 8 nil ?-)
       (consult-gh--set-string-width " > Repo " 40 nil ?-))))))

(defun consult-gh--code-browse-url-action (cand)
  "Browse the url for a code candidate, CAND.

This is an internal action function that gets a candidate, CAND,
from `consult-gh-search-code' and opens the url of the file
containing the code in an external browser.

To use this as the default action for code,
set `consult-gh-code-action' to `consult-gh--code-browse-url-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (path (substring-no-properties (get-text-property 0 :path cand)))
         (url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/HEAD/" path)))
    (funcall (or consult-gh-browse-url-func #'browse-url) url)))

(defun consult-gh--code-view-action (cand)
  "Open code candidate, CAND.

This is a wrapper function around `consult-gh--files-view'.
It parses CAND to extract relevant values
\(e.g. repository, file path, url, ...\)
and passes them to `consult-gh--files-view'.

To use this as the default action on code candidates,
set `consult-gh-code-action' to `consult-gh--code-view-action'."
  (let* ((repo (get-text-property 0 :repo cand))
         (branch (or (get-text-property 0 :branch cand) "HEAD"))
         (code (get-text-property 0 :code cand))
         (tempdir (expand-file-name (concat repo "/" branch "/")
                                    (or consult-gh--current-tempdir (consult-gh--tempdir))))
         (path (get-text-property 0 :path cand))
         (url (get-text-property 0 :url cand)))
    (consult-gh--files-view repo path url nil tempdir code branch)))

(defun consult-gh--dashboard-format (string)
  "Format minibuffer candidates for dashboard items.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh search code ...”\)."
  (let* ((class "dashboard")
         (query "")
         (parts (string-split string "\t"))
         (isPR (car parts))
         (type (if (equal isPR "true") "pr" "issue"))
         (repo (cadr parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (title (caddr parts))
         (number (cadddr parts))
         (state (upcase (cadddr (cdr parts))))
         (date (substring (cadddr (cddr parts)) 0 10))
         (tags (cadddr (cdddr parts)))
         (tags (and (stringp tags) (progn (string-match "\\[map\\[\\(.*\\)\\]" tags)
                                          (concat "[" (match-string 1 tags) "]"))))
         (commentscount (cadddr (cddr (cdddr parts))))
         (reason (cadddr (cddr (cddddr parts))))
         (reason-str (cond
                      ((string-prefix-p "Assigned to" reason) "assigned")
                      ((string-prefix-p "Authored by" reason) "authored")
                      ((string-prefix-p "Mentions " reason) "mentions")
                      ((string-prefix-p "Involves " reason) "involves")))
         (url (cadddr (cdddr (cdddr parts))))
         (face (pcase isPR
                 ("false"
                  (pcase state
                    ("CLOSED" 'consult-gh-success)
                    ("OPEN" 'consult-gh-warning)
                    (_ 'consult-gh-issue)))
                 ("true"
                  (pcase state
                    ("CLOSED" 'consult-gh-error)
                    ("MERGED" 'consult-gh-success)
                    ("OPEN" 'consult-gh-warning)
                    (_ 'consult-gh-pr)))
                 (_ 'consult-gh-issue)))
         (str (concat (consult-gh--set-string-width
                       (concat (propertize (format "%s" user) 'face 'consult-gh-user)
                               "/"
                               (propertize (format "%s" package) 'face 'consult-gh-package)

                               (propertize (format " - %s #%s: " (upcase (substring type 0 2)) number) 'face face)
                               (propertize (format "%s" title) 'face 'consult-gh-default)) 80)
                      (when reason-str (concat "\s\s" (propertize (consult-gh--set-string-width reason-str 8) 'face 'consult-gh-visibility)))
                      (when date (concat "\s\s" (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date)))
                      (when state (concat "\s\s" (propertize (consult-gh--set-string-width state 6) 'face face)))
                      (when commentscount (concat "\s\s" (propertize (consult-gh--set-string-width commentscount 5) 'face ' consult-gh-visibility)))
                      (when tags (concat "\s\s" (propertize tags 'face 'consult-gh-tags))))))
    (add-text-properties 0 1 (list :repo repo
                                   :user user
                                   :package package
                                   :number number
                                   :comm commentscount
                                   :state state
                                   :title title
                                   :tags tags
                                   :date date
                                   :query query
                                   :type type
                                   :url url
                                   :reason reason
                                   :class class) str)
    str))

(defun consult-gh--dashboard-state ()
  "State function for dashboard candidates.

This is passed as STATE to `consult--read' in `consult-gh-dashboard'
and is used to preview or do other actions on the code."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if (and consult-gh-show-preview cand)
             (when-let ((repo (get-text-property 0 :repo cand))
                        (type (get-text-property 0 :type cand))
                        (number (get-text-property 0 :number cand))
                        (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (pcase type
                 ("issue"
                  (consult-gh--issue-view (format "%s" repo) (format "%s" number) buffer)
                  (funcall preview action buffer))
                 ("pr"
                  (consult-gh--pr-view (format "%s" repo) (format "%s" number) buffer)
                  (funcall preview action buffer))
                 (_ (message "Preview is not supoorted for items that are not issues or pull requests!"))))))
        ('return
         cand)))))

(defun consult-gh--dashboard-group (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group items in the dashboard.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-dashboard-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "Repo - Type Number: Title " 78 nil ?-)
       (consult-gh--set-string-width " Reason " 10 nil ?-)
       (consult-gh--set-string-width " Date " 12 nil ?-)
       (consult-gh--set-string-width " State " 8 nil ?-)
       (consult-gh--set-string-width " Comm " 7 nil ?-)
       " Tags ")))))

(defun consult-gh--dashboard-action (cand)
  "View dashboard item, CAND.

This is an internal action function that gets a dashboard candidate, CAND,
from `consult-gh-dashboard' and passes it to default actions for issues or
prs, discussions, etc.

To use this as the default action for issues,
set `consult-gh-dashboard-action' to `consult-gh--dashboard-action'."

  (let* ((type (get-text-property 0 :type cand))
         (url (get-text-property 0 :url cand)))
    (cond
     ((equal type "issue")
      (funcall consult-gh-issue-action cand))
     ((equal type "pr")
      (funcall consult-gh-pr-action cand))
     (url
      (funcall (or consult-gh-browse-url-func #'browse-url) url))
     (t
      (message "cannot open that with `consult-gh--dashboard-action'!")))))

(defun consult-gh--dashboard-browse-url-action (cand)
  "Browse the url for a dashboard candidate, CAND.

This is an internal action function that gets a dashboard candidate, CAND,
from `consult-gh-dashboard' and opens the url of the issue/pr
in an external browser.

To use this as the default action for issues,
set `consult-gh-dashboard-action' to `consult-gh--dashboard-browse-url-action'."
  (let* ((url (substring-no-properties (get-text-property 0  :type cand))))
    (if url (funcall (or consult-gh-browse-url-func #'browse-url) url))))

(defun consult-gh-notifications-make-args ()
  "Make cmd arguments for notifications."
  (list "api" (concat "notifications?sort=updated" (if consult-gh-notifications-show-unread-only "" "&all=true")) "--paginate" "--template" (concat "{{range .}}" "{{.id}}" "\t" "{{.subject.type}}" "\t" "{{.repository.full_name}}" "\t" "{{.subject.title}}" "\t" "{{.reason}}" "\t" "{{.unread}}" "\t" "{{.updated_at}}" "\t" "{{(timeago .updated_at)}}" "\t" "{{.subject.url}}""\n" "{{end}}")))

(defun consult-gh--notifications-format (string)
  "Format minibuffer candidates for notifications.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh search code ...”\)."
  (let* ((class "notification")
         (query "")
         (parts (string-split string "\t"))
         (thread (car parts))
         (type (cadr parts))
         (type (and (stringp type) (cond
                                    ((equal (downcase type) "pullrequest") "pr")
                                    ((equal (downcase type) "issue") "issue")
                                    (t (downcase type)))))
         (repo (caddr parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (title (cadddr parts))
         (reason (cadddr (cdr parts)))
         (unread (cadddr (cddr parts)))
         (face (pcase unread
                 ("true" 'consult-gh-default)
                 ("false" 'consult-gh-tags)
                 (_ 'consult-gh-default)))
         (state (pcase unread
                  ("true" "Unread")
                  ("false" "Seen")
                  (_ "Unknown")))
         (date (substring (cadddr (cdddr parts)) 0 10))
         (reltime (cadddr (cdr (cdddr parts))))
         (url (cadddr (cddr (cdddr parts))))
         (url-parts (and (stringp url) (split-string url "/" t)))
         (number (and url-parts
                      (or
                       (cadr (member "issues" url-parts))
                       (cadr (member "pulls" url-parts)))))
         (title-str (concat (propertize (format "%s" repo) 'face 'consult-gh-repo)
                            (propertize " - " 'face face)
                            (propertize (concat type (if number " #") number) 'face face)
                            (propertize ": " 'face face)
                            (propertize (format "%s" title) 'face face)))
         (_ (if (equal unread "false") (add-face-text-property 0 (length title-str) '(:strike-through t) t title-str)))
         (str (format "%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width title-str 80)
                      (propertize (consult-gh--set-string-width reason 13) 'face 'consult-gh-visibility)
                      (consult-gh--set-string-width (propertize state 'face face) 7)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date))))
    (add-text-properties 0 1 (list :thread thread
                                   :repo repo
                                   :user user
                                   :package package
                                   :number number
                                   :reason reason
                                   :state state
                                   :title title
                                   :date date
                                   :reltime reltime
                                   :query query
                                   :type type
                                   :url url
                                   :reason reason
                                   :class class)
                         str)

    str))

(defun consult-gh--notifications-state ()
  "State function for code candidates.

This is passed as STATE to `consult--read' in `consult-gh-notifications'
and is used to preview or do other actions on the code."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if (and consult-gh-show-preview cand)
             (let* ((repo (get-text-property 0 :repo cand))
                    (type (get-text-property 0 :type cand))
                    (number (get-text-property 0 :number cand))
                    (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (pcase type
                 ("issue" (consult-gh--issue-view (format "%s" repo) (format "%s" number) buffer))
                 ("pr" (consult-gh--pr-view (format "%s" repo) (format "%s" number) buffer))
                 (_ (message "Preview not supported for %s items" type)))
               (when (member type '("issue" "pr")) (funcall preview action
                                                            buffer)))))
        ('return
         cand)))))

(defun consult-gh--notifications-group (cand transform)
  "Group function for notifications candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notifications'
and is used to group items in the natofications.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (consult-gh--group-function cand transform consult-gh-group-notifications-by)))
    (cond
     ((stringp name) name)
     ((equal name t)
      (concat
       (consult-gh--set-string-width "Repo - Type Number: Title " 78 nil ?-)
       (consult-gh--set-string-width " Reason " 15 nil ?-)
       (consult-gh--set-string-width " State " 9 nil ?-)
       (consult-gh--set-string-width " Date " 11 nil ?-))))))

(defun consult-gh--discussion-browse-url (repo title &optional date)
  "Browse the url for a discussion in REPO with TITLE.

Optional argument DATE is latest updated date of discussion."

  (let* ((filter (concat (format "filter=%s in:title repo:%s" title repo)
                         (when date (format " updated:>=%s" date))))
         (query "query=query($filter: String!) {search(query: $filter, type: DISCUSSION, first: 1) { nodes { ... on Discussion { number }}}}")
         (id (consult-gh--command-to-string "api" "graphql" "--paginate" "--cache" "24h" "-F" filter "-f" query "--template" "{{(index .data.search.nodes 0).number}}"))
         (url (concat "https://" (consult-gh--auth-account-host) (format "/%s/discussions/%s" repo id))))
    (when url (funcall (or consult-gh-browse-url-func #'browse-url) url))))

(defun consult-gh--discussion-browse-url-action (cand)
  "Open the discussion of CAND thread in the browser CAND.

This is a wrapper function around `consult-gh--discussion-browse-url'.
It parses CAND to extract relevant values \(e.g. repository's name and
discussion title\) and passes them to `consult-gh--discussion-browse-url'.

To use this as the default action for discussions,
set `consult-gh-discussion-action' to
`consult-gh--discussion-browse-url-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (title (substring-no-properties (format "%s" (get-text-property 0 :title cand))))
         (date (substring-no-properties (get-text-property 0 :date cand))))
    (consult-gh--discussion-browse-url repo title date)))

(defun consult-gh--notifications-action (cand)
  "View notification item, CAND.

This is an internal action function that gets a notification candidate, CAND,
from `consult-gh-notifications' and passes it to default actions for issues or
prs, discussions, etc.

To use this as the default action for issues,
set `consult-gh-notifications-action' to `consult-gh--notifications-action'."

  (let* ((repo (get-text-property 0 :repo cand))
         (type (get-text-property 0 :type cand))
         (url (concat "https://" (consult-gh--auth-account-host) (format "/notifications?query=repo:%s" repo))))
    (pcase type
      ("issue"
       (funcall consult-gh-issue-action cand))
      ((or "pr" "pullrequest")
       (funcall consult-gh-pr-action cand))
      ("discussion"
       (funcall consult-gh-discussion-action cand))
      (_
       (and url (funcall (or consult-gh-browse-url-func #'browse-url) url))))
    t))

(defun consult-gh--notifications-browse-url-action (cand)
  "Browse the url for a notification candidate, CAND.

This is an internal action function that gets a notification candidate,
CAND, from `consult-gh-notifications' and opens the url with relevant
notifications in an external browser.

To use this as the default action for issues,
set `consult-gh-notificatios-action' to
`consult-gh--notifications-browse-url-action'."
  (if-let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
            (url (concat "https://" (consult-gh--auth-account-host) (format "/notifications?query=repo:%s" repo))))
      (funcall (or consult-gh-browse-url-func #'browse-url) url)
    (message "Cannot find the right url to open!")))

(defun consult-gh--notifications-mark-as-read (cand)
  "Mark CAND as read.

This is an internal action function that gets a notification candidate, CAND,
from `consult-gh-notifications' and makrs it as read."
  (when-let ((thread (get-text-property 0 :thread cand)))
    (when (consult-gh--command-to-string "api" (format "notifications/threads/%s" thread) "--silent" "--method" "PATCH" "--paginate")
      (message "marked as read!"))))

(defun consult-gh--notifications-unsubscribe (cand)
  "Unsubscribe from the thread of CAND.

This is an internal action function that gets a notification candidate, CAND,
from `consult-gh-notifications' and unsubscribes from the thread."
  (when-let ((thread (get-text-property 0 :thread cand)))
    (when (consult-gh--command-to-string "api" "--method" "PUT" "-H" "Accept: application/vnd.github+json" "--paginate" "-F" "ignored=true"(format "/notifications/threads/%s/subscription" thread))
      (message "Unsubscribed!"))))

(defun consult-gh--notifications-subscribe (cand)
  "Unsubscribe from the thread of CAND.

This is an internal action function that gets a notification candidate, CAND,
from `consult-gh-notifications' and unsubscribes from the thread."
  (when-let ((thread (get-text-property 0 :thread cand)))
    (when (consult-gh--command-to-string "api" "--method" "PUT" "-H" "Accept: application/vnd.github+json" "--paginate" "-F" "ignored=false" (format "/notifications/threads/%s/subscription" thread))
      (message "Subscribed!"))))

(defvar-keymap consult-gh-repo-view-mode-map
  :doc "Keymap for `consult-gh-repo-view-mode'.")

;;;###autoload
(define-minor-mode consult-gh-repo-view-mode
  "Minor-mode for viewing GitHub repositories."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-repo-view"
  :keymap consult-gh-repo-view-mode-map
  (read-only-mode +1))

(defvar-keymap consult-gh-issue-view-mode-map
  :doc "Keymap for `consult-gh-issue-view-mode'.")

;;;###autoload
(define-minor-mode consult-gh-issue-view-mode
  "Minor-mode for viewing GitHub issues."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-issue-view"
  :keymap consult-gh-issue-view-mode-map
  (read-only-mode +1))

(defvar-keymap consult-gh-pr-view-mode-map
  :doc "Keymap for `consult-gh-pr-view-mode'.")

;;;###autoload
(define-minor-mode consult-gh-pr-view-mode
  "Minor-mode for viewing GitHub pull request."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-pr-view"
  :keymap consult-gh-pr-view-mode-map
  (read-only-mode +1))

(defun consult-gh-topics-edit-header-line ()
  "Create `header-line-format' for `consult-gh-topics-edit-mode'."
  (let* ((topic consult-gh--topic)
         (repo (get-text-property 0 :repo topic))
         (type (get-text-property 0 :type topic))
         (number (get-text-property 0 :number topic))
         (new (get-text-property 0 :new topic))
         (isComment (get-text-property 0 :isComment topic))
         (cand (concat repo (and number (concat ":#" number)))))
    (add-text-properties 0 1 (list :repo repo :number number) cand)
    (list
     (concat (and new "New ") (if isComment
                                  (concat "comment on ")
                                (concat type " for "))
             (buttonize (concat (consult-gh--get-package repo) (and isComment type (concat ": " (upcase type))) (and number (concat "#" number)))
                        `(lambda (&rest _)
                           (if ,new
                               (funcall consult-gh-repo-action ,cand)
                             (pcase ,type
                               ("issue"
                                (funcall consult-gh-issue-action ,cand))
                               ("pr"
                                (funcall consult-gh-pr-action ,cand)))))))
     ".  "
     (substitute-command-keys "When done, use `\\[consult-gh-topics-submit]' to submit or `\\[consult-gh-topics-cancel]' to cancel."))))

(defvar-keymap consult-gh-topics-edit-mode-map
  :doc "Keymap for `consult-gh-topics-edit-mode'.")

(defun consult-gh-topics-edit-mode-on ()
  "Enable `consult-gh-topics-edit-mode'."
  (setq-local header-line-format (consult-gh-topics-edit-header-line)))

(defun consult-gh-topics-edit-mode-off ()
  "Disable `consult-gh-topics-edit-mode'."
  (setq-local header-line-format nil))

;;;###autoload
(define-minor-mode consult-gh-topics-edit-mode
  "Minor-mode for editable consult-gh topics."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-topics-edit"
  :keymap consult-gh-topics-edit-mode-map
  (cond (consult-gh-topics-edit-mode
         (consult-gh-topics-edit-mode-on)
         (when consult-gh-topic-use-capf
           (consult-gh-topics-edit-capf-mode +1)))
        (t
         (consult-gh-topics-edit-mode-off)
         (if consult-gh-topics-edit-capf-mode
             (consult-gh-topics-edit-capf-mode -1)))))

(defun consult-gh-topics-edit-capf-mode-on ()
  "Enable `consult-gh-topics-edit-capf-mode'."
  (add-hook 'completion-at-point-functions #'consult-gh--topics-edit-capf -100 t)
  (add-to-list 'completion-at-point-functions #'consult-gh--topics-edit-capf))

(defun consult-gh-topics-edit-capf-mode-off ()
  "Disable `consult-gh-topics-edit-capf-mode'."
  (remove-hook 'completion-at-point-functions #'consult-gh--topics-edit-capf)
  (remove #'consult-gh--topics-edit-capf completion-at-point-functions))

;;;###autoload
(define-minor-mode consult-gh-topics-edit-capf-mode
  "Minor-mode for completion at point in `consult-gh-topics-edit-comment-mode'.

Helps with autocompleting usernames, issue numbers, etc."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-topics-edit-capf"
  :keymap nil
  (cond (consult-gh-topics-edit-capf-mode
         (consult-gh-topics-edit-capf-mode-on))
        (t
         (consult-gh-topics-edit-capf-mode-off))))

;;;###autoload
(defun consult-gh-auth-switch (&optional host user prompt)
  "Switch between authenticated accounts.

If the optional arguments, HOST and USER are non-nil, use them for
authenticaiton otherwise query the user to select an account.
If PROMPT is non-nil, use it as the query prompt."
  (interactive "P")
  (unless (and host user)
    (let* ((prompt (or prompt "Select Account:"))
           (accounts (consult-gh--auth-accounts))
           (sel (consult--read accounts
                               :prompt prompt
                               :lookup #'consult--lookup-cons
                               :sort nil
                               :annotate (lambda (cand)
                                           (let* ((info (assoc cand accounts))
                                                  (host (cadr info))
                                                  (status (if (caddr info) "active" ""))
                                                  (current (if (equal info consult-gh--auth-current-account) "selected" "")))
                                             (format "\t\t%s\s\s%s\s\s%s"
                                                     (propertize host 'face 'consult-gh-tags)
                                                     (propertize status 'face 'consult-gh-user)
                                                     (propertize current 'face 'consult-gh-visibility)))))))
      (when (and sel (consp sel))
        (setq user (car sel))
        (setq host (cadr sel)))))
  (consult-gh--auth-switch host user))

(defun consult-gh--repo-list-transform (input)
  "Add annotation to repo candidates in `consult-gh-repo-list'.

Format each candidates with `consult-gh--repo-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--repo-format cand input nil))))

(defun consult-gh--repo-list-builder (input)
  "Build gh command line for listing repos of INPUT.

INPUT must be a GitHub user or org as a string e.g. “armindarvish”."

  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-repo-list-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-repo-maxnum)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--repo-list (org)
  "List repos of ORG synchronously.

This runs the command  “gh repo list ORG”
using `consult-gh--command-to-string' to get a list of all repositories
of ORG, and returns the results in a list.

Each candidate is formatted by `consult-gh--repo-format'.

ORG must be the name of a github account as a string e.g. “armindarvish”."
  (let* ((maxnum (format "%s" consult-gh-repo-maxnum))
         (repolist  (or (consult-gh--command-to-string "repo" "list" org "--limit" maxnum) ""))
         (repos (split-string repolist "\n")))
    (mapcar (lambda (src) (consult-gh--repo-format src org nil))  (remove "" repos))))

(defun consult-gh--async-repo-list (prompt builder &optional initial)
  "List repos of GitHub users/organizations asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-repo-list'.

It runs the command `consult-gh--repo-list-builder' in an async process
and returns the results \(list of repos of a user\) as a completion table
that will be passed to `consult--read'.  The completion table gets
dynamically updated as the user types in the minibuffer.
Each candidate in the minibuffer is formatted by
`consult-gh--repo-list-transform' to add annotation and other info to
the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input
          \\(passed as INITITAL to `consult--read'\)"
  (let* ((current-repo (consult-gh--get-repo-from-directory))
         (initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't) (consult-gh--get-username current-repo)))))
    (consult-gh-with-host (consult-gh--auth-account-host)
                          (consult--read (consult--process-collection builder
                                           :transform (consult--async-transform-by-input #'consult-gh--repo-list-transform))
                                         :prompt prompt
                                         :lookup #'consult--lookup-member
                                         :state (funcall #'consult-gh--repo-state)
                                         :initial initial
                                         :group #'consult-gh--repo-group
                                         :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                                                               (append (list
                                                               (when current-repo
                                                                 (consult-gh--get-username current-repo))
                                                               (thing-at-point 'symbol))
                                                              consult-gh--known-orgs-list))
                                         :history '(:input consult-gh--orgs-history)
                                         :require-match t
                                         :category 'consult-gh-repos
                                         :preview-key consult-gh-preview-key
                                         :sort nil))))

;;;###autoload
(defun consult-gh-repo-list (&optional initial noaction prompt)
  "Interactive command to list repos of users/organizations asynchronously.

This is an interactive wrapper function around `consult-gh--async-repo-list'.
It queries the user to enter the name of a GitHub organization/username
in the minibuffer, then fetches a list of repositories for the entered
username and present them as a minibuffer completion table for selection.
The list of candidates in the completion table are dynamically updated
as the user changes the entry.

Upon selection of a candidate:
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-repo-action'.

Additional command line arguments can be passed in the minibuffer entry
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
armindarvish -- -L 100
the async process will run the command “gh repo list armindarvish -L 100”,
which sets the limit for the maximum number of results to 100.

User selection is tracked in `consult-gh--known-orgs-list' for quick access
in the future \(added to future history list\) in future calls.

INITIAL is an optional arg for the initial input in the minibuffer.
\(passed as INITITAL to `consult-gh--async-repo-list'\).

When PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities,
see `consult-grep' and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if (xor current-prefix-arg consult-gh-use-search-to-find-name)
      (setq initial (or initial (consult-gh--get-username (substring-no-properties (get-text-property 0 :repo  (consult-gh-search-repos initial t)))))))
  (let* ((prompt (or prompt "Enter Org Name:  "))
         (sel (consult-gh--async-repo-list prompt #'consult-gh--repo-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (funcall consult-gh-repo-action sel))))

(defun consult-gh--search-repos-transform (input)
  "Add annotation to repo candidates in `consult-gh-search-repos'.

Format each candidates with `consult-gh--repo-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--repo-format cand input t))))

(defun consult-gh--search-repos-builder (input)
  "Build gh command line for searching repositories with INPUT query.

The command arguments such as \(e.g. “gh search repos INPUT”\)."
  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-search-repos-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-repo-maxnum)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--async-search-repos (prompt builder &optional initial)
  "Seacrhes GitHub repositories asynchronously.

This is a non-interactive internal function.  For the interactive version
see `consult-gh-search-repos'.

It runs the command line from `consult-gh--search-repos-builder' in an
async process and returns the results \(list of search results for the
minibuffer input\) as a completion table in minibuffer that will be passed
to `consult--read'.  The completion table gets dynamically updated as the
user types in the minibuffer.  Each candidate in the minibuffer is
formatted by `consult-gh--search-repos-transform' to add annotation and
other info to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'
  INITIAL an optional arg for the initial input in the minibuffer \(passed
          as INITITAL to `consult--read'\)."
  (let* ((initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't) (consult-gh--get-repo-from-directory) nil))))
    (consult-gh-with-host (consult-gh--auth-account-host)
                          (consult--read
                           (consult--process-collection builder
                             :transform (consult--async-transform-by-input #'consult-gh--search-repos-transform))
                           :prompt prompt
                           :lookup #'consult--lookup-member
                           :state (funcall #'consult-gh--repo-state)
                           :initial initial
                           :group #'consult-gh--repo-group
                           :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                                                 (append (list (consult-gh--get-repo-from-directory)
                                                      (consult-gh--get-repo-from-topic)
                                                      (thing-at-point 'symbol))
                                                consult-gh--known-orgs-list))
                           :history '(:input consult-gh--search-repos-history)
                           :require-match t
                           :category 'consult-gh-repos
                           :preview-key consult-gh-preview-key
                           :sort nil))))

;;;###autoload
(defun consult-gh-search-repos (&optional initial noaction prompt)
  "Interactively search GitHub repositories.

This is an interactive wrapper function around
`consult-gh--async-search-repos'.  It queries the user to enter the name of
a GitHub organization/username in the minibuffer, then fetches a list
of repositories for the entered username.  The list of candidates in the
completion table are dynamically updated as the user changes the input.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-repo-action'

Additional commandline arguments can be passed in the minibuffer input
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
consult-gh -- -L 100
and the async process will run “gh search repos -L 100”,
which sets the limit for the maximum number of results to 100.

User selection is tracked in `consult-gh--known-orgs-list' for quick access
in the future \(added to future history list\) in future calls.

INITIAL is an optional arg for the initial input in the minibuffer.
\(passed as INITITAL to `consult-gh--async-repo-list'\).

When PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities,
see `consult-grep' and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (let* ((prompt (or prompt "Search Repos:  "))
         (host (consult-gh--auth-account-host))
         (sel (if (stringp host)
                  (with-environment-variables
                      (("GH_HOST" (or host consult-gh-default-host)))
                    (consult-gh--async-search-repos prompt #'consult-gh--search-repos-builder initial))
                (consult-gh--async-search-repos prompt #'consult-gh--search-repos-builder initial))))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (progn
        (funcall consult-gh-repo-action sel)
        sel))))

(defun consult-gh-orgs (&optional orgs noaction prompt)
  "List repositories of ORGS.

This is a wrapper function around `consult-gh--repo-list'.
If ORGS is nil, this simply calls `consult-gh--repo-list'.
If ORGS is a list, then it runs `consult-gh--repo-list' on every member
of ORGS and returns the results \(repositories of all ORGS\).

If NOACTION is non-nil, return the candidate without running action.
If PROMPT is non-nil, use it as the query prompt."
  (if (not orgs)
      (consult-gh-repo-list nil noaction))
  (let* ((prompt (or prompt "Select Repo: "))
         (candidates (consult--slow-operation "Collecting Repos ..." (apply #'append (mapcar (lambda (org) (consult-gh--repo-list org)) orgs))))
         (sel (consult-gh-with-host (consult-gh--auth-account-host)
                                    (consult--read candidates
                                                   :prompt prompt
                                                   :lookup #'consult--lookup-member
                                                   :state (funcall #'consult-gh--repo-state)
                                                   :group #'consult-gh--repo-group
                                                   :add-history (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                                                                         (append (list (consult-gh--get-repo-from-directory)
(consult-gh--get-repo-from-topic)
(thing-at-point 'symbol))
                                                                        consult-gh--known-repos-list))
                                                   :history 'consult-gh--repos-history
                                                   :require-match t
                                                   :category 'consult-gh-repos
                                                   :preview-key consult-gh-preview-key
                                                   :sort t))))
    (if noaction
        sel
      (funcall consult-gh-repo-action sel))))

;;;###autoload
(defun consult-gh-user-repos (&optional user)
  "List all repos for USER.

This includes repos of orgs of USER.  It uses
`consult-gh--get-current-user-orgs' to get all
orgs of USER."
  (interactive)
  (if user
      (consult-gh-orgs (consult-gh--get-current-user-orgs user t))
    (consult-gh-orgs (or consult-gh--current-user-orgs (consult-gh--get-current-user-orgs nil t)))))

;;;###autoload
(defun consult-gh-favorite-repos ()
  "List repositories of orgs in `consult-gh-favorite-orgs-list'.

Passes `consult-gh-favorite-orgs-list' to `consult-gh-orgs',
a useful command for quickly fetching a list of personal GitHub Repositories
or any other favorite accounts whose repositories are frequently visited."
  (interactive)
  (consult-gh-orgs consult-gh-favorite-orgs-list))

(define-obsolete-function-alias 'consult-gh-default-repos #'consult-gh-favorite-repos "2.0")

;;;###autoload
(defun consult-gh-repo-fork (&optional repos)
  "Interactively fork REPOS.

It runs the command “gh fork repo ...” to fork a repository
using the internal function `consult-gh--repo-fork'

If REPOS not supplied, interactively asks user to pick REPOS."
  (interactive)
  (let* ((repos (or repos (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos nil t))))))
    (if (stringp repos)
        (setq repos (list repos)))
    (mapcar (lambda (repo)
              (let* ((package (consult-gh--get-package repo))
                     (name (if consult-gh-confirm-name-before-fork (read-string (concat "name for " (propertize (format "%s: " repo) 'face 'consult-gh-repo)) package) package)))
                (consult-gh-with-host (consult-gh--auth-account-host) (consult-gh--repo-fork repo name))))
            repos)))

;;;###autoload
(defun consult-gh-repo-clone (&optional repos targetdir)
  "Interactively clone REPOS to TARGETDIR.

It runs the command “gh clone repo ...” to fork a repository
using the internal function `consult-gh--repo-clone'.

If REPOS or TARGETDIR are not supplied, interactively asks user
to pick them."
  (interactive)
  (let* ((repos (or repos (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos nil t)))))
         (targetdir (or targetdir consult-gh-default-clone-directory default-directory))
         (clonedir (if consult-gh-confirm-before-clone (read-directory-name "Select Target Directory: " (file-name-as-directory targetdir)) targetdir)))
    (if (stringp repos)
        (setq repos (list repos)))
    (mapcar (lambda (repo)
              (let* ((package (consult-gh--get-package repo))
                     (name (if consult-gh-confirm-before-clone (read-string (concat "name for " (propertize (format "%s: " repo) 'face 'consult-gh-repo)) package) package)))
                (consult-gh-with-host (consult-gh--auth-account-host)
                                      (consult-gh--repo-clone repo name clonedir))))
            repos)))

(defun consult-gh-repo-create (&optional name local-path owner description visibility make-readme gitignore-template license-key template)
  "Create a new repo with NAME and metadata on GitHub.

This mimicks the same interactive repo creation
from “gh repo create” in the command line.

Description of Arguments:

 NAME               a string; name of repository
 LOCAL-PATH         a string; path to local directory of git repository
 OWNER              a string; user/organization owning the repo
 DESCRIPTION        a string; description for the repo
 VISIBILITY         a string; private|public|internal
 MAKE-README        a boolean; whether to make a readme file or not
 GITIGNORE-TEMPLATE a string; name of gitignore template
 LICENSE-KEY        a string; key for license template
 TEMPLATE           a string; Full name of template repo \(e.g. user/repo\)

For more details, refer to the manual of “gh repo create” and see the
backend functions, `consult-gh--repo-create-scratch',
`consult-gh--repo-create-template', or
`consult-gh--repo-create-push-existing'."
  (interactive "P")
  (let ((answer (consult--read (list (cons "Create a new repository on GitHub from scratch" :scratch)
                                     (cons "Create a new repository on GitHub from a template repository" :template)
                                     (cons "Push an existing local repository to GitHub" :existing))
                               :prompt "What would you like to do?"
                               :lookup #'consult--lookup-cdr
                               :sort nil)))
    (pcase answer
      (':scratch (consult-gh--repo-create-scratch name local-path owner description visibility make-readme gitignore-template license-key))
      (':template (consult-gh--repo-create-template name owner description visibility template))
      (':existing (consult-gh--repo-create-push-existing name local-path owner description visibility)))))

(defun consult-gh--issue-list-transform (input)
"Add annotation to issue candidates in `consult-gh-issue-list'.

Format each candidates with `consult-gh--issue-list-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--issue-list-format cand input nil))))

(defun consult-gh--issue-list-builder (input)
  "Build gh command line for listing issues the INPUT repository.

INPUT must be the full name of a GitHub repository as a string
e.g. “armindarvish/consult-gh”."
  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-issue-list-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-issue-maxnum)))))
    (unless (or (member "-s" flags) (member "--state" flags))
      (setq opts (append opts (list "--state" (format "%s" consult-gh-issues-state-to-show)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--async-issue-list (prompt builder &optional initial)
  "List issues GitHub repos asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-issue-list'.

This runs the command line from `consult-gh--repo-list-builder'
in an async process and returns the results \(list of issues
for a repository\) as a completion table in minibuffer.  The completion
table gets dynamically updated as the user types in the minibuffer to
change the entry.
Each candidate in the minibuffer is formatted by
`consult-gh--issue-list-transform' to add annotation and other info
to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input in the minibuffer
          \(passed as INITITAL to `consult--read'\)"
  (let* ((initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't)
                          (consult-gh--get-repo-from-directory)
                        nil))))
    (consult-gh-with-host (consult-gh--auth-account-host)
        (consult--read
         (consult--process-collection builder
           :transform (consult--async-transform-by-input #'consult-gh--issue-list-transform))
         :prompt prompt
         :lookup #'consult--lookup-member
         :state (funcall #'consult-gh--issue-state)
         :initial initial
         :group #'consult-gh--issue-group
         :require-match t
         :category 'consult-gh-issues
         :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                               (append (list (consult-gh--get-repo-from-directory)
                                    (consult-gh--get-repo-from-topic)
                                    (thing-at-point 'symbol))
                              consult-gh--known-repos-list))
         :history '(:input consult-gh--repos-history)
         :preview-key consult-gh-preview-key
         :sort nil))))

;;;###autoload
(defun consult-gh-issue-list (&optional initial noaction prompt)
  "Interactively list issues of a GitHub repository.

This is an interactive wrapper function around `consult-gh--async-issue-list'.
With prefix ARG, first search for a repo using `consult-gh-search-repos',
then list issues of that selected repo with `consult-gh--async-issue-list'.

It queries the user to enter the full name of a GitHub repository in the
minibuffer \(expected format is “OWNER/REPO”\), then fetches the list of
issues of that repository and present them as a minibuffer completion
table for selection.  The list of candidates in the completion table are
dynamically updated as the user changes the minibuffer input.

Upon selection of a candidate either
 - if NOACTION is non-nil candidate is returned.
 - if NOACTION is nil     candidate is passed to `consult-gh-issue-action'.

Additional command line arguments can be passed in the minibuffer input
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
armindarvish/consult-gh -- -L 100
and the async process will run
“gh issue list --repo armindarvish/consult-gh -L 100”, which sets the limit
for the maximum number of results to 100.

User selection is tracked in `consult-gh--known-repos-list' for quick
access in the future \(added to future history list\) in future calls.

INITIAL is an optional arg for the initial input in the minibuffer.
\(passed as INITITAL to `consult-gh--async-issue-list'\).

If PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'"
  (interactive)
  (if (xor current-prefix-arg consult-gh-use-search-to-find-name)
      (setq initial (or initial (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos initial t))))))
  (let* ((prompt (or prompt "Enter Repo Name:  "))
        (sel (consult-gh--async-issue-list prompt #'consult-gh--issue-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (funcall consult-gh-issue-action sel))))

;;;###autoload
(defun consult-gh-issue-create (&optional repo title body)
  "Create a new issue with TITLE and BODY for REPO.

This mimicks the same interactive issue creation from “gh issue create” in
the terminal.
For more details refer to the manual with “gh issue create --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t))))
          (issueEnabled (gethash :hasIssuesEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasIssuesEnabled"))))
          (_ (unless (eq issueEnabled 't)
               (error "Issue is not enabled for the repo %s" repo)))
          (canAdmin (consult-gh--user-canadmin repo))
          (author (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
          (templates (consult-gh--get-issue-templates repo))
          (template (and templates (consult--read templates
                                                  :prompt "Select a template: "
                                                  :require-match nil
                                                  :lookup #'consult--lookup-cdr
                                                  :sort t)))
          (title (or title (and template (plistp template) (plist-get template :title))))
          (title (and title (stringp title) (not (string-empty-p title)) title))
          (body (or body (and template (plistp template) (plist-get template :body))))
          (body (and body (stringp body) (not (string-empty-p body)) body))
          (buffer (format "*consult-gh-issue-create: %s" repo))
          (topic (or repo "new issue"))
          (type "issue"))

     ;; collect issues of repo for completion at point
     (consult-gh--completion-set-issues topic repo)

     ;; collect prs of repo for completion at point
     (consult-gh--completion-set-prs topic repo)

     ;; collect mentionable users for completion at point
     (consult-gh--completion-set-mentionable-users topic repo)

     (if canAdmin
         (progn
           ;; collect labels for completion at point
           (consult-gh--completion-set-valid-labels topic repo)
           ;; collect valid assignees for completion at point
           (consult-gh--completion-set-assignable-users topic repo)
           ;; collect valid milestones for completion at point
           (consult-gh--completion-set-valid-milestones topic repo)
           ;; collect valid projects for completion at point
           (consult-gh--completion-set-valid-projects topic repo))

       (add-text-properties 0 1 (list :valid-labels nil :assignable-users nil :valid-milestones nil :valid-projects nil) topic))

     (add-text-properties 0 1 (list :number nil :type type :isComment nil :new t :repo (substring-no-properties repo) :author author) topic)


     (with-current-buffer (consult-gh-topics--get-buffer-create buffer "issue" topic)
       (unless (not (= (buffer-size) 0))
         (pcase-let* ((inhibit-read-only t)
                      (`(,title-marker ,header-marker) (consult-gh-topics--markers-for-metadata)))

           (insert (consult-gh-topics--format-field-header-string (concat title-marker "title: ")))
           (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
           (when title (insert title))

           (save-excursion
             (insert "\n")
             (let* ((beg (point))
                    (end nil))
               (when canAdmin
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "assignees: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "labels: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "milestone: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "projects: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n"))
               (insert (consult-gh-topics--format-field-header-string "---\n"))
               (setq end (point))
               (overlay-put (make-overlay beg end) :consult-gh-header t)
               (insert "\n"))

             (and body
                  (pcase major-mode
                    ('gfm-mode (insert body))
                    ('markdown-mode (insert body))
                    ('org-mode (insert (with-temp-buffer
                                         (insert body)
                                         (consult-gh--markdown-to-org)                                        (consult-gh--whole-buffer-string))))
                    ('text-mode (insert body))
                    (_ (insert body)))))
           (cursor-intangible-mode +1))))
     (funcall consult-gh-pop-to-buffer-func buffer))))

;;;###autoload
(defun consult-gh-issue-edit (&optional issue)
  "Edit the ISSUE.

ISSUE must be a propertized text describing an issue similar to one
returned by `consult-gh-issue-list'.

This mimicks the same interactive issue creation from “gh issue edit” in
the terminal.
For more details refer to the manual with “gh issue edit --help”."
  (interactive "P")

  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (if (not consult-gh-issue-view-mode)
       (let* ((repo (or (and issue (get-text-property 0 :repo issue))
                        (get-text-property 0 :repo (consult-gh-search-repos nil t))))
              (canAdmin (consult-gh--user-canadmin repo))
              (sep (consult-gh--get-split-style-character))
              (issue (or issue (consult-gh-issue-list (if canAdmin
                                                          repo
                                                        (concat repo " -- " "--author " "@me" sep))
                                                      t)))
              (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
              (isAuthor (consult-gh--user-isauthor issue)))
         (if (not (or canAdmin isAuthor))
             (message "The curent user, %s, %s to edit this issue" (propertize user 'face 'consult-gh-error) (propertize "does not have permission" 'face 'consult-gh-error))
           (funcall #'consult-gh--issue-view-action issue)
           (consult-gh-issue-edit)))
     (let* ((issue consult-gh--topic)
            (isAuthor (consult-gh--user-isauthor issue))
            (repo (get-text-property 0 :repo issue))
            (canAdmin (consult-gh--user-canadmin repo))
            (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
            (_ (if (not (or canAdmin isAuthor))
                   (message "The curent user, %s, %s to edit this issue" (propertize user 'face 'consult-gh-error) (propertize "does not have permission" 'face 'consult-gh-error))))
            (number (get-text-property 0 :number issue))
            (title (get-text-property 0 :title issue))
            (body (get-text-property 0 :body issue))
            (assignees (get-text-property 0 :assignees issue))
            (labels (get-text-property 0 :labels issue))
            (projects (get-text-property 0 :projects issue))
            (milestone (get-text-property 0 :milestone issue))
            (buffer (format "*consult-gh-issue-edit: %s #%s" repo number))
            (newtopic (format "%s/#%s" repo number))
            (type "issue"))

       (if canAdmin
           ;; collect valid projects for completion at point
           (consult-gh--completion-set-valid-projects newtopic repo)
         (add-text-properties 0 1 (list :valid-projects nil) newtopic))

       (add-text-properties 0 1 (text-properties-at 0 issue) newtopic)
       (add-text-properties 0 1 (list :isComment nil :type type :new nil :original-title title :original-body body :original-assignees assignees :original-labels labels :original-milestone milestone :original-projects projects) newtopic)

       (with-timeout (1 nil)
         (while (not (plist-member (text-properties-at 0 issue) :valid-labels))
           (sit-for 0.01)))
       (add-text-properties 0 1 (text-properties-at 0 issue) newtopic)

       (with-current-buffer (consult-gh-topics--get-buffer-create buffer "issue" newtopic)
         (unless (not (= (buffer-size) 0))
           (pcase-let* ((inhibit-read-only t)
                        (`(,title-marker ,header-marker) (consult-gh-topics--markers-for-metadata)))

             (insert (consult-gh-topics--format-field-header-string (concat title-marker "title: ")))
             (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))

             (save-excursion
               (when title (insert title))
               (insert "\n")
               (let* ((beg (point))
                      (end nil))
                 (when canAdmin
                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "assignees: ")))
                   (cond
                    ((stringp assignees)
                     (insert assignees))
                    ((and assignees (listp assignees))
                     (insert (mapconcat #'identity assignees ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")
                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "labels: ")))
                   (cond
                    ((stringp labels)
                     (insert labels))
                    ((and labels (listp labels))
                     (insert (mapconcat #'identity labels ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")
                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "milestone: ")))
                   (cond
                    ((stringp milestone)
                     (insert milestone))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")
                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "projects: ")))
                   (cond
                    ((stringp projects)
                     (insert projects))
                    ((and projects (listp projects))
                     (insert (mapconcat #'identity projects ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n"))
                 (insert (consult-gh-topics--format-field-header-string "---\n"))
                 (setq end (point))
                 (overlay-put (make-overlay beg end) :consult-gh-header t))
               (when body
                 (pcase major-mode
                   ('gfm-mode (insert body))
                   ('markdown-mode (insert body))
                   ('org-mode (insert (with-temp-buffer
                                        (insert body)
                                        (consult-gh--markdown-to-org)                                        (consult-gh--whole-buffer-string))))
                   ('text-mode (insert body)))))
             (cursor-intangible-mode +1))))
       (funcall consult-gh-pop-to-buffer-func buffer)))))

;;;###autoload
(defun consult-gh-issue-close (&optional issue reason comment)
  "Close the ISSUE with an optional REASON and/or COMMENT.

This mimicks the same function as running “gh issue close” in the terminal.
For more details refer to the manual with “gh issue close --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((issue (or issue consult-gh--topic (consult-gh-issue-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--state " "open") t)))
          (repo (and (stringp issue) (get-text-property 0 :repo issue)))
          (type (and (stringp issue) (get-text-property 0 :type issue)))
          (state (and (stringp issue) (get-text-property 0 :state issue)))
          (number (and (stringp issue) (get-text-property 0 :number issue)))
          (_ (unless (and (equal type "issue") (equal state "OPEN"))
               (error "Can only close an OPEN issue.  Did not get one!")))
          (reason (or reason (consult--read (list (cons "Completed" "completed")
                                                  (cons "Not Planned" "not planned")
                                                  (cons "Do not add reason" ""))
                                            :prompt "Select Reason: "
                                            :lookup #'consult--lookup-cdr
                                            :require-match t)))
          (comment (or comment (consult--read nil
                                              :prompt "Comment: ")))
          (args (list "issue" "close" number "--repo" repo)))
     (when (equal type "issue")
       (setq args (append args
                          (and (stringp comment) (not (string-empty-p comment))
                               (list "--comment" comment))
                          (and (stringp reason) (not (string-empty-p reason))
                               (list "--reason" reason))))
       (consult-gh--make-process (format "consult-gh-issue-close-%s-%s" repo number)
              :when-done (lambda (_ str) (message str))
              :cmd-args args)))))

;;;###autoload
(defun consult-gh-issue-reopen (&optional issue comment)
  "Close the ISSUE with an optional COMMENT.

This mimicks the same function as running “gh issue reopen” in the terminal.
For more details refer to the manual with “gh issue reopen --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((issue (or issue consult-gh--topic (consult-gh-issue-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--state " "closed") t)))
          (repo (and (stringp issue) (get-text-property 0 :repo issue)))
          (type (and (stringp issue) (get-text-property 0 :type issue)))
          (state (and (stringp issue) (get-text-property 0 :state issue)))
          (number (and (stringp issue) (get-text-property 0 :number issue)))
          (_ (unless (and (equal type "issue") (equal state "CLOSED"))
               (error "Can only reopen a CLOSED issue.  Did not get one!")))
          (comment (or comment (consult--read nil
                                              :prompt "Comment: ")))
          (args (list "issue" "reopen" number "--repo" repo)))
     (setq args (append args
                        (and (stringp comment) (not (string-empty-p comment))
                             (list "--comment" comment))))
     (consult-gh--make-process (format "consult-gh-issue-reopen-%s-%s" repo number)
                               :when-done (lambda (_ str) (message str))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-issue-pin (&optional issue)
  "Pin the ISSUE.

This mimicks the same function as running “gh issue pin” in the terminal.
For more details refer to the manual with “gh issue pin --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((consult-gh-issue-list-args (list "issue" "list" "--json" "number,title,isPinned,labels,updatedAt,state" "--template" "\"{{range .}}{{if (not .isPinned)}}{{.number}}\t{{.state}}\t{{.title}}\t{{range .labels}}{{.name}}, {{end}}\t{{.updatedAt}}\n{{end}}{{end}}\"" "--repo"))
          (issue (or issue consult-gh--topic (consult-gh-issue-list (get-text-property 0 :repo (consult-gh-search-repos nil t)) t)))
          (repo (and (stringp issue) (get-text-property 0 :repo issue)))
          (type (and (stringp issue) (get-text-property 0 :type issue)))
          (number (and (stringp issue) (get-text-property 0 :number issue)))
          (_ (unless (equal type "issue")
               (error "Can only pin an issue.  Did not get one!")))
          (args (list "issue" "pin" number "--repo" repo)))
     (consult-gh--make-process (format "consult-gh-issue-pin-%s-%s" repo number)
                               :when-done (lambda (_ str) (message str))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-issue-unpin (&optional issue)
  "Unpin the ISSUE.

This mimicks the same function as running “gh issue unpin” in the terminal.
For more details refer to the manual with “gh issue unpin --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((consult-gh-issue-list-args (list "issue" "list" "--json" "number,title,isPinned,labels,updatedAt,state" "--template" "\"{{range .}}{{if .isPinned}}{{.number}}\t{{.state}}\t{{.title}}\t{{range .labels}}{{.name}}, {{end}}\t{{.updatedAt}}\n{{end}}{{end}}\"" "--repo"))
          (issue (or issue consult-gh--topic (consult-gh-issue-list (get-text-property 0 :repo (consult-gh-search-repos nil t)) t)))
          (repo (and (stringp issue) (get-text-property 0 :repo issue)))
          (type (and (stringp issue) (get-text-property 0 :type issue)))
          (number (and (stringp issue) (get-text-property 0 :number issue)))
          (_ (unless (equal type "issue")
               (and
                (error "Can only pin an issue.  Did not get one!"))))
          (args (list "issue" "unpin" number "--repo" repo)))
     (consult-gh--make-process (format "consult-gh-issue-unpin-%s-%s" repo number)
            :when-done (lambda (_ str) (message str))
            :cmd-args args))))

;;;###autoload
(defun consult-gh-issue-lock (&optional issue reason)
  "Lock the ISSUE with and optional REASON.

This mimicks the same function as running “gh issue lock” in the terminal.
For more details refer to the manual with “gh issue lock --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((issue (or issue consult-gh--topic (consult-gh-issue-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--search " "is:unlocked") t)))
          (repo (and (stringp issue) (get-text-property 0 :repo issue)))
          (type (and (stringp issue) (get-text-property 0 :type issue)))
          (number (and (stringp issue) (get-text-property 0 :number issue)))
          (_ (unless (equal type "issue")
               (error "Can only lock an issue.  Did not get one!")))
          (reason (or reason (consult--read (list (cons "Yay, it's Resolved." "resolved")
                                                  (cons "Off Topic, A!" "off_topic")
                                                  (cons "That's a Spam Bro!" "spam")
                                                  (cons "This is Too Heated, even for my taste!" "too_heated")
                                                  (cons "Do not add a reason" ""))
                                            :prompt "Select Reason: "
                                            :lookup #'consult--lookup-cdr
                                            :require-match t)))
          (args (list "issue" "lock" number "--repo" repo)))
     (setq args (append args
                        (and (stringp reason) (not (string-empty-p reason))
                             (list "--reason" reason))))
     (consult-gh--make-process (format "consult-gh-issue-lock-%s-%s" repo number)
                               :when-done `(lambda (_ str) (if (and str (not (string-empty-p str))) (message str)
                                            (message "%s in %s was %s!" (format "Issue %s" (propertize (concat "#" ,number) 'face 'consult-gh-issue)) (propertize ,repo 'face 'consult-gh-user) (propertize "locked" 'face 'consult-gh-error))))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-issue-unlock (&optional issue)
  "Unlock the ISSUE.

This mimicks the same function as running “gh issue unlock” in the terminal.
For more details refer to the manual with “gh issue unlock --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((issue (or issue consult-gh--topic (consult-gh-issue-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--search " "is:locked") t)))
          (repo (and (stringp issue) (get-text-property 0 :repo issue)))
          (type (and (stringp issue) (get-text-property 0 :type issue)))
          (number (and (stringp issue) (get-text-property 0 :number issue)))
          (_ (unless (equal type "issue")
               (error "Can only lock an issue.  Did not get one!")))
          (args (list "issue" "unlock" number "--repo" repo)))
     (consult-gh--make-process (format "consult-gh-issue-unlock-%s-%s" repo number)
                               :when-done `(lambda (_ str) (if (and str (not (string-empty-p str))) (message str)
                                            (message "%s in %s was %s!" (format "Issue %s" (propertize (concat "#" ,number) 'face 'consult-gh-issue)) (propertize ,repo 'face 'consult-gh-user) (propertize "unlocked" 'face 'consult-gh-success))))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-issue-transfer (&optional issue target-repo)
  "Transfer the ISSUE to TARGET-REPO.

This mimicks the same function as running “gh issue transfer” in the terminal.
For more details refer to the manual with “gh issue transfer --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or issue consult-gh--topic))
          (repo (or (and (stringp topic) (get-text-property 0 :repo topic))
                    (get-text-property 0 :repo (consult-gh-search-repos nil t))))
          (type (or (and (stringp topic) (get-text-property 0 :type topic))
                    "issue"))
          (issueEnabled (if (not (stringp topic)) (gethash :hasIssuesEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasIssuesEnabled"))) t))
          (_ (unless (eq issueEnabled 't)
               (error "Issue is not enabled for the repo %s" repo)))
          (number (or (and (stringp topic) (get-text-property 0 :number topic))
                      (get-text-property 0 :number (consult-gh-issue-list repo t))))
          (_ (unless (equal type "issue")
               (error "Can only transfer an issue.  Did not get one!")))
          (target-repo (or target-repo (get-text-property 0 :repo (consult-gh-search-repos nil t "Search and Select Target Repository: "))))
          (args (list "issue" "transfer" number target-repo "--repo" repo )))
     (when (equal type "issue")
       (consult-gh--make-process "consult-gh-issue-close"
                                 :when-done (lambda (_ str) (message str))
                                 :cmd-args args)))))

;;;###autoload
(defun consult-gh-issue-delete (&optional issue no-confirm)
  "Delete the ISSUE.

When the optional argument, NO-CONFIRM, is non-nil, do not ask for
confirmation.
This mimicks the same function as running “gh issue delete” in the terminal.
For more details refer to the manual with “gh issue delete --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or issue consult-gh--topic))
          (repo (or (and (stringp topic) (get-text-property 0 :repo topic))
                    (get-text-property 0 :repo (consult-gh-search-repos nil t))))
          (type (or (and (stringp topic) (get-text-property 0 :type topic))
                    "issue"))
          (issueEnabled (if (not (stringp topic)) (gethash :hasIssuesEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasIssuesEnabled"))) t))
          (_ (unless (eq issueEnabled 't)
               (error "Issue is not enabled for the repo %s" repo)))
          (number (or (and (stringp topic) (get-text-property 0 :number topic))
                      (get-text-property 0 :number (consult-gh-issue-list repo t))))
          (_ (unless (equal type "issue")
               (error "Can only transfer an issue.  Did not get one!")))
          (args (list "issue" "delete" number "--repo" repo "--yes"))
          (count 1)
          (confirm (if no-confirm number
                     (consult--read nil :prompt (format "Type %s to confirm deletion: " number)))))
     (while (and (< count 5) (not (equal confirm number)))
       (cl-incf count)
       (setq confirm (consult--read nil :prompt (format "Type %s to confirm deletion (Try %s of 5): " number count))))
     (when (and (equal type "issue") (equal confirm number))
       (consult-gh--make-process "consult-gh-issue-close"
                                 :when-done (lambda (_ str) (message str))
                                 :cmd-args args)))))

;;;###autoload
(defun consult-gh-issue-develop (&optional issue base branch-repo)
  "Manage linked branches for an ISSUE.

The optional argument BASE is a base branch to develop off of.
The optional argument BRANCH-REPO is the repo's full name where a branch
should be created (if not in the same repo).

This mimicks the same function as running “gh issue develop” in the terminal.
For more details refer to the manual with “gh issue develop --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or issue consult-gh--topic))
          (repo (or (and (stringp topic) (get-text-property 0 :repo topic))
                    (get-text-property 0 :repo (consult-gh-search-repos nil t))))
          (type (or (and (stringp topic) (get-text-property 0 :type topic))
                    "issue"))
          (issueEnabled (if (not (stringp topic)) (gethash :hasIssuesEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasIssuesEnabled"))) t))
          (_ (unless (eq issueEnabled 't)
               (error "Issue is not enabled for the repo %s" (propertize repo 'face 'consult-gh-repo))))
          (number (or (and (stringp topic) (get-text-property 0 :number topic))
                      (get-text-property 0 :number (consult-gh-issue-list repo t))))
          (_ (unless (equal type "issue")
               (error "Can only use an issue.  Did not get one!")))
          (branches (consult-gh--command-to-string "issue" "develop" "--list" "--repo" repo number))
          (options (append (list (list "Make a New Branch" :new))
                           (mapcar (lambda (item)
                                     (let ((info (split-string item "\t" t)))
                                       (list (car info) (cadr info))))
                                   (split-string branches "[\n\r]" t))))
          (branch (consult--read options
                                 :prompt "Select or Create a Branch: "
                                 :lookup #'consult--lookup-cons
                                 :require-match t
                                 :sort nil))
          (branch-repo (or branch-repo (if (eq (cadr branch) :new)
                                           (get-text-property 0 :repo (consult-gh-search-repos repo t "Select the repo where you want to create the branch: "))
                                         repo)))
          (base (or base
                    (if (eq (cadr branch) :new)
                        (cdr (consult-gh--read-branch branch-repo (format "Select a base branch in %s to create a new one off of" branch-repo))))))
          (args (if (eq (cadr branch) :new)
                    (list "issue" "develop" number "--repo" repo))))
     (cond
      ((and (equal type "issue") (eq (cadr branch) :new))
       (setq args (append args
                          (and (stringp branch-repo)
                               (not (string-empty-p branch-repo))
                               (not (equal branch-repo repo))
                               (list "--branch-repo" branch-repo))
                          (and (stringp base) (not (string-empty-p base))
                               (list "--base" base))))
       (consult-gh--make-process "consult-gh-issue-develop"
                                 :when-done (lambda (_ str) (message str))
                                 :cmd-args args))
      ((stringp (cadr branch))
       (consult-gh-find-file repo (car branch)))))))

(defun consult-gh--search-issues-transform (input)
  "Add annotation to issue candidates in `consult-gh-search-issues'.

Format each candidates with `consult-gh--search-issues-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--search-issues-format cand input nil))))

(defun consult-gh--search-issues-builder (input)
  "Build gh command line for searching issues of INPUT query."
  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-search-issues-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-issue-maxnum)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--async-search-issues (prompt builder &optional initial)
  "Search GitHub issues asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-search-issues'.

This runs the command line from `consult-gh--search-issues-builder' in
an async process and returns the results \(list of search results
for the input\) as a completion table in minibuffer.  The completion table
gets dynamically updated as the user types in the minibuffer.
Each candidate is formatted by `consult-gh--search-issues-transform'
to add annotation and other info to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
      (consult--read
       (consult--process-collection builder
         :transform (consult--async-transform-by-input #'consult-gh--search-issues-transform))
       :prompt prompt
       :lookup #'consult--lookup-member
       :state (funcall #'consult-gh--issue-state)
       :initial initial
       :group #'consult-gh--issue-group
       :require-match t
       :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                             (append (list (consult-gh--get-repo-from-directory)
                                  (consult-gh--get-repo-from-topic)
                                  (thing-at-point 'symbol))
                            consult-gh--known-repos-list))
       :history '(:input consult-gh--search-issues-history)
       :category 'consult-gh-issues
       :preview-key consult-gh-preview-key
       :sort nil)))

;;;###autoload
(defun consult-gh-search-issues (&optional initial repo noaction prompt)
  "Interactively search GitHub issues of REPO.

This is an interactive wrapper function around
`consult-gh--async-search-issues'.  With prefix ARG, first search for a
repo using `consult-gh-search-repos', then search issues of only that
selected repo.

It queries the user for a search term in the minibuffer, then fetches the
list of possible GitHub issue for the entered query and presents them as a
minibuffer completion table for selection.  The list of candidates in the
completion table are dynamically updated as the user changes the entry.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-issue-action'

Additional command line arguments can be passed in the minibuffer input
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
consult-gh -- -L 100
and the async process will run “gh search issues consult-gh -L 100”,
which sets the limit for the maximum number of results to 100.

INITIAL is an optional arg for the initial input in the minibuffer
\(passed as INITITAL to `consult-gh--async-repo-list'\).

If PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if (xor current-prefix-arg consult-gh-use-search-to-find-name)
      (setq repo (or repo (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos repo t))))))
  (let* ((prompt (or prompt "Search Issues:  "))
         (consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-issues prompt #'consult-gh--search-issues-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (funcall consult-gh-issue-action sel))))

(defun consult-gh--pr-list-transform (input)
  "Add annotation to issue candidates in `consult-gh-pr-list'.

Format each candidates with `consult-gh--pr-list-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--pr-list-format cand input nil))))

(defun consult-gh--pr-list-builder (input)
  "Build gh command line for listing pull requests of the INPUT repository.

INPUT must be the full name of a GitHub repository as a string
e.g. “armindarvish/consult-gh”."
  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-pr-list-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-issue-maxnum)))))
    (unless (or (member "-s" flags) (member "--state" flags))
      (setq opts (append opts (list "--state" (format "%s" consult-gh-prs-state-to-show)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--async-pr-list (prompt builder &optional initial)
  "List pull requests of GitHub repos asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-pr-list'.

This runs the command line from `consult-gh--pr-list-builder'
in an async process and returns the results (list of prs for a repository)
as a completion table in minibuffer.  The completion table gets dynamically
updated as the user types in the minibuffer to change the input.
Each candidate in the minibuffer is formatted by
`consult-gh--pr-list-transform' to add annotation to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input in the minibuffer
          \(passed as INITITAL to `consult--read'\)"
  (let* ((initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't)
                          (consult-gh--get-repo-from-directory)
                        nil))))
    (consult-gh-with-host (consult-gh--auth-account-host)
        (consult--read
         (consult--process-collection builder
           :transform (consult--async-transform-by-input #'consult-gh--pr-list-transform))
         :prompt prompt
         :category 'consult-gh-prs
         :lookup #'consult--lookup-member
         :state (funcall #'consult-gh--pr-state)
         :initial initial
         :group #'consult-gh--pr-list-group
         :require-match t
         :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                               (append (list (consult-gh--get-repo-from-directory)
                                    (consult-gh--get-repo-from-topic)
                                    (thing-at-point 'symbol))
                              consult-gh--known-repos-list))
         :history '(:input consult-gh--repos-history)
         :preview-key consult-gh-preview-key
         :sort nil))))

;;;###autoload
(defun consult-gh-pr-list (&optional initial noaction prompt)
  "Interactively list pull requests of a GitHub repository.

This is an interactive wrapper function around `consult-gh--async-pr-list'.
With prefix ARG, first search for a repo using `consult-gh-search-repos',
then list prs of that selected repo with `consult-gh--async-pr-list'.

It queries the user to enter the full name of a GitHub repository in
the minibuffer (expected format is “OWNER/REPO”), then fetches the list
of pull requests for that repository and presents them as a minibuffer
completion table for selection.  The list of candidates in the completion
table are dynamically updated as the user changes the entry.

Upon selection of a candidate either
 - if NOACTION is non-nil candidate is returned
 - if NOACTION is nil     candidate is passed to `consult-gh-pr-action'

Additional command line arguments can be passed in the minibuffer input
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
armindarvish/consult-gh -- -L 100
and the async process will run
“gh pr list --repo armindarvish/consult-gh -L 100”,
which sets the limit for the maximum number of results to 100.

User selection is tracked in `consult-gh--known-repos-list' for quick access
in the future \(added to future history list\) in future calls.

INITIAL is an optional arg for the initial input in the minibuffer
\(passed as INITITAL to `consult-gh--async-issue-list'\).

If PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if (xor current-prefix-arg consult-gh-use-search-to-find-name)
      (setq initial (or initial (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos initial t))))))
  (let* ((prompt (or prompt "Enter Repo Name:  "))
         (sel (consult-gh--async-pr-list prompt #'consult-gh--pr-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel))))

(defun consult-gh-pr-create (&optional repo title body)
  "Create a new pull request with TITLE and BODY for REPO.

This mimicks the same interactive pr creation from “gh pr create”
in the terminal.  For more details refer to the manual with
“gh pr create --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t "Select the target base repo you want to merge to: "))))
          (isForked (eq (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "isFork") :isFork) 't))
          (simrepos (consult-gh-topics--pr-get-similar repo))
          (selection (if isForked
                        (substring-no-properties (consult--read (append simrepos "Other")
                                                                :prompt "That is a forked repo. Which one of the following repos you want to meger to?"
                                         :require-match t))))
          (baserepo (cond
                     ((equal selection "Other")
                      (get-text-property 0 :repo (consult-gh-search-repos (consult-gh--get-package repo) t "Search for the target base repo you want to merge to: ")))
                     ((stringp selection) selection)
                     (t repo)))
          (basebranch (consult--read (consult-gh-topics--pr-get-branches baserepo)
                                     :prompt "Select the branch you want to merge to: "
                                     :sort t))
          (selection (cond
                     ((length> simrepos 0)
                      (consult--read (append (list baserepo) simrepos (list "Other"))
                                                                :prompt "Select the source head repo you want to merge from: "
                                         :require-match t
                                         :sort nil))))
          (headrepo (cond
                     ((equal selection "Other")
                      (get-text-property 0 :repo (consult-gh-search-repos (consult-gh--get-package baserepo) t "Search for the source head repo you want to merge from: ")))
                     ((stringp selection) selection)
                     (t baserepo)))
          (headbranch (consult--read (cond
                                      ((equal baserepo headrepo)
                                       (remove basebranch (consult-gh-topics--pr-get-branches baserepo)))
                                      (t (consult-gh-topics--pr-get-branches headrepo)))
                                     :prompt "Select the head branch: "
                                     :sort t))
          (canAdmin (consult-gh--user-canadmin baserepo))
          (author (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
          (templates (consult-gh--get-pr-templates baserepo))
          (body (or body
                    (if (and templates (length= templates 1))
                        (cdar templates)
                  (and templates (consult--read templates
                                                :prompt "Select a template: "
                                                :require-match t
                                                :lookup #'consult--lookup-cdr
                                                :sort t)))))

          (topic (or repo "new pr"))
          (type "pr")
          (buffer (format "*consult-gh-pr-create: %s:%s->%s:%s" repo headbranch baserepo basebranch)))

     ;; collect issues of repo for completion at point
     (consult-gh--completion-set-issues topic repo)

     ;; collect prs of repo for completion at point
     (consult-gh--completion-set-prs topic repo)

     ;; collect mentionable users for completion at point
     (consult-gh--completion-set-mentionable-users topic repo)

     ;; collect valid refs for completion at point
     (consult-gh--completion-set-pr-refs topic baserepo headrepo nil)

     (if canAdmin
         (progn
           ;; collect labels for completion at point
           (consult-gh--completion-set-valid-labels topic repo)
           ;; collect valid assignees for completion at point
           (consult-gh--completion-set-assignable-users topic repo)
           ;; collect valid milestones for completion at point
           (consult-gh--completion-set-valid-milestones topic repo)
           ;; collect valid projects for completion at point
           (consult-gh--completion-set-valid-projects topic repo))

       (add-text-properties 0 1 (list :valid-labels nil :assignable-users nil :valid-milestones nil :valid-projects nil) topic))

     (add-text-properties 0 1 (list :number nil :type type :isComment nil :new t :repo (substring-no-properties baserepo) :author author :headrepo (substring-no-properties repo) :headbranch (substring-no-properties headbranch) :baserepo (substring-no-properties baserepo) :basebranch (substring-no-properties basebranch)) topic)

     (with-current-buffer (consult-gh-topics--get-buffer-create buffer "pull request" topic)
       (unless (not (= (buffer-size) 0))
         (pcase-let* ((inhibit-read-only t)
                      (`(,title-marker ,header-marker) (consult-gh-topics--markers-for-metadata)))
           (insert (consult-gh-topics--format-field-header-string (concat title-marker "title: ")))
           (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))

           (save-mark-and-excursion
             (when title (insert title))
             (insert "\n")
             (let* ((beg (point))
                    (end nil))

               (insert (consult-gh-topics--format-field-header-string (concat header-marker "base: ")))
               (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
               (when (stringp baserepo)
                 (insert (concat baserepo ":")))
               (when (stringp basebranch)
                 (insert basebranch))
               (insert "\n")

               (insert (consult-gh-topics--format-field-header-string (concat header-marker "head: ")))
               (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
               (when (stringp headrepo)
                 (insert (concat headrepo ":")))
               (when (stringp headbranch)
                 (insert headbranch))
               (insert "\n")

               (when canAdmin
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "reviewers: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "assignees: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "labels: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "milestone: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n")
                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "projects: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (insert "\n"))
               (insert (consult-gh-topics--format-field-header-string "---\n"))
               (setq end (point))
               (overlay-put (make-overlay beg end) :consult-gh-header t)
               (insert "\n"))
             (and body
                  (pcase major-mode
                    ('gfm-mode (insert body))
                    ('markdown-mode (insert body))
                    ('org-mode (insert (with-temp-buffer
                                         (insert body)
                                         (consult-gh--markdown-to-org)                                        (consult-gh--whole-buffer-string))))
                    ('text-mode (insert body)))))
           (cursor-intangible-mode +1))))

     (funcall consult-gh-pop-to-buffer-func buffer))))

;;;###autoload
(defun consult-gh-pr-edit (&optional pr)
  "Edit the PR.

This mimicks the same function as running “gh pr edit” in the terminal.
For more details refer to the manual with “gh pr edit --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (if (not consult-gh-pr-view-mode)
       (let* ((baserepo (or (and pr (get-text-property 0 :baserepo pr))
                        (get-text-property 0 :repo (consult-gh-search-repos nil t))))
              (canAdmin (consult-gh--user-canadmin baserepo))
              (sep (consult-gh--get-split-style-character))
              (pr (or pr (consult-gh-pr-list (if canAdmin
                                                 baserepo
                                               (concat baserepo " -- " "--author " "@me" sep))
                                             t)))
              (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
              (isAuthor (consult-gh--user-isauthor pr)))
         (if (not (or canAdmin isAuthor))
             (message "The curent user, %s, %s to edit this pull request" (propertize user 'face 'consult-gh-error) (propertize "does not have permission" 'face 'consult-gh-error))
           (funcall #'consult-gh--pr-view-action pr)
           (consult-gh-pr-edit)))
     (let* ((pr consult-gh--topic)
            (isAuthor (consult-gh--user-isauthor pr))
            (baserepo (get-text-property 0 :baserepo pr))
            (canAdmin (consult-gh--user-canadmin baserepo))
            (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
            (_ (if (not (or canAdmin isAuthor))
                   (message "The curent user, %s, %s to edit this pull request" (propertize user 'face 'consult-gh-error) (propertize "does not have permission" 'face 'consult-gh-error))))
            (number (get-text-property 0 :number pr))
            (newtopic (format "%s/#%s" baserepo number))
            (title (get-text-property 0 :title pr))
            (body (get-text-property 0 :body pr))
            (reviewers (get-text-property 0 :reviewers pr))
            (assignees (get-text-property 0 :assignees pr))
            (labels (get-text-property 0 :labels pr))
            (projects (get-text-property 0 :projects pr))
            (milestone (get-text-property 0 :milestone pr))
            (basebranch (get-text-property 0 :basebranch pr))
            (buffer (format "*consult-gh-pr-edit: %s #%s" baserepo number))
            (type "pr"))

       ;; collect valid refs for completion at point
     (consult-gh--completion-set-pr-refs newtopic baserepo nil t)

       (if canAdmin
           ;; collect valid projects for completion at point
           (consult-gh--completion-set-valid-projects newtopic baserepo)
         (add-text-properties 0 1 (list :valid-projects nil) newtopic))

       (add-text-properties 0 1 (list :isComment nil :type type :new nil :original-title title :original-body body :original-reviewers reviewers :original-assignees assignees :original-labels labels :original-milestone milestone :original-projects projects :original-basebranch basebranch) newtopic)


       (with-timeout (1 nil)
         (while (not (plist-member (text-properties-at 0 pr) :valid-labels))
           (sit-for 0.01)))
       (add-text-properties 0 1 (text-properties-at 0 pr) newtopic)


       (with-current-buffer (consult-gh-topics--get-buffer-create buffer "pull request" newtopic)
         (unless (not (= (buffer-size) 0))
           (pcase-let* ((inhibit-read-only t)
                        (`(,title-marker ,header-marker) (consult-gh-topics--markers-for-metadata)))

             (insert (consult-gh-topics--format-field-header-string (concat title-marker "title: ")))
             (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))

             (save-mark-and-excursion
               (when title (insert title))
               (insert "\n")
               (let* ((beg (point))
                      (end nil))

                 (insert (consult-gh-topics--format-field-header-string (concat header-marker "base: ")))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                 (when (stringp basebranch)
                   (insert basebranch))
                 (insert "\n")

                 (when canAdmin
                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "reviewers: ")))
                   (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                   (cond
                    ((stringp reviewers)
                     (insert reviewers))
                    ((and reviewers (listp reviewers))
                     (insert (mapconcat #'identity reviewers ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")

                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "assignees: ")))
                   (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                   (cond
                    ((stringp assignees)
                     (insert assignees))
                    ((and assignees (listp assignees))
                     (insert (mapconcat #'identity assignees ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")

                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "labels: ")))
                   (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                   (cond
                    ((stringp labels)
                     (insert labels))
                    ((and labels (listp labels))
                     (insert (mapconcat #'identity labels ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")

                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "milestone: ")))
                   (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                   (cond
                    ((stringp milestone)
                     (insert milestone))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n")

                   (insert (consult-gh-topics--format-field-header-string (concat header-marker "projects: ")))
                   (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert " "))
                   (cond
                    ((stringp projects)
                     (insert projects))
                    ((and projects (listp projects))
                     (insert (mapconcat #'identity projects ", ")))
                    ((derived-mode-p 'markdown-mode)
                     (delete-char -1)
                     (insert " ")))
                   (insert "\n"))

                 (insert (consult-gh-topics--format-field-header-string "---\n"))
                 (when (derived-mode-p 'markdown-mode) (delete-char -1) (insert "\n"))
                 (setq end (point))
                 (overlay-put (make-overlay beg end) :consult-gh-header t))
               (when body
                 (pcase major-mode
                   ('gfm-mode (insert body))
                   ('markdown-mode (insert body))
                   ('org-mode (insert (with-temp-buffer
                                        (insert body)
                                        (consult-gh--markdown-to-org)                                        (consult-gh--whole-buffer-string))))
                   ('text-mode (insert body)))))
             (cursor-intangible-mode +1))))

       (funcall consult-gh-pop-to-buffer-func buffer)))))

;;;###autoload
(defun consult-gh-pr-merge (&optional pr)
  "Merge the PR.

This mimicks the same function as running “gh pr merge” in the terminal.
For more details refer to the manual with “gh pr merge --help”.

PR is a propertized string describing a pull request.  For example, PR can
be the text stored in the buffer-local variable `consult-gh--topic' in a
buffer created by `consult-gh--pr-view'."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (if (not consult-gh-pr-view-mode)
       (let* ((repo (or (and pr (get-text-property 0 :repo pr))
                        (get-text-property 0 :repo (consult-gh-search-repos nil t))))
              (canAdmin (consult-gh--user-canadmin repo))
              (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
              (_ (if (not canAdmin)
                     (message "The user, %s, %s to merege PRs in that repository" (propertize user 'face 'consult-gh-user) (propertize "does not have permissions" 'face 'consult-gh-error))))
              (pr (or pr (consult-gh-pr-list repo t))))
         (funcall #'consult-gh--pr-view-action pr)
         (consult-gh-pr-merge))

     (let* ((pr (or pr consult-gh--topic))
            (repo (get-text-property 0 :repo pr))
            (canAdmin (consult-gh--user-canadmin repo))
            (user (or (car-safe consult-gh--auth-current-account) (car-safe (consult-gh--auth-current-active-account))))
            (_ (if (not canAdmin)
                   (message "The user, %s, %s to merege PRs in that repository" (propertize user 'face 'consult-gh-user) (propertize "does not have permissions" 'face 'consult-gh-error))))
            (number (get-text-property 0 :number pr))
            (state (get-text-property 0 :state pr)))

       (cond
        ((not (equal state "OPEN"))
         (message "Pull request is already %s!" (propertize (downcase state) 'face 'consult-gh-error)))
        (t
         (let* ((auto-merge-state (consult-gh--json-to-hashtable (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" "autoMergeRequest") :autoMergeRequest))
                (action (consult--read (append (list (cons "Merge the pull request" :merge))
                                               (and (not auto-merge-state) (list (cons "Enable auto-merge (after requirements are met)." :auto)))
                                               (and auto-merge-state (list (cons "Disable auto-merge" :disable-auto))))
                                       :prompt "What do you want to do?"
                                       :lookup #'consult--lookup-cdr
                                       :require-match t
                                       :sort nil)))
           (pcase action
             (':merge
              (consult-gh-pr--merge-merge pr))
             (':auto
              (consult-gh-pr--merge-enable-automerge pr))
             (':disable-auto
              (consult-gh-pr--merge-disable-automerge pr))))))))))

;;;###autoload
(defun consult-gh-pr-review (&optional pr)
  "Interactively create a new review on pull request, PR.

PR is a string with properties that identify a github pull requests.
For an example, see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh-pr-create'.
PR defaults to `consult-gh--topic' and if that is nil also, the user is
asked to chose it interactively."
  (interactive "P" consult-gh-pr-view-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or pr consult-gh--topic))
          (newtopic (substring-no-properties topic))
          (repo (or (and (stringp topic) (get-text-property 0 :repo topic))
                    (get-text-property 0 :repo (consult-gh-search-repos nil t))))
          (type (or (and (stringp topic) (get-text-property 0 :type topic))
                    "pr"))
          (number (and (stringp topic) (get-text-property 0 :number topic)))
          (buffer (format "*consult-gh-pr-review: %s - %s #%s review" repo type number))
          (view-buffer (buffer-name (current-buffer))))
     (add-text-properties 0 1 (list :review-buffer buffer :view-buffer view-buffer :target "review") topic)
     (add-text-properties 0 1 (text-properties-at 0 topic) newtopic)
     (add-text-properties 0 1 (list :new t :isComment nil :type "review" :target "review") newtopic)
     (cond
      (topic
       (funcall consult-gh-pop-to-buffer-func (consult-gh-topics--get-buffer-create buffer "review" newtopic)))
      ((equal type "pr")
       (funcall #'consult-gh--pr-view-action (consult-gh-pr-list repo t))
       (consult-gh-pr-review))
      (t (message "Did not get a pull request to review!"))))))

;;;###autoload
(defun consult-gh-pr-close (&optional pr comment delete-branch)
  "Close the PR with an optional COMMENT.

If the optional argument DELETE-BRANCH is non-nil, deletes
the local and remote branch after close.

This mimicks the same function as running “gh pr close” in the terminal.
For more details refer to the manual with “gh pr close --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((pr (or pr consult-gh--topic (consult-gh-pr-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--state " "open") t)))
          (repo (and (stringp pr) (get-text-property 0 :repo pr)))
          (type (and (stringp pr) (get-text-property 0 :type pr)))
          (state (and (stringp pr) (get-text-property 0 :state pr)))
          (number (and (stringp pr) (get-text-property 0 :number pr)))
          (_ (unless (and (equal type "pr") (equal state "OPEN"))
               (error "Can only close an OPEN pr.  Did not get one!")))
          (comment (or comment (consult--read nil
                                              :prompt "Comment: ")))
          (delete-branch (or delete-branch (y-or-n-p "Do you want to delete the remote and local (i.e. in the current directory) branches of this pull request?")))
          (args (list "pr" "close" number "--repo" repo)))
     (when (equal type "pr")
       (setq args (append args
                          (and (stringp comment) (not (string-empty-p comment))
                               (list "--comment" comment))
                          (and delete-branch
                               (list "--delete-branch"))))
       (consult-gh--make-process (format "consult-gh-pr-close-%s-%s" repo number)
                                 :when-done (lambda (_ str) (message str))
                                 :cmd-args args)))))

;;;###autoload
(defun consult-gh-pr-reopen (&optional pr comment)
  "Close the PR with an optional COMMENT.

This mimicks the same function as running “gh pr reopen” in the terminal.
For more details refer to the manual with “gh pr reopen --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((pr (or pr consult-gh--topic (consult-gh-pr-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--state " "closed") t)))
          (repo (and (stringp pr) (get-text-property 0 :repo pr)))
          (type (and (stringp pr) (get-text-property 0 :type pr)))
          (state (and (stringp pr) (get-text-property 0 :state pr)))
          (number (and (stringp pr) (get-text-property 0 :number pr)))
          (_ (unless (and (equal type "pr") (equal state "CLOSED"))
               (error "Can only reopen a CLOSED pull request.  Did not get one!")))
          (comment (or comment (consult--read nil
                                              :prompt "Comment: ")))
          (args (list "pr" "reopen" number "--repo" repo)))
     (setq args (append args
                        (and (stringp comment) (not (string-empty-p comment))
                             (list "--comment" comment))))
     (consult-gh--make-process (format "consult-gh-pr-reopen-%s-%s" repo number)
                               :when-done (lambda (_ str) (message str))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-pr-lock (&optional pr reason)
  "Lock the PR with and optional REASON.

This mimicks the same function as running “gh pr lock” in the terminal.
For more details refer to the manual with “gh pr lock --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((pr (or pr consult-gh--topic (consult-gh-pr-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--search " "is:unlocked") t)))
          (repo (and (stringp pr) (get-text-property 0 :repo pr)))
          (type (and (stringp pr) (get-text-property 0 :type pr)))
          (number (and (stringp pr) (get-text-property 0 :number pr)))
          (_ (unless (equal type "pr")
               (error "Can only lock a pull request.  Did not get one!")))
          (reason (or reason (consult--read (list (cons "Yay, it's Resolved." "resolved")
                                                  (cons "Off Topic, A!" "off_topic")
                                                  (cons "That's a Spam Bro!" "spam")
                                                  (cons "This is Too Heated, even for my taste!" "too_heated")
                                                  (cons "Do not add a reason" ""))
                                            :prompt "Select Reason: "
                                            :lookup #'consult--lookup-cdr
                                            :require-match t)))
          (args (list "pr" "lock" number "--repo" repo)))
     (setq args (append args
                        (and (stringp reason) (not (string-empty-p reason))
                             (list "--reason" reason))))
     (consult-gh--make-process (format "consult-gh-pr-lock-%s-%s" repo number)
                               :when-done `(lambda (_ str) (if (and str (not (string-empty-p str))) (message str)
                                            (message "%s in %s was %s!" (format "Pull request %s" (propertize (concat "#" ,number) 'face 'consult-gh-issue)) (propertize ,repo 'face 'consult-gh-user) (propertize "locked" 'face 'consult-gh-error))))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-pr-unlock (&optional pr)
  "Unlock the PR.

This mimicks the same function as running “gh pr unlock” in the terminal.
For more details refer to the manual with “gh pr unlock --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((pr (or pr consult-gh--topic (consult-gh-pr-list (concat (get-text-property 0 :repo (consult-gh-search-repos nil t)) " -- " "--search " "is:locked") t)))
          (repo (and (stringp pr) (get-text-property 0 :repo pr)))
          (type (and (stringp pr) (get-text-property 0 :type pr)))
          (number (and (stringp pr) (get-text-property 0 :number pr)))
          (_ (unless (equal type "pr")
               (error "Can only lock a pull request.  Did not get one!")))
          (args (list "pr" "unlock" number "--repo" repo)))
     (consult-gh--make-process (format "consult-gh-pr-unlock-%s-%s" repo number)
                               :when-done `(lambda (_ str) (if (and str (not (string-empty-p str))) (message str)
                                            (message "%s in %s was %s!" (format "Pull request %s" (propertize (concat "#" ,number) 'face 'consult-gh-issue)) (propertize ,repo 'face 'consult-gh-user) (propertize "unlocked" 'face 'consult-gh-success))))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-pr-mark-ready (&optional pr)
  "Mark the PR as ready for review.

This mimicks the same function as running “gh pr ready” in the terminal.
For more details refer to the manual with “gh pr ready --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((pr (or pr consult-gh--topic (consult-gh-pr-list (get-text-property 0 :repo (consult-gh-search-repos nil t)) t)))
          (repo (and (stringp pr) (get-text-property 0 :repo pr)))
          (type (and (stringp pr) (get-text-property 0 :type pr)))
          (number (and (stringp pr) (get-text-property 0 :number pr)))
          (_ (unless (equal type "pr")
               (error "Can only mark a pull request.  Did not get one!")))
          (args (list "pr" "ready" number "--repo" repo)))
     (consult-gh--make-process (format "consult-gh-pr-ready-%s-%s" repo number)
                               :when-done (lambda (_ str) (message str))
                               :cmd-args args))))

;;;###autoload
(defun consult-gh-pr-mark-draft (&optional pr)
  "Mark the PR as draft.

This mimicks the same function as running “gh pr ready” with the switch
“--undo” in the terminal.  For more details refer to the manual with
“gh pr ready --help”."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((pr (or pr consult-gh--topic (consult-gh-pr-list (get-text-property 0 :repo (consult-gh-search-repos nil t)) t)))
          (repo (and (stringp pr) (get-text-property 0 :repo pr)))
          (type (and (stringp pr) (get-text-property 0 :type pr)))
          (number (and (stringp pr) (get-text-property 0 :number pr)))
          (_ (unless (equal type "pr")
               (error "Can only mark a pull request.  Did not get one!")))
          (args (list "pr" "ready" number "--repo" repo "--undo")))
     (consult-gh--make-process (format "consult-gh-pr-draft-%s-%s" repo number)
                               :when-done (lambda (_ str) (message str))
                               :cmd-args args))))

(defun consult-gh--search-prs-transform (input)
  "Add annotation to pr candidates in `consult-gh-search-prs'.

Format each candidates with `consult-gh--search-prs-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--search-prs-format cand input nil))))

(defun consult-gh--search-prs-builder (input)
  "Build gh command line for searching pull requests of INPUT query."
  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-search-prs-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-issue-maxnum)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--async-search-prs (prompt builder &optional initial)
  "Search GitHub pull requests asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-search-prs'.

This runs the command line from `consult-gh--search-prs-builder' in
an async process and returns the results (list of search results for
the input) as a completion table in minibuffer.  The completion table gets
dynamically updated as the user types in the minibuffer.
Each candidate in the minibuffer is formatted by
`consult-gh--search-prs-transform' to add annotation to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
                        (consult--read
                         (consult--process-collection builder
                                                      :transform (consult--async-transform-by-input #'consult-gh--search-prs-transform))
                         :prompt prompt
                         :category 'consult-gh-prs
                         :lookup #'consult--lookup-member
                         :state (funcall #'consult-gh--pr-state)
                         :initial initial
                         :group #'consult-gh--pr-search-group
                         :require-match t
                         :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                                               (append (list (consult-gh--get-repo-from-directory)
                                                    (consult-gh--get-repo-from-topic)
                                                    (thing-at-point 'symbol))
                                              consult-gh--known-repos-list))
                         :history '(:input consult-gh--search-prs-history)
                         :preview-key consult-gh-preview-key
                         :sort nil)))

;;;###autoload
(defun consult-gh-search-prs (&optional initial repo noaction prompt)
  "Interactively search GitHub pull requests of REPO.

This is an interactive wrapper function around
`consult-gh--async-search-prs'.  With prefix ARG, first search for a repo
using `consult-gh-search-repos', then search prs of only that selected repo.

It queries the user for a search term in the minibuffer, then fetches
the list of possible GitHub pr candidates for the entered query
and presents them as a minibuffer completion table for selection.
The list of candidates in the completion table are dynamically updated as
the user changes the input.

Upon selection of a candidate either
 - if NOACTION is non-nil candidate is returned
 - if NOACTION is nil     candidate is passed to `consult-gh-pr-action'

Additional command line arguments can be passed in the minibuffer input
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
consult-gh -- -L 100
and the async process will run “gh search prs consult-gh -L 100”,
which sets the limit for the maximum number of results to 100.

INITIAL is an optional arg for the initial input in the minibuffer
\(passed as INITITAL to `consult-gh--async-repo-list'\).

If PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if (xor current-prefix-arg consult-gh-use-search-to-find-name)
      (setq repo (or repo (substring-no-properties (get-text-property 0 :repo  (consult-gh-search-repos repo t))))))
  (let* ((prompt (or prompt "Search Pull-Requests:  "))
         (consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-prs prompt #'consult-gh--search-prs-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel))))

(defun consult-gh--search-code-transform (input)
  "Add annotation to code candidates in `consult-gh-search-code'.

Format each candidates with `consult-gh--search-code-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-gh--search-code-format cand input nil))))

(defun consult-gh--search-code-builder (input)
  "Build gh command line for searching code with INPUT query."
  (pcase-let* ((consult-gh-args (append consult-gh-args consult-gh-search-code-args))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-code-maxnum)))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-gh--async-search-code (prompt builder &optional initial)
  "Seacrh GitHub codes asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-search-code'.

This runs the command line from `consult-gh--search-code-builder' in
an async process and returns the results (list of search results for
the input) as a completion table in minibuffer.  The completion table
gets dynamically updated as the user types in the minibuffer.
Each candidate in the minibuffer is formatted by
`consult-gh--search-code-transform' to add annotation to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
      (consult--read
       (consult--process-collection builder
         :transform (consult--async-transform-by-input #'consult-gh--search-code-transform))
       :prompt prompt
       :category 'consult-gh-codes
       :lookup #'consult--lookup-member
       :state (funcall #'consult-gh--code-state)
       :initial initial
       :group #'consult-gh--code-group
       :require-match t
       :add-history  (mapcar (lambda (item) (concat (consult-gh--get-split-style-character) item))
                             (append (list (consult-gh--get-repo-from-directory)
                                  (consult-gh--get-repo-from-topic)
                                  (thing-at-point 'symbol))
                            consult-gh--known-repos-list))
       :history '(:input consult-gh--search-code-history)
       :preview-key consult-gh-preview-key
       :sort nil)))

;;;###autoload
(defun consult-gh-search-code (&optional initial repo noaction prompt)
  "Interactively search GitHub codes.

This is an interactive wrapper function around
`consult-gh--async-search-code'.  With prefix ARG, first search for a repo
using `consult-gh-search-repos', then search for code only on that
selected repo.

If REPO is non-nil search for code in REPO.

It queries the user for a search term in the minibuffer, then fetches
the list of possible GitHub code candidates for the entered query and
presents them as a minibuffer completion table for selection.
The list of candidates in the completion table are dynamically updated
as the user changes the input.

Upon selection of a candidate either
 - if NOACTION is non-nil candidate is returned
 - if NOACTION is nil     candidate is passed to `consult-gh-pr-action'

Additional command line arguments can be passed in the minibuffer input
by typing `--` followed by command line arguments.
For example the user can enter the following in the minibuffer:
react -- -L 100
and the async process will run “gh search code react -L 100”,
which sets the limit for the maximum number of results to 100.

INITIAL is an optional arg for the initial input in the minibuffer
\(passed as INITITAL to `consult-gh--async-search-code'\).

If PROMPT is non-nil, use it as the query prompt.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (setq consult-gh--open-files-list nil
        consult-gh--current-tempdir (consult-gh--tempdir))
  (if (xor current-prefix-arg consult-gh-use-search-to-find-name)
      (setq repo (or repo (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos repo t))))))
  (let* ((prompt (or prompt "Search Code:  "))
         (consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-code prompt #'consult-gh--search-code-builder initial)))
    (setq consult-gh--open-files-list nil)
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (funcall consult-gh-code-action sel))))

;;;###autoload
(defun consult-gh-find-file (&optional repo branch initial noaction prompt)
  "Interactively find files of a REPO in BRANCH.

Queries the user for name of a REPO, expected format is “OWNER/REPO”
\(e.g. armindarvish/consult-gh\), then fetches all the branches on
that repo and asks the user to select one BRANCH.  Then presents the
file contents of the REPO and BRANCH for selection.

Upon selection of a candidate either
 - if NOACTION is non-nil candidate is returned
 - if NOACTION is nil     candidate is passed to `consult-gh-file-action'

INITIAL is an optional arg for the initial input in the minibuffer
\(passed as INITITAL to `consult-read'\)."
  (interactive)
  (setq consult-gh--open-files-list nil
        consult-gh--current-tempdir (consult-gh--tempdir))
  (let* ((prompt (or prompt "Select File: "))
         (repo (or repo (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos repo t)))))
         (branch (or branch (format "%s" (cdr (consult-gh--read-branch repo)))))
         (candidates (mapcar #'consult-gh--file-format (consult-gh--files-nodirectory-items repo branch)))
         (sel (consult-gh-with-host
               (consult-gh--auth-account-host)
               (consult--read candidates
                              :prompt prompt
                              :lookup #'consult--lookup-member
                              :state (funcall #'consult-gh--file-state)
                              :require-match t
                              :annotate (lambda (cand) (funcall (consult-gh--file-annotate) candidates cand))
                              :history t
                              :sort nil
                              :add-history (concat (consult-gh--get-split-style-character) (thing-at-point 'filename))
                              :history 'consult-gh--files-history
                              :category 'consult-gh-files
                              :preview-key consult-gh-preview-key
                              :initial initial))))

    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))

    (if noaction
        sel
      (funcall consult-gh-file-action sel))))

(defun consult-gh--search-dashboard-transform (cands)
  "Add annotation to dashboard CANDS in `consult-gh-dashboard'.

Format each candidate with `dashboard-format'."
  (cl-loop for cand in cands
           collect (if (and (stringp cand) (not (string-empty-p cand)))
                       (consult-gh--dashboard-format cand))))

(defun consult-gh--dashboard-issues-assigned-builder (input)
  "Find all the Issues/PRs assigned to `consult-gh--get-current-username'.

INPUT is passed as extra arguments to “gh search issues”."
  (pcase-let* ((cmd
                (append consult-gh-args (list "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--assignee" "@me" "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url,commentsCount" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" "{{.commentsCount}}" "\t"
 (format "Assigned to %s" (or (consult-gh--get-current-username) "me")) "\n" "{{end}}"))))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-dashboard-maxnum)))))
    (cons (append cmd opts (remove nil (list arg))) nil)))

(defun consult-gh--dashboard-issues-authored-builder (input)
  "Find all the Issues/PRs authored by `consult-gh--get-current-username'.

INPUT is passed as extra arguments to “gh search issues”."
  (pcase-let* ((cmd
                (append consult-gh-args (list "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--include-prs" "--author" "@me" "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url,commentsCount" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" "{{.commentsCount}}" "\t" (format "Authored by %s" (or (consult-gh--get-current-username) "me")) "\n" "{{end}}"))))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-dashboard-maxnum)))))
    (cons (append cmd opts (remove nil (list arg))) nil)))

(defun consult-gh--dashboard-issues-mentions-builder (input)
  "Find all the Issues/PRs that mentions `consult-gh--get-current-username'.

INPUT is passed as extra arguments to “gh search issues”."
  (pcase-let* ((cmd
                (append consult-gh-args (list "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--include-prs" "--mentions" "@me" "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url,commentsCount" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" "{{.commentsCount}}" "\t" (format "Mentions %s" (or (consult-gh--get-current-username) "me")) "\n" "{{end}}"))))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-dashboard-maxnum)))))
    (cons (append cmd opts (remove nil (list arg))) nil)))

(defun consult-gh--dashboard-issues-involves-builder (input)
  "Find all the Issues/PRs that involves `consult-gh--get-current-username'.

INPUT is passed as extra arguments to “gh search issues”."
  (pcase-let* ((user (consult-gh--get-current-username))
               (cmd
                (append consult-gh-args (list "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--include-prs" "--involves" "@me" "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url,commentsCount" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" "{{.commentsCount}}" "\t" (format "Involves %s" (or user "me")) "\n" "{{end}}") "--" (concat "-author:" (or user "@me")))))
               (`(,arg . ,opts) (consult-gh--split-command input))
               (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (setq opts (append opts (list "--limit" (format "%s" consult-gh-dashboard-maxnum)))))
    (cons (append cmd opts (remove nil (list arg))) nil)))

(defvar consult-gh--dashboard-assigned-to-user
  (list :name "Assigned to me"
        :narrow ?a
        :async (consult--process-collection #'consult-gh--dashboard-issues-assigned-builder
                 :transform (consult--async-transform #'consult-gh--search-dashboard-transform)
                 :min-input 0)
        :async-wrap (lambda (sink) (lambda (action) (if (stringp action) (funcall sink (propertize action 'consult--force t))
                                                      (funcall sink action))))
        :group #'consult-gh--dashboard-group
        :state #'consult-gh--dashboard-state
        :require-match t
        :category 'consult-gh-issues
        :preview-key consult-gh-preview-key
        :sort t)
"Source for dashboard items assigned to user.")

(defvar consult-gh--dashboard-authored-by-user
  (list :name "Authored by me"
        :narrow ?w
        :async (consult--process-collection #'consult-gh--dashboard-issues-authored-builder
                 :transform (consult--async-transform #'consult-gh--search-dashboard-transform)
                 :min-input 0)
        :async-wrap (lambda (sink) (lambda (action) (if (stringp action) (funcall sink (propertize action 'consult--force t))
                                                      (funcall sink action))))
        :group #'consult-gh--dashboard-group
        :state #'consult-gh--dashboard-state
        :require-match t
        :category 'consult-gh-issues
        :preview-key consult-gh-preview-key
        :sort t)
"Source for dashboard items authored by user.")

(defvar consult-gh--dashboard-mentions-user
  (list :name "Mentions me"
        :narrow ?m
        :async (consult--process-collection #'consult-gh--dashboard-issues-mentions-builder
                 :transform (consult--async-transform #'consult-gh--search-dashboard-transform)
                 :min-input 0)
        :async-wrap (lambda (sink) (lambda (action) (if (stringp action) (funcall sink (propertize action 'consult--force t))
                                                      (funcall sink action))))
        :group #'consult-gh--dashboard-group
        :state #'consult-gh--dashboard-state
        :require-match t
        :category 'consult-gh-issues
        :preview-key consult-gh-preview-key
        :sort t)
"Source for dashboard items that mentions user.")

(defvar consult-gh--dashboard-involves-user
  (list :name "Involves me"
        :narrow ?i
        :async (consult--process-collection #'consult-gh--dashboard-issues-involves-builder
                 :transform (consult--async-transform #'consult-gh--search-dashboard-transform)
                 :min-input 0)
        :async-wrap (lambda (sink) (lambda (action) (if (stringp action) (funcall sink (propertize action 'consult--force t))
                                                      (funcall sink action))))
        :group #'consult-gh--dashboard-group
        :state #'consult-gh--dashboard-state
        :require-match t
        :category 'consult-gh-issues
        :preview-key consult-gh-preview-key
        :sort t)
"Source for dashboard items that involves user.")

(defun consult-gh--dashboard (prompt &optional initial)
  "Search current user's work on GitHub.

This is a non-interactive internal function.
For the interactive version see `consult-gh-dashboard'.

This searches relevant (e.g. mentioned, authored, review-requested, etc.)
issues and pull-requests for the `consult-gh--get-current-username'.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (consult--multi '(consult-gh--dashboard-assigned-to-user
                     consult-gh--dashboard-mentions-user
                     consult-gh--dashboard-involves-user
                     consult-gh--dashboard-authored-by-user)
                   :prompt prompt
                   :group #'consult-gh--dashboard-group
                   :history '(:input consult-gh--dashboard-history)
                   :sort nil
                   :initial initial)))

;;;###autoload
(defun consult-gh-dashboard (&optional initial noaction prompt)
  "Search GitHub for current user's work on GitHub.

This is an interactive wrapper function around
`consult-gh--dashboard'.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-issue-action'

INITIAL is an optional arg for the initial input in the minibuffer.

If PROMPT is non-nil, use it as the query prompt."
  (interactive)
  (let* ((prompt (or prompt "Search Dashboard:  "))
         (sel (consult-gh--dashboard prompt initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0  :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (and (stringp sel) (funcall consult-gh-dashboard-action sel)))))

(defun consult-gh--notifications-items ()
  "Find all the user's notifications."
  (let* ((notifications (string-split (apply #'consult-gh--command-to-string (funcall consult-gh-notifications-args-func)) "\n\\|\r" t)))
    (cl-delete-duplicates (delq nil (mapcar (lambda (string) (consult-gh--notifications-format string))
                                            notifications)))))

(defun consult-gh--notifications (prompt &optional initial)
  "Search GitHub issues asynchronously.

This is a non-interactive internal function.
For the interactive version see `consult-gh-search-issues'.

This runs the command line from `consult-gh--search-issues-builder' in
an async process and returns the results \(list of search results
for the input\) as a completion table in minibuffer.  The completion table
gets dynamically updated as the user types in the minibuffer.
Each candidate is formatted by `consult-gh--search-issues-transform'
to add annotation and other info to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  BUILDER an async builder function passed to `consult--process-collection'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (if-let ((candidates (consult-gh--notifications-items)))
       (consult--read
        candidates
        :prompt prompt
        :lookup #'consult--lookup-member
        :state (funcall #'consult-gh--notifications-state)
        :initial initial
        :group #'consult-gh--notifications-group
        :require-match t
        :history 'consult-gh--notifications-history
        :category 'consult-gh-notifications
        :preview-key consult-gh-preview-key
        :sort nil)
     (progn (message "No new notifications!") nil))))

;;;###autoload
(defun consult-gh-notifications (&optional initial noaction prompt)
  "Search GitHub for User's work on GitHub.

This is an interactive wrapper function around
`consult-gh--dashboard'.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-issue-action'

INITIAL is an optional arg for the initial input in the minibuffer.

If PROMPT is non-nil, use it as the query prompt."
  (interactive)
  (let* ((prompt (or prompt "Select Notification:  "))
         (sel (consult-gh--notifications prompt initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list reponame))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list username))
    (if noaction
        sel
      (and (stringp sel) (funcall consult-gh-notifications-action sel)
           (consult-gh--notifications-mark-as-read sel)))))

;;;###autoload
(defun consult-gh-topics-comment-create (&optional topic)
  "Interactively create a new comment on TOPIC.

TOPIC is a string with properties that identify a topic to comment on.
For and example, see the buffer-local variable `consult-gh--topic' in the
buffer generated by `consult-gh--pr-view' or `consult-gh--issue-view'.
TOPIC defaults to `consult-gh--topic', and if that is also nil,
then the user is asked to chose the TOPIC interactively."
  (interactive "P")
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or topic consult-gh--topic))
          (newtopic (substring-no-properties topic))
          (repo (or (and (stringp topic) (get-text-property 0 :repo topic))
                    (get-text-property 0 :repo (consult-gh-search-repos nil t))))
          (type (or (and (stringp topic) (get-text-property 0 :type topic))
                    (consult--read  (list (cons "Issues" "issue") (cons "Pull Requests" "pr"))
                                    :prompt "What topic are you looking for? "
                                    :lookup #'consult--lookup-cdr
                                    :require-match t
                                    :sort nil)))
          (add-to-review (if (get-text-property 0 :review-buffer topic)
                             (consult--read '(("Add comments to the existing review draft" . t)
                                              ("Submit a separate single comment" . nil))
                                            :prompt "There is an existing review draft for this PR.  What do you want to do?"
                                            :lookup #'consult--lookup-cdr
                                            :require-match t
                                            :sort nil)))
          (number (and (stringp topic) (get-text-property 0 :number topic)))
          (options (append (and (not add-to-review) (list (cons "The Issue/Pull request itself." type)))
                           (and (plist-get (get-text-property (point) :consult-gh) :file)
                                (list (cons "The file at point." "file")))
                           (and (plist-get (get-text-property (point) :consult-gh) :code)
                                (list (cons "The line of code at pont." "code")))
                           (and (not add-to-review) (plist-get (get-text-property (point) :consult-gh) :comment-id)
                                (list (cons "Reply to the comment at point." "reply")))))

          (target (and number
                       (if (length> options 1)
                           (consult--read options
                                          :prompt "What do you want to comment on?"
                                          :lookup #'consult--lookup-cdr
                                          :require-match t
                                          :sort nil)
                         (cdar options))))
          (buffer (concat (format "*consult-gh-comment: %s - %s #%s %s" repo type number target)
                          (and (equal target "code") (number-to-string (point))))))


     (add-text-properties 0 1 (text-properties-at 0 topic) newtopic)
     (add-text-properties 0 1 (list :new t :target target :isComment t :type "comment") newtopic)

     (when topic
       (cond
        ((member target '("pr" "issue" "discussion" "topic")))
        ((equal target "code")
         (let* ((info (get-text-property (point) :consult-gh))
                (pos (consult-gh--get-line-and-side-inside-diff))
                (side (car pos))
                (line (cadr pos))
                (startside (caddr pos))
                (startline (cadddr pos))
                (snippet (if (region-active-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (buffer-substring-no-properties
                            (pos-bol) (pos-eol)))))
           (if (not line)
               (error "Cannot comment on that line")
             (progn
               (setq info (append info (list :line line :side side :startline startline :startside startside :snippet snippet :subject-type "line")))
               (if add-to-review (setq target "review"))
               (add-text-properties 0 1 (list :comment-info info :target target) newtopic)))))
        ((equal target "file")
         (let ((info (get-text-property (point) :consult-gh)))
           (when add-to-review
             (if (y-or-n-p "Currently, there is no support for comments on files inside a review.  But, you can submit a comment on this file as an independent review.  Do you want to proceed? ")
                 (progn (setq target "review-comment")
                        (add-text-properties 0 1 (list :comment-info (append info (list :subject-type "file")) :target target) newtopic))
               (error "Canceled!")))))
        ((equal target "reply")
         (if-let (info (get-text-property (point) :consult-gh))
             (add-text-properties 0 1 (list :comment-info info) newtopic)))))
     (cond
      (topic
       (funcall consult-gh-pop-to-buffer-func (consult-gh-topics--get-buffer-create buffer "comment" newtopic)))
      (type
       (pcase type
         ("issue"
          (funcall #'consult-gh--issue-view-action (consult-gh-issue-list repo t)))
         ("pr"
          (funcall #'consult-gh--pr-view-action (consult-gh-pr-list repo t)))))
      (t (message "Did not get a topic to comment on!"))))))

;;;###autoload
(defun consult-gh-topics-cancel ()
  "Cancel the current topic or comment."
  (interactive nil consult-gh-topics-edit-mode)
  (funcall consult-gh-quit-window-func t nil)
  (message "Canceled!"))

;;;###autoload
(defun consult-gh-topics-submit (&optional topic)
  "Submit TOPIC.

TOPIC is a string with properties that identify a consult-gh topic such as
a comment, issue, pull request, pull request review, etc.
For an example, see the buffer-local variable `consult-gh--topic' in the
buffer generated by commands such as `consult-gh-pr-create',
`consult-gh-issue-create', `consult-gh--pr-view', `consult-gh--issue-view',
`consult-gh-pr-review', or `consult-gh-topics-comment-create'.
TOPIC defaults to `consult-gh--topic', and if that is also nil,
then the user is asked to chose the TOPIC interactively."
  (interactive "P" consult-gh-topics-edit-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (if consult-gh-topics-edit-mode
       (let* ((topic (or topic consult-gh--topic))
              (repo (get-text-property 0 :repo topic))
              (type (get-text-property 0 :type topic))
              (isComment (get-text-property 0 :isComment topic))
              (target (get-text-property 0 :target topic))
              (number (get-text-property 0 :number topic))
              (new (get-text-property 0 :new topic)))
         (cond
          ((and isComment (member target '("issue" "pr" "discussion" "topic")))
           (let ((comment (consult-gh-topics--buffer-string)))
             (and (consult-gh-topics--topic-comment-submit comment repo target number)
                  (message "Comment Submitted!")
                  (funcall consult-gh-quit-window-func t))))
          ((and isComment (equal target "code"))
           (let* ((comment (consult-gh-topics--buffer-string))
                  (info (get-text-property 0 :comment-info topic))
                  (commit-id (plist-get info :commit-id))
                  (path (plist-get info :path))
                  (line (plist-get info :line))
                  (side (plist-get info :side))
                  (startline (plist-get info :startline))
                  (startside (plist-get info :startside)))
             (and (consult-gh-topics--code-comment-submit comment repo number commit-id path line side startline startside)
                  (message "Comment Submitted!")
                  (funcall consult-gh-quit-window-func t))))
          ((and isComment (equal target "file"))
           (let* ((comment (consult-gh-topics--buffer-string))
                  (info (get-text-property 0 :comment-info topic))
                  (commit-id (plist-get info :commit-id))
                  (path (plist-get info :path)))
             (and (consult-gh-topics--file-comment-submit comment repo number commit-id path)
                  (message "Comment Submitted!")
                  (funcall consult-gh-quit-window-func t))))
          ((and isComment (equal target "reply"))
           (let* ((comment (consult-gh-topics--buffer-string))
                  (info (get-text-property 0 :comment-info topic))
                  (comment-id (plist-get info :comment-id))
                  (reply-url (plist-get info :reply-url)))
             (and (consult-gh-topics--reply-comment-submit comment comment-id reply-url)
                  (message "Comment Submitted!")
                  (funcall consult-gh-quit-window-func t))))
          ((and isComment (equal target "review-comment"))
           (when (y-or-n-p "This will submit this one comment as a pull request review.  Do you want to proceed?")
           (consult-gh-topics--pr-review-comment-submit topic)))
          ((and isComment (equal target "review"))
           (consult-gh-topics--pr-review-append-comment topic))
          ((and new (equal target "review"))
           (consult-gh-topics--pr-review-presubmit topic))
          ((and new (equal type "issue"))
           (consult-gh-topics--issue-create-presubmit topic))
          ((and (not new) (equal type "issue"))
           (consult-gh-topics--edit-issue-presubmit topic))
          ((and new (equal type "pr"))
           (consult-gh-topics--pr-create-presubmit topic))
          ((and (not new) (equal type "pr"))
           (consult-gh-topics--edit-pr-presubmit topic))
          ((and new (equal type "merge commit"))
           (consult-gh-topics--pr-merge-presubmit topic))))
     (message "Not in a consult-gh topic editing buffer!"))))

;;;###autoload
(defun consult-gh-topics-open-in-browser (&optional topic)
  "Open the TOPIC of the current buffer in the browser.

TOPIC is a text with properties that identify a consult-gh topic such as
a comment, issue, pull request, pull request review, etc.
For an example, see the buffer-local variable `consult-gh--topic' in the
buffer generated by commands such as `consult-gh-pr-create',
`consult-gh-issue-create' `consult-gh--pr-view', `consult-gh--issue-view',
`consult-gh-pr-review', or `consult-gh-topics-comment-create'.
TOPIC defaults to `consult-gh--topic'.

This funciton uses `consult-gh-browse-url-func' for opening a url in the
browser."
  (interactive "P" consult-gh-pr-view-mode consult-gh-issue-view-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (let* ((topic (or topic consult-gh--topic))
          (type (and (stringp topic) (get-text-property 0 :type topic)))
          (repo (and (stringp topic) (get-text-property 0 :repo topic)))
          (branch (and (stringp topic) (get-text-property 0 :branch topic)))
          (path (and (stringp topic) (get-text-property 0 :path topic)))
          (number (and (stringp topic) (get-text-property 0 :number topic)))
          (local-info (get-text-property (point) :consult-gh))
          (local-url (or (plist-get local-info :url)
                         (plist-get local-info :comment-url)
                         (plist-get local-info :commit-url)))
          (url (or local-url (and (stringp type) (pcase type
                                                   ("repo"
                                                    (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
                                                   ("file"
                                                    (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/blob/%s/%s" branch path)))
                                                   ("issue"
                                                    (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/issues/%s" number)))
                                                   ("pr"
                                                    (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" number))))))))
     (if (stringp url)
         (funcall (or consult-gh-browse-url-func #'browse-url) url)
       (message "No topic to browse in this buffer!")))))

;;;###autoload
(defun consult-gh-enable-default-keybindings ()
  "Enable default keybindings for all minor modes in consult-gh."
  (interactive)
  ;; consult-gh-repo-view-mode-map
  (consult-gh--enable-keybindings-alist consult-gh-repo-view-mode-map  consult-gh--repo-view-mode-keybinding-alist)

  ;; consult-gh-issue-view-mode-map
  (consult-gh--enable-keybindings-alist consult-gh-issue-view-mode-map  consult-gh--issue-view-mode-keybinding-alist)

  ;; consult-gh-pr-view-mode-map
  (consult-gh--enable-keybindings-alist consult-gh-pr-view-mode-map  consult-gh--pr-view-mode-keybinding-alist)


  ;; consult-gh-topics-edit-mode-map
  (consult-gh--enable-keybindings-alist consult-gh-topics-edit-mode-map consult-gh--topics-edit-mode-keybinding-alist))

;;;###autoload
(defun consult-gh-disable-default-keybindings ()
  "Disable default keybindings for minor modes in consult-gh."
  (interactive)
  ;; consult-gh-repo-view-mode-map
  (consult-gh--disable-keybindings-alist consult-gh-repo-view-mode-map  consult-gh--repo-view-mode-keybinding-alist)

  ;; consult-gh-issue-view-mode-map
  (consult-gh--disable-keybindings-alist consult-gh-issue-view-mode-map  consult-gh--issue-view-mode-keybinding-alist)

  ;; consult-gh-pr-view-mode-map
  (consult-gh--disable-keybindings-alist consult-gh-pr-view-mode-map  consult-gh--pr-view-mode-keybinding-alist)

  ;; consult-gh-topics-edit-mode-map
  (consult-gh--disable-keybindings-alist consult-gh-topics-edit-mode-map consult-gh--topics-edit-mode-keybinding-alist))

;;;###autoload
(defun consult-gh-refresh-view ()
  "Refresh the buffer viewing a consult-gh topic."
  (interactive nil consult-gh-pr-view-mode consult-gh-pr-issue-mode)
  (consult-gh-with-host
   (consult-gh--auth-account-host)
   (when-let* ((topic consult-gh--topic)
               (type (get-text-property 0 :type topic))
               (repo (get-text-property 0 :repo topic))
               (number (get-text-property 0 :number topic)))
     (cond
      ((equal type "repo")
       (funcall #'consult-gh--repo-view repo (current-buffer)))
      ((equal type "issue")
       (funcall #'consult-gh--issue-view repo number (current-buffer)))
      ((equal type "pr")
       (funcall #'consult-gh--pr-view repo number (current-buffer)))))))

;;;###autoload
(defun consult-gh (&rest args)
  "Convinient wrapper function for favorite interactive command.

Calls the function in `consult-gh-default-interactive-command'
and passes ARGS to it."
  (interactive)
  (apply (or consult-gh-default-interactive-command #'consult-gh-search-repos) args))

;;; provide `consult-gh' module

(provide 'consult-gh)

;;; consult-gh.el ends here
