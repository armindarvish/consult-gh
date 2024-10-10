;;; consult-gh.el --- Consulting GitHub Client -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.0
;; Package-Requires: ((emacs "29.1") (consult "1.0") (markdown-mode "2.6") (ox-gfm "1.0"))
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
;; pull erquests, codes, and etc.

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
  "Whether to hide reacd notifications?"
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-notifications-args-func #'consult-gh-notifications-make-args
  "Additional arguments for `consult-gh-notifications'.

Common options include:

 - `consult-gh-notifications-make-args' Make args to see unread notifications
 - A custom function                     A function that takes
                                         no input argument."
  :group 'consult-gh
  :type '(choice (const :tag "Default Funciton" consult-gh-notifications-make-args)
                 (function :tag "Custom Function")))

(defcustom consult-gh-browse-url-func #'browse-url
  "What function to call browsing a url?

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

(defcustom consult-gh-switch-buffer-func #'switch-to-buffer
  "What function to call when switching buffers?

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
temporary diretories."
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

(defcustom consult-gh-large-file-warning-threshold large-file-warning-threshold
  "Threshold for size of file to require confirmation for preview/open/save.

Files larger than this value in size will require user confirmation
before previewing, opening or saving the file.

Default value is set by `large-file-warning-threshold'.
If nil, no cofnirmation is required."
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

(defcustom consult-gh-repo-preview-mode nil
  "Major mode to preview repository READMEs.

Choices are:
  - \='nil            Use major-mode associated with orginal file extension
  - \='markdown-mode  Use \='markdown-mode if available
  - \='org-mode       Use \='org-mode"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Guess major mode based on file format " nil)
                 (const :tag "Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)))

(defcustom consult-gh-issue-preview-mode 'markdown-mode
  "Major mode to preview issues and pull requests.

Choices are:
  - \='nil            Use \='fundamental-mode
  - \='markdown-mode  Use \='markdown-mode when available
  - \='org-mode       Use \='org-mode"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)
                 (const :tag "Use fundamental-mode" nil)))

(defcustom consult-gh-topic-major-mode 'markdown-mode
  "Major mode for editing comments on issues or pull requests.

Choices are:
  - \='nil            Use \='text-mode
  - \='markdown-mode  Use \='markdown-mode when available
  - \='org-mode       Use \='org-mode"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)
                 (const :tag "Use text-mode" nil)))

(defcustom consult-gh-topic-use-capf t
  "Use `consult-gh--topics-edit-capf' for `completion-at-point'.

When non-nil, `consult-gh--topics-edit-capf' ia used in
`consult-gh-topic-major-mode' buffer for autocompleting
issue/pr numbers or user names."
  :group 'consult-gh
  :type '(choice (const :tag "Use autocompletion" t)
                 (const :tag "Do not use autocompletion" nil)))


(make-obsolete-variable 'consult-gh-preview-buffer-mode "Use `consult-gh-repo-preview-mode', or `consult-gh-issue-preview-mode' instead." "1.1")

(defcustom consult-gh-default-orgs-list (list)
  "List of default GitHub orgs.

This can be a list of orgs or a function returning a list"
  :group 'consult-gh
  :type '(repeat (string :tag "GitHub Organization (i.e. Username)")))

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
  "Prefix label to use for pullrequests in `consult-gh--topics-edit-capf'."
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
  "What field to use to group results in pr search?

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
  - A symbol:  loads the branch naemd in this variable.

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
                 (function :tag "Browse Brnaches and Files inside Emacs" #'consult-gh--repo-browse-files-action)
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
  "What function to call when a PR is selected?

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
  "What function to call when a code is selected?

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
  :type '(choice (const :tag "Browse the Discussion URL" consult-gh--discussion-browse-url-actionn)
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
  :type '(choice (function :tag "(Default) Search Rpositories"  consult-gh-search-repos)
                 (function :tag "List default repos of user" consult-gh-default-repos)
                 (function :tag "Open transient menu" consult-gh-transient)
                 (function :tag "Other custom interactive command")))

;;; Other Variables
(defvar consult-gh-category 'consult-gh
  "Category symbol for the `consult-gh' package.")

(defvar consult-gh-repos-category 'consult-gh-repos
  "Category symbol for repos in `consult-gh' package.")

(defvar consult-gh-issues-category 'consult-gh-issues
  "Category symbol for issues in `consult-gh' package.")

(defvar consult-gh-prs-category 'consult-gh-prs
  "Category symbol for PRs in `consult-gh' package.")

(defvar consult-gh-codes-category 'consult-gh-codes
  "Category symbol for codes in `consult-gh' package.")

(defvar consult-gh-notifications-category 'consult-gh-notifications
  "Category symbol for notifications in `consult-gh' package.")

(defvar consult-gh-orgs-category 'consult-gh-orgs
  "Category symbol for Orgs in `consult-gh' package.")

(defvar consult-gh-files-category 'consult-gh-files
  "Category symbol for files in `consult-gh' package.")

(defvar consult-gh--preview-buffers-list (list)
  "List of currently open preview buffer.")

(defvar consult-gh--orgs-history nil
  "History variable for Orgs used in `consult-gh-repo-list'.")

(defvar consult-gh--repos-history nil
  "History variable for repos.

This is used in `consult-gh-issue-list' and `consult-gh-pr-list'.")

(defvar consult-gh--notifications-history nil
  "History variable for notifications.

This is used in `consult-gh-notifications'.")

(defvar consult-gh--search-repos-history nil
  "History variable for searching repos in `consult-gh-search-repos'.")

(defvar consult-gh--search-issues-history nil
  "History variable for issues used in `consult-gh-search-issues'.")

(defvar consult-gh--search-prs-history nil
  "History variable for pull requests used in `consult-gh-search-prs'.")

(defvar consult-gh--search-code-history nil
  "History variable for pull requests used in `consult-gh-search-code'.")

(defvar consult-gh--files-history nil
  "History variable for files used in `consult-gh-find-file'.")

(defvar consult-gh--known-orgs-list nil
  "List of previously visited Orgs.")

(defvar consult-gh--known-repos-list nil
  "List of previously visited repos.")

(defvar consult-gh--open-files-list nil
  "List of currently open files.")

(defvar consult-gh--current-tempdir nil
  "Current temporary directory.")

(defvar consult-gh--async-process-buffer-name " *consult-gh-async*"
  "Name of buffer for async processes.")

(defvar consult-gh--async-log-buffer " *consult-gh-async-log*"
  "Name of buffer for logging async process  errors.")

(defvar consult-gh--current-input nil
  "Current input of user query.")

(defvar consult-gh--auth-current-account nil
  "Current logged-in and active account.
This is a list of \='(USERNAME HOST IF-ACTIVE)")

(defvar consult-gh-default-host "github.com"
  "Defualt host of GitHub.")

(defvar-local consult-gh--topic nil
  "Topic in consult-gh preview buffers.")

(defvar consult-gh--override-group-by nil
  "Override grouping based on user input.

This is used to change grouping dynamically.")

(defvar consult-gh--issue-view-json-fields "assignees,author,body,closedAt,comments,createdAt,labels,milestone,number,projectItems,state,title,updatedAt,url"
  "String of comma separated json fields to retrive for viewing issues.")

(defvar consult-gh--pr-view-json-fields "additions,assignees,author,baseRefName,body,closedAt,comments,commits,createdAt,deletions,files,headRefName,headRepository,headRepositoryOwner,labels,mergeable,milestone,number,projectItems,reviewDecision,reviewRequests,reviews,state,statusCheckRollup,title,updatedAt,url"
  "String of comma separated json fields to retrive for viewing prs.")

;;; Faces
(defface consult-gh-success-face
  `((t :inherit 'success))
  "The face used to show issues or PRS that are successfully dealt with.

\(e.g. “closed” issues or “merged” PRS)\ when listing or searching
issues and PRS with `consult-gh'.

By default inherits from `success'.")

(defface consult-gh-warning-face
  `((t :inherit 'warning))
  "The face to show currently open issues or PRS.

By default inherits from `warning'.")

(defface consult-gh-error-face
  `((t :inherit 'error))
  "The face to show closed PRS.

By default inherits from `error'.")

(defface consult-gh-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "Highlight match face in preview buffers.

By default, inherits from `consult-highlight-match'.")

(defface consult-gh-preview-match-face
  `((t :inherit 'consult-preview-match))
  "Highlight match face in preview buffers.

 By default, inherits from `consult-preview-match'.
This face is for example used to highlight the matches to the user's
search queries \(e.g. when using `consult-gh-search-repos')\ or
code snippets \(e.g. when using `consult-gh-search-code')\ in preview buffer.")

(defface consult-gh-default-face
  `((t :inherit 'default))
  "Default face in minibuffer annotations.

By default, inherits from `default'.")

(defface consult-gh-user-face
  `((t :inherit 'font-lock-constant-face))
  "User face in minibuffer annotations.

By default, inherits from `font-lock-constant-face'.")

(defface consult-gh-package-face
  `((t :inherit 'font-lock-type-face))
  "Packageface in minibuffer annotations.

By default, inherits from `font-lock-type-face'.")

(defface consult-gh-repo-face
  `((t :inherit 'font-lock-type-face))
  "Repository face in minibuffer annotations.

By default, inherits from `font-lock-type-face'.")

(defface consult-gh-issue-face
  `((t :inherit 'warning))
  "Issue number face in minibuffer annotations.

By default, inherits from `warning'.")

(defface consult-gh-pr-face
  `((t :inherit 'warning))
  "Pull request number face in minibuffer annotations.

By default, inherits from `warning'.")


(defface consult-gh-branch-face
  `((t :inherit 'font-lock-string-face))
  "Branch face in minibuffer annotations.

By default, inherits from `font-lock-string-face'.")

(defface consult-gh-visibility-face
  `((t :inherit 'font-lock-warning-face))
  "Visibility face in minibuffer annotations.

By default, inherits from `font-lock-warning-face'.")

(defface consult-gh-date-face
  `((t :inherit 'font-lock-keyword-face))
  "Date face in minibuffer annotations.

By default, inherits from `font-lock-keyword-face'.")

(defface consult-gh-tags-face
  `((t :inherit 'font-lock-comment-face))
  "Tags/Comments face in minibuffer annotations.

By default, inherits from `font-lock-comment-face'.")

(defface consult-gh-description-face
  `((t :inherit 'font-lock-builtin-face))
  "Repository description face in minibuffer annotations.

By default, inherits from `font-lock-builtin-face'.")

(defface consult-gh-code-face
  `((t :inherit 'font-lock-variable-use-face))
  "Code snippets face in minibuffer annotations.

By default, inherits from `font-lock-vairable-use-face'.")

(defface consult-gh-url-face
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
                                    'consult-gh-highlight-match-face nil str))
          (setq m (cddr m))))))
  str)

(defun consult-gh--whole-buffer-string (&optional buffer)
  "Get whole content of the BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
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
          (while (re-search-backward "^\\[\\([^fn].*\\)\\]:" nil t)
            (replace-match "[fn:\\1] ")))))
    nil))

(defun consult-gh--markdown-to-org-emphasis (&optional buffer)
  "Convert markdown style emphasis to \='org-mode style emphasis in BUFFER.

Uses simple regexp replacements."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^-\\{2\\}$" nil t)
            (delete-char -2)
            (insert "-----\n")
            (while (re-search-backward "\\(^[a-zA-Z]+:[[:blank:]]\\)" nil t)
              (replace-match "#+\\1" nil nil)))
          (while (re-search-forward "#\\|\\*\\{1,2\\}\\(?1:.+?\\)\\*\\{1,2\\}\\|_\\{1,2\\}\\(?2:.+?\\)_\\{1,2\\}\\|`\\(?3:[^`].+?\\)`\\|```\\(?4:.*\n\\)\\(?5:[[:ascii:][:nonascii:]]*?\\)```" nil t)
            (pcase (match-string-no-properties 0)
              ("#" (if (looking-at "#\\|[[:blank:]]")
                       (progn
                         (delete-char -1)
                         (insert "*"))))

              ((pred (lambda (el) (string-match-p "\\*\\{1\\}[^\\*]*?\\*\\{1\\}" el)))
               (replace-match "/\\1/"))

              ((pred (lambda (el) (string-match-p "\\*\\{2\\}.+?\\*\\{2\\}" el)))
               (replace-match "*\\1*"))

              ((pred (lambda (el) (string-match-p "_\\{1\\}[^_]*?_\\{1\\}" el)))
               (replace-match "/\\2/"))

              ((pred (lambda (el) (string-match-p "_\\{2\\}.+?_\\{2\\}" el)))
               (replace-match "*\\2*"))

              ((pred (lambda (el) (string-match-p "`[^`].+?`" el)))
               (replace-match "=\\3="))

              ((pred (lambda (el) (string-match-p "```.*\n[[:ascii:][:nonascii:]]*```" el)))
               (replace-match "#+begin_src \\4\n\\5\n#+end_src\n")))))))
    nil))

(defun consult-gh--markdown-to-org-links (&optional buffer)
  "Convert arkdown links to \='org-mode links in BUFFER.

Uses simple regexp replacements."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-min))
          (while (re-search-forward "\\[\\(?1:.+?\\)\\]\\[\\]\\{1\\}\\|\\[\\(?2:.[^\\[]+?\\)\\]\\[\\(?3:.[^\\[]+?\\)\\]\\{1\\}\\|\\[\\(?4:.+?\\)\\]\(#\\(?5:.+?\\)\)\\{1\\}\\|.\\[\\(?6:.+?\\)\\]\(\\(?7:[^#].+?\\)\)\\{1\\}\\|\\[\\(?8:.+?\\)\\]\(\\(?9:.+?\\)\)\\{1\\}" nil t)
            (pcase (match-string-no-properties 0)
              ((pred (lambda (el) (string-match-p "\\[.+?\\]\\[\\]\\{1\\}" el)))
               (replace-match "[fn:\\1]"))

              ((pred (lambda (el) (string-match-p "\\[.[^\\[]+?\\]\\[.[^\\[]+?\\]\\{1\\}" el)))
               (replace-match "\\2 [fn:\\3]"))

              ((pred (lambda (el) (string-match-p "\\[.+?\\]\(#.+?\)\\{1\\}" el)))
               (replace-match "[[*\\5][\\4]]"))

              ((pred (lambda (el) (string-match-p "!\\[.*\\]\([^#].*\)" el)))
               (replace-match "[[\\7][\\6]]"))

              ((pred (lambda (el) (string-match-p "[[:blank:]]\\[.*\\]\([^#].*\)" el)))
               (replace-match " [[\\7][\\6]]"))

              ((pred (lambda (el) (string-match-p "\\[.+?\\]\(.+?\)\\{1\\}" el)))
               (replace-match "[[*\\9][\\8]]"))))

          (goto-char (point-min))
          (while
              (re-search-forward
               "\\[fn:\\(.+?\\)\\]\\{1\\}" nil t)
            (pcase (match-string 0)
              ((pred (lambda (el) (string-match-p "\\[fn:.+?[[:blank:]].+?\\]\\{1\\}" (substring-no-properties el))))
               (progn
                 (replace-regexp-in-region "[[:blank:]]" "_" (match-beginning 1) (match-end 1)))))))))
    nil))

(defun consult-gh--markdown-to-org (&optional buffer is-issue)
  "Convert from markdown format to \='org-mode format in BUFFER.

This is used for viewing repos \(a.k.a. fetching README file of repos\)
or issue, when `consult-gh-repo-preview-mode' or
`consult-gh-issue-preview-mode'  is set to \='org-mode.

when IS-ISSUE is non-nil sets 1 heading per comment."
  (let ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name))))
    (with-current-buffer buffer
      (consult-gh--markdown-to-org-footnotes buffer)
      (consult-gh--markdown-to-org-emphasis buffer)
      (consult-gh--markdown-to-org-links buffer)
      (when is-issue (consult-gh--markdown-to-org-comment-headings buffer))
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
etc. in org format."
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

(defun consult-gh-topics--buffer-string ()
  "Get buffer string for consult-gh-topics."
  (if (and (derived-mode-p 'org-mode))
      (consult-gh--org-to-markdown)
    (consult-gh--whole-buffer-string)))

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
  "Log FORMATTED ARGS to variable `consult-gh--async-log-buffer'."
  (with-current-buffer (get-buffer-create consult-gh--async-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format formatted args))))

(defun consult-gh--make-process (name &optional filter-func &rest args)
  "Make asynchron process with NAME and pass ARGS to “gh” program.

This command runs gh program asynchronously and applies the FILTER-FUNC
to the output.
NAME is passed as :name t `make-process'.
FILTER-FUNC is passed as :filter to `make-process'."
  (if (executable-find "gh")
      (progn
        (when-let ((proc (get-process name)))
          (delete-process proc))
        (let* ((args (append (list "gh") args))
               (proc-buf (generate-new-buffer consult-gh--async-process-buffer-name))
               (filter (or (and (functionp filter-func)
                                filter-func)
                           (lambda (_ str) str)))
               (proc-sentinel
                `(lambda (_ event)
                   (if (string-prefix-p "finished" event)
                       (with-current-buffer ,proc-buf
                         (widen)
                         (funcall ,filter nil (buffer-string))
                         (erase-buffer))
                     (message
                      "consult-gh--async-process sentinel: event=%s"
                      (string-trim event)))
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
          (consult-gh--async-log "consult-gh--make-process started %S\n" args)
          (make-process :name name
                        :buffer proc-buf
                        :noquery t
                        :command args
                        :connection-type 'pipe
                        :sentinel proc-sentinel)
          nil))
    (progn
      (message (propertize "\"gh\" is not found on this system" 'face 'warning))
      nil)))

;;; Backend `gh` related functions
(defun consult-gh--call-process (&rest args)
  "Run “gh” in the command line and passes ARGS as command-line arguments.

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

If there are errors passes them to *Messages*.
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

(defun consult-gh--json-to-hashtable (json &optional key)
  "Convert a JSON object to a hash table.

Uses lists for arrays and symbols for keys.
If optional argument KEY is non-nil, returns only the value of KEY."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'keyword)
        (json-false :false))
    (if key
        (gethash key (json-read-from-string json))
      (json-read-from-string json))))

(defun consult-gh--get-current-username ()
  "Get the currently logged in user.

Runs “gh api user” and returns the login field of json data."
  (consult-gh--json-to-hashtable (cadr (consult-gh--api-get-json "user")) :login))

(defun consult-gh--get-current-orgs (&optional include-user)
  "Get the organizations for currently logged in user.

Runs “gh api user/orgs” and returns the login field of json data.
When INCLUDE-USER is non-nil, add the name of the user the list."
  (let ((data (consult-gh--api-get-json "user/orgs")))
    (if (eq (car data) 0)
        (let ((table (consult-gh--json-to-hashtable (cadr data))))
          (cond
           ((listp table)
            (append (mapcar (lambda (tab) (gethash :login tab)) table)
                    (if include-user (list (consult-gh--get-current-username)))))
           ((hash-table-p table)
            (append (list (gethash :login table))
                    (if include-user (list (consult-gh--get-current-username)))))
           (t (if include-user (list (consult-gh--get-current-username)))))))))

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
  "List template repository for authenticated user.

If the optional argument USER is non-nil, gets repos of User instead."
  (let ((endpoint (if user (format "users/%s/repos" user) "user/repos")))
    (delq nil (mapcar (lambda (item) (when (eq (gethash :is_template item) t)
                                       (gethash :full_name item)))
                      (consult-gh--json-to-hashtable
                       (cadr
                        (consult-gh--api-get-json endpoint)))))))

(defun consult-gh--get-repo-from-directory (&optional dir)
  "Return the full name of the GitHub repository in current directory.

If optional arg DIR is nil, use DIR instead of current directory.
Formats the output as “[HOST/]OWNER/REPO” if any, otherwise returns nil."
  (let* ((default-directory (or dir default-directory))
         (response (consult-gh--call-process "repo" "view" "--json" "nameWithOwner" "--jq" ".nameWithOwner")))
    (if (eq (car response) 0)
        (if (not (string-empty-p (cadr response)))
            (string-trim (cadr response))
          nil)
      nil)))

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
  "Parse DIFF to extract sections per file.

Returns an alist with key value pairs of (file . diff)"
  (let ((chunks nil)
        (p nil))
    (with-temp-buffer
      (save-match-data
        (insert diff)
        (goto-char (point-max))
        (setq p (point))
        (while (re-search-backward "^--- \\(?1:.*\\)\n\\+\\+\\+ \\(?2:.*\\)\n" nil t)
          (let ((filea (match-string 1))
                (fileb (match-string 2))
                (start (or (match-end 2) (point)))
                (file nil))
            (when filea
              (if (equal filea "/dev/null") (setq filea nil) (setq filea (string-trim-left filea "a/"))))
            (when fileb
              (if (equal fileb "/dev/null") (setq fileb nil) (setq fileb (string-trim-left fileb "b/"))))
            (cond
             ((and filea fileb (setq file (concat "modified\t" filea))))
             (fileb (setq file (concat "new file\t" fileb)))
             (filea (setq file (concat "deleted\s\t" filea))))
            (when (looking-at "---") (push (list file (buffer-substring start p)) chunks))
            (re-search-backward "diff --git" nil t)
            (setq p (point))))))
    chunks))

(defun consult-gh--group-function (cand transform &optional group-by)
  "Group candidates by GROUP-BY keyword.

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
  "Get a table of assignbale users for REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "assignableUsers"))
         (table (and (stringp json)  (consult-gh--json-to-hashtable json :assignableUsers))))
    (and table (listp table) (mapcar (lambda (item) (gethash :login item)) table))))

(defun consult-gh--get-mentionable-users (repo)
  "Get a table of mentionable users for REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "mentionableUsers"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :mentionableUsers))))
    (and table (listp table) (mapcar (lambda (item) (gethash :login item)) table))))

(defun consult-gh--get-labels (repo)
  "Get a list of labels for REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "labels"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :labels))))
    (and table (listp table) (mapcar (lambda (item) (gethash :name item)) table))))

(defun consult-gh--get-milestones (repo)
  "Get a list of milestones for REPO."
  (let* ((json (consult-gh--command-to-string "repo" "view" repo "--json" "milestones"))
         (table (and (stringp json) (consult-gh--json-to-hashtable json :milestones))))
    (and table (listp table) (mapcar (lambda (item) (gethash :title item)) table))))

(defun consult-gh--get-projects (repo)
  "Get a list of projects for REPO."
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

;;; Backend functions for `consult-gh'.

;; Buffers
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
                                  `(lambda (_ out)
                                     (add-text-properties 0 1 (list :issues (consult-gh--completion-get-issue-list out)) ,topic))
                                  "issue" "list" "--repo" repo "--state" "all" "--limit" consult-gh-completion-max-items "--search" "sort:updated" "--json" "number,title"))))

(defun consult-gh--completion-set-prs (&optional topic repo)
  "Make async process to get list of pullrequests of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."
  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic))))
    (consult-gh--make-process "consult-gh-pr-list"
                              `(lambda (_ out)
                                 (add-text-properties 0 1 (list :prs (consult-gh--completion-get-issue-list out)) ,topic))
                              "pr" "list" "--repo" repo "--state" "all" "--search" "sort:updated" "--limit" consult-gh-completion-max-items "--json" "number,title")))

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
                              `(lambda (_ out)
                                 (add-text-properties 0 1 (list :mentionable-users (consult-gh--completion-get-mentionable-users-list out))
                                                      ,topic))
                              "repo" "view" repo "--json" "mentionableUsers")))

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
                              `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :assignable-users (consult-gh--completion-get-assignable-users-list out))
                                                      ,topic))
                              "repo" "view" repo "--json" "assignableUsers")))

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
                              `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-labels (consult-gh--completion-get-labels-list out))
                                                      ,topic))
                              "repo" "view" repo "--json" "labels")))

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
                              `(lambda (_ out)
                                 (add-text-properties 0 1 (list
                                                           :valid-milestones (consult-gh--completion-get-milestones-list out))
                                                      ,topic))
                              "repo" "view" repo "--json" "milestones")))

(defun consult-gh--completion-get-projects-list (string)
  "Filter function to parse STRING, json output of “gh view repo”.

This is a filter function suitable for passing to
`consult-gh--make-process'."
  (mapcar (lambda (item) (and (hash-table-p item) (gethash :title item))) (gethash :Nodes (consult-gh--json-to-hashtable string :projectsV2))))

(defun consult-gh--completion-set-valid-projects (&optional topic repo)
  "Make async process to get list of milestones of REPO in TOPIC.

When TOPIC is nil, uses buffer-local variable `consult-gh--topic'."

  (let* ((topic (or topic consult-gh--topic))
         (repo (or repo (get-text-property 0 :repo topic)))
         (projectsEnabled (gethash :hasProjectsEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasProjectsEnabled")))))
    (if (and (eq projectsEnabled 't) (member "read:project" (consult-gh--auth-get-token-scopes)))
        (consult-gh--make-process "consult-gh-valid-projects"
                                  `(lambda (_ out)
                                     (add-text-properties 0 1 (list
                                                               :valid-projects (ignore-errors (consult-gh--completion-get-projects-list out)))
                                                          ,topic))
                                  "repo" "view" repo "--json" "projectsV2"))))

(defun consult-gh--topics-edit-capf ()
  "Complettion at point for editing comments.

Completes for issue/pr numbers or user names."
  (save-match-data
    (when (and (consult-gh-topics-edit-mode) (or (looking-back "@[^[:space:]]*?" nil) (looking-back "#[^[:space:]]*?\\|#[^[:digit:]]+.*?" nil)))
      (let* ((begin (save-excursion (if (looking-back "@[^[:space:]]*?\\|#.*?" nil)
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
                               (when (looking-back "#\\|@" nil) (delete-char -1))
                               (insert (or (car (split-string str "\t" t)) str)))

              :exclusive 'no
              :category 'string)))))

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
         (host (or host consult-gh-default-host)))
    (car-safe (seq-filter (lambda (acc) (and (equal (cadr acc) host) (caddr acc) acc)) accounts))))

(setq consult-gh--auth-current-account (consult-gh--auth-current-active-account))

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
      (run-hook-with-args 'consult-gh-auth-post-switch-hook host user)
      (message str)))
  (message "HOST and USER need to be provided as strings.")))

(defun consult-gh--auth-get-token-scopes (&optional account host)
  "Return a list token scopes for ACCOUNT on HOST."
  (let* ((account (or account (consult-gh--auth-current-active-account host)))
         (account-name (car account))
         (account-host (cadr account))
         (str (consult-gh--command-to-string "auth" "status")))
    (print account-name)
    (when
        (string-match (format "Logged in to %s account %s [[:ascii:][:nonascii:]]*?Token scopes: \\(.+\\)?" account-host account-name) str)
      (let ((m (match-string 1 str)))
        (and (stringp m) (split-string m ", " t "['\s\n\t]"))))))

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

(defun consult-gh--read-branch (repo)
  "Query the user to select a branch of REPO.

REPO must be  a Github repository full name
for example “armindarvish/consult-gh”."
  (pcase consult-gh-default-branch-to-load
    ('confirm
     (if (y-or-n-p "Load Default HEAD branch?")
         (cons repo "HEAD")
       (cons repo (completing-read
                   (concat "Select Branch for "
                           (propertize (format "\"%s\"" repo) 'face 'consult-gh-default-face)
                           ": ")
                   (consult-gh--files-branches-list-items repo)))))
    ('ask
     (cons repo (completing-read
                 (concat "Select Branch for "
                         (propertize (format "\"%s\"" repo) 'face 'consult-gh-default-face)
                         ": ")
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
              (path (car cons))
              (path (string-join (mapcar #'identity (string-split path "/")) (propertize "/" 'face 'consult-gh-default-face)))
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
                                   :class class) str)
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
                           (propertize size 'face 'consult-gh-visibility-face)
                           (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face) "@" (propertize branch 'face 'consult-gh-branch-face))))
              (cand (substring-no-properties cand)))
        (concat
         (consult-gh--justify-left str cand  (* 1.5 (frame-width)))
         (propertize url 'face 'consult-gh-url-face))
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
            `consult-gh-highlight-match-face' in the minibuffer."
  (let* ((class "repo")
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
                       (and user (propertize user 'face 'consult-gh-user-face))
                       (and package "/")
                       (and package (propertize package 'face 'consult-gh-package-face)))
                      (consult-gh--justify-left (propertize visibility 'face 'consult-gh-visibility-face) repo (frame-width))
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                      (propertize description 'face 'consult-gh-description-face))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :description description :visibility visibility :date date :query query :class class) str)
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
                                 (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
                      ((stringp match-str)
                       (highlight-regexp match-str 'consult-gh-preview-match-face)))))
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

(defun consult-gh--repo-insert-readme-markdown (repo)
  "Insert REPO's Readme in markdown format at point."
  (save-mark-and-excursion
    (insert (cadr (consult-gh--call-process "repo" "view" repo)))
    (if (featurep 'markdown-mode)
        (progn
          (require 'markdown-mode nil t)
          (markdown-mode)
          (markdown-display-inline-images))
      (normal-mode))
    nil))

(defun consult-gh--repo-insert-readme-org (repo)
  "Insert REPO's Readme in org format at point."
  (save-mark-and-excursion
    (let* ((org-display-remote-inline-images 'download)
           (info (cadr (consult-gh--api-get-json (format "repos/%s" repo))))
           (name (consult-gh--json-to-hashtable info :name))
           (desc (consult-gh--json-to-hashtable info :description))
           (readme (cadr (consult-gh--api-get-json (format "repos/%s/readme" repo))))
           (path (consult-gh--json-to-hashtable readme :path))
           (extension (and (stringp path) (file-name-extension path)))
           (content (consult-gh--json-to-hashtable readme :content)))
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
        (consult-gh--markdown-to-org))
       (t
        (org-mode)))))
  nil)

(defun consult-gh--repo-insert-readme (repo)
  "Insert REPO's Readme at point."
  (save-mark-and-excursion
    (let* ((info (cadr (consult-gh--api-get-json (format "repos/%s" repo))))
           (name (consult-gh--json-to-hashtable info :name))
           (desc (consult-gh--json-to-hashtable info :description))
           (readme (cadr (consult-gh--api-get-json (format "repos/%s/readme" repo))))
           (path (consult-gh--json-to-hashtable readme :path))
           (content (consult-gh--json-to-hashtable readme :content)))
      (setq-local buffer-file-name path)
      (insert ""
              (or comment-start "") "name:\t" (or name "") (or comment-end "") "\n"
              (or comment-start "") "description:\t" (or desc "") (or comment-end "") "\n"
              (make-string 5 ?\-)
              "\n\n")
      (when content
        (insert (base64-decode-string content))
        (set-buffer-file-coding-system 'raw-text))
      (normal-mode)))
  nil)

(defun consult-gh--repo-view (repo &optional buffer)
  "Open REPO's Readme in an Emacs buffer, BUFFER.

This is an internal function that takes REPO, the full name of
a GitHub repository \(e.g. “armindarvish/consult-gh”\) and
shows the README of that repo in an Emacs buffer.

It fetches the preview from GitHub by “gh repo view REPO”
and puts the response as raw text in the buffer defined by
the optional input, BUFFER.  If BUFFER is nil, a buffer named by
`consult-gh-preview-buffer-name' is used instead.

If `consult-gh-repo-preview-mode' is non-nil, uses it to set the
major-mode, otherwise uses the major mode associated with the README's
file extension (e.g. .md, .org, .rst).

Description of Arguments:

REPO   the name of the repository to be previewed.
BUFFER an optional buffer the preview should be shown in."
  (with-current-buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name))
    (erase-buffer)
    (pcase consult-gh-repo-preview-mode
      ('markdown-mode
       (consult-gh--repo-insert-readme-markdown repo))
      ('org-mode
       (consult-gh--repo-insert-readme-org repo))
      (_ (consult-gh--repo-insert-readme repo)))
    (goto-char (point-min))
    (current-buffer)))

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
  (if (consult-gh--command-to-string "repo" "clone" (format "%s" repo) (expand-file-name name targetdir))
      (progn
        (run-hook-with-args 'consult-gh-repo-post-clone-hook (expand-file-name name targetdir))
        (message "repo %s was cloned to %s" (propertize repo 'face 'font-lock-keyword-face) (propertize (expand-file-name name targetdir) 'face 'font-lock-type-face))))
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
    (consult-gh--command-to-string "repo" "fork" (format "%s" repo) "--fork-name" name)
    (message "repo %s was forked to %s" (propertize repo 'face 'font-lock-keyword-face) (propertize forkrepo 'face 'font-lock-warning-face))
    (run-hook-with-args 'consult-gh-repo-post-fork-hook forkrepo)
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
         (owner (or owner (consult--read (consult-gh--get-current-orgs t)
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
                                                                  :prompt (format "Select %s template" (propertize "license " 'face 'consult-gh-warning-face))
                                                                  :lookup #'consult--lookup-cdr
                                                                  :require-match t
                                                                  :sort nil))))
         (confirm (y-or-n-p (format "This will create %s as a %s repository on GitHub.  Continue?" (propertize name 'face 'consult-gh-repo-face) (propertize visibility 'face 'warning))))
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
         (owner (or owner (consult--read (consult-gh--get-current-orgs t)
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
      (let* ((confirm (y-or-n-p (format "This will create %s as a %s repository on GitHub.  Continue?" (propertize name 'face 'consult-gh-repo-face) (propertize visibility 'face 'warning))))
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
             (owner (or owner (consult--read (consult-gh--get-current-orgs t)
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
             (confirm (y-or-n-p (format "This will create %s as a %s repository on GitHub.  Continue?" (propertize name 'face 'consult-gh-repo-face) (propertize visibility 'face 'warning))))
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
            with `consult-gh-highlight-match-face' in the minibuffer."
  (let* ((class "issue")
         (parts (string-split string "\t"))
         (repo (car (consult--command-split input)))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (car parts))
         (state (upcase (cadr parts)))
         (face (pcase state
                 ("CLOSED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-warning-face)
                 (_ 'consult-gh-issue-face)))
         (title (cadr (cdr parts)))
         (tags (cadr (cdr (cdr parts))))
         (date (cadr (cdr (cdr (cdr parts)))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                      (propertize (consult-gh--set-string-width state 8) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                      (propertize (consult-gh--set-string-width tags 18) 'face 'consult-gh-tags-face)
                      (consult-gh--set-string-width (concat (and user (propertize user 'face 'consult-gh-user-face)) (and package "/") (and package (propertize package 'face 'consult-gh-package-face))) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :number number :state state :title title :tags tags :date date :query query :class class) str)
    str))

(defun consult-gh--search-issues-format (string input highlight)
  "Format candidates for issues.

Description of Arguments:

  STRING the output of a “gh” call
         \(e.g. “gh search issues ...”\).
  INPUT  the query from the user
         \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
           with `consult-gh-highlight-match-face' in the minibuffer."
  (let* ((class "issue")
         (parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (cadr parts))
         (state (upcase (cadr (cdr parts))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-warning-face)
                 (_ 'consult-gh-issue-face)))
         (title (cadr (cdr (cdr parts))))
         (tags (cadr (cdr (cdr (cdr parts)))))
         (date (cadr (cdr (cdr (cdr (cdr parts))))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                      (propertize (consult-gh--set-string-width state 8) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                      (propertize (consult-gh--set-string-width tags 18) 'face 'consult-gh-tags-face)
                      (consult-gh--set-string-width (concat (and user (propertize user 'face 'consult-gh-user-face )) (and package "/") (and package (propertize package 'face 'consult-gh-package-face))) 40))))
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
                     :class class)
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
                                 (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
                      ((stringp match-str)
                       (highlight-regexp match-str 'consult-gh-preview-match-face)))))
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

(defun consult-gh--issue-read-json (repo number)
  "Get json response of issue of NUMBER in REPO.

Runs an async shell command with the command:
gh issue view NUMBER --repo REPO --json `consult-gh--issue-view-json-fields'
, and returns the output as a hash-table."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false :false))
    (json-read-from-string (consult-gh--command-to-string "issue" "view" number "--repo" repo "--json" consult-gh--issue-view-json-fields))))

(defun consult-gh--issue-get-commenters (table)
  "Get list of commenters on an issue.

Retrives a list of all commenters for the issue
stored in TABLE, a hash-table output
from `consult-gh--issue-read-json'."
  (let* ((author (gethash :login (gethash :author table)))
         (assignees (gethash :assignees table))
         (assignees (and (listp assignees) (mapcar (lambda (item) (and (hash-table-p item) (gethash :login item))) assignees)))
         (comments (gethash :comments table))
         (commenters (and (listp comments) (mapcar (lambda (comment)
                                                     (when (hash-table-p comment)
                                                       (gethash :login (gethash :author comment))))
                                                   comments))))
    (cl-remove-duplicates (delq nil (append (list author) assignees commenters)) :test #'equal)))

(defun consult-gh--issue-format-header (repo number table)
  "Format a header for an issue of NUMBER in REPO.

TABLE is a hash-table output containing issue information
from `consult-gh--issue-read-json'.  Returns a formatted string containing
the header section for `consult-gh--issue-view'."
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
         (labels (and (listp labels) (mapconcat (lambda (item) (and (hash-table-p item) (gethash :name item))) labels "\s\s")))
         (milestone (gethash :milestone table))
         (milestone (and milestone (concat (format "%s-%s\s\s(%s)"
                                                   (or (gethash :number milestone) "")
                                                   (or (gethash :title milestone) "N/A")
                                                   (or (gethash :description milestone) "no description")))))
         (assignees (gethash :assignees table))
         (assignees (and (listp assignees) (mapconcat (lambda (item) (and (hash-table-p item) (gethash :login item))) assignees ",\s"))))

    (concat "title: " title "\n"
            "author: " author "\n"
            "repository: " repo "\n"
            "number: " number "\n"
            "state: " state "\n"
            "lastUpdated: " updatedAt "\n"
            (and url (concat "url: " url "\n"))
            (and closedAt (concat "closed: " closedAt "\n"))
            (and labels (concat "labels: " "[ " labels " ]""\n"))
            (and milestone (concat "milestone: " milestone "\n"))
            (and assignees (concat "assignees: " assignees "\n"))
            "\n--\n")))

(defun consult-gh--issue-format-comments (table)
  "Format a comments section for an issue.

TABLE is a hash-table output containing issue information
from `consult-gh--issue-read-json'.  Returns a formatted string containing
the comments section for `consult-gh--issue-view'."
  (let* ((author (gethash :login (gethash :author table)))
         (body (gethash :body table))
         (createdAt (gethash :createdAt table))
         (url (gethash :url table))
         (comments (gethash :comments table))
         (comments (sort comments
                         :key (lambda (k)
                                (gethash :createdAt k))))

         (header-marker "#"))
    (concat author " " (consult-gh--time-ago createdAt)
            " " (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)) "\n"
            body "\n"
            (when (listp comments)
              (mapconcat (lambda (comment)
                           (when (hash-table-p comment)
                             (let* ((author (gethash :author comment))
                                    (author (and author (gethash :login author)))
                                    (authorAssociation (gethash :authorAssociation comment))
                                    (authorAssociation (unless (equal authorAssociation "NONE")
                                                         authorAssociation))
                                    (createdAt (gethash :createdAt comment))
                                    (createdAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)))
                                    (body (gethash :body comment)))
                               (save-match-data
                                 (when (and body (string-match (concat "^" header-marker " .*$")  body ))
                                   (setq body (with-temp-buffer
                                                (insert body)
                                                (goto-char (point-min))
                                                (while (re-search-forward (concat "^" header-marker " +?.*$") nil t)
                                                  (replace-match (concat header-marker "\\&")))
                                                (buffer-string)))))
                               (concat header-marker " "
                                       (and author (concat author " "))
                                       (and authorAssociation (concat "(" authorAssociation ")"))
                                       (and createdAt (concat (consult-gh--time-ago createdAt) " " createdAt))
                                       "\n"
                                       (and body (concat body "\n"))
                                       "\n----------\n"))))
                         comments "\n")))))

(defun consult-gh--issue-view (repo number &optional buffer title)
  "Open ISSUE of REPO in an Emacs buffer, BUFFER.

This is an internal function that takes REPO, the full name of a
repository \(e.g. “armindarvish/consult-gh”\) and ISSUE,
a issue number of that repository, and shows
the contents of the issue in an Emacs buffer.

It fetches the preview of the ISSUE by runing the command
“gh issue view ISSUE --repo REPO” using `consult-gh--call-process'
and put it as raw text in either BUFFER or if BUFFER is nil,
in a buffer named by `consult-gh-preview-buffer-name'.
If `consult-gh-issue-preview-mode' is non-nil, uses it as
major-mode, otherwise shows the raw text in \='fundamental-mode.

Description of Arguments:

  REPO is the name of the repository to be previewed.
  NUMBER is the issue number
  BUFFER is an optional buffer the preview should be shown in.
  TITLE  is an optional title string

To use this as the default action for repos,
see `consult-gh--issue-view-action'."
  (let* ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
         (table (consult-gh--issue-read-json repo number))
         (commenters (and table (consult-gh--issue-get-commenters table)))
         (header-text (consult-gh--issue-format-header repo number table))
         (title (or title (car (split-string header-text "\n" t))))
         (title (string-trim-left title "title: "))
         (conversation-text (consult-gh--issue-format-comments table))
         (comment-btn (buttonize "# Add New Comment" (lambda (&rest _) (consult-gh-comment-create))))
         (topic (format "%s/#%s" repo number)))

    ;; collect issues of repo for completion at point
    (consult-gh--completion-set-issues topic repo)

    ;; collect prs of repo for completion at point
    (consult-gh--completion-set-prs topic repo)

    ;; collect mentionable users for completion at point
    (consult-gh--completion-set-mentionable-users topic repo)

    (add-text-properties 0 1 (list :repo repo :type "issue" :commenters (mapcar (lambda (item) (concat "@" item)) commenters) :number number :title title) topic)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert header-text)
        (insert conversation-text)
        (insert comment-btn)
        (pcase consult-gh-issue-preview-mode
          ('markdown-mode
           (if (featurep 'markdown-mode)
               (progn
                 (markdown-mode)
                 (markdown-display-inline-images))
             (message "markdown-mode not available")))
          ('org-mode
           (let ((org-display-remote-inline-images 'download))
             (consult-gh--markdown-to-org buffer))))
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
         (confirm (get-buffer buffername)))

    (if (and confirm
             (y-or-n-p "Issue is already open in another buffer.  Do you want to reload in the same buffer?"))
        (funcall consult-gh-switch-buffer-func (consult-gh--issue-view repo number (get-buffer buffername)))
      (progn
        (and confirm (message "Opening in a new buffer..."))
        (funcall consult-gh-switch-buffer-func (consult-gh--issue-view repo number))
        (rename-buffer buffername t)))))

(defun consult-gh-topics--issue-get-metadata (&optional issue buffer)
  "Get metadata of ISSUE in BUFFER.

When ISSUE is nil, use `consult-gh--topic'.
When BUFFER is nil use current buffer."

  (let* ((issue (or issue consult-gh--topic))
         (repo (get-text-property 0 :repo issue))
         (assignees (get-text-property 0 :assignees issue))
         (labels (get-text-property 0 :labels issue))
         (milestone (get-text-property 0 :milestone issue))
         (projects  (get-text-property 0 :projects issue))
         (valid-assignees (append (get-text-property 0 :assignable-user issue) (list "@me")))
         (valid-labels (get-text-property 0 :valid-labels issue))
         (valid-projects (get-text-property 0 :valid-projects issue))
         (valid-milestones (get-text-property 0 :valid-milestones issue)))

    (when (and (derived-mode-p 'org-mode) (consult-gh--user-canadmin repo))
      (let* ((org-assignees (cadar (org-collect-keywords '("assignees"))))
             (org-labels (cadar (org-collect-keywords '("labels"))))
             (org-milestone (cadar (org-collect-keywords '("milestone"))))
             (org-projects (cadar (org-collect-keywords '("projects")))))

        (and (stringp org-assignees)
             (setq assignees (cl-remove-duplicates
                              (cl-remove-if-not
                               (lambda (item) (member item valid-assignees))
                               (append assignees (split-string org-assignees "," t "[ \t]+")))
                              :test #'equal)))

        (and (stringp org-labels)
             (setq labels (cl-remove-duplicates
                           (cl-remove-if-not
                            (lambda (item) (member item valid-labels))
                            (append labels (split-string org-labels "," t "[ \t]+")))
                           :test #'equal)))

        (and (stringp org-milestone)
             (setq milestone (if (member org-milestone (consult-gh--get-milestones repo)) org-milestone milestone)))

        (and (stringp org-projects)
             (setq projects (cl-remove-duplicates
                             (cl-remove-if-not
                              (lambda (item) (member item valid-projects))
                              (append projects (split-string org-projects "," t "[ \t]+")))
                             :test #'equal)))))

    (list (cons "assignees" assignees)
          (cons "labels" labels)
          (cons "milestone" milestone)
          (cons "projects" projects))))

(defun consult-gh-topics--issue-get-title-and-body (&optional buffer-or-string)
  "Parse the current buffer to get title and body of comment.

When BUFFER-OR-STRING is non-nil parse that instead of current buffer."
  (let* ((text (cond
                ((buffer-live-p buffer-or-string)
                 (consult-gh--whole-buffer-string buffer-or-string))
                ((stringp buffer-or-string) buffer-or-string)
                (t (consult-gh--whole-buffer-string)))))
    (with-temp-buffer
      (insert text)
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

(defun consult-gh-topics--issue-add-metadata (&optional repo issue)
  "Add metadata to ISSUE of REPO.

This is used when creating new issues for REPO."
  (let* ((issue (or issue consult-gh--topic))
         (meta (consult-gh-topics--issue-get-metadata issue))
         (repo (or repo (get-text-property 0 :repo issue)))
         (canAdmin (consult-gh--user-canadmin repo)))
    (when canAdmin
      ;; add assignees
      (and (y-or-n-p "Would you like to add assignees?")
           (let* ((current (cdr (assoc "assignees" meta)))
                  (table (consult-gh--get-assignable-users repo))
                  (users (append (list "@me") table))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Users: " users)) :test #'equal)))
             (add-text-properties 0 1 (list :assignees (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) issue)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+assignees: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :assignees issue) ", ") nil nil nil 1))))))

      ;; add labels
      (and (y-or-n-p "Would you like to add lables?")
           (let* ((current (cdr (assoc "labels" meta)))
                  (labels (consult-gh--get-labels repo))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Labels: " labels)) :test #'equal)))
             (add-text-properties 0 1 (list :labels (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) issue)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+labels: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :labels issue) ", ") nil nil nil 1))))))

      ;; add projects
      (and (y-or-n-p "Would you like to add projects?")
           (let* ((current (cdr (assoc "projects" meta)))
                  (table (consult-gh--get-projects repo))
                  (table (and (hash-table-p table) (gethash :Nodes table)))
                  (projects (and (listp table) (mapcar (lambda (item) (gethash :title item)) table)))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Projects: " projects)) :test #'equal)))
             (add-text-properties 0 1 (list :projects (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) issue)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+projects: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :projects issue) ", ") nil nil nil 1))))))

      ;; add a milestone
      (and (y-or-n-p "Would you like to add a milestone?")
           (let* ((current (cdr (assoc "milestone" meta)))
                  (milestones (consult-gh--get-milestones repo))
                  (selection (completing-read "Select a Milestone: " milestones)))
             (add-text-properties 0 1 (list :milestone selection) issue)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+milestone: \\(?1:.*\\)" nil t)
                                 (replace-match (get-text-property 0 :milestone issue) nil nil nil 1))))))
      (setq consult-gh--topic issue))))

(defun consult-gh-topics--issue-submit (repo title body &optional assignees labels milestone projects web)
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

(defun consult-gh-topics--issue-presubmit (issue)
  "Prepare ISSUE to submit.

ISSUE is a plist with key value pairs that identify a github
issue.  ISSUE for example can be the plist stored in the buffer-local variable
`consult-gh--topic' in the buffer generated by
`consult-gh-issue-create'."
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
          (consult-gh-topics--issue-add-metadata)
          (setq next (consult--read nextsteps
                                    :prompt "Choose what to do next? "
                                    :lookup #'consult--lookup-cdr
                                    :sort nil)))
        (pcase-let* ((`(,title . ,body) (consult-gh-topics--issue-get-title-and-body (consult-gh-topics--buffer-string)))
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
            (':browser (and (consult-gh-topics--issue-submit repo title body assignees labels milestone projects t)))
            (':submit (and (consult-gh-topics--issue-submit repo title body assignees labels milestone projects nil) (kill-buffer))))))
    (message "Not in an issue editing buffer!")))

(defun consult-gh--pr-list-format (string input highlight)
  "Format minibuffer candidates for pull requests.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh pr list ...”\)
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\)
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match-face' in the minibuffer."
  (let* ((class "pr")
         (parts (string-split string "\t"))
         (repo (car (consult--command-split input)))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (car parts))
         (state (upcase (cadr (cdr (cdr parts)))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-error-face)
                 ("MERGED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-repo-face)
                 (_ 'consult-gh-pr-face)))
         (branch (cadr (cdr parts)))
         (title (cadr parts))
         (date (cadr (cdr (cdr (cdr parts)))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face  face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                      (propertize (consult-gh--set-string-width state 6) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                      (propertize (consult-gh--set-string-width branch 24) 'face 'consult-gh-branch-face)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :number number :state state :title title :branch branch :date date :query query :class class) str)
str))

(defun consult-gh--search-prs-format (string input highlight)
  "Format minibuffer candidates for pull requests.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh search prs ...”\)
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match-face' in the minibuffer."

  (let* ((class "pr")
         (parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (number (cadr parts))
         (state (upcase (cadr (cdr parts))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-error-face)
                 ("MERGED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-repo-face)
                 (_ 'consult-gh-pr-face)))
         (title (cadr (cdr (cdr parts))))
         (tags (cadr (cdr (cdr (cdr parts)))))
         (date (cadr (cdr (cdr (cdr (cdr parts))))))
         (date (if (and (stringp date) (length> date 9)) (substring date 0 10) date))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" number) 'face  face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                      (propertize (consult-gh--set-string-width state 6) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                      (propertize (consult-gh--set-string-width tags 18) 'face 'consult-gh-tags-face)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :number number :state state :title title :tags tags :date date :query query :class class) str)
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
                   (consult-gh--pr-view repo number buffer)
                   (with-current-buffer buffer
                     (if consult-gh-highlight-matches
                         (cond
                          ((listp match-str)
                           (mapc (lambda (item)
                                     (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
                          ((stringp match-str)
                           (highlight-regexp match-str 'consult-gh-preview-match-face)))))
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

(defun consult-gh--pr-read-json (repo number)
  "Get json response of pull request of NUMBER in REPO.

Runs an async shell command with the command:
gh pr view NUMBER --REPO --json `consult-gh--pr-view-json-fields'
, and returns the output as a hash-table."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false :false))
    (json-read-from-string (consult-gh--command-to-string "pr" "view" number "--repo" repo "--json" consult-gh--pr-view-json-fields))))

(defun consult-gh--pr-get-comments (table)
  "Get comments and reviews of a pull request.

Retrives a list of comments and reviews for PR stored in TABLE,
a hash-table output from `consult-gh--pr-read-json'."
  (let* ((comments (gethash :comments table))
         (reviews (gethash :reviews table)))
    (append comments reviews)))

(defun consult-gh--pr-get-commenters (table)
  "Get list of commenters on a pull request.

Retrives a list of all commenters and reviewers for PR
stored in TABLE, a hash-table output
from `consult-gh--pr-read-json'."
  (let* ((author (gethash :login (gethash :author table)))
         (assignees (gethash :assignees table))
         (assignees (and (listp assignees) (mapcar (lambda (item) (and (hash-table-p item) (gethash :login item))) assignees)))
         (comments (consult-gh--pr-get-comments table))
         (commenters (and (listp comments) (mapcar (lambda (comment)
                                                     (when (hash-table-p comment)
                                                       (gethash :login (gethash :author comment))))
                                                   comments))))
    (cl-remove-duplicates (delq nil (append (list author) assignees commenters)) :test #'equal)))

(defun consult-gh--pr-format-header (repo number table)
  "Format a header for a pull reqeust of NUMBER in REPO.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing
the header section for `consult-gh--pr-view'."
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
         (labels (and (listp labels) (mapconcat (lambda (item) (and (hash-table-p item) (gethash :name item))) labels "\s\s")))
         (milestone (gethash :milestone table))
         (milestone (and milestone (concat (format "%s-%s\s\s(%s)"
                                                   (or (gethash :number milestone) "")
                                                   (or (gethash :title milestone) "N/A")
                                                   (or (gethash :description milestone) "no description")))))
         (assignees (gethash :assignees table))
         (assignees (and (listp assignees) (mapconcat (lambda (item) (and (hash-table-p item) (gethash :login item))) assignees ",\s"))))

    (concat "title: " title "\n"
            "author: " author "\n"
            "repository: " (format "%s:%s" repo baseRef) "\n"
            "number: " number "\n"
            "ref: " repo ":" baseRef " <- " (and headRepoOwner (concat headRepoOwner "/")) (and headRepo (format "%s:" headRepo)) headRef "\n"
            "state: " state "\n"
            "lastUpdated: " updatedAt "\n"
            (and url (concat "url: " url "\n"))
            (and closedAt (concat "closed: " closedAt "\n"))
            (and labels (concat "labels: " "[ " labels " ]""\n"))
            (and milestone (concat "milestone: " milestone "\n"))
            (and assignees (concat "assignees: " assignees "\n"))
            "\n--\n")))

(defun consult-gh--pr-format-commits (table)
  "Format the commits section for a pull request.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing
the commits section for `consult-gh--pr-view'."
  (let* ((commits (gethash :commits table))
         (url (gethash :url table)))
    (concat "# Commits\n"
            (mapconcat (lambda (commit) (when (hash-table-p commit)
                                          (let* ((oid  (gethash :oid commit))
                                                 (authors (gethash :authors commit))
                                                 (authors (and (listp authors) (mapconcat (lambda (author) (gethash :login author)) authors ",\s")))
                                                 (date (gethash :committedDate commit))
                                                 (date (and (stringp date) (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time date)))))
                                            (concat "## " "[" (substring oid 0 6) "]" (format "(%s/commits/%s)" url oid) " by " authors " at " date "\n"))))
                       commits)
            "\n----------\n")))

(defun consult-gh--pr-format-comments (table)
  "Format a comments section for a pull request.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing
the comments section for `consult-gh--pr-view'."
  (let* ((author (gethash :login (gethash :author table)))
         (body (gethash :body table))
         (createdAt (gethash :createdAt table))
         (url (gethash :url table))
         (comments (consult-gh--pr-get-comments table))
         (comments (sort comments
                         :key (lambda (k)
                                (or (gethash :createdAt k) (gethash :submittedAt k)))))
         (header-marker "#"))
    (concat header-marker " Conversation\n"
            header-marker header-marker " " author " " (consult-gh--time-ago createdAt)
            " " (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)) "\n"
            body "\n"
            (when (listp comments)
              (mapconcat (lambda (comment)
                           (when (hash-table-p comment)
                             (let* ((author (gethash :author comment))
                                    (author (and author (gethash :login author)))
                                    (authorAssociation (gethash :authorAssociation comment))
                                    (authorAssociation (unless (equal authorAssociation "NONE")
                                                         authorAssociation))
                                    (createdAt (or (gethash :createdAt comment)
                                                   (gethash :submittedAt comment)))
                                    (commit (gethash :commit comment))
                                    (oid (and commit (gethash :oid commit)))
                                    (createdAt (format-time-string "<%Y-%m-%d %H:%M>" (date-to-time createdAt)))
                                    (body (gethash :body comment)))
                               (save-match-data
                                 (when (and body (string-match (concat "^" header-marker "+?\s.*$")  body))
                                   (setq body (with-current-buffer (get-buffer-create "consult-gh-test")
                                                (erase-buffer)
                                                (insert body)
                                                (goto-char (point-min))
                                                (while (re-search-forward (concat "^" header-marker "+?\s.*$") nil t)
                                                  (replace-match (concat header-marker header-marker "\\&")))
                                                (buffer-string)))))
                               (concat header-marker header-marker " "
                                       (and author (concat author " "))
                                       (and authorAssociation (concat "(" authorAssociation ")"))
                                       (and createdAt (concat (consult-gh--time-ago createdAt) " " createdAt))
                                       "\n"
                                       (and commit oid (concat "\ncommit: " "[" (substring oid 0 6) "]" (format "(%s/commits/%s)" url oid) "\n------"))
                                       (and body (concat "\n" body "\n"))
                                       "\n----------\n"))))
                         comments "\n")))))

(defun consult-gh--pr-format-files-changed (table)
  "Format a changed files section for a pull request.

TABLE is a hash-table output containing pull request information
from `consult-gh--pr-read-json'.  Returns a formatted string containing the
files changed section for `consult-gh--pr-view'."
  (let ((files (gethash :files table))
        (additions (gethash :additions table))
        (deletions (gethash :deletions table)))
    (and (listp files) (concat "# Files Changed - " (format "%s file(s)" (length files)) (and additions (format ", %s additions(+)" additions)) (and deletions (format ", %s deletions(-)" deletions)) "\n"))))

(defun consult-gh--pr-insert-diffs (diffs)
  "Insert formtted DIFFS in PR view buffer.

This is used in `consult-gh--pr-view'."
  (mapcar (lambda (chunk)
            (if (consp chunk)
                (pcase consult-gh-issue-preview-mode
                  ('markdown-mode (insert (concat "## " (car-safe chunk) "\n```diff\n" (cadr chunk) "\n```\n")))
                  ('org-mode
                   (insert (concat "** " (car-safe chunk) "\n"))
                   (insert "#+begin_src diff\n")
                   (let ((content (save-match-data
                                    (with-temp-buffer
                                      (and (stringp (cadr chunk)) (insert (cadr chunk)))
                                      (goto-char (point-min))
                                      (while (re-search-forward "#\\+begin_src\\|#\\+end_src" nil t)
                                        (replace-match ",\\&" nil nil))
                                      (buffer-string)))))
                     (and (stringp content) (insert content)))
                   (insert "\n#+end_src\n"))
                  (_ (insert chunk)))))
          diffs))

(defun consult-gh--pr-view (repo number &optional buffer title)
  "Open pull request, NUMBER of REPO in an Emacs buffer, BUFFER.

This is an internal function that takes REPO, the full name of a repository
\(e.g. “armindarvish/consult-gh”\) and NUMBER, a pr number of REPO,
and shows the contents of the pull request in an Emacs buffer.

It fetches the details of the pull request by calling
`consult-gh--pr-read-json' and parses and formats it in markdown syntax,
and puts it in either BUFFER, or if BUFFER is nil, in a buffer named by
`consult-gh-preview-buffer-name'.  If `consult-gh-issue-preview-mode'
is non-nil, uses it as major-mode for BUFFER, otherwise shows the raw text
in \='text-mode.

Description of Arguments:

  REPO   the full name of the repository to be previewed.
  NUMBER    pull request number
  BUFFER an optional buffer the preview should be shown in.
  TITLE  is an optional title string

To use this as the default action for PRs, see
`consult-gh--pr-view-action'."
  (let* ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
         (table (consult-gh--pr-read-json repo number))
         (users (and table (consult-gh--pr-get-commenters table)))
         (header-text (consult-gh--pr-format-header repo number table))
         (title (or title (car (split-string header-text "\n" t))))
         (title (string-trim-left title "title: "))
         (commits-text (consult-gh--pr-format-commits table))
         (conversation-text (consult-gh--pr-format-comments table))
         (file-change-text (consult-gh--pr-format-files-changed table))
         (diff (consult-gh--command-to-string "pr" "diff" number "--repo" repo))
         (diff-chunks (and (stringp diff) (consult-gh--parse-diff diff)))
         (comment-btn (buttonize "# Add New Comment"
                                 (lambda (&rest _)
                                   (consult-gh-comment-create))))
         (topic (format "%s/#%s" repo number)))

    ;; collect issues of repo for completion at point
    (consult-gh--completion-set-issues topic repo)

    ;; collect prs of repo for completion at point
    (consult-gh--completion-set-prs topic repo)

    ;; collect mentionable users for completion at point
    (consult-gh--completion-set-mentionable-users topic repo)

    (add-text-properties 0 1 (list :repo repo :type "pr" :number number :title title) topic)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert header-text)
        (insert commits-text)
        (insert conversation-text)
        (insert file-change-text)
        (save-excursion
          (insert "\n")
          (insert comment-btn)
          (pcase consult-gh-issue-preview-mode
            ('markdown-mode
             (if (featurep 'markdown-mode)
                 (progn
                   (markdown-mode)
                   (markdown-display-inline-images))
               (message "markdown-mode not available")))
            ('org-mode
             (let ((org-display-remote-inline-images 'download))
               (consult-gh--markdown-to-org buffer)))
            (_ ())))
        (consult-gh--pr-insert-diffs diff-chunks)
        (insert "\n")
        (set-buffer-file-coding-system 'unix)
        (goto-char (point-min))
        (save-excursion
          (while (re-search-forward "\r\n" nil t)
            (replace-match "\n")))
        (outline-hide-sublevels 1)
        (consult-gh-pr-view-mode +1)
        (setq-local consult-gh--topic topic)
        (current-buffer)))))

(defun consult-gh--pr-view-action (cand)
  "Opens the preview of a pull request candidate, CAND.

This is a wrapper function around `consult-gh--pr-view'.  It parses CAND
to extract relevant values \(e.g. repository's name and pull request
number\) and passes them to `consult-gh--pr-view'.

To use this as the default action for prs,
set `consult-gh-pr-action' to `consult-gh--pr-view-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (format "%s" (get-text-property 0 :number cand))))
         (buffername  (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/pull/" number "*"))
         (confirm (get-buffer buffername)))
    (if (and confirm
             (y-or-n-p "PR is already open in another buffer.  Do you want to reload in the same buffer?"))
        (funcall consult-gh-switch-buffer-func (consult-gh--pr-view repo number (get-buffer buffername)))
      (progn
        (and confirm (message "Opening in a new buffer..."))
        (funcall consult-gh-switch-buffer-func (consult-gh--pr-view repo number))
        (rename-buffer buffername t)))))

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

  (let* ((table (consult-gh--json-to-hashtable (consult-gh--command-to-string "api" (format "repos/%s/forks" repo)))))
    (and (listp table)  (mapcar (lambda (item) (gethash :full_name item)) table))))

(defun consult-gh-topics--pr-get-parent (repo)
  "Get the upstream parent of REPO."
  (let* ((table (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "parent")))
         (parent (and (hash-table-p table) (gethash :parent table))))
    (and (hash-table-p parent) (concat (gethash :login (gethash :owner parent)) "/" (gethash :name parent)))))

(defun consult-gh-topics--pr-get-siblings (repo)
  "Get the siblings of REPO."
  (let* ((current repo)
         (forks (list)))
    (while-let ((parent (consult-gh-topics--pr-get-parent repo)))
      (setq forks (append forks (consult-gh-topics--pr-get-forks parent)))
      (setq repo parent))
    (print current)
    (cl-remove-duplicates (remove current (delq nil forks)) :test #'equal)))

(defun consult-gh-topics--pr-get-similar (repo)
  "Get all the similar repositories to REPO."
  (let* ((parent (consult-gh-topics--pr-get-parent repo))
         (forks (consult-gh-topics--pr-get-forks repo))
         (siblings (consult-gh-topics--pr-get-siblings repo)))
    (cl-remove-duplicates (append (list parent) forks siblings) :test #'equal)))

(defun consult-gh-topics--pr-get-metadata (&optional pr buffer)
  "Get metadata of PR in BUFFER.

When PR is nil, use `consult-gh--topic'.
When BUFFER is nil use current buffer."

  (let* ((pr (or pr consult-gh--topic))
         (repo (get-text-property 0 :repo pr))
         (reviewers (get-text-property 0 :reviewers pr))
         (assignees (get-text-property 0 :assignees pr))
         (labels (get-text-property 0 :labels pr))
         (milestone (get-text-property 0 :milestone pr))
         (projects  (get-text-property 0 :projects pr))
         (valid-assignees (append (get-text-property 0 :assignable-user pr) (list "@me")))
         (valid-labels (get-text-property 0 :valid-labels pr))
         (valid-projects (get-text-property 0 :valid-projects pr))
         (valid-milestones (get-text-property 0 :valid-milestones pr)))

    (when (and (derived-mode-p 'org-mode) (consult-gh--user-canadmin repo))
      (let* ((org-reviewers (cadar (org-collect-keywords '("reviewers"))))
             (org-assignees (cadar (org-collect-keywords '("assignees"))))
             (org-labels (cadar (org-collect-keywords '("labels"))))
             (org-milestone (cadar (org-collect-keywords '("milestone"))))
             (org-projects (cadar (org-collect-keywords '("projects")))))

        (and (stringp org-reviewers)
             (setq reviewers (cl-remove-duplicates
                              (cl-remove-if-not
                               (lambda (item) (member item valid-assignees))
                               (append reviewers (split-string org-reviewers "," t "[ \t]+")))
                              :test #'equal)))

        (and (stringp org-assignees)
             (setq assignees (cl-remove-duplicates
                              (cl-remove-if-not
                               (lambda (item) (member item valid-assignees))
                               (append assignees (split-string org-assignees "," t "[ \t]+")))
                              :test #'equal)))

        (and (stringp org-labels)
             (setq labels (cl-remove-duplicates
                           (cl-remove-if-not
                            (lambda (item) (member item valid-labels))
                            (append labels (split-string org-labels "," t "[ \t]+")))
                           :test #'equal)))

        (and (stringp org-milestone)
             (setq milestone (if (member org-milestone (consult-gh--get-milestones repo)) org-milestone milestone)))

        (and (stringp org-projects)
             (setq projects (cl-remove-duplicates
                             (cl-remove-if-not
                              (lambda (item) (member item valid-projects))
                              (append projects (split-string org-projects "," t "[ \t]+")))
                             :test #'equal)))))

    (list (cons "reviewers" reviewers)
          (cons "assignees" assignees)
          (cons "labels" labels)
          (cons "milestone" milestone)
          (cons "projects" projects))))

(defun consult-gh-topics--pr-add-metadata (&optional repo pr)
  "Add metadata to PR topic for REPO.

This is used for creating new pull requests."

  (let* ((pr (or pr consult-gh--topic))
         (meta (consult-gh-topics--pr-get-metadata pr))
         (repo (or repo (get-text-property 0 :repo pr)))
         (canAdmin (consult-gh--user-canadmin repo)))
    (when canAdmin

      ;; add reviewers
      (and (y-or-n-p "Would you like to add reviewers?")
           (let* ((current (cdr (assoc "reviewers" meta)))
                  (table (consult-gh--get-assignable-users repo))
                  (users (append (list "@me") table))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Users: " users)) :test #'equal)))
             (add-text-properties 0 1 (list :reviewers (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+reviewers: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :reviewers pr) ", ") nil nil nil 1))))))

      ;; add assignees
      (and (y-or-n-p "Would you like to add assignees?")
           (let* ((current (cdr (assoc "assignees" meta)))
                  (table (consult-gh--get-assignable-users repo))
                  (users (append (list "@me") table))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Users: " users)) :test #'equal)))
             (add-text-properties 0 1 (list :assignees (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+assignees: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :assignees pr) ", ") nil nil nil 1))))))

      ;; add labels
      (and (y-or-n-p "Would you like to add lables?")
           (let* ((current (cdr (assoc "labels" meta)))
                  (labels (consult-gh--get-labels repo))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Labels: " labels)) :test #'equal)))
             (add-text-properties 0 1 (list :labels (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+labels: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :labels pr) ", ") nil nil nil 1))))))

      ;; add projects
      (and (y-or-n-p "Would you like to add projects?")
           (let* ((current (cdr (assoc "projects" meta)))
                  (table (consult-gh--get-projects repo))
                  (table (and (hash-table-p table) (gethash :Nodes table)))
                  (projects (and (listp table) (mapcar (lambda (item) (gethash :title item)) table)))
                  (selection (cl-remove-duplicates (delq nil (completing-read-multiple "Select Projects: " projects)) :test #'equal)))
             (add-text-properties 0 1 (list :projects (cl-remove-duplicates (delq nil (append current selection)) :test #'equal)) pr)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+projects: \\(?1:.*\\)" nil t)
                                 (replace-match (mapconcat #'identity (get-text-property 0 :projects pr) ", ") nil nil nil 1))))))

      ;; add a milestone
      (and (y-or-n-p "Would you like to add a milestone?")
           (let* ((current (cdr (assoc "milestone" meta)))
                  (milestones (consult-gh--get-milestones repo))
                  (selection (completing-read "Select a Milestone: " milestones)))
             (add-text-properties 0 1 (list :milestone selection) pr)

             (when (derived-mode-p 'org-mode)
               (save-excursion (goto-char (point-min))
                               (when (re-search-forward "^#\\+milestone: \\(?1:.*\\)" nil t)
                                 (replace-match (get-text-property 0 :milestone pr) nil nil nil 1))))))
      (setq consult-gh--topic pr))))

(defun consult-gh-topics--pr-submit (repo head base title body &optional reviewers assignees labels milestone projects draft fill web)
  "Create a new pull request in REPO with TITLE and BODY.

Description of Arguments:
  REPO      a string; full name of the repository
  HEAD      a string; name of the head branch (source for merge)
  BASE      a string; name of the base branch (target for merge)
  TITLE     a string; title of the pr
  BODY      a string; body of the pr
  REVEIWERS a list of strings; list of reviewers
  ASSIGNEES a list of strings; list of assignees
  LABELS    a list of strings; list of labels
  MILESTONE a string; a milestone
  PROJECTS  a list of strings; list of projects
  DRAFT     a boolean; whether to submit pull request as draft?
  FILL      a string; whether to add commit details?
              this can either be t, first, or verbose
  WEB       a boolean; whether to continuing editing on the web?"
  (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t))))
         (title (or title (consult--read nil :prompt "Title: ")))
         (body (or body (consult--read nil :prompt "Body: ")))
         (head (or head (consult--read (consult-gh-topics--pr-get-branches repo)
                                       :prompt "Select the head repository you want to merge from: "
                                       :require-match t
                                       :sort t)))
         (base (or base (consult--read (remove head (consult-gh-topics--pr-get-branches repo))
                                       :prompt "Select the base branch you want to merge into: "
                                       :require-match t
                                       :sort t)))
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
    (when (and repo head base title body)
      (setq args (delq nil (append args
                                   (list "--repo" repo)
                                   (list "--head" head)
                                   (list "--base" base)
                                   (list "--title" title)
                                   (list "--body" body)
                                   (and reviewers (list "--reviewer" reviewer))
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

(defun consult-gh-topics--pr-presubmit (pr)
  "Prepare PR to submit.

PR is a plist with key value pairs that identify a github
pull requests.  PR, for example, can be the plist stored in the buffer-local
variable `consult-gh--topic' in the buffer generated by
`consult-gh-pr-create'."
  (if consult-gh-topics-edit-mode
      (let* ((pr (or pr (consult-gh--topic)))
             (repo (get-text-property 0 :repo pr))
             (head (get-text-property 0 :head pr))
             (base (get-text-property 0 :base pr))
             (canAdmin (consult-gh--user-canadmin repo))
             (nextsteps (append (list (cons "Submit" :submit))
                                (list (cons "Submit as Draft" :draft))
                                (list (cons "Continue in the Browser" :browser))
                                (and canAdmin (list (cons "Add Metadata" :metadata)))
                                (list (cons "Cancel" :cancel))))
             (next (consult--read nextsteps
                                  :prompt "Choose what to do next? "
                                  :lookup #'consult--lookup-cdr
                                  :sort nil)))
        (while (eq next ':metadata)
          (consult-gh-topics--pr-add-metadata)
          (setq next (consult--read nextsteps
                                    :prompt "Choose what to do next? "
                                    :lookup #'consult--lookup-cdr
                                    :sort nil)))

        (pcase-let* ((`(,title . ,body) (consult-gh-topics--issue-get-title-and-body (consult-gh-topics--buffer-string)))
                     (title (or title
                                (and (derived-mode-p 'org-mode)
                                     (cadar (org-collect-keywords
                                             '("title"))))
                                ""))
                     (body (or body ""))
                     (metadata (consult-gh-topics--pr-get-metadata))
                     (reviewers (cdr (assoc "reviewers" metadata)))
                     (assignees (cdr (assoc "assignees" metadata)))
                     (labels (cdr (assoc "labels" metadata)))
                     (milestone (cdr (assoc "milestone" metadata)))
                     (projects (cdr (assoc "projects" metadata))))
          (pcase next
            (':browser (and (consult-gh-topics--pr-submit repo head base title body reviewers assignees labels milestone projects nil nil t)))
            (':submit (and (consult-gh-topics--pr-submit repo head base title body reviewers assignees labels milestone projects nil nil nil) (kill-buffer)))
            (':draft (and (consult-gh-topics--pr-submit repo head base title body reviewers assignees labels milestone projects t nil nil) (kill-buffer))))))
    (message "Not in a pull requests editing buffer!")))

(defun consult-gh-topics--comment-create (&optional topic)
  "Interactively create a new comment post on TOPIC."
  (let* ((repo (or repo ))
         (type (or type (consult--read  (list (cons "Issues" "issue") (cons "Pull Requests" "pr"))
                                        :prompt "What topic are you looking for? "
                                        :lookup #'consult--lookup-cdr
                                        :require-match t
                                        :sort nil)))
         (number (or number (pcase type
                              ("issue" (get-text-property 0 :number (consult-gh-issue-list repo t)))
                              ("pr" (get-text-property 0 :number (consult-gh-or-list repo t))))))
         (topic (or topic consult-gh--topic))
         (buffer (get-buffer-create (format "*consult-gh-comment: %s - %s #%s" repo type number)))
         (existing-buffer (not (= (buffer-size buffer) 0))))
    (add-text-properties 0 1 (list :new t :isComment t) topic)
    (when (and existing-buffer (y-or-n-p "You have an existing draft comment on this topic.  Would like to resume editing that one?"))
      (setq existing-buffer t))
    (with-current-buffer buffer
      (unless existing-buffer
        (erase-buffer)
        (cond
         ((equal consult-gh-topic-major-mode 'markdown-mode)
          (markdown-mode))
         ((equal consult-gh-topic-major-mode 'org-mode)
          (org-mode))
         (t
          (text-mode))))
      (setq-local consult-gh--topic topic)
      (consult-gh-topics-edit-mode +1)
      (goto-char (point-max))
      (with-no-warnings (outline-show-all)))
    (funcall consult-gh-switch-buffer-func buffer)))

(defun consult-gh-topics--comment-submit (&optional comment repo type number)
  "Submit the COMMENT on topic of TYPE and NUMBER in REPO.

This command submits the content of the COMMENT string github api for topic
of TYPE (e.g. issue, pr, ...) and id NUMBER.

Description of Arguments:
  REPO   a string; full name of target epository
  TYPE   a string; type of target topic (e.g. issue, pr, ...)
  NUMBER a string; id number for issue, pr, or ..."
  (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t))))
         (type (or type (consult--read  (list (cons "Issues" "issue") (cons "Pull Requests" "pr"))
                                        :prompt "What topic are you looking for? "
                                        :lookup #'consult--lookup-cdr
                                        :require-match t
                                        :sort nil)))
         (number (or number (pcase type
                              ("issue" (get-text-property 0 :number (consult-gh-issue-list repo t)))
                              ("pr" (get-text-property 0 :number (consult-gh-or-list repo t))))))
         (comment (or comment (consult--read nil :prompt "Comment: "))))

    (if (string-empty-p comment)
        (message "Comment cannot be empty!")
      (pcase type
        ((or "issue" "pr")
         (and
          (consult-gh--command-to-string "api" (format "repos/%s/issues/%s/comments" repo number) "-f" (format "body=%s" comment))
          (message "Comment Submitted!")))
        ("discussion"
         (message "Commenting on discussions is not supported, yet!"))))))

(defun consult-gh--search-code-format (string input highlight)
  "Format minibuffer candidates for code.

Description of Arguments:

  STRING    the output of a “gh” call
            \(e.g. “gh search code ...”\).
  INPUT     the query from the user
            \(a.k.a. command line argument passed to the gh call\).
  HIGHLIGHT if non-nil, input is highlighted
            with `consult-gh-highlight-match-face' in the minibuffer."
  (let* ((class "code")
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
                      (consult-gh--set-string-width (propertize code 'face  'consult-gh-code-face) 100)
                      (propertize path 'face 'consult-gh-url-face)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40))))
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t)))))
    (add-text-properties 0 1 (list :repo repo :user user :package package :code code :path path :url url :query query :class class :branch branch) str)
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
                           (highlight-regexp (string-trim code) 'consult-gh-preview-match-face))
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
         (reason (cadddr (cddr (cdddr parts))))
         (reason-str (cond
                      ((string-prefix-p "Assigned to" reason) "assigned")
                      ((string-prefix-p "Authored by" reason) "owned")
                      ((string-prefix-p "Mentions " reason) "mentions")
                      ((string-prefix-p "Involves " reason) "involves")
                      ((string-prefix-p "Involves " reason) "request")))
         (url (cadddr (cdddr (cdddr parts))))
         (face (pcase isPR
                 ("false"
                  (pcase state
                    ("CLOSED" 'consult-gh-success-face)
                    ("OPEN" 'consult-gh-warning-face)
                    (_ 'consult-gh-issue-face)))
                 ("true"
                  (pcase state
                    ("CLOSED" 'consult-gh-error-face)
                    ("MERGED" 'consult-gh-success-face)
                    ("OPEN" 'consult-gh-warning-face)
                    (_ 'consult-gh-pr-face)))
                 (_ 'consult-gh-issue-face)))
         (str (concat (consult-gh--set-string-width
                       (concat (propertize (format "%s" user) 'face 'consult-gh-user-face)
                               "/"
                               (propertize (format "%s" package) 'face 'consult-gh-package-face)

                               (propertize (format " - %s #%s: " (upcase (substring type 0 2)) number) 'face face)
                               (propertize (format "%s" title) 'face 'consult-gh-default-face)) 85)
                      (when reason-str (concat "\s\s" (propertize (consult-gh--set-string-width reason-str 8) 'face 'consult-gh-visibility-face)))
                      (when date (concat "\s\s" (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)))
                      (when state (concat "\s\s" (propertize (consult-gh--set-string-width state 6) 'face face)))
                      (when tags (concat "\s\s" (propertize tags 'face 'consult-gh-tags-face))))))
    (add-text-properties 0 1 (list :repo repo
                                   :user user
                                   :package package
                                   :number number
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
       (consult-gh--set-string-width "Repo - Type Number: Title " 83 nil ?-)
       (consult-gh--set-string-width " Reason " 10 nil ?-)
       (consult-gh--set-string-width " Date " 12 nil ?-)
       (consult-gh--set-string-width " State " 8 nil ?-)
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
                 ("true" 'consult-gh-default-face)
                 ("false" 'consult-gh-tags-face)
                 (_ 'consult-gh-default-face)))
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
         (title-str (concat (propertize (format "%s" repo) 'face 'consult-gh-repo-face)
                            (propertize " - " 'face face)
                            (propertize (concat type (if number " #") number) 'face face)
                            (propertize ": " 'face face)
                            (propertize (format "%s" title) 'face face)))
         (_ (if (equal unread "false") (add-face-text-property 0 (length title-str) '(:strike-through t) t title-str)))
         (str (format "%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width title-str 80)
                      (propertize (consult-gh--set-string-width reason 13) 'face 'consult-gh-visibility-face)
                      (consult-gh--set-string-width (propertize state 'face face) 7)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face))))
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
    (consult-gh--command-to-string "api" (format "notifications/threads/%s" thread) "--silent" "--method" "PATCH")))

(defvar-keymap consult-gh-issue-view-mode-map
  :doc "Consult-gh topics keymap."
  "C-c C-r" #'consult-gh-comment-create
  "C-c o" #'consult-gh-topics-open-in-browser)

;;;###autoload
(define-minor-mode consult-gh-issue-view-mode
  "Minor-mode for viewing issues."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-issue-view"
  :keymap consult-gh-issue-view-mode-map
  (read-only-mode +1))

(defvar-keymap consult-gh-pr-view-mode-map
  :doc "Consult-gh topics keymap."
  "C-c C-r" #'consult-gh-comment-create
  "C-c o" #'consult-gh-topics-open-in-browser)

;;;###autoload
(define-minor-mode consult-gh-pr-view-mode
  "Minor-mode for viewing issues."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-pr-view"
  :keymap consult-gh-pr-view-mode-map
  (read-only-mode +1))

(defun consult-gh-topics-edit-header-line ()
  "Create `header-line-format' for consult-gh-topics."
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
  :doc "Consult-gh topics keymap."
  "C-c C-c" #'consult-gh-topics-submit
  "C-c C-k" #'consult-gh-topics-cancel)

(defun consult-gh-topics-edit-mode-on ()
  "Enable `consult-gh-topics-edit-mode'."
  (setq-local header-line-format (consult-gh-topics-edit-header-line)))

(defun consult-gh-topics-edit-mode-off ()
  "Disable `consult-gh-topics-edit-mode'."
  (setq-local header-line-format nil))

;;;###autoload
(define-minor-mode consult-gh-topics-edit-mode
  "Minor-mode for viewing topics."
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

Helps with autocompleting username and issue numbers, etc."
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
                                                     (propertize host 'face 'consult-gh-tags-face)
                                                     (propertize status 'face 'consult-gh-user-face)
                                                     (propertize current 'face 'consult-gh-visibility-face)))))))
      (when (and sel (consp sel))
        (setq user (car sel))
        (setq host (cadr sel)))))
  (consult-gh--auth-switch host user))

(defun consult-gh--repo-list-transform (async &rest _)
  "Add annotation to repo candidates in `consult-gh-repo-list'.

Returns ASYNC function after formatting results with
`consult-gh--repo-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--repo-list-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--repo-format string consult-gh--current-input nil))
                   (funcall ,async action)))))))

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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input
          \\(passed as INITITAL to `consult--read'\)"
  (let* ((candidates (consult--async-command builder
                       (consult-gh--repo-list-transform builder)))
         (current-repo (consult-gh--get-repo-from-directory))
         (initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't) (consult-gh--get-username current-repo)))))
    (consult-gh-with-host (consult-gh--auth-account-host)
        (consult--read candidates
                   :prompt prompt
                   :lookup #'consult--lookup-member
                   :state (funcall #'consult-gh--repo-state)
                   :initial (consult--async-split-initial initial)
                   :group #'consult-gh--repo-group
                   :add-history (append (list
                                         (if current-repo
                                             (consult--async-split-initial (consult-gh--get-username current-repo)))
                                         (consult--async-split-thingatpt 'symbol))
                                        consult-gh--known-orgs-list)
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
  (if current-prefix-arg
      (setq initial (or initial (format "%s" (car (string-split (car (consult-gh-search-repos initial t)) "/"))))))
  (let* ((prompt (or prompt "Enter Org Name:  "))
         (sel (consult-gh--async-repo-list prompt #'consult-gh--repo-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-repo-action sel))))

(defun consult-gh--search-repos-transform (async &rest _)
  "Add annotation to repo candidates in `consult-gh-search-repos'.

Returns ASYNC function after formatting results with
`consult-gh--repo-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--search-repos-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--repo-format string consult-gh--current-input t))
                   (funcall ,async action)))))))

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

This is a non-interactive internal function.
For the interactive version see `consult-gh-search-repos'.

It runs the command line from `consult-gh--search-repos-builder'
in an async process and returns the results (list of search results
for the minibuffer input) as a completion table in minibuffer that will
be passed to `consult--read'.  The completion table gets dynamically updated
as the user types in the minibuffer.  Each candidate in the minibuffer
is formatted by `consult-gh--search-repos-transform'
to add annotation and other info to the candidate.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'.\)
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer
          \\(passed as INITITAL to `consult--read'.\)"
  (let* ((initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't) (consult-gh--get-repo-from-directory) nil))))
    (consult-gh-with-host (consult-gh--auth-account-host)
        (consult--read
         (consult--async-command builder
           (consult-gh--search-repos-transform builder))
         :prompt prompt
         :lookup #'consult--lookup-member
         :state (funcall #'consult-gh--repo-state)
         :initial (consult--async-split-initial initial)
         :group #'consult-gh--repo-group
         :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                              consult-gh--known-repos-list)
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
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
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

If NOACTION is non-nil, return the candidate without runing action.
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
                                                   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                                                                        consult-gh--known-repos-list)
                                                   :history 'consult-gh--repos-history
                                                   :require-match t
                                                   :category 'consult-gh-repos
                                                   :preview-key consult-gh-preview-key
                                                   :sort t))))
    (if noaction
        sel
      (funcall consult-gh-repo-action sel))))

;;;###autoload
(defun consult-gh-default-repos ()
  "List repositories of orgs in `consult-gh-default-orgs-list'.

Passes `consult-gh-default-orgs-list' to `consult-gh-orgs',
a useful command for quickly fetching a list of personal GitHub Repositories
or any other favorite accounts whose repositories are frequently visited."
  (interactive)
  (consult-gh-orgs consult-gh-default-orgs-list))

;;;###autoload
(defun consult-gh-repo-fork (&optional repos)
  "Interactively fork REPOS.

It runs the command “gh fork repo ...” to fork a repository
using the internal function `consult-gh--repo-fork'

If REPOS not supplied, interactively asks user to pick REPOS."
  (interactive)
  (let* ((consult-gh-prioritize-local-folder (if (eq consult-gh-prioritize-local-folder 'suggest) consult-gh-prioritize-local-folder nil))
         (repos (or repos (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos nil t))))))
    (if (stringp repos)
        (setq repos (list repos)))
    (mapcar (lambda (repo)
              (let* ((package (car (last (split-string repo "\/"))))
                     (name (if consult-gh-confirm-name-before-fork (read-string (concat "name for " (propertize (format "%s: " repo) 'face 'font-lock-keyword-face)) package) package)))
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
  (let* ((consult-gh-prioritize-local-folder (if (eq consult-gh-prioritize-local-folder 'suggest) consult-gh-prioritize-local-folder nil))
         (repos (or repos (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos nil t)))))
         (targetdir (or targetdir consult-gh-default-clone-directory))
         (clonedir (if consult-gh-confirm-before-clone (read-directory-name "Select Target Directory: " (file-name-as-directory targetdir)) (or targetdir default-directory))))
    (if (stringp repos)
        (setq repos (list repos)))
    (mapcar (lambda (repo)
              (let* ((package (consult-gh--get-package repo))
                     (name (if consult-gh-confirm-before-clone (read-string (concat "name for " (propertize (format "%s: " repo) 'face 'font-lock-keyword-face)) package) package)))
                (consult-gh-with-host (consult-gh--auth-account-host)
                                      (consult-gh--repo-clone repo name clonedir))))
            repos)))

(defun consult-gh-repo-create (&optional name local-repo owner description visibility make-readme gitignore-template license-key template)
  "Create a new repo, CAND, on GitHub.

This mimicks the same interactive repo creation
from “gh repo create” in the command line.

For description of see functions NAME, OWNER, DESCRIPTION, VISIBILITY,
MAKE-README, GITIGNORE-TEMPLATE, and LICENSE-KEY
see `consult-gh--repo-create-scratch'.
For description of TEMPLATE see `consult-gh--repo-create-template'.
For description of LOCAL-REPO see directory argument in
`consult-gh--repo-create-push-existing'."
  (interactive "P")
  (let ((answer (consult--read (list (cons "Create a new repository on GitHub from scratch" :scratch)
                                     (cons "Create a new repository on GitHub from a template repository" :template)
                                     (cons "Push an existing local repository to GitHub" :existing))
                               :prompt "What would you like to do?"
                               :lookup #'consult--lookup-cdr
                               :sort nil)))
    (pcase answer
      (':scratch (consult-gh--repo-create-scratch name local-repo owner description visibility make-readme gitignore-template license-key))
      (':template (consult-gh--repo-create-template name owner description visibility template))
      (':existing (consult-gh--repo-create-push-existing name local-repo owner description visibility)))))

(defun consult-gh--issue-list-transform (async &rest _)
  "Add annotation to issue candidates in `consult-gh-issue-list'.

Returns ASYNC function after formatting results with
`consult-gh--issue-list-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--issue-list-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--issue-list-format string consult-gh--current-input nil))
                   (funcall ,async action)))))))

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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer
          \(passed as INITITAL to `consult--read'\)"
  (let* ((initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't)
                          (consult-gh--get-repo-from-directory)
                        nil))))
    (consult-gh-with-host (consult-gh--auth-account-host)
        (consult--read
         (consult--async-command builder
           (consult-gh--issue-list-transform builder))
         :prompt prompt
         :lookup #'consult--lookup-member
         :state (funcall #'consult-gh--issue-state)
         :initial (consult--async-split-initial initial)
         :group #'consult-gh--issue-group
         :require-match t
         :category 'consult-gh-issues
         :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory))
                                    (consult--async-split-thingatpt 'symbol))
                              consult-gh--known-repos-list)
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
  (if current-prefix-arg
      (setq initial (or initial (format "%s" (car (consult-gh-search-repos initial t))))))
  (let* ((prompt (or prompt "Enter Repo Name:  "))
        (sel (consult-gh--async-issue-list prompt #'consult-gh--issue-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-issue-action sel))))

(defun consult-gh-issue-create (&optional repo title)
  "Create a new issue with TITLE for REPO.

This mimicks the same interactive issue creation
from “gh issue create” in the command line."
  (interactive "P")
  (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t))))
         (issueEnabled (gethash :hasIssuesEnabled (consult-gh--json-to-hashtable (consult-gh--command-to-string "repo" "view" repo "--json" "hasIssuesEnabled"))))
         (_ (unless (eq issueEnabled 't)
              (error "Issue is not enabled for the repo %s" repo)))
         (templates (consult-gh--get-issue-templates repo))
         (template (and templates (consult--read templates
                                                 :prompt "Select a template: "
                                                 :require-match t
                                                 :lookup #'consult--lookup-cdr
                                                 :sort t)))
         (title (and template (plistp template) (plist-get template :title)))
         (title (and (not (string-empty-p title)) title))
         (body (and template (plistp template) (plist-get template :body)))
         (body (and (not (string-empty-p body)) body))
         (canAdmin (consult-gh--user-canadmin repo))
         (buffer (get-buffer-create (format "*consult-gh-issue-create: %s" repo)))
         (existing (not (= (buffer-size buffer) 0)))
         (topic (or repo "new issue"))
         (type "issue")
         (confirm (if existing
                      (consult--read
                       (list (cons "Resume editing the existing draft." :resume)
                             (cons "Create a new issue from sratch, but do not discard the old one." :new)
                             (cons "Discard the old issue and create a new one from scratch." :replace))
                       :prompt "You have already started another issue for this repository.  Would like to resume editing that one or start a new one? "
                       :lookup #'consult--lookup-cdr
                       :sort nil
                       :require-match t))))

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

    (add-text-properties 0 1 (list :number nil :type "issue" :isComment nil :new t :repo (substring-no-properties repo)) topic)

    (when existing
      (cond
       ((eq confirm :resume) (setq existing t))
       ((eq confirm :replace) (setq existing nil))
       ((eq confirm :new)
        (setq existing nil)
        (setq buffer (generate-new-buffer (format "*consult-gh-issue-create: %s" repo) nil)))))

    (with-current-buffer buffer
      (unless existing
        (erase-buffer)
        (cond
         ((equal consult-gh-topic-major-mode 'markdown-mode)
          (markdown-mode)
          (insert "# title:  ")
          (save-excursion
            (and title (insert (concat title "\n")))
            (and body (insert (concat boddy "\n")))))
         ((equal consult-gh-topic-major-mode 'org-mode)
          (org-mode)
          (insert "#+title:  ")
          (save-excursion
            (and title (insert (concat title "\n")))
            (when (consult-gh--user-canadmin repo)
              (insert "\n"
                      "#+assignees: \n"
                      "#+labels: \n"
                      "#+milestone: \n"
                      "#+projects: \n\n"))
            (and body (insert (with-temp-buffer (insert body)
                                                (consult-gh--markdown-to-org (current-buffer))
                                                (consult-gh--whole-buffer-string))))))
         (t
          (text-mode)
          (insert "# title:  ")
          (save-excursion
            (and title (insert (concat title "\n")))
            (and body (insert (concat boddy "\n")))))))
      (setq-local consult-gh--topic topic)
      (consult-gh-topics-edit-mode +1)
      (with-no-warnings (outline-show-all)))
    (funcall consult-gh-switch-buffer-func buffer)))

(defun consult-gh--search-issues-transform (async &rest _)
  "Add annotation to issue candidates in `consult-gh-search-issues'.

Returns ASYNC function after formatting results with
`consult-gh--search-issues-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--search-issues-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--search-issues-format string consult-gh--current-input t))
                   (funcall ,async action)))))))

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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
      (consult--read
       (consult--async-command builder
         (consult-gh--search-issues-transform builder))
       :prompt prompt
       :lookup #'consult--lookup-member
       :state (funcall #'consult-gh--issue-state)
       :initial (consult--async-split-initial initial)
       :group #'consult-gh--issue-group
       :require-match t
       :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                            consult-gh--known-repos-list)
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
  (if current-prefix-arg
      (setq repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
  (let* ((prompt (or prompt "Search Issues:  "))
         (consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-issues prompt #'consult-gh--search-issues-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-issue-action sel))))

(defun consult-gh--pr-list-transform (async &rest _)
  "Add annotation to issue candidates in `consult-gh-pr-list'.

Returns ASYNC function after formatting results with
`consult-gh--pr-list-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--pr-list-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--pr-list-format string consult-gh--current-input nil))
                   (funcall ,async action)))))))

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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer
          \(passed as INITITAL to `consult--read'\)"
  (let* ((initial (or initial
                      (if (equal consult-gh-prioritize-local-folder 't)
                          (consult-gh--get-repo-from-directory)
                        nil))))
    (consult-gh-with-host (consult-gh--auth-account-host)
        (consult--read
         (consult--async-command builder
           (consult-gh--pr-list-transform builder))
         :prompt prompt
         :category 'consult-gh-prs
         :lookup #'consult--lookup-member
         :state (funcall #'consult-gh--pr-state)
         :initial (consult--async-split-initial initial)
         :group #'consult-gh--pr-list-group
         :require-match t
         :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                              consult-gh--known-repos-list)
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
  (if current-prefix-arg
      (setq initial (or initial (format "%s" (car (consult-gh-search-repos initial t))))))
  (let* ((prompt (or prompt "Enter Repo Name:  "))
         (sel (consult-gh--async-pr-list prompt #'consult-gh--pr-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel))))

(defun consult-gh-pr-create (&optional repo title)
  "Create a new pull request with TITLE for REPO.

This mimicks the same interactive pr creation
from “gh pr create” in the command line."

  (interactive "P")
  (let* ((repo (or repo (get-text-property 0 :repo (consult-gh-search-repos nil t "Select the head repo you want to merge from: "))))
         (canAdmin (consult-gh--user-canadmin repo))
         (head (consult--read (consult-gh-topics--pr-get-branches repo)
                              :prompt "Select the head branch: "
                              :sort t))
         (baserepo (if (y-or-n-p "Would you like to merge into another branch in the same repo?")
                       repo
                     (consult--read (consult-gh-topics--pr-get-similar repo)
                                    :prompt "Select from one of the following similar repos: "
                                    :require-match nil
                                    :sort t)))
         (basebranch (consult--read (cond
                                     ((equal baserepo repo)
                                      (remove head (consult-gh-topics--pr-get-branches baserepo)))
                                     (t (consult-gh-topics--pr-get-branches baserepo)))
                                    :prompt "Select the head repository you want to merge from: "
                                    :sort t))
         (base (cond
                ((and (not (equal baserepo repo))
                      (equal (consult-gh--get-package repo) (consult-gh--get-package baserepo)))
                 (concat (consult-gh--get-username baserepo) ":" basebranch))
                ((equal baserepo repo) basebranch)
                (t (concat baserepo ":" basebranch))))
         (templates (consult-gh--get-pr-templates baserepo))
         (body (if (and templates (length= templates 1))
                   (cdar templates)
                 (and templates (consult--read templates
                                               :prompt "Select a template: "
                                               :require-match t
                                               :lookup #'consult--lookup-cdr
                                               :sort t))))

         (buffer (get-buffer-create (format "*consult-gh-pr-create: %s (%s->%s)" repo head base)))
         (existing (not (= (buffer-size buffer) 0)))
         (topic (or repo "new pr"))
         (type "pr")
         (confirm (if existing
                      (consult--read
                       (list (cons "Resume editing the existing draft." :resume)
                             (cons "Create a new pull request from sratch, but do not discard the old one." :new)
                             (cons "Discard the old draft and create a new one from scratch." :replace))
                       :prompt "You have already started another pull request for this repository.  Would like to resume editing that one or start a new one? "
                       :lookup #'consult--lookup-cdr
                       :sort nil
                       :require-match t))))

    ;; collect issues of repo for completion at point
    (consult-gh--completion-set-issues topic repo)

    ;; collect prs of repo for completion at point
    (consult-gh--completion-set-prs topic repo)

    ;; collect mentionable users for completion at point
    (consult-gh--completion-set-mentionable-users topic repo)


    (add-text-properties 0 1 (list :number nil :type "pr" :isComment nil :new t :repo (substring-no-properties repo) :head head :base base) topic)

    (when existing
      (cond
       ((eq confirm :resume) (setq existing t))
       ((eq confirm :replace) (setq existing nil))
       ((eq confirm :new)
        (setq existing nil)
        (setq buffer (generate-new-buffer (format "*consult-gh-pr-create: %s (%s->%s)" repo head base) nil)))))

    (with-current-buffer buffer
      (unless existing
        (erase-buffer)
        (cond
         ((equal consult-gh-topic-major-mode 'markdown-mode)
          (markdown-mode)
          (insert "# title:  ")
          (save-excursion
            (and body (insert (concat body "\n")))))
         ((equal consult-gh-topic-major-mode 'org-mode)
          (org-mode)
          (insert "#+title:  ")
          (save-excursion
            (when (consult-gh--user-canadmin repo)
              (insert "\n"
                      "#+reviewers: \n"
                      "#+assignees: \n"
                      "#+labels: \n"
                      "#+milestone: \n"
                      "#+projects: \n\n"))
            (and body (insert (with-temp-buffer (insert body)
                                                (consult-gh--markdown-to-org (current-buffer))
                                                (consult-gh--whole-buffer-string))))))
         (t
          (text-mode)
          (insert "# title:  ")
          (save-excursion
            (and body (insert (concat body "\n")))))))
      (setq-local consult-gh--topic topic)
      (consult-gh-topics-edit-mode +1)
      (with-no-warnings (outline-show-all)))

    (funcall consult-gh-switch-buffer-func buffer)))

(defun consult-gh--search-prs-transform (async &rest _)
  "Add annotation to pr candidates in `consult-gh-search-prs'.

Returns ASYNC function after formatting results with
`consult-gh--search-prs-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--search-prs-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--search-prs-format string consult-gh--current-input t))
                   (funcall ,async action)))))))

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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
      (consult--read
       (consult--async-command builder
         (consult-gh--search-prs-transform builder))
       :prompt prompt
       :category 'consult-gh-prs
       :lookup #'consult--lookup-member
       :state (funcall #'consult-gh--pr-state)
       :initial (consult--async-split-initial initial)
       :group #'consult-gh--pr-search-group
       :require-match t
       :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory))
                                  (consult--async-split-thingatpt 'symbol)))
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
  (if current-prefix-arg
      (setq repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
  (let* ((prompt (or prompt "Search Pull-Requests:  "))
         (consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-prs prompt #'consult-gh--search-prs-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel))))

(defun consult-gh--search-code-transform (async &rest _)
  "Add annotation to code candidates in `consult-gh-search-code'.

Returns ASYNC function after formatting results with
`consult-gh--search-code-format'.
BUILDER is the command line builder function \(e.g.
`consult-gh--search-code-builder'\)."
  (let ((consult-gh--current-input nil))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq consult-gh--current-input action)
         (funcall ,async action))
        (t (mapcar (lambda (string)
                     (consult-gh--search-code-format string consult-gh--current-input t))
                   (funcall ,async action)))))))

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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
      (consult--read
       (consult--async-command builder
         (consult-gh--search-code-transform builder))
       :prompt prompt
       :category 'consult-gh-codes
       :lookup #'consult--lookup-member
       :state (funcall #'consult-gh--code-state)
       :initial (consult--async-split-initial initial)
       :group #'consult-gh--code-group
       :require-match t
       :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol)))
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
  (if current-prefix-arg
      (setq repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
  (let* ((prompt (or prompt "Search Code:  "))
         (consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-code prompt #'consult-gh--search-code-builder initial)))
    (setq consult-gh--open-files-list nil)
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
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
         (sel (consult-gh-with-host (consult-gh--auth-account-host)
                                    (consult--read candidates
                                                   :prompt prompt
                                                   :lookup #'consult--lookup-member
                                                   :state (funcall #'consult-gh--file-state)
                                                   :require-match t
                                                   :annotate (lambda (cand) (funcall (consult-gh--file-annotate) candidates cand))
                                                   :history t
                                                   :sort nil
                                                   :add-history (consult--async-split-thingatpt 'filename)
                                                   :history 'consult-gh--files-history
                                                   :category 'consult-gh-files
                                                   :preview-key consult-gh-preview-key
                                                   :initial initial))))

    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))

    (if noaction
        sel
      (funcall consult-gh-file-action sel))))

(defun consult-gh--dashboard-collect-assigned (&optional user &rest _)
  "Find all the Issues/PRs assigned to USER."
  (let ((issues (consult-gh--command-to-string "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--assignee" (or user "@me") "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" (format "Assigned to %s" (or user "me")) "\n" "{{end}}")))
        (prs (consult-gh--command-to-string "search" "prs" "--state" consult-gh-prs-state-to-show "--sort" "updated" "--review-requested" (or user "@me") "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" (format "Review Requested from %s" (or user "me")) "\n" "{{end}}"))))
    (append (and (stringp issues)
                 (cl-delete-duplicates (delq nil (split-string issues "\n\\|\r" t))))
            (and (stringp prs)
                 (cl-delete-duplicates (delq nil (split-string prs "\n\\|\r" t)))))))

(defun consult-gh--dashboard-collect-author (&optional user &rest _)
  "Find all the Issues/PRs authored by USER."
  (let ((issues (consult-gh--command-to-string "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--include-prs" "--author" (or user "@me") "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" (format "Authored by %s" (or user "me")) "\n" "{{end}}"))))
    (and (stringp issues)
         (cl-delete-duplicates (delq nil (string-split issues "\n\\|\r" t))))))

(defun consult-gh--dashboard-collect-involves (&optional user &rest _)
  "Find all the Issues/PRs that involve USER."
  (let ((issues (consult-gh--command-to-string "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--include-prs" "--involves" (or user "@me") "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}" "\t" (format "Involves %s" (or user "me")) "\n" "{{end}}") "--" (concat "-author:" (or user "@me")))))
    (and (stringp issues)
         (cl-delete-duplicates (delq nil (string-split issues "\n\\|\r" t))))))

(defun consult-gh--dashboard-collect-mentions (&optional user &rest _)
  "Find all the Issues/PRs that mention USER."
  (let ((issues (consult-gh--command-to-string "search" "issues" "--state" consult-gh-issues-state-to-show "--sort" "updated" "--include-prs" "--mentions" (or user "@me") "--json" "isPullRequest,repository,title,number,labels,updatedAt,state,url" "--template" (concat "{{range .}}" "{{.isPullRequest}}" "\t" "{{.repository.nameWithOwner}}" "\t" "{{.title}}" "\t" "{{.number}}" "\t" "{{.state}}" "\t" "{{.updatedAt}}" "\t" "{{.labels}}" "\t" "{{.url}}"  "\t" (format "Mentions %s" (or user "me")) "\n" "{{end}}"))))
    (and (stringp issues)
         (cl-delete-duplicates (delq nil (string-split issues "\n\\|\r" t))))))

(defun consult-gh--dashboard-items (&optional user &rest args)
  "Find all the relevant Issues/PRs for USER.
ARGS are extra arguments that will be passed to each funciton
in `consult-gh-dashboard-items-functions'."
  (let* ((items (list)))
    (cl-loop for func in consult-gh-dashboard-items-functions
             do (setq items (append items (cl-delete-duplicates (delq nil (apply func user args))))))
    (mapcar #'consult-gh--dashboard-format items)))

(defun consult-gh--dashboard (prompt &optional initial user)
  "Search USER's work on GitHub.

This is a non-interactive internal function.
For the interactive version see `consult-gh-dashboard'.

This searches relevant (e.g. mentioned, owned, review-requested, etc.)
issues and pull-requests for the USER.

Description of Arguments:

  PROMPT  the prompt in the minibuffer
          \(passed as PROMPT to `consult--red'\)
  USER    name of the GitHub user/or organization.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
                        (if-let ((candidates (consult-gh--dashboard-items user)))
                            (consult--read
                             candidates
                             :prompt prompt
                             :lookup #'consult--lookup-member
                             :state (funcall #'consult-gh--dashboard-state)
                             :initial initial
                             :group #'consult-gh--dashboard-group
                             :require-match t
                             :history 'consult-gh--search-issues-history
                             :category 'consult-gh-issues
                             :preview-key consult-gh-preview-key
                             :sort t)
                          (progn
                            (message "no items in the dashboard")
                            nil))))

;;;###autoload
(defun consult-gh-dashboard (&optional initial user noaction prompt)
  "Search GitHub for USER's work on GitHub.

This is an interactive wrapper function around
`consult-gh--dashboard'.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-issue-action'

INITIAL is an optional arg for the initial input in the minibuffer.

If PROMPT is non-nil, use it as the query prompt."
  (interactive)
  (let* ((prompt (or prompt "Search Dashboard:  "))
         (sel (consult-gh--dashboard prompt initial user)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0  :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
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
  BUILDER an async builder function passed to `consult--async-command'.
  INITIAL an optional arg for the initial input in the minibuffer.
          \(passed as INITITAL to `consult--read'\)"
  (consult-gh-with-host (consult-gh--auth-account-host)
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
         (sel (consult-gh--notifications "Select Notification:  " initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (and (stringp sel) (funcall consult-gh-notifications-action sel)
           (consult-gh--notifications-mark-as-read sel)))))

;;;###autoload
(defun consult-gh-comment-create (&optional topic)
  "Interactively create a new comment post on TOPIC.

When TOPIC is nil, queries the user to chose the TOPIC interactively."
  (interactive "P")
  (let* ((topic (or topic consult-gh--topic))
         (repo (or (and (stringp topic) (get-text-property 0 :repo topic))
                   (get-text-property 0 :repo (consult-gh-search-repos nil t))))
         (type (or (and (stringp topic) (get-text-property 0 :type topic))
                   (consult--read  (list (cons "Issues" "issue") (cons "Pull Requests" "pr"))
                                   :prompt "What topic are you looking for? "
                                   :lookup #'consult--lookup-cdr
                                   :require-match t
                                   :sort nil)))
         (number (and (stringp topic) (get-text-property 0 :number topic)))
         (buffer (get-buffer-create (format "*consult-gh-comment: %s - %s #%s" repo type number)))
         (existing-buffer (not (= (buffer-size buffer) 0))))

    (cond
     (topic
      (add-text-properties 0 1 (list :new t :isComment t) topic)
      (when (and existing-buffer (y-or-n-p "Buffer already exists.  Would you like to resume editing comment in the same buffer?"))
        (setq existing-buffer t))
      (with-current-buffer buffer
        (unless existing-buffer
          (erase-buffer)
          (cond
           ((equal consult-gh-topic-major-mode 'markdown-mode)
            (markdown-mode))
           ((equal consult-gh-topic-major-mode 'org-mode)
            (org-mode))
           (t
            (text-mode))))
        (setq-local consult-gh--topic topic)
        (consult-gh-topics-edit-mode +1)
        (goto-char (point-max))
        (with-no-warnings (outline-show-all)))
      (funcall consult-gh-switch-buffer-func buffer))
     (type
      (pcase type
        ("issue"
         (funcall #'consult-gh--issue-view-action (consult-gh-issue-list repo t))
         (consult-gh-comment-create))
        ("pr"
         (funcall #'consult-gh--pr-view-action (consult-gh-pr-list repo t))
         (consult-gh-comment-create))))
     (t (message "Did not get a topic to comment on!")))))

;;;###autoload
(defun consult-gh-topics-cancel ()
  "Cancel comment."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun consult-gh-topics-submit (&optional topic)
  "Submit comment on TOPIC."
  (interactive)
  (if consult-gh-topics-edit-mode
      (let* ((topic (or topic consult-gh--topic))
             (repo (get-text-property 0 :repo topic))
             (type (get-text-property 0 :type topic))
             (isComment (get-text-property 0 :isComment topic))
             (number (get-text-property 0 :number topic))
             (new (get-text-property 0 :new topic)))

        (cond
         (isComment
          (let ((comment (consult-gh-topics--buffer-string)))
            (consult-gh-topics--comment-submit comment repo type number)))
         ((and new (equal type "issue"))
          (consult-gh-topics--issue-presubmit topic))
         ((and new (equal type "pr"))
          (consult-gh-topics--pr-presubmit topic))))
    (message "Not in a consult-gh topic editing buffer!")))

;;;###autoload
(defun consult-gh-topics-open-in-browser (&optional topic)
  "Open the TOPIC of the current buffer in the browser.

Uses `consult-gh-browse-url-func'."
  (interactive)
  (let* ((topic (or topic consult-gh--topic))
         (type (and (stringp topic) (get-text-property 0 :type topic)))
         (repo (and (stringp topic) (get-text-property 0 :repo topic)))
         (branch (and (stringp topic) (get-text-property 0 :branch topic)))
         (path (and (stringp topic) (get-text-property 0 :path topic)))
         (number (and (stringp topic) (get-text-property 0 :number topic)))
         (url (and (stringp type) (pcase type
                                    ("file"
                                     (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/blob/%s/%s" branch path)))
                                    ("issue"
                                     (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/issues/%s" number)))
                                    ("pr"
                                     (concat (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")) (format "/pull/%s" number)))))))
    (if (stringp url)
        (funcall (or consult-gh-browse-url-func #'browse-url) url)
      (message "No topic to browse in this buffer!"))))

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
