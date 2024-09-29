;;; consult-gh.el --- Consulting GitHub Client -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 2.0
;; Package-Requires: ((emacs "29.1") (consult "1.0") (markdown-mode "2.6"))
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
(require 'org)

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
                 (function :tag "Browse URL in Firefox" browse-url-firefoxn)
                 (function :tag "Browse URL in Chrome" browse-url-chrome)
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

(defcustom consult-gh-topic-comment-mode 'markdown-mode
  "Major mode for editing comments on issues or pull requests.

Choices are:
  - \='nil            Use \='text-mode
  - \='markdown-mode  Use \='markdown-mode when available
  - \='org-mode       Use \='org-mode"
  :group 'consult-gh
  :type '(choice (const :tag "(Default) Use markdown mode" markdown-mode)
                 (const :tag "Use org mode" org-mode)
                 (const :tag "Use text-mode" nil)))

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

By default it is set to :repo, but can be any of:

  t         Use headers for marginalia info
  nil       Do not group
  :user     group by repository owner
  :type     group by candidate's type (e.g. issue, pr, ....)
  :url      group by URL
  symbol    group by another property of the candidate"
  :type '(radio (const :tag "(Default) Use Headers of Marginalia Info" t)
                (const :tag "Do Not Group" nil)
                (const :tag "Repository's full name" :repo)
                (const :tag "Repository's owner" :user)
                (const :tag "Repository's package name" :package)
                (const :tag "Type of Item" :type)
                (const :tag "Custom other field (constant)")))

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

(defcustom consult-gh-notifications-group-function #'consult-gh--notifications-group-by-date
  "What function to use for grouping items in `consult-gh-notifications'?

Common options include:

 - `consult-gh--notifications-group-by-date'   by date (e.g. latest first)
 - `consult-gh--notifications-group-by-reason' by reason of reason
                                               (e.g. mentions)
 - `consult-gh--notifications-group-by-type'   by type (e.g. issue or pr)
 - `consult-gh--notifications-group-by-repo'   by repository's name
 - `consult-gh--notifications-group-by-state'  by status (e.g. read or unread)
 - A custom function:                          A function with similar
                                               format as examples above"
  :group 'consult-gh
  :type '(choice (function :tag "Group by date (e.g. latest notification first)" consult-gh--notifications-group-by-date)
                 (function :tag "Group by Reason (e.g. mentions, involves,...)" consult-gh--notifications-group-by-reason)
                 (function :tag "Group by topic type (e.g. issue or pr)" consult-gh--notifications-group-by-type)
                 (function :tag "Group by repository's name" consult-gh--notifications-group-by-repo)
                 (function :tag "Group by state (e.g. read or unread notification" consult-gh--notifications-group-by-state)
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

(defcustom consult-gh-dashboard-group-function #'consult-gh--dashboard-group-by-reason
  "What function to use for grouping items in `consult-gh-dashboard'?

Common options include:

 - `consult-gh--dashboard-group-by-reason' by reason of reason
                                              (e.g. mentions)
 - `consult-gh--dashboard-group-by-date'      by date of latest update
 - `consult-gh--dashboard-group-by-type'      by type (e.g. issue or pr)
 - `consult-gh--dashboard-group-by-repo'      by repository's name
 - A custom function:                         A function with similar
                                              format as examples above"
  :group 'consult-gh
  :type '(choice (function :tag "Group by Reason (e.g. mentions, involves,...)" consult-gh--dashboard-group-by-reason)
                 (function :tag "Group by topic type (e.g. issue or pr)" consult-gh--dashboard-group-by-type)
                 (function :tag "Group by repository's name" consult-gh--dashboard-group-by-repo)
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

(defvar consult-gh--current-input nil
  "Current input of user query.")

(defvar consult-gh--auth-current-account nil
  "Current logged-in and active account.
This is a list of \='(USERNAME HOST IF-ACTIVE)")

(defvar consult-gh-default-host "github.com"
  "Defualt host of GitHub.")

(defvar-local consult-gh--topic nil
  "Topic in consult-gh prview buffers.")

(defvar consult-gh--override-group-by nil
  "Override grouping based on user input.

This is used to change grouping dynamically.")

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
          (while (re-search-forward "\\[\\(?1:.+?\\)\\]\\[\\]\\{1\\}\\|\\[\\(?2:.[^\\[]+?\\)\\]\\[\\(?3:.[^\\[]+?\\)\\]\\{1\\}\\|\\[\\(?4:.+?\\)\\]\(#\\(?5:.+?\\)\)\\{1\\}\\|.\\[\\(?6:.+?\\)\\]\(\\(?7:[^#].+?\\)\)\\{1\\}" nil t)
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
               (replace-match " [[\\7][\\6]]"))))

          (goto-char (point-min))
          (while
              (re-search-forward
               "\\[fn:\\(.+?\\)\\]\\{1\\}" nil t)
            (pcase (match-string 0)
              ((pred (lambda (el) (string-match-p "\\[fn:.+?[[:blank:]].+?\\]\\{1\\}" (substring-no-properties el))))
               (progn
                 (replace-regexp-in-region "[[:blank:]]" "_" (match-beginning 1) (match-end 1)))))))))
    nil))

(defun consult-gh--markdown-to-org (&optional buffer)
  "Convert from markdown format to \='org-mode format in BUFFER.

This is used for viewing repos \(a.k.a. fetching README file of repos\)
or issue, when `consult-gh-repo-preview-mode' or
`consult-gh-issue-preview-mode'  is set to \='org-mode."
  (let ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name))))
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

;;; Backend `gh` related functions
(defun consult-gh--call-process (&rest args)
  "Run “gh” in the command line and passes ARGS as command-line arguments.

Returns a list where the CAR is exit status
\(e.g. 0 means success and non-zero means error\) and CADR is the output's text.
If gh is not found it returns \(127 “”\)
and a message saying “gh” is not found."
  (if (executable-find "gh")
      (with-temp-buffer
        (set-buffer-file-coding-system 'cp1047)
        (consult-gh-with-host (consult-gh--auth-account-host)
          (list (apply #'call-process "gh" nil (current-buffer) nil args)
                (replace-regexp-in-string "" "\n"
                                        (buffer-string)))))
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

(defun consult-gh--api-json-to-hashtable (json &optional key)
  "Convert a JSON object to a hash table.

Uses lists for arrays and symbols for keys.

ig optional argument KEY is non-nil, returns only the value of KEY."
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
  (consult-gh--api-json-to-hashtable (cadr (consult-gh--api-get-json "user")) :login))

(defun consult-gh--get-current-orgs (&optional include-user)
  "Get the organizations for currently logged in user.

Runs “gh api user/orgs” and returns the login field of json data.
When INCLUDE-USER is non-nil, add the name of the user the list."
  (let ((data (consult-gh--api-get-json "user/orgs")))
    (if (eq (car data) 0)
        (let ((table (consult-gh--api-json-to-hashtable (cadr data))))
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
          (consult-gh--api-json-to-hashtable
           (cadr
            (consult-gh--api-get-json "licenses")))))

(defun consult-gh--get-user-template-repos (&optional user)
  "List template repository for authenticated user.

If the optional argument USER is non-nil, gets repos of User instead."
  (let ((endpoint (if user (format "users/%s/repos" user) "user/repos")))
    (delq nil (mapcar (lambda (item) (when (eq (gethash :is_template item) t)
                                       (gethash :full_name item)))
                      (consult-gh--api-json-to-hashtable
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

It constructs built-in arguments for count and page, ..., and
it also sets `consult-gh--override-group-by' if and argument
for grouping is provided in options."
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

(defun consult-gh--files-get-branches (repo)
  "List branches of REPO, in json format.

uses `consult-gh--api-get-json' to get branches from GitHub API."
  (consult-gh--api-get-json (concat "repos/" repo "/branches")))

(defun consult-gh--files-branches-hashtable-to-list (table repo)
  "Convert TABLE with branches of REPO to a list of propertized text.

TABLE can for example be obtained by converting the json object from
`consult-gh--files-get-branches' to a hash table
by using `consult-gh--api-json-to-hashtable'."
  (mapcar (lambda (item) (cons (gethash :name item)
                               `(:repo ,repo
                                       :branch ,(gethash :name item)
                                       :url ,(gethash :url item))))
          table))

(defun consult-gh--files-branches-list-items (repo)
  "Return REPO's information in propertized text format.

Uses `consult-gh--files-get-branches',
`consult-gh--files-branches-hashtable-to-list',
and `consult-gh--api-json-to-hashtable'."
  (let ((response (consult-gh--files-get-branches repo)))
    (if (eq (car response) 0)
        (consult-gh--files-branches-hashtable-to-list
         (consult-gh--api-json-to-hashtable (cadr response)) repo)
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
                      (consult-gh--api-json-to-hashtable (cadr response) :tree) repo branch))
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
         (content (if (eq (car response) 0) (consult-gh--api-json-to-hashtable (cadr response) :content)
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
  (let ((name (car (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s"))))))
    (if transform (substring cand) name)))

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
  (let* ((name (consult-gh--group-function cand transform)))
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
           (name (consult-gh--api-json-to-hashtable info :name))
           (desc (consult-gh--api-json-to-hashtable info :description))
           (readme (cadr (consult-gh--api-get-json (format "repos/%s/readme" repo))))
           (path (consult-gh--api-json-to-hashtable readme :path))
           (extension (and (stringp path) (file-name-extension path)))
           (content (consult-gh--api-json-to-hashtable readme :content)))
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
           (name (consult-gh--api-json-to-hashtable info :name))
           (desc (consult-gh--api-json-to-hashtable info :description))
           (readme (cadr (consult-gh--api-get-json (format "repos/%s/readme" repo))))
           (path (consult-gh--api-json-to-hashtable readme :path))
           (content (consult-gh--api-json-to-hashtable readme :content)))
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
  (let* ((name (consult-gh--group-function cand transform)))
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
         (text-main (cadr (consult-gh--call-process "issue" "view" number "--repo" repo)))
         (title (or title (and (stringp text-main) (car-safe (split-string text-main "\n" t)))))
         (title (and (stringp title) (string-trim-left title "title:\t")))
         (text-comments (cadr (consult-gh--call-process "issue" "view" number "--repo" repo "--comments")))
         (topic (format "%s/#%s" repo number)))
    (add-text-properties 0 1 (list :repo repo :type "issue" :number number :title title) topic)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (string-trim text-main))
      (insert "\n--\n")
      (insert (string-trim text-comments))
      (goto-char (point-min-marker))
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
        (_ ()))
      (consult-gh-issue-view-mode +1)
      (setq-local consult-gh--topic topic)
      (current-buffer))))

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
         (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/issues/" number "*")))
    (switch-to-buffer (consult-gh--issue-view repo number))
    (rename-buffer buffername t)))

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
  (let* ((name (consult-gh--group-function cand transform)))
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
  (let* ((name (consult-gh--group-function cand transform)))
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

(defun consult-gh--pr-view (repo number &optional buffer title)
  "Open pull request, PR of REPO in an Emacs buffer, BUFFER.

This is an internal function that takes REPO, the full name of a repository
\(e.g. “armindarvish/consult-gh”\) and PR, a pr number of that repository,
and shows the contents of the pr in an Emacs buffer.

It fetches the preview of the PR by runing “gh or view PR --repo REPO”
using `consult-gh--call-process' and puts it as raw text in either
BUFFER, or if BUFFER is nil, in a buffer named by
`consult-gh-preview-buffer-name'.

If `consult-gh-issue-preview-mode' is non-nil, uses it as
major-mode, otherwise shows the raw text in \='fundamental-mode.

Description of Arguments:

  REPO   the full name of the repository to be previewed.
  NUMBER    pull request number
  BUFFER an optional buffer the preview should be shown in.
  TITLE  is an optional title string

To use this as the default action for PRs,
see `consult-gh--pr-view-action'."
  (let* ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
        (text-main (cadr (consult-gh--call-process "pr" "view" number "--repo" repo)))
        (title (or title (and (stringp text-main) (car-safe (split-string text-main "\n" t)))))
        (title (and (stringp title) (string-trim-left title "title:\t")))
        (text-comments (cadr (consult-gh--call-process "pr" "view" number "--repo" repo "--comments")))
        (topic (format "%s/#%s" repo number)))
    (add-text-properties 0 1 (list :repo repo :type "pr" :number number :title title) topic)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (string-trim text-main))
      (insert "\n--\n")
      (insert (string-trim text-comments))
      (goto-char (point-min-marker))
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
        (_ ()))
      (consult-gh-pr-view-mode +1)
      (setq-local consult-gh--topic topic)
      (current-buffer))))

(defun consult-gh--pr-view-action (cand)
  "Opens the preview of a pull request candidate, CAND.

This is a wrapper function around `consult-gh--pr-view'.  It parses CAND
to extract relevant values \(e.g. repository's name and pull request
number\) and passes them to `consult-gh--pr-view'.

To use this as the default action for prs,
set `consult-gh-pr-action' to `consult-gh--pr-view-action'."
  (let* ((repo (substring-no-properties (get-text-property 0 :repo cand)))
         (number (substring-no-properties (format "%s" (get-text-property 0 :number cand))))
         (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/pull/" number "*")))
    (switch-to-buffer (consult-gh--pr-view repo number))
    (rename-buffer buffername t)))

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
  (let* ((name (consult-gh--group-function cand transform)))
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

(defun consult-gh--dashboard-no-group (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group items in the dashboard.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (concat
                  (consult-gh--set-string-width "Repo - Type Number: Title " 83 nil ?-)
                  (consult-gh--set-string-width " Reason " 10 nil ?-)
                  (consult-gh--set-string-width " Date " 12 nil ?-)
                  (consult-gh--set-string-width " State " 8 nil ?-)
                  " Tags ")))
    (if transform (substring cand) name)))

(defun consult-gh--dashboard-group-by-date (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group issues by their update date.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :date cand))))
    (if transform (substring cand) name)))

(defun consult-gh--dashboard-group-by-type (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group issues by their state e.g. “Open”, “Closed”, “Merged”,
etc.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :type cand))))
    (if transform (substring cand) name)))

(defun consult-gh--dashboard-group-by-reason (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group dashboard items by reason e.g. “Mentions”, “Assigned”,
“Review Requested”, etc.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (get-text-property 0 :reason cand)))
    (if transform (substring cand) name)))

(defun consult-gh--dashboard-group-by-repo (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group items in the dashboard by repository names.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (car (last (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s")))))))
    (if transform (substring cand) name)))

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
  "Make cmd aerguments for notifications."
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

(defun consult-gh--notifications-no-group (cand transform)
  "Group function for dashboard candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-dashboard'
and is used to group items in the dashboard.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let* ((name (concat
                  (consult-gh--set-string-width "Repo - Type Number: Title " 78 nil ?-)
                  (consult-gh--set-string-width " Reason " 15 nil ?-)
                  (consult-gh--set-string-width " State " 9 nil ?-)
                  (consult-gh--set-string-width " Date " 11 nil ?-))))
    (if transform (substring cand) name)))

(defun consult-gh--notifications-group-by-date (cand transform)
  "Group function for notification candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notification',
and is used to group notifications by their update date.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :date cand))))
    (if transform (substring cand) name)))

(defun consult-gh--notifications-group-by-relative-timeago (cand transform)
  "Group function for notification candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notification',
and is used to group notifications by their update date.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :reltime cand))))
    (if transform (substring cand) name)))

(defun consult-gh--notifications-group-by-type (cand transform)
  "Group function for notification candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notification',
and is used to group notifications by their type e.g. “issue”, “pr”, etc.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :type cand))))
    (if transform (substring cand) name)))

(defun consult-gh--notifications-group-by-reason (cand transform)
  "Group function for notifications candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notifications',
and is used to group dashboard items by reason e.g. “subscribed” or
“assigned”, etc.

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (get-text-property 0 :reason cand)))
    (if transform (substring cand) name)))

(defun consult-gh--notifications-group-by-repo (cand transform)
  "Group function for notification candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notification',
and is used to group notifications by their repository

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :repo cand))))
    (if transform (substring cand) name)))

(defun consult-gh--notifications-group-by-state (cand transform)
  "Group function for notification candidates, CAND.

This is passed as GROUP to `consult--read' in `consult-gh-notification',
and is used to group notifications by their state e.g. “unread” or “seen”

If TRANSFORM is non-nil, the CAND itself is returned."
  (let ((name (upcase (get-text-property 0 :state cand))))
    (if transform (substring cand) name)))

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
  "C-c C-r" #'consult-gh-topics-create-comment
  "C-c o" #'consult-gh-topics-open-in-browser)

;;;###autoload
(define-minor-mode consult-gh-issue-view-mode
  "Minor-mode for viewing issues."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-issue-view"
  :keymap consult-gh-issue-view-mode-map)

(defvar-keymap consult-gh-pr-view-mode-map
  :doc "Consult-gh topics keymap."
  "C-c C-r" #'consult-gh-topics-create-comment
  "C-c o" #'consult-gh-topics-open-in-browser)

;;;###autoload
(define-minor-mode consult-gh-pr-view-mode
  "Minor-mode for viewing issues."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-pr-view"
  :keymap consult-gh-pr-view-mode-map)

(defun consult-gh-topics-edit-comment-header-line ()
  "Create `header-line-format' for consult-gh-topics."
  (let* ((topic consult-gh--topic)
         (repo (get-text-property 0 :repo topic))
         (type (get-text-property 0 :type topic))
         (number (get-text-property 0 :number topic))
         (cand (format "%s:#%s" repo number)))
    (add-text-properties 0 1 (list :repo repo :number number) cand)
    (list
     (concat "New comment on "
             (buttonize (format "%s: %s #%s" (consult-gh--get-package repo) (upcase type) number)
                        `(lambda (&rest _)
                           (pcase ,type
                             ("issue"
                              (funcall consult-gh-issue-action ,cand))
                             ("pr"
                              (funcall consult-gh-pr-action ,cand))))))
     ".  "
     (substitute-command-keys "When done, use `\\[consult-gh-topics-comment-submit]' to submit or `\\[consult-gh-topics-comment-cancel]' to cancel."))))

(defvar-keymap consult-gh-topics-edit-comment-mode-map
  :doc "Consult-gh topics keymap."
  "C-c C-c" #'consult-gh-topics-comment-submit
  "C-c C-k" #'consult-gh-topics-comment-cancel)

;;;###autoload
(define-minor-mode consult-gh-topics-edit-comment-mode
  "Minor-mode for viewing topics."
  :init-value nil
  :global nil
  :group 'consult-gh
  :lighter " consult-gh-topics-edit-comment"
  :keymap consult-gh-topics-edit-comment-mode-map
  (setq-local header-line-format (consult-gh-topics-edit-comment-header-line)))

;;;###autoload
(defun consult-gh-auth-switch (&optional host user)
  "Switch between authenticated accounts.

If the optional arguments, HOST and USER are non-nil, use them for
authenticaiton otherwise query the user to select an account."
  (interactive "P")
  (unless (and host user)
    (let* ((accounts (consult-gh--auth-accounts))
           (sel (consult--read accounts
                               :prompt "Select Account:"
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
(defun consult-gh-repo-list (&optional initial noaction)
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

For more details on consult--async functionalities,
see `consult-grep' and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if current-prefix-arg
      (setq initial (or initial (format "%s" (car (string-split (car (consult-gh-search-repos initial t)) "/"))))))
  (let* ((sel (consult-gh--async-repo-list "Enter Org Name:  " #'consult-gh--repo-list-builder initial)))
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
(defun consult-gh-search-repos (&optional initial noaction)
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

For more details on consult--async functionalities,
see `consult-grep' and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (let* ((host (consult-gh--auth-account-host))
         (sel (if (stringp host)
                  (with-environment-variables
                      (("GH_HOST" (or host consult-gh-default-host)))
          (consult-gh--async-search-repos "Search Repos:  " #'consult-gh--search-repos-builder initial))
                (consult-gh--async-search-repos "Search Repos:  " #'consult-gh--search-repos-builder initial))))
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

(defun consult-gh-orgs (&optional orgs noaction)
  "List repositories of ORGS.

This is a wrapper function around `consult-gh--repo-list'.
If ORGS is nil, this simply calls `consult-gh--repo-list'.
If ORGS is a list, then it runs `consult-gh--repo-list' on every member
of ORGS and returns the results \(repositories of all ORGS\).

if NOACTION is non-nil, return the candidate without runing action."
  (if (not orgs)
      (consult-gh-repo-list nil noaction))
  (let* ((candidates (consult--slow-operation "Collecting Repos ..." (apply #'append (mapcar (lambda (org) (consult-gh--repo-list org)) orgs))))
         (sel (consult-gh-with-host (consult-gh--auth-account-host)
                  (consult--read candidates
                                 :prompt "Select Repo: "
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
(defun consult-gh-issue-list (&optional initial noaction)
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

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'"
  (interactive)
  (if current-prefix-arg
      (setq initial (or initial (format "%s" (car (consult-gh-search-repos initial t))))))
  (let ((sel (consult-gh--async-issue-list "Enter Repo Name:  " #'consult-gh--issue-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-issue-action sel))))

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
(defun consult-gh-search-issues (&optional initial repo noaction)
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

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if current-prefix-arg
      (setq repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
  (let* ((consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-issues "Search Issues:  " #'consult-gh--search-issues-builder initial)))
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
(defun consult-gh-pr-list (&optional initial noaction)
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

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if current-prefix-arg
      (setq initial (or initial (format "%s" (car (consult-gh-search-repos initial t))))))

  (let ((sel (consult-gh--async-pr-list "Enter Repo Name:  " #'consult-gh--pr-list-builder initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel))))

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
(defun consult-gh-search-prs (&optional initial repo noaction)
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

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (if current-prefix-arg
      (setq repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
  (let* ((consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-prs "Search Pull-Requests:  " #'consult-gh--search-prs-builder initial)))
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
(defun consult-gh-search-code (&optional initial repo noaction)
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

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive)
  (setq consult-gh--open-files-list nil
        consult-gh--current-tempdir (consult-gh--tempdir))
  (if current-prefix-arg
      (setq repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
  (let* ((consult-gh-args (if repo (append consult-gh-args `("--repo " ,(format "%s" repo))) consult-gh-args))
         (sel (consult-gh--async-search-code "Search Code:  " #'consult-gh--search-code-builder initial)))
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
(defun consult-gh-find-file (&optional repo branch initial noaction)
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
  (let* ((repo (or repo (substring-no-properties (get-text-property 0 :repo (consult-gh-search-repos repo t)))))
         (branch (or branch (format "%s" (cdr (consult-gh--read-branch repo)))))
         (candidates (mapcar #'consult-gh--file-format (consult-gh--files-nodirectory-items repo branch)))
         (sel (consult-gh-with-host (consult-gh--auth-account-host)
                                    (consult--read candidates
                                                   :prompt "Select File: "
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
                             :group consult-gh-dashboard-group-function
                             :require-match t
                             :history 'consult-gh--search-issues-history
                             :category 'consult-gh-issues
                             :preview-key consult-gh-preview-key
                             :sort t)
                          (progn
                            (message "no items in the dashboard")
                            nil))))

;;;###autoload
(defun consult-gh-dashboard (&optional initial user noaction)
  "Search GitHub for USER's work on GitHub.

This is an interactive wrapper function around
`consult-gh--dashboard'.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-issue-action'

INITIAL is an optional arg for the initial input in the minibuffer."
  (interactive)
  (let* ((sel (consult-gh--dashboard "Search Dashboard:  " initial user)))
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
                             :group consult-gh-notifications-group-function
                             :require-match t
                             :history 'consult-gh--notifications-history
                             :category 'consult-gh-notifications
                             :preview-key consult-gh-preview-key
                             :sort nil)
                          (progn (message "No new notifications!") nil))))

;;;###autoload
(defun consult-gh-notifications (&optional initial noaction)
  "Search GitHub for User's work on GitHub.

This is an interactive wrapper function around
`consult-gh--dashboard'.

Upon selection of a candidate either
 - if NOACTION is non-nil  candidate is returned
 - if NOACTION is nil      candidate is passed to `consult-gh-issue-action'

INITIAL is an optional arg for the initial input in the minibuffer."
  (interactive)
  (let* ((sel (consult-gh--notifications "Select Notification:  " initial)))
    ;;add org and repo to known lists
    (when-let ((reponame (and (stringp sel) (get-text-property 0 :repo sel))))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (and (stringp sel) (get-text-property 0 :user sel))))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (and (stringp sel) (funcall consult-gh-notifications-action sel)
           (consult-gh--notifications-mark-as-read sel)))))

(defun consult-gh-topics-comment-cancel ()
  "Cancel comment."
  (interactive)
  (kill-buffer (current-buffer)))

(defun consult-gh-topics-comment-submit (&optional topic)
  "Submit comment on TOPIC."
  (interactive)
  (save-mark-and-excursion
    (let* ((text (buffer-string))
           (topic (or topic consult-gh--topic))
           (repo (get-text-property 0 :repo topic))
           (type (get-text-property 0 :type topic))
           (number (get-text-property 0 :number topic)))
      (when text
        (if (string-empty-p text)
            (message "Comment is empty!")
          (pcase type
            ((or "issue" "pr")
             (and
              (consult-gh--command-to-string "api" (format "repos/%s/issues/%s/comments" repo number) "-f" (format "body=%s" text))
              (kill-buffer)))
            ("discussion"
             (message "Commenting on discussions is not supported, yet!"))))))))

(defun consult-gh-topics-create-comment (&optional topic)
  "Interactivel create a new comment post on TOPIC."
  (interactive "P")
  (let* ((topic (or topic consult-gh--topic))
         (repo (get-text-property 0 :repo topic))
         (type (get-text-property 0 :type topic))
         (number (get-text-property 0 :number topic))
         (buffer (get-buffer-create (format "*consult-gh-topics-comment: %s - %s #%s" repo type number)))
         (existing nil))
    (cond
     ((not (= (buffer-size buffer) 0))
      (when (y-or-n-p "Buffer already exists.  Would you like to resume editing comment in the same buffer?")
        (setq existing t)))
     (t (setq existing nil)))
    (with-current-buffer buffer
      (unless existing
        (erase-buffer)
        (cond
         ((equal consult-gh-topic-comment-mode 'markdown-mode)
          (markdown-mode))
         ((equal consult-gh-topic-comment-mode 'org-mode)
          (org-mode))
         (t
          (text-mode))))
      (setq-local consult-gh--topic topic)
      (consult-gh-topics-edit-comment-mode +1)
      (goto-char (point-max))
      (with-no-warnings (outline-show-all)))
    (switch-to-buffer buffer)))

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

(defun consult-gh (&rest args)
  "Convinient wrapper function for favorite interactive command.

Calls the function in `consult-gh-default-interactive-command'
and passes ARGS to it."
  (interactive)
  (apply (or consult-gh-default-interactive-command #'consult-gh-search-repos) args))

;;; provide `consult-gh' module

(provide 'consult-gh)

;;; consult-gh.el ends here
