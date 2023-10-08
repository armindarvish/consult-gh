;;; consult-gh.el --- Consulting GitHub Client -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.11
;; Package-Requires: ((emacs "27.1") (consult "0.34") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: convenience, matching, tools, vc

;;; Commentary:

;;; Code:

;;; Requirements

(eval-when-compile
(require 'consult)
(require 'json)
)

(require 'crm)

;;; Group

(defgroup consult-gh nil
  "Consulting GitHub CLI"
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'magit
  :prefix "consult-gh-")

;;; Customization Variables

(defcustom consult-gh-args '("gh")
  "Command line arguments to call GitHub CLI used in async calls.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-gh
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-gh-tempdir (expand-file-name "consult-gh" temporary-file-directory)
  "Temporary file directory for the `consult-gh' package. This directory is used for storing temporary files when pulling files for viewing"
  :group 'consult-gh
  :type 'string
  )

(defcustom consult-gh-crm-separator crm-separator
  "Separator for multiple selections with completing-read-multiple. for more info see `crm-separator'. Uses crm-separator for default."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-repo-maxnum 30
  "Maximum number of output for showing repos with gh list and search operations normally passed to \"--limit\" in the command line. The default is set to gh's default number which is 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-issue-maxnum 30
  "Maximum number of output for `gh issue list` and search operations normally passed to \"--limit\" in the command line. The default is set to gh's default number which is 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-pr-maxnum 30
  "Maximum number of output for `gh pr list` and search operations normally passed to \"--limit\" in the command line. The default is set to gh's default number which is 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-code-maxnum 30
  "Maximum number of output for `gh search code` normally passed to \"--limit\" in the command line. The default is set to gh's default number which is 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-issues-state-to-show "open"
  "The state of issues that will be listed by `consult-gh-issue-list' functions. This is what is passed to \"--state\" argument in the command line when runing `gh issue list`. The possible options are \"open\", \"closed\" or\"all\". The default value is, \"open\", the same s `gh` default value."
  :group 'consult-gh
  :type '(choice "open" "closed" "all"))

(defcustom consult-gh-prs-state-to-show "open"
  "The state of issues that will be listed by `consult-gh-issue-list' functions. This is what is passed to \"--state\" argument in the command line when runing `gh issue list`. The possible options are \"open\", \"closed\" or\"all\". The default value is, \"open\", the same s `gh` default value."
  :group 'consult-gh
  :type '(choice "open" "closed" "merged" "all"))

(defcustom consult-gh-large-file-warning-threshold large-file-warning-threshold
  "Maximum size of file above which `consult-gh' requests a confirmation for previewing, opening or saving the file. Default value is set by `large-file-warning-threshold'."
  :group 'consult-gh
  :type '(choice integer (const :tag "Never request confirmation" nil)))

(defcustom consult-gh-prioritize-local-folder 'suggest
  "This varibale defines how `gh` selects repositories and it can either be the symbol 'suggest or a boolean.

If 'suggest, consult-gh adds the git repository from the local folder (a.k.a. `default-directory'), to the history list so it can quickly be accessed by navigating history lists (i.e. `next-history-element' (default keybinding `M-n`)) when running commands such as `consult-gh-issue-list' or `consult-gh-find-file'.

If t, `consult-gh' uses the git repository from the local folder (a.k.a. `default-directory') as initial-input value for commands such as `consult-gh-issue-list' or `consult-gh-find-file'. The user can still change the entry. If there is no GitHub repository in the current folder, it falls back on no initial-value.

If nil, consult-gh ignores the GitHub repository from the local folder (a.k.a. `default-directory') (default keybinding `M-n`)."

  :group 'consult-gh
  :type '(choice boolean (symbol 'suggest)))

(defcustom consult-gh-preview-buffer-mode 'markdown-mode
  "Major mode to show README of repositories in preview. choices are 'markdown-mode or 'org-mode"
  :group 'consult-gh
  :type 'symbol)

(defcustom consult-gh-default-orgs-list (list)
  "List of default github orgs. A good choice would be to add personal accounts or frequently visited github accounts to this list"
  :group 'consult-gh
  :type 'list)

(defcustom consult-gh-preview-buffer-name "*consult-gh-preview*"
  "Default name to use for preview buffers showing repo readmes retrieved by \"gh repo view\"."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-show-preview nil
  "This variable determines whether `consult-gh' shows previews. It turns previews on/off globally for all categories: repos, issues, files."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-preview-key consult-preview-key
  "Preview key for consult-gh. This is similar `consult-preview-key' but explicitly for consult-gh and it is used by all categories: repos, issues, files in consult-gh. Commands that use this include `consult-gh-orgs', `consult-gh-search-repos', `consult-gh-issue-list',`consult-gh-find-file', and etc."
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))

(defcustom consult-gh-default-clone-directory "~/"
  "Default directory to clone github repos used by `consult-gh-repo-clone' and `consult-gh--repo-clone-action'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-default-save-directory "~/Downloads/"
  "Default directory to save files pulled from github (for single files and not cloning repositories) used by `consult-gh--files-save-file-action'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-confirm-before-clone t
  "This variable defines whether `consult-gh' queries the user for a path and a name before cloning a repo or uses the default directory and package name. It's useful to set this to nil when cloning multiple repos all at once frequently."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-confirm-name-before-fork nil
  "This variable defines whether `consult-gh' queries the user for a name before forking a repo or uses the default repo name. By default it is set to nil."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-ask-for-path-before-save t
  "This variable defines whether `consult-gh' queries the user for a path before saving a file or uses the default directory and `buffer-file-name'. It may be useful to set this to nil if saving multiple files all at once frequently."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-default-branch-to-load "ask"
  "This determines how `consult-gh' loads repository branches. Possible Values are:

\"confirm\": Ask for confirmation if \"HEAD\" branch should be loaded. If the answer is no, then the user gets to chose a different branch.
\"ask\": Asks the user to select a branch.
'nil: loads the \"HEAD\" branch
A STRING: loads the branch STRING.
*Note that setting this to a STRING would mean that this STRING is used for any repository that is fetched with `consult-gh' and if the branch does not exist, it will cause an error. Therefore using a STRING is not recommended as a general case but in temporary settings where one is sure the branch exists on the repositories being fetched.*"
  :group 'consult-gh
  :type '(choice "confirm" "ask" string (const nil)))

(defcustom consult-gh-repo-action #'consult-gh--repo-browse-url-action
  "This variable defines the function that is used when selecting a repo. By default it is bound to `consult-gh--repo-browse-url-action', but can be changed to other actions such as `Consult-gh--repo-browse-files-action', `consult-gh--repo-view-action' `consult-gh--repo-clone-action', `consult-gh--repo-fork-action' or any other user-defined function that follows patterns similar to those."
  :group 'consult-gh
  :type 'function)

(defcustom consult-gh-issue-action #'consult-gh--issue-browse-url-action
  "This variable defines the function that is used when selecting an issue. By default it is bound to `consult-gh--issue-browse-url-action', but can be changed to other actions such as `consult-gh--issue-view-action' or similar user-defined custom actions."
  :group 'consult-gh
  :type 'function)

(defcustom consult-gh-pr-action #'consult-gh--pr-browse-url-action
  "This variable defines the function that is used when selecting a pr. By default it is bound to `consult-gh--pr-browse-url-action', but can be changed to other actions such as `consult-gh--pr-view-action' or similar user-defined custom actions."
  :group 'consult-gh
  :type 'function)

(defcustom consult-gh-code-action #'consult-gh--code-browse-url-action
  "This variable defines the function that is used when selecting a code search result. By default it is bound to `consult-gh--code-browse-url-action',but can be changed to other actions such as `consult-gh--code-view-action', or similar user-defined custom actions"
  :group 'consult-gh
  :type 'function)

(defcustom consult-gh-file-action #'consult-gh--files-browse-url-action
  "This variable defines the function that is used when selecting a file. By default it is bound to `consult-gh--browse-files-url-action',but can be changed to other actions such as `consult-gh--files-view-action', `consult-gh--files-save-file-action', or similar user-defined custom actions"
  :group 'consult-gh
  :type 'function)

(defcustom consult-gh-highlight-matches t
  "This variable defines whether `consult-gh' queries the user for a path before saving a file or uses the default directory and `buffer-file-name'. It may be useful to set this to nil if saving multiple files all at once frequently."
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
  "Category symbol for prs in `consult-gh' package.")

(defvar consult-gh-codes-category 'consult-gh-codes
  "Category symbol for codes in `consult-gh' package.")

(defvar consult-gh-orgs-category 'consult-gh-orgs
  "Category symbol for orgs in `consult-gh' package.")

(defvar consult-gh-files-category 'consult-gh-files
  "Category symbol for files in `consult-gh' package.")

(defvar consult-gh--preview-buffers-list (list)
  "List of currently open preview buffers")

(defvar consult-gh--orgs-history nil
  "History variable for orgs used in  `consult-gh-repo-list'.")

(defvar consult-gh--repos-history nil
  "History variable for repos used in `consult-gh-issue-list', and `consult-gh-pr-list'.")

(defvar consult-gh--search-repos-history nil
  "History variable for searching repos in  `consult-gh-search-repos'.")

(defvar consult-gh--search-issues-history nil
  "History variable for issues used in  `consult-gh-search-issues' .")

(defvar consult-gh--search-prs-history nil
  "History variable for pull requaests used in  `consult-gh-search-issues' .")

(defvar consult-gh--search-code-history nil
  "History variable for pull requaests used in  `consult-gh-search-code' .")

(defvar consult-gh--files-history nil
  "History variable for files used in  `consult-gh-find-file' .")

(defvar consult-gh--known-orgs-list nil
  "List of previously visited orgs by `consult-gh-orgs'.")

(defvar consult-gh--known-repos-list nil
  "List of previously visited repos for `consult-gh-search-repos'.")

;;; Faces
(defface consult-gh-success-face
  `((t :inherit 'success))
  "the face for user annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-constant-face'.")

(defface consult-gh-warning-face
  `((t :inherit 'warning))
  "the face for user annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-constant-face'.")

(defface consult-gh-error-face
  `((t :inherit 'error))
  "the face for user annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-constant-face'.")

(defface consult-gh-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "the face for user annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-constant-face'.")

(defface consult-gh-preview-match-face
  `((t :inherit 'consult-preview-match))
  "the face for user annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-constant-face'.")

(defface consult-gh-default-face
  `((t :inherit 'default))
  "default face used for listing items in minibuffer by `consult-gh'; by default inherits from `default'.")

(defface consult-gh-user-face
  `((t :inherit 'font-lock-constant-face))
  "the face for user annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-constant-face'.")

(defface consult-gh-package-face
  `((t :inherit 'font-lock-type-face))
  "the face for package annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-type-face'.")

(defface consult-gh-repo-face
  `((t :inherit 'font-lock-type-face))
  "the face for repository annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-type-face'.")

(defface consult-gh-issue-face
  `((t :inherit 'warning))
"the face for issue number annotation in minibuffer by `consult-gh'; by default inherits from `error'.")

(defface consult-gh-pr-face
  `((t :inherit 'warning))
"the face for issue number annotation in minibuffer by `consult-gh'; by default inherits from `error'.")


(defface consult-gh-branch-face
  `((t :inherit 'font-lock-string-face))
  "the face for branch annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-string-face'.")

(defface consult-gh-visibility-face
  `((t :inherit 'font-lock-warning-face))
"the face visibility annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-warning-face'.")

(defface consult-gh-date-face
  `((t :inherit 'font-lock-keyword-face))
  "the face for date annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-keyword-face'.")

(defface consult-gh-tags-face
  `((t :inherit 'font-lock-comment-face))
  "the face for tags/comments annotation in minibuffer by `consult-gh'; by default inherits from `font-lock-comment-face'.")

(defface consult-gh-description-face
  `((t :inherit 'font-lock-builtin-face))
  "the face for repository description annotation in minibuffer by `consult-gh'. by default inherits from `font-lock-builtin-face'.")

(defface consult-gh-code-face
  `((t :inherit 'font-lock-variable-use-face))
  "the face for repository description annotation in minibuffer by `consult-gh'. by default inherits from `font-lock-builtin-face'.")

(defface consult-gh-url-face
  `((t :inherit 'link))
"the face for url annotation in minibuffer by `consult-gh'. by default inherits from `link'.")

;;; Utility functions

(defun consult-gh--nonutf-cleanup (string)
"Remove non UTF-8 characters if any in the string."
  (string-join
   (delq nil (mapcar (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                     string))))

(defun consult-gh--set-string-width (string width &optional prepend)
  "Sets the STRING width to a fixed value, WIDTH. If the String is longer than width, it truncates the string and add an ellipsis, \"...\". If the string is shorter it adds whitespace to the string.
If PREPEND is non-nil, it truncates or adds whitespace from the beginning of string, instead of the end."
  (let* ((string (format "%s" string))
         (w (string-width string)))
    (when (< w width)
      (if prepend
          (setq string (format "%s%s" (make-string (- width w) ?\s) (substring string)))
        (setq string (format "%s%s" (substring string) (make-string (- width w) ?\s)))))
    (when (> w width)
      (if prepend
          (setq string (format "...%s" (substring string (- w (- width 3)) w)))
        (setq string (format "%s..." (substring string 0 (- width (+ w 3)))))))
    string))

(defun consult-gh--justify-left (string prefix maxwidth)
  "Set the string width by calling `consult-gh--set-string-width' in a way that string+prefix (e.g. `(concat prefix string)`) fits within maxwidth or a fraction of it. This is used for aligning marginalia info in minibuffer when using `consult-gh'."
  (let ((s (string-width string))
        (w (string-width prefix)))
    (cond ((< (+ s w) (floor (/ maxwidth 2)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 2))  w) t))
          ((< (+ s w) (floor (/ maxwidth 1.8)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.8))  w) t))
          ((< (+ s w) (floor (/ maxwidth 1.6)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.6))  w) t))
          ((< (+ s w) (floor (/ maxwidth 1.4)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.4)) w) t))
          ((< (+ s w) (floor (/ maxwidth 1.2)))
           (consult-gh--set-string-width string (- (floor (/ maxwidth 1.2)) w) t))
          ((< (+ s w) maxwidth)
           (consult-gh--set-string-width string (- maxwidth w) t))
          (t string)
          )
    ))

(defun consult-gh--highlight-match (regexp str ignore-case)
  "Highlight REGEXPS in STR.
If a regular expression contains capturing groups, only these are highlighted.

If no capturing groups are used highlight the whole match.  Case is ignored

if IGNORE-CASE is non-nil.

(This is copied from `consult--highlight-regexps'.)
"
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
"Convert markdown style footnotes to org-mode style footnotes by regexp replacements."
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
"Convert markdown style emphasis to org-mode style emphasis by regexp replacements."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^-\\{2\\}$" nil t)
          (delete-char -2)
          (insert "=================================\n")
          (replace-regexp "\\(^[a-zA-Z]+:[[:blank:]]\\)" "#+\\1" nil 0 (point-marker) nil nil))
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
               (replace-match "#+begin_src \\4\n\\5\\6\n#+end_src\n")))))))
    nil))

(defun consult-gh--markdown-to-org-links (&optional buffer)
"Convert markdown links to org-mode links by regexp replacements."
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
  "Convert from markdown format to org-mode format. This is used for viewing repos (a.k.a. fetching README file of repos) if `consult-gh-preview-buffer-mode' is set to 'org-mode."
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
      )))

;;; Backend `gh` related functions

(defun consult-gh--call-process (&rest args)
 "Run \"gh\" with args and return outputs as a list where the CAR is exit status (e.g. 0 means success and non-zero means error) and CADR is the output. If gh is not found we return '(127 \"\") and a message saying \"gh\" is not found."
(if (executable-find "gh")
      (with-temp-buffer
        (set-buffer-file-coding-system 'cp1047)
        (list (apply 'call-process "gh" nil (current-buffer) nil args)
                         (replace-regexp-in-string "" "\n"
                                                   (buffer-string))))
  (progn
      (message (propertize "\"gh\" is not found on this system" 'face 'warning))
      '(127 ""))
))

(defun consult-gh--command-to-string (&rest args)
  "Run \"gh\" with args and return output as a string if there is no error. If there are erros pass them to *Messages*."
  (let ((out (apply #'consult-gh--call-process args)))
          (if (= (car out) 0)
              (cadr out)
            (progn
              (message (cadr out))
              nil)
            )))

(defun consult-gh--api-get-json (arg )
"Makes a github api call to get response in json format by passing the arg (e.g. a github api url) to \"gh api -H Accept:application/vnd.github+json\" command."
  (consult-gh--call-process "api" "-H" "Accept: application/vnd.github+json" arg))

(defun consult-gh--api-json-to-hashtable (json &optional key)
"Converts a json object to a hashtable with lists for arrays and symbols for keys."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'keyword)
        (json-false :false))
    (if key
        (gethash key (json-read-from-string json))
      (json-read-from-string json))))

(defun consult-gh--get-current-username ()
"Gets the currently logged in user by running `gh api user` and returning the login field."
 (consult-gh--api-json-to-hashtable (cadr (consult-gh--api-get-json "user")) :login))

(defun consult-gh--get-repo-from-directory (&optional dir)
"Returns the full name of the GitHub repository in the current folder (a.k.a. `default-directory') in the format \"[HOST/]OWNER/REPO\" if any, otherwise returns nil."
(let* ((default-directory (or dir default-directory))
      (response (consult-gh--call-process "repo" "view" "--json" "nameWithOwner" "--jq" ".nameWithOwner")))
(if (eq (car response) 0)
    (if (not (string-empty-p (cadr response)))
    (string-trim (cadr response))
    nil)
  nil)
))

(defun consult-gh--split-repo (repo &optional separators)
  (let ((separators (or separators "\/")))
  (string-split repo "\/")))

(defun consult-gh--get-username (repo)
  (car (consult-gh--split-repo repo)))

(defun consult-gh--get-package (repo)
  (cadr (consult-gh--split-repo repo)))

(defun consult-gh--code-search-regexp-compiler (input ignore-case)
  "Compile the INPUT string to a list of regular expressions.
The function should return a pair, the list of regular expressions and a
highlight function.  The highlight function should take a single
argument, the string to highlight given the INPUT.  TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'.  If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
  (setq input (consult--split-escaped input))
  (cons input
         (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
       (apply-partially #'consult--highlight-regexps regexps ignore-case))))

;;; Backend functions for `consult-gh'.

;; Buffers
(defun consult-gh-kill-preview-buffers ()
"Kill all open preview buffers stored in `consult-gh--preview-buffers-list'. It asks for confirmation if the buffer is modified and removes the buffers that are killed from the list."
  (interactive)
  (when consult-gh--preview-buffers-list
    (mapcar (lambda (buff) (if (buffer-live-p buff)
                             (kill-buffer buff))
               (unless (buffer-live-p buff)
                             (setq consult-gh--preview-buffers-list (delete buff consult-gh--preview-buffers-list)))
               ) consult-gh--preview-buffers-list)
    )
)

(defun consult-gh--files-get-branches (repo)
"List branches of a repository in json format by passing repo and \"branches\" to `consult-gh--api-get-json'."
  (consult-gh--api-get-json (concat "repos/" repo "/branches")))

(defun consult-gh--files-branches-hashtable-to-list (table repo)
"Converts a hashtable containing repository branches to a list of propertized text. The hashtable can for example be obtained by converting the json object from `consult-gh--files-get-branches' to a hashtable by using `consult-gh--api-json-to-hashtable'."
    (mapcar (lambda (item) (cons (gethash :name item) `(:repo ,repo :branch ,(gethash :name item) :url ,(gethash :url item)))) table))

(defun consult-gh--files-branches-list-items (repo)
"Gets a lit of propertized text that contains information about branches of the repository repo on GitHub by using  `consult-gh--files-get-branches', `consult-gh--files-branches-hashtable-to-list' and `consult-gh--api-json-to-hashtable'."
(let ((response (consult-gh--files-get-branches repo)))
  (if (eq (car response) 0)
      (consult-gh--files-branches-hashtable-to-list (consult-gh--api-json-to-hashtable (cadr response)) repo)
    (message (cadr response)))))

(defun consult-gh--read-branch (repo)
  (pcase consult-gh-default-branch-to-load
    ("confirm"
     (if (y-or-n-p "Load Default HEAD branch?")
         (cons repo "HEAD")
       (cons repo (completing-read (concat "Select Branch for " (propertize (format "\"%s\"" repo) 'face 'consult-gh-default-face) ": ") (consult-gh--files-branches-list-items repo)))))
    ("ask"
     (cons repo (completing-read (concat "Select Branch for " (propertize (format "\"%s\"" repo) 'face 'consult-gh-default-face) ": ") (consult-gh--files-branches-list-items repo))))
    ('nil
     (cons repo "HEAD"))
    (_
     (cons repo (format "%s" consult-gh-default-branch-to-load)))))

(defun consult-gh--files-get-trees (repo &optional branch)
"Gets a recursive git \"tree\" of repo and branch in json object format by using `consult-gh--api-get-json'. "
  (let ((branch (or branch "HEAD")))
  (consult-gh--api-get-json (concat "repos/" repo "/git/trees/" branch ":?recursive=1"))))

(defun consult-gh--files-table-to-list (table repo &optional branch)
"converts a hashtable containing git tree information of repo and branch to list of propertized texts formatted properly to be sent to  `consult-gh-find-file'."
   (let ((branch (or branch "HEAD")))
    (mapcar (lambda (item) (cons (gethash :path item) `(:repo ,repo :branch ,branch :url ,(gethash :url item) :path ,(gethash :path item) :size ,(gethash :size item)))) table)))

(defun consult-gh--files-list-items (repo &optional branch)
"Fetches a list of files in repo and branch from GitHub. The format ois propertized text that include informaiton about the file generated by `consult-gh--files-table-to-list'. This list can be passed to `consult-gh-find-file'."
(let* ((branch (or branch "HEAD"))
       (response (consult-gh--files-get-trees repo branch))
       )
  (if (eq (car response) 0)
     (delete-dups (consult-gh--files-table-to-list (consult-gh--api-json-to-hashtable (cadr response) :tree) repo branch))
    (message (cadr response)))))

(defun consult-gh--files-nodirectory-items (repo &optional branch)
"Fetches a list of files in repo and branch from GitHub. The format ois propertized text that include informaiton about the file generated by `consult-gh--files-table-to-list'. This list can be passed to `consult-gh-find-file'."
(let* ((branch (or branch "HEAD"))
       (items (consult-gh--files-list-items repo branch))
       )
  (mapcar (lambda (item) (unless (plist-get (cdr item) :size) (setq items (delete item items)))) items)
  items))

(defun consult-gh--files-get-content (url)
"Fetches the contents of file at url retrieved from github api by `consult-gh--api-get-json' and decodes it into raw text."
  (let* ((response (consult-gh--api-get-json url))
        (content (if (eq (car response) 0) (consult-gh--api-json-to-hashtable (cadr response) :content)
                   nil)))
    (if content
        (base64-decode-string content)
      "")))

(defun consult-gh--file-format (cons)
  (when-let* ((path (car cons))
         (path (string-join (mapcar (lambda (x) x) (string-split path "/")) (propertize "/" 'face 'consult-gh-path-face)))
         (info (cdr cons))
         (repo (plist-get info :repo))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (size (plist-get info :size))
         (branch (plist-get info :branch))
         (url (plist-get info :url))
         (str path)
         (str (propertize str ':repo repo ':user user ':package package ':path path ':url url ':size size ':branch branch))
         )
    (cons str (list :repo repo :user user :package package :path path :url url :branch branch :size size))))

(defun consult-gh--file-lookup ()
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (path (plist-get info :path)))
    (cons path info))))

(defun consult-gh--file-state ()
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
    (pcase action
            ('preview
             (if cand
             (let* ((repo (plist-get (cdr cand) :repo))
                    (path (plist-get (cdr cand) :path))
                    (branch (or (plist-get (cdr cand) :branch) "HEAD"))
                    (url (plist-get (cdr cand) :url))
                    (tempdir (expand-file-name (concat (make-temp-name (concat repo "/")) "/" branch "/") consult-gh-tempdir))
                    (file-p (or (file-name-extension path) (plist-get (cdr cand) :size)))
                    (file-size (and file-p (plist-get (cdr cand) :size)))
                    (confirm (if (and file-p (>= file-size consult-gh-large-file-warning-threshold))
                                 (yes-or-no-p (format "File is %s Bytes. Do you really want to load it?" file-size))
                               t))
                    (prefix (concat (file-name-sans-extension  (file-name-nondirectory path))))
                    (suffix (concat "." (file-name-extension path)))
                    (temp-file (expand-file-name path tempdir))
                    (_ (and file-p confirm (make-directory (file-name-directory temp-file) t)))
                    (text (and file-p confirm (consult-gh--files-get-content url)))
                    (_ (and file-p confirm (with-temp-file temp-file (insert text) (set-buffer-file-coding-system 'raw-text)
                                                   )))
                    (buffer (or (and file-p confirm (with-temp-buffer (find-file-noselect temp-file t))) nil)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (funcall preview action
                        buffer
                         ))))
             ))
    ))

(defun consult-gh--file-annotate ()
"Annotate each file in `consult-gh-find-file' by size of the file. For more info on annotation refer to `consult''s manual, particularly 'consult--read' and `consult--read-annotate' documentation."
(lambda (cands cand)
  (if-let* ((info (cdr (assoc cand cands)))
            (size (format "%s Bytes" (plist-get info :size)))
            (repo (format "%s" (plist-get info :repo)))
            (user (car (string-split repo "\/")))
            (package (cadr (string-split repo "\/")))
            (branch (format "%s" (plist-get info :branch)))
            (url (format "%s" (plist-get info :url)))
            (str (format "\s%s\s\s%s -- "
                     (propertize size 'face 'consult-gh-visibility-face)
                     (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face) "@" (propertize branch 'face 'consult-gh-branch-face))
                     ))
            (cand (substring-no-properties cand))
            )
      (concat
       (consult-gh--justify-left str cand  (* 1.5 (frame-width)))
      (propertize url 'face 'consult-gh-url-face))
   nil)
  ))

(defun consult-gh--file-group (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (car (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s"))))))
  (if transform (substring cand) name)))

(defun consult-gh--files-view (repo path url &optional no-select tempdir jump-to-str)
  "The action function that gets the \"path\" to a file within a \"repo\" and the \"url\" of the file on GitHub API and puts the contents in a temporary file buffer. It fethces the content from Github by `consult-gh--files-get-content' and insert it into a temporary file stored under `consult-gh-tempdir' in apropriate subdirectories for repo and branch. If the optional input no-select is nil, it switches to the buffer by find-file, otherwise it does not swith-to-buffer and only returns the name of the buffer.

repo is name of the repo in the format \"arimindarvish//consult-gh\"
path is the realtive path of the file to the root of repo
url is the url of the file as retrieved from GitHub API
no-select is aboolean for whether to swith-to-buffer or not
tempdir is the directory where the temporary file is saved

Output is the buffer visiting the file."
  (let* ((tempdir (or tempdir consult-gh-tempdir))
         (prefix (concat (file-name-sans-extension (file-name-nondirectory path))))
         (suffix (concat "." (file-name-extension path)))
         (temp-file (expand-file-name path tempdir))
         (text (consult-gh--files-get-content url)))
         (make-directory (file-name-directory temp-file) t)
         (with-temp-file temp-file
           (insert  text)
           (set-buffer-file-coding-system 'raw-text)
           )
         (if no-select
             (find-file-noselect temp-file)
            (with-current-buffer (find-file temp-file)
             (if jump-to-str
                 (progn
                 ;;(highlight-regexp (string-trim highlight-str) 'match)
                 (goto-char (point-min))
                 (search-forward jump-to-str nil t)
                 (consult-gh-recenter 'middle))
               nil
               )
            (add-to-list 'consult-gh--preview-buffers-list (current-buffer))
            )
         )))

(defun consult-gh--files-view-action (cand)
  "Default action to run on selected item in `consult-gh'."
    (let* ((info (cdr cand))
           (repo (plist-get info :repo))
           (path (plist-get info :path))
           (url (plist-get info :url))
           (branch (or (plist-get info :branch) "HEAD"))
           (consult-gh-tempdir (expand-file-name (concat (make-temp-name (format "%s/" repo)) "/" branch "/") consult-gh-tempdir))
           (file-p (or (file-name-extension path) (plist-get info :size)))
           (file-size (and file-p (plist-get info :size)))
           (confirm t))
      (when (>= file-size consult-gh-large-file-warning-threshold)
        (if (yes-or-no-p (format "File is %s Bytes. Do you really want to load it?" file-size))
         (setq confirm t)
       (setq confirm nil)))
      (if (and file-p confirm)
          (consult-gh--files-view repo path url)
      )))

(defun consult-gh--files-browse-url-action (cand)
"The action function that gets a candidate from `consult-gh-find-file' and opens the url of the file in a browser. To use this as the default action in `consult-gh-find-file', set `consult-gh-file-action' to #'consult-gh--files-browse-url-action."
  (let* ((info (cdr cand))
           (repo (plist-get info :repo))
           (path (plist-get info :path))
           (branch (plist-get info :branch))
           (url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/" branch "/" path)))
        (browse-url url)))

(defun consult-gh--files-save-file-action (cand)
  "The action function that gets a selection from `consult-gh-find-file' and saves it. If `consult-gh-ask-for-path-before-save' is non-nil, it queries the user for the path the file should be saved at otherwise it saves the file under `consult-gh-default-save-directory' with the buffer-file-name as the name of the file."
    (let* ((info (cdr cand))
           (repo (plist-get info :repo))
           (path (plist-get info :path))
           (url (plist-get info :url))
           (file-p (or (file-name-extension path) (plist-get info :size)))
           (file-size (and file-p (plist-get info :size)))
           (filename (and file-p (file-name-nondirectory path)))
           (targetpath (if consult-gh-ask-for-path-before-save
                           (file-truename (read-file-name "Save As: " consult-gh-default-save-directory filename nil filename))
                         consult-gh-default-save-directory))
           (confirm t))
   (when (>= file-size consult-gh-large-file-warning-threshold)
     (if (yes-or-no-p (format "File is %s Bytes. Do you really want to load it?" file-size))
         (setq confirm t)
       (setq confirm nil)))
(let ((buffer (and file-p (consult-gh--files-view repo path url t))))
      (if (and file-p confirm)
    (save-mark-and-excursion
      (save-restriction
        (with-current-buffer buffer
          (write-file targetpath t))
        ))))))

(defun consult-gh--repo-lookup ()
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (repo (plist-get info :repo)))
    (cons (format "%s" repo) info))))

(defun consult-gh--repo-state ()
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
      (pcase action
            ('preview
             (if cand
                 (when-let ((repo (plist-get (cdr cand) :repo))
                            (query (plist-get (cdr cand) :query))
                            (match-str (consult--build-args query))
                       (buffer (get-buffer-create consult-gh-preview-buffer-name)))
                   (add-to-list 'consult-gh--preview-buffers-list buffer)
                   (consult-gh--repo-view (format "%s" repo) buffer)
                   (with-current-buffer buffer
                     (if consult-gh-highlight-matches
                     (cond
                      ((listp match-str)
                       (mapcar (lambda (item)
                                 (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
                      ((stringp match-str)
                        (highlight-regexp match-str 'consult-gh-preview-match-face))
                      )))
               (funcall preview action
                       buffer
                        )
                   )

             ))
            ('return
             cand)
             )))
      )

(defun consult-gh--repo-format (string input highlight)
  (let* ((parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (description (cadr parts))
         (visibility (cadr (cdr parts)))
         (date (substring (cadr (cdr (cdr parts))) 0 10))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (w (string-width repo))
         (s (string-width visibility))
         (str (format "%s\s\s%s\s\s%s\s\s%s"
                  (concat
                   (propertize user 'face 'consult-gh-user-face )
                   "/"
                   (propertize package 'face 'consult-gh-package-face))
                  (consult-gh--justify-left (propertize visibility 'face 'consult-gh-visibility-face) repo (frame-width))
                  (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                  (propertize description 'face 'consult-gh-description-face)))
         (str (propertize str :repo repo :user user :package package :description description :visibility visibility :date date :query query))
         )
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
         (mapcar (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t))))
      str)
    (cons str (list :repo repo :user user :package package :date date :description description :visibility visibility :query query))))

(defun consult-gh--repo-group (cand transform)
  "Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
  (let ((name (car (string-split (replace-regexp-in-string " " "" (format "%s" (car (remove " " (remove "" (string-split (substring-no-properties cand) "\s")))) "/")) "/")))
        )
    (if transform (substring cand) name)))

(defun consult-gh--repo-browse-url-action (cand)
"The action function that gets a repo candidate for example from `consult-gh-search-repos' and opens the url of the repo on github in a browser. To use this as the default action in `consult-gh-search-repos', set `consult-gh-repo-action' to #'consult-gh--repo-browse-url-action."
  (let* ((response (consult-gh--call-process "browse" "--repo" (substring-no-properties cand) "--no-browser"))
        (url (string-trim (cadr response))))
    (if (eq (car response) 0)
        (browse-url url)
      (message url))
))

(defun consult-gh--repo-view (repo &optional buffer)
  "This function accepts a repo name and an optional buffer as input arguments and shows the preview of the repo (a.k.a. the README file) in that buffer. It fethces the preview from Github by `gh repo view name-or-repo` using `consult-gh--call-process'. Then puts the response as raw text in the buffer defined by optional input arg `buffer` or in the buffer by `consult-gh-preview-buffer-name'. If `consult-gh-preview-buffer-mode' is set to either 'markdown-mode or 'org-mode, it sets the major mode of the buffer accordingly otherwise it shows the raw text in fundamental-mode.
repo is the name of the repository to be previewed.
buffer is an optional buffer the preview should be shown in.
"
(let ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
        (text (cadr (consult-gh--call-process "repo" "view" repo))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (goto-char (point-min-marker))
      (pcase consult-gh-preview-buffer-mode
        ('markdown-mode
         (if (featurep 'markdown-mode)
             (progn
             (require 'markdown-mode)
             (markdown-mode)
             (markdown-display-inline-images))
             (message "markdown-mode not available")))
        ('org-mode
         (let ((org-display-remote-inline-images 'download))
         (consult-gh--markdown-to-org buffer)
         ))
        (_ ()))
      )
    ))

(defun consult-gh--repo-view-action (cand)
  "The action function that gets a repo candidate for example from `consult-gh-search-repos' and opens a preview in an emacs buffer using `consult-gh--repo-view'."
    (let* ((repo (substring-no-properties cand))
          (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "*")))
      (consult-gh--repo-view repo)
      (switch-to-buffer (get-buffer-create consult-gh-preview-buffer-name))
      (rename-buffer buffername t)
      ))

(defun consult-gh--repo-browse-files-action (cand)
  "The action function that gets a repo candidate for example from `consult-gh-search-repos' and opens the file contents by runing `consult-gh-find-file'."
    (let* ((repo (plist-get (cdr cand) :repo)))
      (consult-gh-find-file repo)
      ))

(defvar consult-gh-repo-post-clone-hook nil
"Function(s) called after `consult-gh--repo-clone'.
Full path of the cloned repo is passed to these functions as input arg.")

(defun consult-gh--repo-clone (repo name targetdir &rest args)
"This is an internal function for non-interactive use. For interactive use see `consult-gh-repo-clone'. It clones the repository defined by `repo` to targetdir/name path by runing `gh clone repo ...`."
  (consult-gh--command-to-string "repo" "clone" (format "%s" repo) (expand-file-name name targetdir))
  (run-hook-with-args 'consult-gh-repo-post-clone-hook (expand-file-name name targetdir))
   (message (format "repo %s was cloned to %s" (propertize repo 'face 'font-lock-keyword-face) (propertize (expand-file-name name targetdir) 'face 'font-lock-type-face)))
   (let ((inhibit-message t))
   (expand-file-name name targetdir))
  )

(defun consult-gh--repo-clone-action (cand)
"The action function that gets a repo candidate for example from `consult-gh-search-repos' and clones the repository using `consult-gh-repo-clone'. If `consult-gh-confirm-before-clone' is nil it runs the internal non-interacctive function `consult-gh--repo-clone' that clones the directory in `consult-gh-default-clone-directory'."
(let* ((reponame (plist-get (cdr cand) :repo))
       (package (car (last (split-string reponame "\/")))))
    (if consult-gh-confirm-before-clone
        (let* ((targetdir (read-directory-name (concat "Select Directory for " (propertize (format "%s: " reponame) 'face 'font-lock-keyword-face)) (or consult-gh-default-clone-directory default-directory) default-directory))
        (name (read-string "name: " package)))
          (consult-gh--repo-clone reponame package targetdir))
      (consult-gh--repo-clone reponame package consult-gh-default-clone-directory))
    ))

(defvar consult-gh-repo-post-fork-hook nil
"Function(s) called after `consult-gh--repo-fork'.
Full name of the forked repo e.g. \"armindarvish/consult-gh\" is passed to these functions as input arg.")

(defun consult-gh--repo-fork (repo &optional name)
"This is an internal function for non-interactive use. For interactive uses see `consult-gh-repo-fork'. It forks the repository defined by `repo` to the current user account logged in with `gh` command line tool."
(let* ((package (car (last (split-string repo "\/"))))
      (name (or name package))
      (forkrepo (concat (consult-gh--get-current-username) "/" name)))
(consult-gh--command-to-string "repo" "fork" (format "%s" repo) "--fork-name" name)
(message (format "repo %s was forked to %s" (propertize repo 'face 'font-lock-keyword-face) (propertize forkrepo 'face 'font-lock-warning-face)))
(run-hook-with-args 'consult-gh-repo-post-fork-hook forkrepo)
  (let ((inhibit-message t))
    forkrepo)
))


(defun consult-gh--repo-fork-action (cand)
"The action function that gets a repo candidate for example from `consult-gh-search-repos' and forks the repository to current user's github account (the account logged in with `gh` command line tool)."
     (let* ((reponame (plist-get (cdr cand) :repo)))
      (consult-gh--repo-fork reponame)
    ))

(defun consult-gh--issue-lookup ()
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (title (plist-get info :title))
           (issue (plist-get info :issue)))
    (cons (format "%s:%s" issue title) info))))

(defun consult-gh--issue-state ()
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
          (pcase action
            ('preview
             (if cand
                 (when-let ((repo (plist-get (cdr cand) :repo))
                            (query (plist-get (cdr cand) :query))
                            (issue (plist-get (cdr cand) :issue))
                            (match-str (consult--build-args query))
                   (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (consult-gh--issue-view (format "%s" repo) (format "%s" issue) buffer)
               (with-current-buffer buffer
                 (if consult-gh-highlight-matches
                 (cond
                      ((listp match-str)
                       (mapcar (lambda (item)
                                 (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
                      ((stringp match-str)
                        (highlight-regexp match-str 'consult-gh-preview-match-face))
                      )))
               (funcall preview action
                         buffer
                        ))
             ))
            ('return
             cand)
             )))
      )

(defun consult-gh--issue-list-format (string input highlight)
  (let* ((parts (string-split string "\t"))
         (repo input)
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (issue (car parts))
         (state (upcase (cadr parts)))
         (face (pcase state
                 ("CLOSED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-warning-face)
                 (_ 'consult-gh-issue-face)
                 ))
         (title (cadr (cdr parts)))
         (tags (cadr (cdr (cdr parts))))
         (date (substring (cadr (cdr (cdr (cdr parts)))) 0 10))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                  (consult-gh--set-string-width (concat (propertize (format "%s" issue) 'face face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                  (propertize (consult-gh--set-string-width state 8) 'face face)
                  (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                  (propertize (consult-gh--set-string-width tags 24) 'face 'consult-gh-tags-face)
                  (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40)))
         (str (propertize str :repo repo :user user :package package :issue issue :state state :title title :tags tags :date date :query query))
         (str (if highlight (consult-gh--highlight-match repo str t) str))
         )
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
         (mapcar (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t))))
      str)
    (cons str (list :repo repo :user user :package package :issue issue :state state :title title :tags tags :date date :query query))))

(defun consult-gh--search-issues-format (string input highlight)
  (let* ((parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (issue (cadr parts))
         (state (upcase (cadr (cdr parts))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-warning-face)
                 (_ 'consult-gh-issue-face)
                 ))
         (title (cadr (cdr (cdr parts))))
         (tags (cadr (cdr (cdr (cdr parts)))))
         (date (substring (cadr (cdr (cdr (cdr (cdr parts))))) 0 10))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                  (consult-gh--set-string-width (concat (propertize (format "%s" issue) 'face face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 80)
                  (propertize (consult-gh--set-string-width state 8) 'face face)
                  (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                 (propertize (consult-gh--set-string-width tags 24) 'face 'consult-gh-tags-face)
                 (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40)
                  ))
         (str (propertize str :repo repo :user user :package package :issue issue :state state :title title :tags tags :date date :query query))
         )
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
         (mapcar (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t))))
      str)
    (cons str  (list :repo repo :user user :issue issue :state state :title title :tags tags :date date :query query))))

(defun consult-gh--issue-group-by-state (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (replace-regexp-in-string " " "" (format "%s" (cadr (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s"))))))))
  (if transform (substring cand) name)))

(defun consult-gh--issue-group-by-repo (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (car (last (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s")))))))
  (if transform (substring cand) name)))

(defun consult-gh--issue-browse-url-action (cand)
"The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens the url of the issue on github in a browser. To use this as the default action in `consult-gh-issue-list', set `consult-gh-issue-action' to #'consult-gh--issue-browse-url-action."
(let* ((info (cdr cand))
      (repo (substring-no-properties (plist-get info :repo)))
      (issue (substring-no-properties (plist-get info :issue))))
(consult-gh--call-process "issue" "view" "--repo" repo  "--web" issue)))

(defun consult-gh--issue-view (repo issue &optional buffer)
  "This function accepts a repo name and an issue number plus an optional buffer as input arguments and shows the preview of the issue (title and description) in that buffer. It fethces the preview from Github by `gh issue view --repo name-or-repo view --issue-number` using `consult-gh--call-process'. Then puts the response as raw text in the buffer defined by optional input arg `buffer` or in the buffer by `consult-gh-preview-buffer-name'. If `consult-gh-preview-buffer-mode' is set to either 'markdown-mode or 'org-mode, it sets the major mode of the buffer accordingly otherwise it shows the raw text in fundamental-mode.
repo is the name of the repository where the issue belongs.
issue is the issue number
buffer is an optional buffer the preview should be shown in.
"
  (let ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
        (text-main (cadr (consult-gh--call-process "issue" "view" issue "--repo" repo)))
        (text-comments (cadr (consult-gh--call-process "issue" "view" issue "--repo" repo "--comments"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (string-trim text-main))
      (insert "\n--\n")
      (insert (string-trim text-comments))
      (goto-char (point-min-marker))
      (pcase consult-gh-preview-buffer-mode
        ('markdown-mode
         (if (featurep 'markdown-mode)
             (progn
             (markdown-mode)
             (markdown-display-inline-images))
             (message "markdown-mode not available")))
        ('org-mode
         (let ((org-display-remote-inline-images 'download))
         (consult-gh--markdown-to-org buffer)
         ))
        (_ ()))
      )
    ))

(defun consult-gh--issue-view-action (cand)
   "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in an emacs buffer using `consult-gh--issue-view'."
    (let* ((info (cdr cand))
           (repo (substring-no-properties (plist-get info :repo)))
           (issue (substring-no-properties (format "%s" (plist-get info :issue))))
           (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/issues/" issue "*")))
      (consult-gh--issue-view repo issue)
      (switch-to-buffer (get-buffer-create consult-gh-preview-buffer-name))
      (rename-buffer buffername t)
      ))

(defun consult-gh--pr-lookup ()
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (title (plist-get info :title))
           (pr (plist-get info :pr)))
    (cons (format "%s:%s" pr title) info))))

(defun consult-gh--pr-state ()
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
      (if cand
          (pcase action
            ('preview
             (if cand
                 (when-let ((repo (plist-get (cdr cand) :repo))
                            (pr (plist-get (cdr cand) :pr))
                            (query (plist-get (cdr cand) :query))
                            (match-str (consult--build-args query))
                   (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (consult-gh--pr-view repo pr buffer)
               (with-current-buffer buffer
                 (if consult-gh-highlight-matches
                     (cond
                      ((listp match-str)
                       (mapcar (lambda (item)
                                 (highlight-regexp item 'consult-gh-preview-match-face)) match-str))
                      ((stringp match-str)
                        (highlight-regexp match-str 'consult-gh-preview-match-face))
                      )))
               (funcall preview action
                         buffer
                        ))
             )
             )
            ('return
             cand)
             )))
      ))

(defun consult-gh--pr-list-format (string input highlight)
  (let* ((parts (string-split string "\t"))
         (repo input)
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (pr (car parts))
         (state (upcase (cadr (cdr (cdr parts)))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-error-face)
                 ("MERGED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-repo-face)
                 (_ 'consult-gh-pr-face)
                 ))
         (branch (cadr (cdr parts)))
         (title (cadr parts))
         (date (substring (cadr (cdr (cdr (cdr parts)))) 0 10))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                  (consult-gh--set-string-width (concat (propertize (format "%s" pr) 'face  face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                  (propertize (consult-gh--set-string-width state 8) 'face face)
                  (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                  (propertize (consult-gh--set-string-width branch 24) 'face 'consult-gh-branch-face)
                  (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40)))
         (str (propertize str :repo repo :user user :package package :pr pr :state state :title title :branch branch :date date :query query))
         )
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
         (mapcar (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t))))
      str)
    (cons str (list :repo repo :user user :package package :pr pr :state state :title title :branch branch :date date :query query))))

(defun consult-gh--search-prs-format (string input highlight)
  (let* ((parts (string-split string "\t"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (pr (cadr parts))
         (state (upcase (cadr (cdr parts))))
         (face (pcase state
                 ("CLOSED" 'consult-gh-error-face)
                 ("MERGED" 'consult-gh-success-face)
                 ("OPEN" 'consult-gh-repo-face)
                 (_ 'consult-gh-pr-face)
                 ))
         (title (cadr (cdr (cdr parts))))
         (tags (cadr (cdr (cdr (cdr parts)))))
         (date (substring (cadr (cdr (cdr (cdr (cdr parts))))) 0 10))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s\s\s%s\s\s%s"
                      (consult-gh--set-string-width (concat (propertize (format "%s" pr) 'face  face) ":" (propertize (format "%s" title) 'face 'consult-gh-default-face)) 70)
                      (propertize (consult-gh--set-string-width state 8) 'face face)
                      (propertize (consult-gh--set-string-width date 10) 'face 'consult-gh-date-face)
                      (propertize (consult-gh--set-string-width tags 40) 'face 'consult-gh-tags-face)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40)))
         (str (propertize str :repo repo :user user :package package :pr pr :state state :title title :tags tags :date date :query query))
         )
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t))))
      str)
    (cons str  (list :repo repo :user user :pr pr :state state :title title :tags tags :date date :query query))))

(defun consult-gh--pr-group-by-state (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (replace-regexp-in-string " " "" (format "%s" (cadr (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s"))))))))
  (if transform (substring cand) name)))

(defun consult-gh--pr-group-by-repo (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (car (last (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s")))))))
  (if transform (substring cand) name)))

(defun consult-gh--pr-browse-url-action (cand)
"The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens the url of the issue on github in a browser. To use this as the default action in `consult-gh-issue-list', set `consult-gh-issue-action' to #'consult-gh--issue-browse-url-action."
(let* ((info (cdr cand))
      (repo (substring-no-properties (plist-get info :repo)))
      (pr (substring-no-properties (plist-get info :pr))))
(consult-gh--call-process "pr" "view" "--repo" repo  "--web" pr)))

(defun consult-gh--pr-view (repo pr &optional buffer)
  "This function accepts a repo name and an issue number plus an optional buffer as input arguments and shows the preview of the issue (title and description) in that buffer. It fethces the preview from Github by `gh issue view --repo name-or-repo view --issue-number` using `consult-gh--call-process'. Then puts the response as raw text in the buffer defined by optional input arg `buffer` or in the buffer by `consult-gh-preview-buffer-name'. If `consult-gh-preview-buffer-mode' is set to either 'markdown-mode or 'org-mode, it sets the major mode of the buffer accordingly otherwise it shows the raw text in fundamental-mode.
repo is the name of the repository where the issue belongs.
issue is the issue number
buffer is an optional buffer the preview should be shown in.
"
  (let ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
        (text-main (cadr (consult-gh--call-process "pr" "view" pr "--repo" repo)))
        (text-comments (cadr (consult-gh--call-process "pr" "view" pr "--repo" repo "--comments"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (string-trim text-main))
      (insert "\n--\n")
      (insert (string-trim text-comments))
      (goto-char (point-min-marker))
      (pcase consult-gh-preview-buffer-mode
        ('markdown-mode
         (if (featurep 'markdown-mode)
             (progn
             (markdown-mode)
             (markdown-display-inline-images))
             (message "markdown-mode not available")))
        ('org-mode
         (let ((org-display-remote-inline-images 'download))
         (consult-gh--markdown-to-org buffer)
         ))
        (_ ()))
      )
    ))

(defun consult-gh--pr-view-action (cand)
   "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in an emacs buffer using `consult-gh--issue-view'."
    (let* ((info (cdr cand))
           (repo (substring-no-properties (plist-get info :repo)))
           (pr (substring-no-properties (format "%s" (plist-get info :pr))))
           (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/pull/" pr "*")))
      (consult-gh--pr-view repo pr)
      (switch-to-buffer (get-buffer-create consult-gh-preview-buffer-name))
      (rename-buffer buffername t)
      ))

(defun consult-gh--code-lookup ()
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (repo (plist-get info :repo))
           (path (plist-get info :path)))
    (cons (format "%s:%s" repo path) info))))

(defun consult-gh--code-state ()
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
      (if cand
          (pcase action
            ('preview
             (if cand
                 (let* ((repo (plist-get (cdr cand) :repo))
                    (path (plist-get (cdr cand) :path))
                    (branch (or (plist-get (cdr cand) :branch) "HEAD"))
                    (query (plist-get (cdr cand) :query))
                    (code (plist-get (cdr cand) :code))
                    (url (plist-get (cdr cand) :url))
                    (tempdir (expand-file-name (concat (make-temp-name (concat repo "/")) "/" branch "/") consult-gh-tempdir))
                    (prefix (concat (file-name-sans-extension  (file-name-nondirectory path))))
                    (suffix (concat "." (file-name-extension path)))
                    (temp-file (expand-file-name path tempdir))
                    (_ (make-directory (file-name-directory temp-file) t))
                    (text (consult-gh--files-get-content url))
                    (_ (with-temp-file temp-file (insert text) (set-buffer-file-coding-system 'raw-text)
                                                   ))
                    (buffer (or (with-temp-buffer (find-file-noselect temp-file t))
                                                   nil)))
                    (when buffer
                      (with-current-buffer buffer
                        (if consult-gh-highlight-matches
                                   (highlight-regexp (string-trim code) 'consult-gh-preview-match-face))
                      (goto-char (point-min))
                      (search-forward code nil t)
                                 ))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (funcall preview action
                        buffer
                         )
               (with-current-buffer buffer
                   (consult-gh-recenter 'middle) nil)
               )
             ))
            ('return
             cand)
             )))
      ))

(defun consult-gh--search-code-format (string input highlight)
  (let* ((parts (string-split string ":"))
         (repo (car parts))
         (user (consult-gh--get-username repo))
         (package (consult-gh--get-package repo))
         (path (format "%s" (cadr parts)))
         (url (format "repos/%s/contents/%s" repo path))
         (path (concat "./" path))
         (code (mapcar (lambda (x) (replace-regexp-in-string "\t" "\s\s" (replace-regexp-in-string "\n" "\\n" (format "%s" x)))) (cdr (cdr parts))))
         (code (string-join code ":"))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\t%s\t%s"
                      (consult-gh--set-string-width (propertize code 'face  'consult-gh-code-face) 100)
                      (propertize path 'face 'consult-gh-url-face)
                      (consult-gh--set-string-width (concat (propertize user 'face 'consult-gh-user-face ) "/" (propertize package 'face 'consult-gh-package-face)) 40)))
         (str (propertize str ':repo repo ':user user ':package package ':code code ':path path ':url url ':query query))
         )
    (if (and consult-gh-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-gh--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-gh--highlight-match match-str str t))))
      str)
    (cons str  (list :repo repo :user user :package package :code code :path path :url url :query query))))

(defun consult-gh--code-group (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((repo (car (last (remove "" (string-split (substring-no-properties cand) "\t" t "\s*")))))
      (path (replace-regexp-in-string "\t" "" (format "%s" (cadr (remove "\t" (remove "" (string-split (substring-no-properties cand) "\t" t "\s"))))))))
  (if transform (substring cand) (format "%s -- %s" repo path))))

(defun consult-gh--code-browse-url-action (cand)
"The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens the url of the issue on github in a browser. To use this as the default action in `consult-gh-issue-list', set `consult-gh-issue-action' to #'consult-gh--issue-browse-url-action."
(let* ((info (cdr cand))
      (repo (substring-no-properties (plist-get info :repo)))
      (path (substring-no-properties (plist-get info :path)))
      (url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/HEAD/" path)))
(browse-url url)))

(defun consult-gh--code-view-action (cand)
  "Default action to run on selected item in `consult-gh'."
    (let* ((info (cdr cand))
           (repo (plist-get info :repo))
           (branch (or (plist-get info :branch) "HEAD"))
           (query (plist-get info :query))
           (code (plist-get info :code))
           (consult-gh-tempdir (expand-file-name (concat (make-temp-name (format "%s/" repo)) "/" branch "/") consult-gh-tempdir))
           (path (plist-get info :path))
           (url (plist-get info :url)))
      (consult-gh--files-view repo path url nil nil code)
      ))

(defun consult-gh--async-lookup ()
  (lambda (sel cands &rest args)
    (assoc sel cands)))

(defun consult-gh--async-state ()
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
      (if cand
          (pcase action
            ('preview
             )
            ('return
             cand)
             )))
      ))

(defun consult-gh--async-format (string input)
  (let* ((string (format "%s" string))
         (str (consult-gh--highlight-match input string t))
         )
    (cons str (list))))

(defun consult-gh--async-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--async-format string input))
        (funcall ,async action)))
       ))))

(defun consult-gh--async-group (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (replace-regexp-in-string " " "" (format "%s" (car (remove " " (remove "" (string-split (substring-no-properties cand) "\s\s"))))))))
  (if transform (substring cand) name)))

(defun consult-gh--async-builder (input)
  (pcase-let*
      ((cmd (consult--build-args consult-gh-args))
       (`(,arg \, opts) (consult--command-split input))
       (flags (append cmd opts)))
    (unless (or (member "-L" flags) (member "--limit" flags))
      (cond
       ((and (member "search" flags) (member "repos" flags))
        (setq opts
              (append opts
                      (list "-L" (format "%s" consult-gh-repo-maxnum)))))
       ((and (member "search" flags) (member "issues" flags))
        (setq opts
              (append opts
                      (list "-L" (format "%s" consult-gh-issue-maxnum)))))))
    (pcase-let*
        ((`(,re \, hl) (funcall consult--regexp-compiler arg 'emacs t)))
      (when re
        (cons
         (append cmd (list (consult--join-regexps re 'emacs)) opts) hl)))))

(defun consult-gh--async (prompt builder initial category)
  (consult--read
   (consult--async-command builder
     (consult-gh--async-transform builder)
     ;; (consult--async-highlight builder)
     ;; (consult--async-map (lambda (x) (consult-gh--async-highlight-match x)))
     ;;(consult--async-map (lambda (x) (consult-gh--async-format-repo x)))
     ;; ;;(consult--grep-format builder)
     ;;(consult--async-transform consult-gh--async-format-repo)
     )
   :prompt prompt
   ;;:lookup (consult-gh--async-lookup)
   ;;:state (funcall #'consult-gh--async-state)
   :initial (consult--async-split-initial initial)
   ;;:group #'consult-gh--repo-group
   :add-history (consult--async-split-thingatpt 'symbol)
   :require-match t
   :category (make-symbol (concat "consult-gh-" category))
   :sort nil))

(defun consult-gh-async (&optional arg1 arg2 initial &rest args)
  (interactive)
   (let* ((arg1 (or arg1 (completing-read "Select Action: " '("repo" "issues" "search"))))
         (arg2 (or arg2 (if (equal arg1 "search") (completing-read "Select Target: " '("repos" "issues")) nil)))
         ;; (rest_args (if rest_args (if (listp rest_args) rest_args (list rest_args)) nil))
         (consult-gh-args (append consult-gh-args (list arg1 arg2)))
         )
     (consult-gh--async "Search Repo:  " #'consult-gh--async-builder initial arg2))
  )

(defun consult-gh--repo-list-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--repo-format string input nil))
        (funcall ,async action)))
       ))))

(defun consult-gh--repo-list-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args'("repo" "list")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
"Get a list of repos of \"organization\" org and format each as a text with properties to pass to consult. It fetches a list of repos by runing \"gh repo list org\" and returns a list of propertized strings containing name of repos and their information such as visibility date updated, etc.

org is the name of a github account in string format e.g. \"armindarvish\"."
  (let* ((maxnum (format "%s" consult-gh-repo-maxnum))
         (repolist  (or (consult-gh--command-to-string "repo" "list" org "--limit" maxnum) ""))
         (repos (split-string repolist "\n"))
         )
    (mapcar (lambda (src) (consult-gh--repo-format src org nil))  (remove "" repos)))
    )

(defun consult-gh--async-repo-list (prompt builder &optional initial)
  (let ((candidates (consult--async-command builder
                      (consult-gh--repo-list-transform builder)
                      )))
    (consult--read candidates
                   :prompt prompt
                   :lookup (consult-gh--repo-lookup)
                   :state (funcall #'consult-gh--repo-state)
                   :initial (consult--async-split-initial initial)
                   :group #'consult-gh--repo-group
                   :add-history (append (list (consult--async-split-initial (consult-gh--get-username (consult-gh--get-repo-from-directory))) (consult--async-split-thingatpt 'symbol))
                                        consult-gh--known-orgs-list
                                        )
                   :history '(:input consult-gh--orgs-history)
                   :require-match t
                   :category 'consult-gh-repos
                   :preview-key consult-gh-preview-key
                   :sort nil)))

(defun consult-gh-repo-list (&optional initial noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
    (if current-prefix-arg
      (let* ((initial (or initial (format "%s" (car (string-split (car (consult-gh-search-repos initial t)) "/"))))))
        (consult-gh--async-repo-list "Enter Org Name:  " #'consult-gh--repo-list-builder initial))
      (consult-gh--async-repo-list "Enter Org Name:  " #'consult-gh--repo-list-builder initial))))

    ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))

    (if noaction
        sel
      (funcall consult-gh-repo-action sel))))

(defun consult-gh--search-repos-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
                    (consult-gh--repo-format string input t))
        (funcall ,async action)))
       ))))

(defun consult-gh--search-repos-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args '("search" "repos")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
  (consult--read
   (consult--async-command builder
     (consult-gh--search-repos-transform builder)
     )
   :prompt prompt
   :lookup (consult-gh--repo-lookup)
   :state (funcall #'consult-gh--repo-state)
   :initial (consult--async-split-initial initial)
   :group #'consult-gh--repo-group
   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                        consult-gh--known-repos-list
                                  )
   :history '(:input consult-gh--search-repos-history)
   :require-match t
   :category 'consult-gh-repos
   :preview-key consult-gh-preview-key
   :sort nil))

(defun consult-gh-search-repos (&optional initial noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
         (consult-gh--async-search-repos "Search Repos:  " #'consult-gh--search-repos-builder initial)))

    ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))

    (if noaction
        sel
      (progn
        (funcall consult-gh-repo-action sel)
        sel))))

(defun consult-gh-orgs (&optional orgs)
"Runs the interactive command in the minibuffer that queries the user for name of organizations (a.k.a. GitHub usernames) and returns a list of repositories of those organizations for further actions.
The user can provide multiple orgs by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh repo list name-of-the-org`) to fetch the list of repositories of those accounts and show them back to the user.
It uses `consult-gh--make-source-from-org' to create the list of items for consult and saves the history in `consult-gh--repos-history'. It also keep tracks of previously selected orgs by the user in `consult-gh--known-orgs-list' and offers them as possible entries in future runs of `consult-gh-orgs'."
  (interactive)
  (if (not orgs)
      (consult-gh-repo-list)
    (let* (
        (candidates (consult--slow-operation "Collecting Repos ..."  (apply #'append (mapcar (lambda (org) (consult-gh--repo-list org)) orgs)))))
        (consult--read candidates
                    :prompt "Select Repo: "
                    :require-match t
                    :sort t
                    :group #'consult-gh--repo-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh-repos
                    :preview-key consult-gh-preview-key
                    ))))

(defun consult-gh-default-repos ()
"Runs the interactive command consult `consult-gh-orgs' with the list of organizations (a.k.a. Github usernames) stored in `consult-gh-default-orgs-list'. This is a useful command for quickly fetching a list of personal Github Repositories or any other favorite accounts whose repositories are frequently visited."
  (interactive)
(consult-gh-orgs consult-gh-default-orgs-list))

(defun consult-gh-repo-fork (&optional repos)
"Interactively forks the repository defined by `repo` to the current user account logged in with `gh` command line tool after confirming name. It uses `gh fork repo ...`."
  (interactive)
  (let* ((consult-gh-prioritize-local-folder (if (eq consult-gh-prioritize-local-folder 'suggest) consult-gh-prioritize-local-folder nil))
         (repos (or repos (substring-no-properties (car (consult-gh-search-repos nil t))))))
    (if (stringp repos)
         (setq repos (list repos)))
    (mapcar (lambda (repo)
              (let* ((package (car (last (split-string repo "\/"))))
                     (name (if consult-gh-confirm-name-before-fork (read-string (concat "name for " (propertize (format "%s: " repo) 'face 'font-lock-keyword-face)) package) package)))
  (consult-gh--repo-fork repo name))) repos)
    ))

(defun consult-gh-repo-clone (&optional repos targetdir)
"Interactively clones the repo to targetdir directory. It uses the internal function `consult-gh--repo-clone' which in turn runs `gh clone repo ...`.
If repo or targetdir are not supplied, interactively asks user for those values."
  (interactive)
  (let* ((consult-gh-prioritize-local-folder (if (eq consult-gh-prioritize-local-folder 'suggest) consult-gh-prioritize-local-folder nil))
         (repos (or repos (substring-no-properties (car (consult-gh-search-repos nil t)))))
         (targetdir (or targetdir consult-gh-default-clone-directory))
         (clonedir (if consult-gh-confirm-before-clone (read-directory-name "Select Target Directory: " targetdir default-directory) targetdir)))
    (if (stringp repos)
         (setq repos (list repos)))
    (mapcar (lambda (repo)
              (let* ((package (consult-gh--get-package repo))
                     (name (if consult-gh-confirm-before-clone (read-string (concat "name for " (propertize (format "%s: " repo) 'face 'font-lock-keyword-face)) package) package)))
              (consult-gh--repo-clone repo name clonedir))
    ) repos)))

(defun consult-gh--issue-list-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--issue-list-format string input nil))
        (funcall ,async action)))
       ))))

(defun consult-gh--issue-list-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args '("issue" "list" "--repo")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
  (consult--read
   (consult--async-command builder
     (consult-gh--issue-list-transform builder)
       )
   :prompt prompt
   :lookup (consult-gh--issue-lookup)
   :state (funcall #'consult-gh--issue-state)
   :initial (consult--async-split-initial initial)
   :group #'consult-gh--issue-group-by-state
   :require-match t
   :category 'consult-gh-issues
   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                        consult-gh--known-repos-list
                                  )
   :history '(:input consult-gh--repos-history)
   :preview-key consult-gh-preview-key
   :sort nil))

(defun consult-gh-issue-list (&optional initial noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
         (if current-prefix-arg
             (let* ((initial (or initial (format "%s" (car (consult-gh-search-repos initial t))))))
               (consult-gh--async-issue-list "Enter Repo Name:  " #'consult-gh--issue-list-builder initial))
           (consult-gh--async-issue-list "Enter Repo Name:  " #'consult-gh--issue-list-builder initial))))
    ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
    (funcall consult-gh-issue-action sel))))

(defun consult-gh--search-issues-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--search-issues-format string input t))
        (funcall ,async action)))
       ))))

(defun consult-gh--search-issues-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args'("search" "issues")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
  (consult--read
   (consult--async-command builder
     (consult-gh--search-issues-transform builder)
     )
   :prompt prompt
   :lookup (consult-gh--issue-lookup)
   :state (funcall #'consult-gh--issue-state)
   :initial (consult--async-split-initial initial)
   :group #'consult-gh--issue-group-by-repo
   :require-match t
   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                        consult-gh--known-repos-list
                                  )
   :history '(:input consult-gh--search-issues-history)
   :category 'consult-gh-issues
   :preview-key consult-gh-preview-key
   :sort nil))

(defun consult-gh-search-issues (&optional initial repo noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
  (if current-prefix-arg
    (let ((repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
      (consult-gh-issue-list (concat repo (consult--async-split-initial initial)))
      )
  (consult-gh--async-search-issues "Search Issues:  " #'consult-gh--search-issues-builder initial))))
    ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
    (funcall consult-gh-issue-action sel)
    )))

(defun consult-gh--pr-list-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--pr-list-format string input nil))
        (funcall ,async action)))
       ))))

(defun consult-gh--pr-list-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args '("pr" "list" "--repo")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
  (consult--read
   (consult--async-command builder
     (consult-gh--pr-list-transform builder)
       )
   :prompt prompt
   :category 'consult-gh-prs
   :lookup (consult-gh--pr-lookup)
   :state (funcall #'consult-gh--pr-state)
   :initial (consult--async-split-initial initial)
   :group #'consult-gh--pr-group-by-state
   :require-match t
   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol))
                        consult-gh--known-repos-list
                                  )
   :history '(:input consult-gh--repos-history)
   :preview-key consult-gh-preview-key
   :sort nil))

(defun consult-gh-pr-list (&optional initial noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
         (if current-prefix-arg
             (let* ((initial (or initial (format "%s" (car (consult-gh-search-repos initial t))))))
               (consult-gh--async-pr-list "Enter Repo Name:  " #'consult-gh--pr-list-builder initial))
           (consult-gh--async-pr-list "Enter Repo Name:  " #'consult-gh--pr-list-builder initial))))
    ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel))))

(defun consult-gh--search-prs-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--search-prs-format string input t))
        (funcall ,async action)))
       ))))

(defun consult-gh--search-prs-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args '("search" "prs")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
  (consult--read
   (consult--async-command builder
     (consult-gh--search-prs-transform builder)
     )
   :prompt prompt
   :category 'consult-gh-prs
   :lookup (consult-gh--pr-lookup)
   :state (funcall #'consult-gh--pr-state)
   :initial (consult--async-split-initial initial)
   :group #'consult-gh--pr-group-by-repo
   :require-match t
   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol)))
   :history '(:input consult-gh--search-prs-history)
   :preview-key consult-gh-preview-key
   :sort nil))

(defun consult-gh-search-prs (&optional initial repo noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
         (if current-prefix-arg
             (let ((repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
               (consult-gh-pr-list (concat repo (consult--async-split-initial initial)))
               )
           (consult-gh--async-search-prs "Search Pull-Requests:  " #'consult-gh--search-prs-builder initial))))
     ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-pr-action sel)
      )))

(defun consult-gh--search-code-transform (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command line builder function."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-gh--search-code-format string input t))
        (funcall ,async action)))
       ))))

(defun consult-gh--search-code-builder (input)
  "Build gh command line for searching issues from the input string"
  (pcase-let* ((consult-gh-args (append consult-gh-args '("search" "code")))
               (cmd (consult--build-args consult-gh-args))
               (`(,arg . ,opts) (consult--command-split input))
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
  (consult--read
   (consult--async-command builder
     (consult-gh--search-code-transform builder)
     )
   :prompt prompt
   :category 'consult-gh-codes
   :lookup (consult-gh--code-lookup)
   :state (funcall #'consult-gh--code-state)
   :initial (consult--async-split-initial initial)
   :group #'consult-gh--code-group
   :require-match t
   :add-history (append (list (consult--async-split-initial  (consult-gh--get-repo-from-directory)) (consult--async-split-thingatpt 'symbol)))
   :history '(:input consult-gh--search-code-history)
   :preview-key consult-gh-preview-key
   :sort nil))

(defun consult-gh-search-code (&optional initial repo noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let ((sel
         (if current-prefix-arg
             (let ((repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t))))))
               )
           (consult-gh--async-search-code "Search Code:  " #'consult-gh--search-code-builder initial))))
     ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))
    (if noaction
        sel
      (funcall consult-gh-code-action sel)
      )))

(defun consult-gh-find-file (&optional repo branch noaction)
  "Runs the interactive command in the minibuffer that queries the user for name of repos in the format `OWNER/REPO` e.g. armindarvish/consult-gh as well as a string as search term and returns the list of searhc matches for the string in issues of thae repos for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search issues string --repo name-of-the-repo`) to search the issues for particular repositories and shows them back to the user.
It uses `consult-gh--make-source-from-search-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-issues'."
  (interactive)
  (let* ((repo (or repo (substring-no-properties (car (consult-gh-search-repos repo t)))))
         (branch (or branch (format "%s" (cdr (consult-gh--read-branch repo)))))
         (candidates (mapcar #'consult-gh--file-format (consult-gh--files-nodirectory-items repo branch)))
         (sel (consult--read candidates
                             :prompt "Select File: "
                             :lookup (consult-gh--file-lookup)
                             :state (funcall #'consult-gh--file-state)
                             :require-match t
                             :annotate (lambda (cand) (funcall (consult-gh--file-annotate) candidates cand))
                             :history t
                             :sort nil
                             :add-history (consult--async-split-thingatpt 'filename)
                             :history 'consult-gh--files-history
                             :category 'consult-gh-files
                             :preview-key consult-gh-preview-key
                             )))

    ;;add org and repo to known lists
    (when-let ((reponame (plist-get (cdr sel) :repo)))
      (add-to-history 'consult-gh--known-repos-list (consult--async-split-initial reponame)))
    (when-let ((username (plist-get (cdr sel) :user)))
      (add-to-history 'consult-gh--known-orgs-list (consult--async-split-initial username)))

    (if noaction
        sel
      (funcall consult-gh-file-action sel)
      )
    ))

;;; provide `consult-gh' module

(provide 'consult-gh)

;;; filename ends here
