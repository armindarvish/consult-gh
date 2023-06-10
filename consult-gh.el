;;; consult-gh.el --- Consulting GitHub Client -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.1
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

(defcustom consult-gh-tempdir nil
"Temporary file directory for the `consult-gh' package. This directory is used for storing temporary files when pulling files for viewing"
  :group 'consult-gh
  :type 'string
)

;; set the tempdir to system's default temporary directory
(customize-set-variable 'consult-gh-tempdir (expand-file-name "consult-gh" temporary-file-directory))

(defcustom consult-gh--default-maxnum 30
  "Maximum number of output for gh list and search operations normally passed to \"--limit\" in the command line. The default is set to gh's default number which is 30"
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-crm-separator nil
  "Separator for multiple selections with completing-read-multiple. for more info see `crm-separator'. Uses crm-separator for default."
  :group 'consult-gh
  :type 'string)

;; set consult-gh-crm-separator to defualt crm-separator
(customize-set-variable 'consult-gh-crm-separator (or crm-separator crm-default-separator))

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

(defcustom consult-gh-ask-for-path-before-save t
  "This variable defines whether `consult-gh' queries the user for a path before saving a file or uses the default directory and `buffer-file-name'. It may be useful to set this to nil if saving multiple files all at once frequently."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-default-branch-to-load "ask"
  "This determines how `consult-gh' loads repository branches. Possible Values are:
\"confirm\": Ask for confirmation if \"HEAD\" branch should be loaded. If the nswer is no, then the user gets to chose a different branch.
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

(defcustom consult-gh-file-action #'consult-gh--files-browse-url-action
  "This variable defines the function that is used when selecting a file. By default it is bound to `consult-gh--browse-files-url-action',but can be changed to other actions such as `consult-gh--files-view-action', `consult-gh--files-save-file-action', or similar user-defined custom actions"
  :group 'consult-gh
  :type 'function)

;;;Other Variables
(defvar consult-gh-category 'consult-gh
  "Category symbol for the `consult-gh' package.")

(defvar consult-gh-repos-category 'consult-gh-repos
  "Category symbol for the `consult-gh' package.")

(defvar consult-gh--issues-category 'consult-gh-issues
  "Category symbol for the `consult-gh' package.")


(defvar consult-gh-files-category 'consult-gh-files
  "Category symbol for the `consult-gh' package.")

(defvar consult-gh--preview-buffers-list (list)
  "List of currently open preview buffers")

(defvar consult-gh--repos-history nil
  "History variable for repos used in `consult-gh-search-repos'.")

(defvar consult-gh--org-history nil
  "History variable for orgs used in  `consult-gh-orgs' .")

(defvar consult-gh--issues-history nil
  "History variable for isseus used in  `consult-gh-search-issues' .")

(defvar consult-gh--known-orgs-list nil
  "List of previously visited orgs by `consult-gh-orgs'.")

(defvar consult-gh--known-repos-list nil
  "List of previously visited repos for `consult-gh-search-repos'.")

(defface consult-gh-default-face
  `((t :inherit 'default)) "default face used for listing items in minibuffer by `consult-gh'.")
(defface consult-gh-visibility-face
  `((t :inherit 'font-lock-variable-face)) "the face for repository visibility annotation in minibuffer by `consult-gh'. by default inherits from font-lock-variable-face")
(defface consult-gh-user-face
  `((t :inherit 'font-lock-warning-face)) "the face for user annotation in minibuffer by `consult-gh'. by default inherits from font-lock-warning-face")
(defface consult-gh-date-face
  `((t :inherit 'font-lock-keyword-face)) "the face for date annotation in minibuffer by `consult-gh'. by default inherits from font-lock-keyword-face")
(defface consult-gh-tags-face
  `((t :inherit 'font-lock-comment-face)) "the face for tags/comments annotation in minibuffer by `consult-gh'. by default inherits from font-lock-comment-face")

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

(defun consult-gh--api-get-json (arg)
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

(defun consult-gh--output-cleanup (string)
"Remove non UTF-8 characters if any in the string. For example, this is used in `consult-gh--repo-clone-action' and `consult-gh--repo-fork-action' to clean up the string before passing it to other functions."
  (string-join
   (delq nil (mapcar (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                     string))))

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
          (while (re-search-forward "#\\|\\*\\{1,2\\}\\(?1:.+?\\)\\*\\{1,2\\}|_\\{1,2\\}\\(?2:.+?\\)_\\{1,2\\}\\|`\\(?3:[^`].+?\\)`\\|```\\(?4:.*\n\\)\\(?5:[^`]*\\)```" nil t)
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

              ((pred (lambda (el) (string-match-p "```.*\n[^`]*```" el)))
               (replace-match "#+begin_src \\4\n\\5\n#+end_src\n")))))))
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
    (mapcar (lambda (item) (propertize (gethash :name item) ':repo repo ':branch (gethash :name item) ':url (gethash :url item))) table))

(defun consult-gh--files-branches-list-items (repo)
"Gets a lit of propertized text that contains information about branches of the repository repo on GitHub by using  `consult-gh--files-get-branches', `consult-gh--files-branches-hashtable-to-list' and `consult-gh--api-json-to-hashtable'."
(let ((response (consult-gh--files-get-branches repo)))
  (if (eq (car response) 0)
      (consult-gh--files-branches-hashtable-to-list (consult-gh--api-json-to-hashtable (cadr response)) repo)
    (message (cadr response)))))

(defun consult-gh--files-get-trees (repo &optional branch)
"Gets a recursive git \"tree\" of repo and branch in json object format by using `consult-gh--api-get-json'. "
  (let ((branch (or branch "HEAD")))
  (consult-gh--api-get-json (concat "repos/" repo "/git/trees/" branch ":?recursive=1"))))

(defun consult-gh--files-table-to-list (table repo &optional branch)
"converts a hashtable containing git tree information of repo and branch to list of propertized texts formatted properly to be sent to  `consult-gh-find-file'."
   (let ((branch (or branch "HEAD")))
    (mapcar (lambda (item) (propertize (gethash :path item) ':repo repo ':branch branch ':url (gethash :url item) ':path (gethash :path item) ':size (gethash :size item))) table)))

(defun consult-gh--files-list-items (repo &optional branch)
"Fetches a list of files in repo and branch from GitHub. The format ois propertized text that include informaiton about the file generated by `consult-gh--files-table-to-list'. This list can be passed to `consult-gh-find-file'."
(let* ((branch (or branch "HEAD"))
       (response (consult-gh--files-get-trees repo branch))
       )
  (if (eq (car response) 0)
     (delete-dups (sort (consult-gh--files-table-to-list (consult-gh--api-json-to-hashtable (cadr response) :tree) repo branch) 'string<))
    (message (cadr response)))))

(defun consult-gh--files-get-content (url)
"Fetches the contents of file at url retrieved from github api by `consult-gh--api-get-json' and decodes it into raw text."
  (let* ((response (consult-gh--api-get-json url))
        (content (if (eq (car response) 0) (consult-gh--api-json-to-hashtable (cadr response) :content)
                   nil)))
    (if content
        (base64-decode-string content)
      "")))

(defun consult-gh--files-narrow (item)
"Create narrowing function for items in `consult-gh-find-file' by the first letter of the name of the user/organization. for example \"a\" will be used for narrowing to files in the repo \"armindarvish\\consult-gh\"."
  (if (stringp item)
    (cons (string-to-char (substring-no-properties item)) (substring-no-properties item))))

(defun consult-gh--files-browse-url-action ()
"The action function that gets a candidate from `consult-gh-find-file' and opens the url of the file in a browser. To use this as the default action in `consult-gh-find-file', set `consult-gh-file-action' to #'consult-gh--files-browse-url-action."
(lambda (cand)
  (let* ((path (substring-no-properties (get-text-property 0 ':path cand)))
        (repo (substring-no-properties (get-text-property 0 ':repo cand)))
        (branch (substring-no-properties (get-text-property 0 ':branch cand)))
        (url (concat (string-trim (consult-gh--command-to-string "browse" "--repo" repo "--no-browser")) "/blob/" branch "HEAD/" path)))
        (browse-url url))))

(defun consult-gh--files-view (repo path url &optional no-select tempdir)
  "The action function that gets the \"path\" to a file within a \"repo\" and the \"url\" of the file on GitHub API and puts the contents in a temporary file buffer. It fethces the content from Github by `consult-gh--files-get-content' and insert it into a temporary file stored under `consult-gh-tempdir' in apropriate subdirectories for repo and branch. If the optional input no-select is nil, it switches to the buffer by find-file, otherwise it does not swith-to-buffer and only returns the name of the buffer.

repo is name of the repo in the format \"arimindarvish//consult-gh\"
path is the realtive path of the file to the root of repo
url is the url of the file as retrieved from GitHub API
no-select is aboolean for whether to swith-to-buffer or not
tempdir is the directory where the temporary file is saved

Output is the buffer visiting the file."
  (let* ((tempdir (or tempdir consult-gh-tempdir))
         (prefix (concat (file-name-sans-extension  (file-name-nondirectory path))))
         (suffix (concat "." (file-name-extension path)))
         (temp-file (expand-file-name path tempdir))
         (text (consult-gh--files-get-content url)))

         (make-directory (file-name-directory temp-file) t)
         (with-temp-file temp-file
           (insert text)
           (set-buffer-file-coding-system 'raw-text)
           )
         (if no-select
             (find-file-noselect temp-file)
           (progn
             (find-file temp-file)
            (add-to-list 'consult-gh--preview-buffers-list (current-buffer)))
         )))

(defun consult-gh--files-view-action ()
  "Default action to run on selected item in `consult-gh'."
  (lambda (cand)
    (let* ((repo (get-text-property 0 ':repo cand))
           (path (get-text-property 0 ':path cand))
           (url (get-text-property 0 ':url cand))
           (file-p (or (file-name-extension path) (get-text-property 0 ':size cand))))
      (if file-p
          (consult-gh--files-view repo path url)
      ))))

(defun consult-gh--files-save-file-action ()
  "The action function that gets a selection from `consult-gh-find-file' and saves it. If `consult-gh-ask-for-path-before-save' is non-nil, it queries the user for the path the file should be saved at otherwise it saves the file under `consult-gh-default-save-directory' with the buffer-file-name as the name of the file."
(lambda (cand)
    (let* ((repo (get-text-property 0 ':repo cand))
           (path (get-text-property 0 ':path cand))
           (url (get-text-property 0 ':url cand))
           (file-p (or (file-name-extension path) (get-text-property 0 ':size cand)))
           (filename (and file-p (file-name-nondirectory path)))
           (buffer (and file-p (consult-gh--files-view repo path url t))))
    (if file-p
    (save-mark-and-excursion
      (save-restriction
        (with-current-buffer buffer
          (if consult-gh-ask-for-path-before-save
          (write-file (read-file-name "Save As: " consult-gh-default-save-directory filename nil filename) t)
          (write-file consult-gh-default-save-directory t)
          )
        )))))))

(defun consult-gh--files-group (cand transform)
"Grouping function for the list of items in `consult-gh-find-file'. It groups files by the name of the repository and the branch in the format \"user//repo[@branch]\"ve.g. \"armindarvish\\consult-gh[@main]\"."
  (let ((name (concat (get-text-property 0 ':repo cand) "[@" (get-text-property 0 ':branch cand) "]")))
           (if transform (substring cand) name)))

(defun consult-gh--files-preview ()
"The state function used in `consult-gh-find-file'. It creates a preview buffer for the file at point selected in the consult-gh-find-file minibuffer. It fetches the contents of the file from GitHub by `consult-gh--files-get-content' and puts the content as raw text in a temporary buffer then runs `consult--buffer-preview' on that buffer.
For more info on state functions refer to `consult''s manual, and particularly `consult--read' and documentation and various consult state functions such as `consult--file-state'."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
      (pcase action
        ('preview
         (if cand
             (let* ((repo (get-text-property 0 ':repo cand))
                    (path (get-text-property 0 ':path cand))
                    (branch (get-text-property 0 ':branch cand))
                    (url (get-text-property 0 ':url cand))
                    (file-p (or (file-name-extension path) (get-text-property 0 ':size cand)))
                    (tempdir (expand-file-name (concat repo "/" branch) consult-gh-tempdir))
                    (prefix (concat (file-name-sans-extension  (file-name-nondirectory path))))
                    (suffix (concat "." (file-name-extension path)))
                    (temp-file (expand-file-name path tempdir))
                    (_ (and file-p (make-directory (file-name-directory temp-file) t)))
                    (text (and file-p (consult-gh--files-get-content url)))
                    (_ (and file-p (with-temp-file temp-file (insert text) (set-buffer-file-coding-system 'raw-text)
                                                   )))
                    (buffer (or (and file-p (with-temp-buffer (find-file-noselect temp-file t))) nil)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (funcall preview action
                        (and
                         cand
                         buffer
                         ))) ()))
        ))))

(defun consult-gh--files-annotate ()
"Annotate each file in `consult-gh-find-file' by size of the file. For more info on annotation refer to `consult''s manual, particularly 'consult--read' and `consult--read-annotate' documentation."
(lambda (cand)
  (if-let* ((size (get-text-property 0 :size cand))
            (size (format "%s Bytes" size)))
      (progn
        (setq size (propertize size 'face 'consult-gh-visibility-face))
        (format "\t%s" size)
     )
   nil)
  ))

(defun consult-gh--repo-list (org)
"Get a list of repos of \"organization\" org and format each as a text with properties to pass to consult. It fetches a list of repos by runing \"gh repo list org\" and returns a list of propertized strings containing name of repos and their information such as visibility date updated, etc.

org is the name of a github account in string format e.g. \"armindarvish\"."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (repolist  (or (consult-gh--command-to-string "repo" "list" org "--limit" maxnum) ""))
         (repos (mapcar (lambda (s) (string-split s "\t")) (split-string repolist "\n"))))

    (remove "" (mapcar (lambda (src) (propertize (car src) ':repo (car src) ':user (car (string-split (car src) "\/")) ':description (cadr src) ':visible (cadr (cdr src)) ':version (cadr (cdr (cdr src))))) repos)))
    )

(defun consult-gh--repo-browse-url-action ()
"The action function that gets a repo candidate for example from `consult-gh-search-repos' and opens the url of the repo on github in a browser. To use this as the default action in `consult-gh-search-repos', set `consult-gh-repo-action' to #'consult-gh--repo-browse-url-action."
(lambda (cand)
  (let* ((response (consult-gh--call-process "browse" "--repo" (substring-no-properties cand) "--no-browser"))
        (url (string-trim (cadr response))))
    (if (eq (car response) 0)
        (browse-url url)
      (message url))
)))

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

(defun consult-gh--repo-view-action ()
  "The action function that gets a repo candidate for example from `consult-gh-search-repos' and opens a preview in an emacs buffer using `consult-gh--repo-view'."
  (lambda (cand)
    (let* ((repo (substring-no-properties cand))
          (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "*")))
      (consult-gh--repo-view repo)
      (switch-to-buffer (get-buffer-create consult-gh-preview-buffer-name))
      (rename-buffer buffername t)
      )))

(defun consult-gh--repo-browse-files-action ()
  "The action function that gets a repo candidate for example from `consult-gh-search-repos' and opens the file contents by runing `consult-gh-find-file'."
  (lambda (cand)
    (let* ((repo (get-text-property 0 ':repo cand)))
      (consult-gh-find-file (list repo))
      )))

(defun consult-gh--repo-clone (repo targetdir name)
"This is an internal function for non-interactive use. For interactive use see `consult-gh-repo-clone'. It clones the repository defined by `repo` to targetdir/name path by runing `gh clone repo ...`."
  (consult-gh--command-to-string "repo" "clone" (format "%s" repo) (expand-file-name name targetdir))
  (message (format "repo %s was cloned to %s" (propertize repo 'face 'font-lock-keyword-face) (propertize (expand-file-name name targetdir) 'face 'font-lock-type-face))))


(defun consult-gh-repo-clone (&optional repo targetdir name)
"Interactively clones the repo to targetdir/name directory after confirming names and dir. It uses the internal function `consult-gh--repo-clone' which in turn runs `gh clone repo ...`.
If repo, targetdir and name are not supplied interactively asks user for those values."
  (interactive)
  (let ((repo (read-string "repo: " repo))
        (targetdir (read-directory-name "target directory: " targetdir))
        (name (read-string "name: " name))
        )
  (consult-gh--repo-clone repo targetdir name)
    ))

(defun consult-gh--repo-clone-action ()
"The action function that gets a repo candidate for example from `consult-gh-search-repos' and clones the repository using `consult-gh-repo-clone'. If `consult-gh-confirm-before-clone' is nil it runs the internal non-interacctive function `consult-gh--repo-clone' that clones the directory in `consult-gh-default-clone-directory'."
  (lambda (cand)
    (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties cand))))
         (package (car (last (split-string reponame "\/"))))
         )
    (if consult-gh-confirm-before-clone
        (consult-gh-repo-clone reponame consult-gh-default-clone-directory package)
      (consult-gh--repo-clone reponame consult-gh-default-clone-directory package))
    )))

(defun consult-gh--repo-fork (repo &rest args)
"This is an internal function for non-interactive use. For interactive uses see `consult-gh-repo-fork'. It forks the repository defined by `repo` to the current user account logged in with `gh` command line tool."
  (consult-gh--command-to-string "repo" "fork" (format "%s" repo) )
  (message (format "repo %s was forked" (propertize repo 'face 'font-lock-keyword-face))))

(defun consult-gh-repo-fork (&optional repo name &rest args)
"Interactively forks the repository defined by `repo` to the current user account logged in with `gh` command line tool after confirming name. It uses `gh fork repo ...`."
  (interactive)
  (let* ((repo (read-string "repo: " repo))
        (package (car (last (split-string repo "\/"))))
        (name (read-string "name: " package)))
  (consult-gh--repo-fork repo  "--fork-name" name args)
    ))

(defun consult-gh--repo-fork-action ()
"The action function that gets a repo candidate for example from `consult-gh-search-repos' and forks the repository to current user's github account (the account logged in with `gh` command line tool)."
  (lambda (cand)
     (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties cand)))))
      (consult-gh--repo-fork reponame)
    )))

(defun consult-gh--repo-group (cand transform)
"Grouping function for the list of items in `consult-gh-search-repos'. It groups repos by the name of the repository e.g. \"armindarvish\"."
  (let ((name (car (string-split (substring cand) "\/"))))
           (if transform (substring cand) name)))

(defun consult-gh--repo-preview ()
"The preview function used in `consult-gh-search-repos'. It creates a preview buffer for the currrent repo at point selected in the consult-gh-search-repos minibuffer and shows the preview (a.k.a. the README file) of the repo at point. It fetches the preview from GitHub by `consult-gh--repo-view' and puts the content in a temporary buffer as defined by `consult-gh-preview-buffer-name' then runs `consult--buffer-preview' on that buffer.
For more info on state functions refer to `consult''s manual, and particularly `consult--read' and documentation and various consult state functions such as `consult--file-state'."
  (lambda (action cand)
    (let ((preview (consult--buffer-preview)))
      (if cand
          (pcase action
            ('preview
             (let ((repo (substring-no-properties cand))
                   (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (consult-gh--repo-view repo buffer)
               (funcall preview action
                        (and
                         cand
                         buffer
                         )
                        ))
             )
            )
        ))))

(defun consult-gh--repo-narrow (item)
"Create narrowing function for items in `consult-gh-search-repos' by the first letter of the name of the user/organization. for example `a` will be used for narrowing to the repo \"armindarvish\\consult-gh\"."
  (if (stringp item)
    (cons (string-to-char (substring-no-properties item)) (substring-no-properties item))))

(defun consult-gh--repo-annotate ()
"Annotate each file in `consult-gh-search-repos' by the name of the user/owner, repo visibility (e.g. public or private) and the date the repo has been updated last. For more info on annotation refer to `consult''s manual, particularly 'consult--read' and `consult--read-annotate' documentation."
(lambda (cand)
  (if-let ((user (format "%s" (get-text-property 0 :user cand)))
         (visible (format "%s" (get-text-property 0 :visible cand)))
         (date (format "%s" (get-text-property 0 :version cand))))

      (progn
        (setq user (propertize user 'face 'consult-gh-user-face)
              visible (propertize visible 'face 'consult-gh-visibility-face)
              date (propertize date 'face 'consult-gh-date-face))
        (format "%s\t%s\t%s" user visible date)
     )
    nil)
))

(defun consult-gh--search-repos (repo)
"Search for repos with \"gh search repos\" and return a list of items each formatted with properties to pass to consult."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (repolist  (or (consult-gh--command-to-string "search" "repos" repo "--limit" maxnum) ""))
         (repos (mapcar (lambda (s) (string-split s "\t")) (split-string repolist "\n"))))
    (remove "" (mapcar (lambda (src) (propertize (car src) ':repo (car src) ':user (car (string-split (car src) "\/")) ':description (cadr src) ':visible (cadr (cdr src)) ':version (cadr (cdr (cdr src))))) repos)))
    )

(defun consult-gh--issue-list (repo)
"Get a list of issues of the repository `repo` and format each as a text with properties to pass to `consult-gh-issue-list'. It fetches a list of issues by runing \"gh issuee --repo name-of-repo list\" and returns a list of propertized strings containing title of issue name of the repo and other relevant information such as discription tags and date of the issue, etc.

repo is the name of the repository for which the issues should be listed in a string format e.g. \"armindarvish\\consult-gh\"."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (issueslist  (or (consult-gh--command-to-string "issue" "--repo" repo "list" "--limit" maxnum) ""))
         (issues (mapcar (lambda (s) (string-split s "\t")) (split-string issueslist "\n"))))
    (remove ":" (remove "" (mapcar (lambda (src) (propertize (concat (car src) ":" (cadr (cdr src))) ':issue (string-trim (car src) "#") ':repo repo ':status (cadr src) ':description (cadr (cdr src)) ':tags (cadr (cdr (cdr src))) ':date (cadr (cdr (cdr (cdr src)))))) issues))
   ))
    )

(defun consult-gh--issue-browse-url-action ()
"The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens the url of the issue on github in a browser. To use this as the default action in `consult-gh-issue-list', set `consult-gh-issue-action' to #'consult-gh--issue-browse-url-action."
(lambda (cand)
  (consult-gh--call-process "issue" "view" "--repo" (substring-no-properties (get-text-property 0 :repo cand))  "--web" (substring-no-properties (get-text-property 0 :issue cand)))
  ))

(defun consult-gh--issue-view (repo issue &optional buffer)
  "This function accepts a repo name and an issue number plus an optional buffer as input arguments and shows the preview of the issue (title and description) in that buffer. It fethces the preview from Github by `gh issue view --repo name-or-repo view --issue-number` using `consult-gh--call-process'. Then puts the response as raw text in the buffer defined by optional input arg `buffer` or in the buffer by `consult-gh-preview-buffer-name'. If `consult-gh-preview-buffer-mode' is set to either 'markdown-mode or 'org-mode, it sets the major mode of the buffer accordingly otherwise it shows the raw text in fundamental-mode.
repo is the name of the repository where the issue belongs.
issue is the issue number
buffer is an optional buffer the preview should be shown in.
"
  (let ((buffer (or buffer (get-buffer-create consult-gh-preview-buffer-name)))
        (text (cadr (consult-gh--call-process "issue" "--repo" repo "view" issue))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
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

(defun consult-gh--issue-view-action ()
   "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in an emacs buffer using `consult-gh--issue-view'."
  (lambda (cand)
    (let* ((repo (substring (get-text-property 0 :repo cand)))
          (issue (substring (get-text-property 0 :issue cand)))
          (buffername (concat (string-trim consult-gh-preview-buffer-name "" "*") ":" repo "/issues/" issue "*")))
      (consult-gh--issue-view repo issue)
      (switch-to-buffer (get-buffer-create consult-gh-preview-buffer-name))
      (rename-buffer buffername t)
      )))

(defun consult-gh--issue-preview ()
"The preview function used in `consult-gh-issue-list'. It creates a preview buffer for the current issue at point selected in the consult-gh-issue-list minibuffer and shows the preview of the issue. It fetches the preview from GitHub by `consult-gh--issue-view' and puts the content in the temporary `consult-gh-preview-buffer-name' buffer then runs `consult--buffer-preview' on that buffer.
For more info on state functions refer to `consult''s manual, and particularly `consult--read' and documentation and various consult state functions such as `consult--file-state'."
  (lambda (action cand)
    (let ((preview (consult--buffer-preview)))
      (if cand
          (pcase action
            ('preview
             (let ((repo (substring (get-text-property 0 :repo cand)))
                   (issue (substring (get-text-property 0 :issue cand)))
                   (buffer (get-buffer-create consult-gh-preview-buffer-name)))
               (add-to-list 'consult-gh--preview-buffers-list buffer)
               (consult-gh--issue-view repo issue buffer)
               (funcall preview action
                        (and
                         cand
                         buffer
                         )
                        ))
             )
            )
        ))))

(defun consult-gh--issue-group (cand transform)
"Grouping function for the list of items in `consult-gh-issue-list'. It groups issues by the status of the issue e.g. \"Open\"."
(let ((name (substring (get-text-property 0 :status cand))))
           (if transform (substring cand) name)))

(defun consult-gh--issue-annotate ()
"Annotate each file in `consult-gh-issue-list' by the name of the repo, the status of the issue (e.g. open or close), tags and the date of the issue. For more info on annotation refer to `consult''s manual, particularly 'consult--read' and `consult--read-annotate' documentation."
(lambda (cand)
  ;; (format "%s" cand)
  (if-let ((repo (format "%s" (get-text-property 0 :repo cand)))
         (status (format "%s" (get-text-property 0 :status cand)))
         (tags (format "%s" (get-text-property 0 :tags cand)))
         (date (format "%s" (get-text-property 0 :date cand))))
      (progn
        (setq status (propertize status 'face 'consult-gh-user-face)
              tags (propertize tags 'face 'consult-gh-visibility-face)
              date (propertize date 'face 'consult-gh-date-face))
        (format "%s\t%s\t%s" status tags date)
     )
    nil)
))

(defun consult-gh--make-source-from-org  (org)
"Create a source for consult from the repos of the organization to use in `consult-gh-orgs'. It fethces the list by using `consult-gh--repo-list' which in turn uses `gh repo list name-of-the-org`. This is used by the interactive command `consult-gh-orgs'
For more info on consult dources see `consult''s manual for example documentaion on `consult--multi' and `consult-buffer-sources'."
                  `(:narrow ,(consult-gh--repo-narrow org)
                    :category 'consult-gh
                    :items  ,(consult-gh--repo-list org)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-repo-action)
                    :annotate ,(consult-gh--repo-annotate)
                    :state ,(and consult-gh-show-preview #'consult-gh--repo-preview)
                    :defualt t
                    :history t
                    :sort t
                    ))

(defun consult-gh--make-source-from-search-repo  (repo)
"Create a source for consult from the repos return by search GitHub for `repo` by using `consult-gh--search-repos' which in turn uses `gh search repos name-of-the-repo`. This is used by the interactive command `consult-gh-search-repos'.
For more info on consult dources see `consult''s manual for example documentaion on `consult--multi' and `consult-buffer-sources'."
                  `(:narrow ,(consult-gh--repo-narrow repo)
                    :category 'consult-gh
                    :items  ,(consult-gh--search-repos repo)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-repo-action)
                    :annotate ,(consult-gh--repo-annotate)
                    :state ,(and consult-gh-show-preview #'consult-gh--repo-preview)
                    :default t
                    :history t
                    :sort t
                    ))

(defun consult-gh--make-source-from-issues (repo)
"Create a source for consult from the issues retrieved by fetching all the issues of the `repo` from GitHub by using `consult-gh--issue-list' which in turn uses `gh search issues --repo name-of-the-repo`. This is used by the interactive command `consult-gh-issue-list'.
For more info on consult dources see `consult''s manual for example documentaion on `consult--multi' and `consult-buffer-sources'."
                  `(:category 'consult-gh
                    :items  ,(consult-gh--issue-list repo)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-issue-action)
                    :annotate ,(consult-gh--issue-annotate)
                    :state ,(and consult-gh-show-preview #'consult-gh--issue-preview)
                    :default t
                    :history t
                    :sort t
                    ))

(defun consult-gh--make-source-from-files  (repo &optional branch)
"Create a source for consult from the file tree retrieved by fetching all the files of the `repo` under the specified `branch` using `consult-gh--files-list-items' which in turn uses `gh api ...` to get the file tree. This is used by the interactive command `consult-gh-find-file'.
For more info on consult dources see `consult''s manual for example documentaion on `consult--multi' and `consult-buffer-sources' or `consult-dir-sources' ."
                  `(:narrow ,(consult-gh--files-narrow repo)
                    :category 'consult-files
                    :items  ,(consult-gh--files-list-items repo branch)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-file-action)
                    :annotate ,(consult-gh--files-annotate)
                    :state ,(and consult-gh-show-preview #'consult-gh--files-preview)
                    :default t
                    :history t
                    :sort t
                    ))

(defun consult-gh-orgs (&optional orgs)
"Runs the interactive command in the minibuffer that queries the user for name of organizations (a.k.a. GitHub usernames) and returns a list of repositories of those organizations for further actions.
The user can provide multiple orgs by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh repo list name-of-the-org`) to fetch the list of repositories of those accounts and show them back to the user.
It uses `consult-gh--make-source-from-org' to create the list of items for consult and saves the history in `consult-gh--repos-history'. It also keep tracks of previously selected orgs by the user in `consult-gh--known-orgs-list' and offers them as possible entries in future runs of `consult-gh-orgs'."
  (interactive
   (let* ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups (append consult-gh-default-orgs-list consult-gh--known-orgs-list)) (list))))
   (list (delete-dups (completing-read-multiple "Search GitHub Users/Organization: " candidates nil nil nil 'consult-gh--org-history nil t)))))

  (let ((candidates (consult--slow-operation "Collecting Repos ..." (mapcar #'consult-gh--make-source-from-org orgs))))
    (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
      (progn
          (setq consult-gh--known-orgs-list (append consult-gh--known-orgs-list orgs))
          (consult--multi candidates
                    :prompt "Select User/Organization: "
                    :require-match nil
                    :sort t
                    :group #'consult-gh--repo-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh
                    :sort t
                    :preview-key consult-gh-preview-key
                    )))))

(defun consult-gh-default-repos ()
"Runs the interactive command consult `consult-gh-orgs' with the list of organizations (a.k.a. Github usernames) stored in `consult-gh-default-orgs-list'. This is a useful command for quickly fetching a list of personal Github Repositories or any other favorite accounts whose repositories are frequently visited."
  (interactive)
(consult-gh-orgs consult-gh-default-orgs-list))

(defun consult-gh-search-repos (&optional repos)
"Runs the interactive command in the minibuffer that queries the user for name of repos to search for and returns a list of possible entries in the format user/repo (e.g. armindarvish/consult-gh) for further actions such as viewing, cloning, forking, ...
The user can provide multiple search terms by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh search repos name-of-the-repo`) to fetch the list of repositories and show them back to the user.
It uses `consult-gh--make-source-from-search-repo' to create the list of items for consult and saves the history in `consult-gh--repos-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-search-repos'."
  (interactive
   (let* ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups consult-gh--known-repos-list) (list))))
   (list (delete-dups (completing-read-multiple "Search GitHub Repositories: " candidates nil nil nil nil nil t)))))
  (let ((candidates (consult--slow-operation "Collecting Repos ..." (mapcar #'consult-gh--make-source-from-search-repo repos))))
    (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
      (progn
          (setq consult-gh--known-repos-list (append consult-gh--known-repos-list repos))
          (consult--multi candidates
                    :prompt "Select Repositories(s): "
                    :require-match t
                    :sort nil
                    :group #'consult-gh--repo-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh
                    :sort t
                    :preview-key consult-gh-preview-key
                    ))
      (message (concat "consult-gh: " (propertize "no repositories matched your search!" 'face 'warning))))))

(defun consult-gh-find-file (&optional repos)
"Runs the interactive command in the minibuffer that queries the user for name of repos in the format `user/repo` e.g. armindarvish/consult-gh and then asks for the branch depending on the variable `consult-gh-default-branch-to-load' and returns the file tree of that repo and branch to the user for further actions such as viewing in emacs or the browser, saving as local files, ...
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh api repos/name-of-the-repo/git/trees/branch`) to fetch the file tree for a particular repository and branch and shows them back to the user.
It uses `consult-gh--make-source-from-files' to create the list of the files for consult. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-find-file'."
  (interactive
   (let* ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups consult-gh--known-repos-list) (list))))
     (list (completing-read-multiple "Repo(s) in User/Repo format (e.g. armindarvish/consult-gh): " candidates nil nil nil nil nil t))))
  (let ((branches (list)))
    (pcase consult-gh-default-branch-to-load
      ("confirm"
    (if (y-or-n-p "Load Default HEAD branch?")
        (setq branches (mapcar (lambda (repo) (cons repo "HEAD")) repos))
      (setq branches (cl-loop for repo in repos
                              collect (cons repo (completing-read (concat "Select Branch for " (propertize (format "\"%s\"" repo) 'face 'consult-gh-default-face) ": ") (consult-gh--files-branches-list-items repo)))))))
      ("ask"
       (setq branches (cl-loop for repo in repos
                              collect (cons repo (completing-read (concat "Select Branch for " (propertize (format "\"%s\"" repo) 'face 'consult-gh-default-face) ": ") (consult-gh--files-branches-list-items repo))))))
      ('nil
        (setq branches (mapcar (lambda (repo) (cons repo "HEAD")) repos))
        )
      (_
        (setq branches (mapcar (lambda (repo) (cons repo (format "%s" consult-gh-default-branch-to-load))) repos))))
    (let ((consult-gh-tempdir (expand-file-name (make-temp-name "") consult-gh-tempdir))
          (candidates (consult--slow-operation "Collecting Contents ..." (mapcar (lambda (repo) (consult-gh--make-source-from-files repo (alist-get repo branches))) repos)))
          )
      (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
          (progn
            (setq consult-gh--known-repos-list (append consult-gh--known-repos-list repos))
            (consult--multi candidates
                            :prompt "Select File: "
                            :require-match t
                            :sort t
                            :group #'consult-gh--files-group
                            ;;:history 'consult-gh--repos-history
                            :category 'consult-gh-files
                            :sort t
                            :preview-key consult-gh-preview-key
                            ))
        (message (concat "consult-gh: " (propertize "no contents matched your repo!" 'face 'warning)))))))

(defun consult-gh-issue-list (&optional repos)
"Runs the interactive command in the minibuffer that queries the user for name of repos in the format `user/repo` e.g. armindarvish/consult-gh and returns the list of issues for that repo. for further actions such as viewing in emacs or the browser.
The user can provide multiple repos by using the `consult-gh-crm-separator' similar to how `crm-separator' works in `completing-read-multiple'. Under the hood this command is using `consult' and particularly `consult--multi', which in turn runs macros of `completing-read' and passes the results to the GitHub command-line tool `gh` (e.g. by runing `gh issue --repo name-of-the-repo list`) to fetch the list of issues for a particular repository and shows them back to the user.
It uses `consult-gh--make-source-from-issues' to create the list of items for consult and saves the history in `consult-gh--issues-history'. It also keep tracks of previously selected repos by the user in `consult-gh--known-repos-list' and offers them as possible entries in future runs of `consult-gh-issue-list'."
  (interactive
   (let* ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups consult-gh--known-repos-list) (list))))
   (list (delete-dups (completing-read-multiple "Search GitHub Repositories: " candidates nil nil nil nil nil t)))))
  (let ((candidates (consult--slow-operation "Collecting Issues ..." (mapcar #'consult-gh--make-source-from-issues repos))))
    (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
      (progn
          (setq consult-gh--known-repos-list (append consult-gh--known-repos-list repos))
          (consult--multi candidates
                    :prompt "Select Issue(s): "
                    :require-match t
                    :sort t
                    :group #'consult-gh--issue-group
                    :preview-key 'any
                    :history 'consult-gh--issues-history
                    :category 'consult-gh
                    :sort t
                    :preview-key consult-gh-preview-key
                    )
          )
      (message (concat "consult-gh: " (propertize "no repositories matched your search!" 'face 'warning))))))

(provide 'consult-gh)

;;; filename ends here
