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

(eval-when-compile
(require 'consult)
)

(defgroup consult-gh nil
  "Consulting GitHub CLI"
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'magit
  :prefix "consult-gh-")

(defcustom consult-gh-category 'consult-gh
  "Category symbol for the `consult-gh' package."
  :group 'consult-gh
  :type 'symbol)

(defcustom consult-gh--default-maxnum 30
  "Maximum number of output for gh list operations normally passed top \"--limit\" in the command line."
  :group 'consult-gh
  :type 'integer)

(defcustom consult-gh-crm-separator "[\s]"
  "Separator for multiple selections with completing-read-multiple. for more info see `crm-separator'."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-preview-buffer-mode 'markdown-mode
  "Separator for multiple selections with completing-read-multiple. for more info see `crm-separator'."
  :group 'consult-gh
  :type 'symbol)

(defcustom consult-gh-default-orgs-list (list)
  "List of default github orgs for `consult-gh' package."
  :group 'consult-gh
  :type 'list)

(defcustom consult-gh-default-clone-directory nil
  "Default directory to clone github repos in for `consult-gh' package."
  :group 'consult-gh
  :type 'string)

(defcustom consult-gh-confirm-before-clone t
  "This variable defines whether `consult-gh' queries the user for directory and name before cloning a repo or uses the default directory and package name. It's useful to set this to nil if you want to clone multiple repos without all at once."
  :group 'consult-gh
  :type 'boolean)

(defcustom consult-gh-repo-action #'consult-gh--repo-browse-url-action
  "This variable defines the function that is used when selecting a repo. By default it is set to `consult-gh--repo-browse-url-action', but you can cahnge it to other actions such as `consult-gh--repo-clone-action' or `consult-gh--repo-fork-action'."
  :group 'consult-gh
  :type 'function)

(defcustom consult-gh-issue-action #'consult-gh--browse-issue-url-action
  "This variable defines the function that is used when selecting an issue. By default it is set to `consult-gh--browse-issue-url-action', but you can change it to other actions."
  :group 'consult-gh
  :type 'function)

(defvar consult-gh--repos-history nil
  "History variable for repos used in `consult-gh-search-repos'.")

(defvar consult-gh--org-history nil
  "History variable for orgs used in  `consult-gh-repo-list' .")

(defvar consult-gh--known-orgs-list nil
  "List of previously visited orgs for `consult-gh'.")


(defvar consult-gh--known-repos-list nil
  "List of previously visited orgs for `consult-gh'.")

(defface consult-gh-default-face
  `((t :inherit 'default)) "default face used items in the list")
(defface consult-gh-visibility-face
  `((t :inherit 'font-lock-variable-face)) "inherit from font-lock-variable-face for repos visibility")
(defface consult-gh-user-face
  `((t :inherit 'font-lock-warning-face)) "inherit from font-lock-warning-face for the user")
(defface consult-gh-date-face
  `((t :inherit 'font-lock-keyword-face)) "inherit from font-lock-keyword-face for the date")
(defface consult-gh-tags-face
  `((t :inherit 'font-lock-comment-face)) "inherit from font-lock-comment-face for the tags")

(defun consult-gh--output-cleanup (string)
"Remove non UTF-8 characters if any in the string. This is used in "
  (string-join
   (delq nil (mapcar (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                     string))))

(defun consult-gh--markdown-to-org (&optional buffer)
  (let ((buffer (if buffer buffer (get-buffer-create "*consult-gh-preview*"))))
    (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward "^[[:blank:]]*\\(?1:```\\)\\(?2:\\s *?\\)\\(\s\\|\t\\)*$" nil t)
          (replace-match "#+end_src"))
        (while (re-search-backward "^[[:blank:]]*\\(?1:```\\)" nil t)
          (replace-match "#+begin_src\s"))
        (goto-char (point-min))
        (while (re-search-forward  "`\\|`[[:blank:]]\\|`\\.\\|^`[^`\n]" nil t)
          (replace-match "="))
        (goto-char (point-min))
        (while (re-search-forward "#*[[:blank:]]" nil t)
          (replace-match "*"))
        ))
    )

(defun consult-gh--call-process (&rest args)
(if (executable-find "gh")
      (with-temp-buffer
        (set-buffer-file-coding-system 'cp1047)
        (list (apply 'call-process "gh" nil (current-buffer) nil args)
                         (replace-regexp-in-string "" "\n"
                                                   (buffer-string))))
  (progn
      (message (propertize "\"gh\" is not found on this system" 'face 'warning))
      '(0 ""))
))

(defun consult-gh--command-to-string (&rest args)
  "Run \"gh\" with args and return output if no errors. If there are erros pass them to *Messages*."
  (let ((out (apply #'consult-gh--call-process args)))
          (if (= (car out) 0)
              (cadr out)
            (progn
              (message (cadr out))
              nil)
            )))

(defun consult-gh--repo-list (org)
"Get a list of repos of \"organization\" and format each as a text with properties to pass to consult."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (repolist  (or (consult-gh--command-to-string "repo" "list" org "--limit" maxnum) ""))
         (repos (mapcar (lambda (s) (string-split s "\t")) (split-string repolist "\n"))))
    (remove "" (mapcar (lambda (src) (propertize (car src) ':user (car (string-split (car src) "\/")) ':description (cadr src) ':visible (cadr (cdr src)) ':version (cadr (cdr (cdr src))))) repos)))
    )

(defun consult-gh--repo-browse-url-action ()
"Default action to run on selected itesm in `consult-gh'."
(lambda (cand)
  (browse-url (concat "https://github.com/" (substring cand)))
))

(defun consult-gh--repo-clone (repo targetdir name)
"Clone the repo to targetdir/name directory. It uses \"gh clone repo ...\"."
  (consult-gh--command-to-string "repo" "clone" (format "%s" repo) (expand-file-name name targetdir))
  (message (format "repo %s was cloned to %s" (propertize repo 'face 'font-lock-keyword-face) (propertize (expand-file-name name targetdir) 'face 'font-lock-type-face))))


(defun consult-gh-repo-clone (&optional repo targetdir name)
  (interactive)
  (let ((repo (read-string "repo: " repo))
        (targetdir (read-directory-name "target directory: " targetdir))
        (name (read-string "name: " name))
        )
  (consult-gh--repo-clone repo targetdir name)
    ))

(defun consult-gh--repo-clone-action ()
  (lambda (cand)
    (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties cand))))
         (package (car (last (split-string reponame "\/"))))
         )
    (if consult-gh-confirm-before-clone
        (consult-gh-repo-clone reponame consult-gh-default-clone-directory package)
      (consult-gh--repo-clone reponame consult-gh-default-clone-directory package))
    )))

(defun consult-gh--repo-fork (repo &rest args)
"Clone the repo to targetdir/name directory. It uses \"gh clone repo ...\"."
  (consult-gh--command-to-string "repo" "fork" (format "%s" repo) )
  (message (format "repo %s was forked" (propertize repo 'face 'font-lock-keyword-face) )))

(defun consult-gh-repo-fork (&optional repo name &rest args)
  (interactive)
  (let* ((repo (read-string "repo: " repo))
        (package (car (last (split-string repo "\/"))))
        (name (read-string "name: " package)))
  (consult-gh--repo-fork repo  "--fork-name" name args)
    ))

(defun consult-gh--repo-fork-action ()
  (lambda (cand)
     (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties cand)))))
      (consult-gh--repo-fork reponame)
    )))

(defun consult-gh--repo-group (cand transform)
"Group the list of item in `consult-gh' by the name of the user"
  (let ((name (car (string-split (substring cand) "\/"))))
           (if transform (substring cand) name)))

(defun consult-gh--repo-narrow (item)
"Create narrowing function for items in `consult-gh' by the first letter of the name of the user/organization."
  (if (stringp item)
    (cons (string-to-char (substring-no-properties item)) (substring-no-properties item))))

(defun consult-gh--repo-annotate ()
"Annotate each repo in `consult-gh' by user, visibility and date."
(lambda (cand)
  ;; (format "%s" cand)
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
    (remove "" (mapcar (lambda (src) (propertize (car src) ':user (car (string-split (car src) "\/")) ':description (cadr src) ':visible (cadr (cdr src)) ':version (cadr (cdr (cdr src))))) repos)))
    )

(defun consult-gh--issue-list (repo)
"search issues of a repo with \"gh issue list\" and return a list of items for viewing."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (issueslist  (or (consult-gh--command-to-string "issue" "--repo" repo "list" "--limit" maxnum) ""))
         (issues (mapcar (lambda (s) (string-split s "\t")) (split-string issueslist "\n"))))
    (remove "" (mapcar (lambda (src) (propertize (car src) ':repo repo ':status (cadr src) ':description (cadr (cdr src)) ':tags (cadr (cdr (cdr src))) ':date (cadr (cdr (cdr (cdr src)))))) issues))
   )
    )

(defun consult-gh--browse-issue-url-action ()
"Default action to run on selected itesm in `consult-gh'."
(lambda (cand)
  (browse-url (concat "https://github.com/" (substring (get-text-property 0 :repo cand)) "\/issues\/" (string-trim (consult-gh--output-cleanup (substring cand)) "#"))
)))

(defun consult-gh--issue-view (repo issue &optional buffer)
  "Default action to run on selected item in `consult-gh'."
  (let ((buffer (or buffer (get-buffer-create "*consult-gh-preview*")))
        (text (cadr (consult-gh--call-process "issue" "--repo" repo "view" issue))))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-max-marker))
      (insert text)
      (goto-char (point-min-marker))
      (pcase consult-gh-preview-buffer-mode
        ('markdown-mode (markdown-mode)
                        )
        ('org-mode
         (consult-gh--markdown-to-org buffer)
         (org-mode)
         (org-fold-show-all))
        (_ ()))
      )
    ))

(defun consult-gh--issue-preview-action ()
  "Default action to run on selected item in `consult-gh'."
  (lambda (cand)
    (let ((repo (substring (get-text-property 0 :repo cand)))
          (issue (string-trim (consult-gh--output-cleanup (substring cand)) "#")))
      (consult-gh--issue-view repo issue)
      )))

;; (setq my:test1 (consult-gh--issue-list "cli/cli"))
;; (funcall (funcall #'consult-gh--issue-preview-action) (car my:test1))

(defun consult-gh--issue-preview ()
  (lambda (action cand)
    (let ((preview (consult--buffer-preview)))
      (if cand
          ;;(print cand)
          (let ((repo (substring (get-text-property 0 :repo cand)))
                (issue (string-trim (consult-gh--output-cleanup (substring cand)) "#"))
                (buffer (get-buffer-create "*consult-gh-preview*")))
            (consult-gh--issue-view repo issue buffer)
            (funcall preview action
                     (and
                      cand
                      (eq action 'preview)
                      buffer
                      )
                     ))
        ))
    ))


(setq consult-gh-preview-buffer-mode 'markdown-mode)

(defun consult-gh--issue-group (cand transform)
"Group the list of issues in a repo by the status of the issues"
(let ((name (substring (get-text-property 0 :status cand))))
           (if transform (substring cand) name)))

(defun consult-gh--issue-narrow (item)
"Create narrowing function for issues in a repo by the status of the issue"
    (cons (string-to-char (substring-no-properties item)) (substring-no-properties item)))

(defun consult-gh--issue-annotate ()
"Annotate each issue by description, status, repo and date."
(lambda (cand)
  ;; (format "%s" cand)
  (if-let ((repo (format "%s" (get-text-property 0 :repo cand)))
         (description (format "%s" (get-text-property 0 :description cand)))
         (status (format "%s" (get-text-property 0 :status cand)))
         (tags (format "%s" (get-text-property 0 :tags cand)))
         (date (format "%s" (get-text-property 0 :date cand))))

      (progn
        (setq status (propertize status 'face 'consult-gh-user-face)
              description (propertize description 'face 'consult-gh-visibility-face)
              tags (propertize tags 'face 'consult-gh-tags-face)
              date (propertize date 'face 'consult-gh-date-face))
        (format "%s\t%s\t%s\t%s" description status tags date)
     )
    nil)
))

(defun consult-gh--make-source-from-org  (org)
"Create a source for consult from the repos of the organization to use in `consult-gh-repo-list'."
                  `(:narrow ,(consult-gh--repo-narrow org)
                    :category 'consult-gh
                    :items  ,(consult-gh--repo-list org)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-repo-action)
                    :annotate ,(consult-gh--repo-annotate)
                    :defualt t
                    :history t
                    ))

(defun consult-gh--make-source-from-search-repo  (repo)
"Create a source for consult from the search results for repo to use in `consult-gh-search-repos'."
                  `(:narrow ,(consult-gh--repo-narrow repo)
                    :category 'consult-gh
                    :items  ,(consult-gh--search-repos repo)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-repo-action)
                    :annotate ,(consult-gh--repo-annotate)
                    :default t
                    :history t
                    ))

(defun consult-gh--make-source-from-issues (repo)
"Create a source for consult from the repos of the organization to use in `consult-gh-repo-list'."
                  `(;;:narrow ,(consult-gh--repo-narrow org)
                    :category 'consult-gh
                    :items  ,(consult-gh--issue-list repo)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-issue-action)
                    :annotate ,(consult-gh--issue-annotate)
                    :state ,#'consult-gh--issue-preview
                    :default t
                    :history t
                    ))

(defun consult-gh-repo-list (repo)
"Get a list of organizations from the user and provide their repos."
  (interactive
   (let ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups (append consult-gh-default-orgs-list consult-gh--known-orgs-list)) (list))))
   (list (delete-dups (completing-read-multiple "GitHub Org: " candidates nil nil nil 'consult-gh--org-history nil t)))))

  (let ((candidates (consult--slow-operation "Collecting Repos ..." (mapcar #'consult-gh--make-source-from-org orgs))))
    (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
      (progn
          (setq consult-gh--known-orgs-list (append consult-gh--known-orgs-list orgs))
          (consult--multi candidates
                    :require-match t
                    :sort t
                    :group #'consult-gh--repo-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh
                    ))
      )))

(defun consult-gh-default-repos ()
"Show the repos from default organizaitons."
  (interactive)
(consult-gh-repo-list consult-gh-default-orgs-list))

(defun consult-gh-search-repos (repos)
"Get a list of repos from the user and return the results in `consult-gh' menu by runing \"gh search repos\"."
  (interactive
   (let ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups consult-gh--known-repos-list) (list))))
   (list (delete-dups (completing-read-multiple "Repos: " candidates nil nil nil nil nil t)))))
  (let ((candidates (consult--slow-operation "Collecting Repos ..." (mapcar #'consult-gh--make-source-from-search-repo repos))))
    (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
      (progn
          (setq consult-gh--known-repos-list (append consult-gh--known-repos-list repos))
          (consult--multi candidates
                    :require-match t
                    :sort t
                    :group #'consult-gh--repo-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh
                    ))
      (message (concat "consult-gh: " (propertize "no repositories matched your search!" 'face 'warning))))))

(defun consult-gh-issue-list (repos)
"Get a list of repos from the user and return the results in `consult-gh' menu by runing \"gh search repos\"."
  (interactive
   (let ((crm-separator consult-gh-crm-separator)
         (candidates (or (delete-dups consult-gh--known-repos-list) (list))))
   (list (delete-dups (completing-read-multiple "Repos: " candidates nil nil nil nil nil t)))))
  (let ((candidates (consult--slow-operation "Collecting Repos ..." (mapcar #'consult-gh--make-source-from-issues repos))))
    (if (not (member nil (mapcar (lambda (cand) (plist-get cand :items)) candidates)))
      (progn
          (setq consult-gh--known-repos-list (append consult-gh--known-repos-list repos))
          (consult--multi candidates
                    :require-match t
                    :sort t
                    :group #'consult-gh--issue-group
                    :preview-key 'any
                    ;;:history 'consult-gh--repos-history
                    :category 'consult-gh
                    )
          )
      (message (concat "consult-gh: " (propertize "no repositories matched your search!" 'face 'warning))))
    (kill-buffer (get-buffer "*consult-gh-preview*"))))

(provide 'consult-gh)

;;; filename ends here
