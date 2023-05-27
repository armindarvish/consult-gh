;;; consult-gh.el --- Consulting GitHub Client -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.34") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

;;;###autoload

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

(defcustom consult-gh-action #'consult-gh--browse-url-action
  "This variable defines the function that is used when selecting an item. By default it is set to `consult-gh--browse-url-action', but you can cahnge it to other actions such as `consult-gh--clone-repo-action'."
  :group 'consult-gh
  :type 'function)

(defvar consult-gh--repos-history nil
  "History variable for repos used in `consult-gh-search-repos'.")

(defvar consult-gh--org-history nil
  "History variable for orgs used in  `consult-gh-orgs' .")

(defvar consult-gh--known-orgs-list nil
  "List of previously visited orgs for `consult-gh'.")


(defvar consult-gh--known-repos-list nil
  "List of previously visited orgs for `consult-gh'.")

(defface consult-gh-default-face
  `((t :inherit 'default)) "default face used items in the list")
(defface consult-gh-visibility-face
  `((t :inherit 'font-lock-doc-face)) "inherit from font-lock-doc-face for repos visibility")
(defface consult-gh-user-face
  `((t :inherit 'font-lock-warning-face)) "inherit from font-lock-warning-face for the user")
(defface consult-gh-date-face
  `((t :inherit 'font-lock-keyword-face)) "inherit from font-lock-keyword-face for the date")

(defun consult-gh--call-process (&rest args)
  "Run \"gh\" with args and return output if no errors. If there are erros pass them to *Messages*."
  (if (executable-find "gh")
      (with-temp-buffer
        (let ((out (list (apply 'call-process "gh" nil (current-buffer) nil args)
                         (buffer-string))))
          (if (= (car out) 0)
              (cadr out)
            (progn
              (message (cadr out))
              nil)
            )))
    (progn
      (message (propertize "\"gh\" is not found on this system" 'face 'warning))
      nil)
    ))

(defun consult-gh--output-cleanup (string)
"REmove non UTF-8 characters if any in the string. This is used in "
  (string-join
   (delq nil (mapcar (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                     string))))

(defun consult-gh--get-repos-of-org (org)
"Get a list of repos of \"organization\" and format each as a text with properties to pass to consult."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (repolist  (or (consult-gh--call-process "repo" "list" org "--limit" maxnum) ""))
         (repos (mapcar (lambda (s) (string-split s "\t")) (split-string repolist "\n"))))
    (remove "" (mapcar (lambda (src) (propertize (consult-gh--output-cleanup (car src)) ':user (car (string-split (car src) "\/")) ':description (cadr src) ':visibility (cadr (cdr src)) ':version (cadr (cdr (cdr src))))) repos)))
    )

(defun consult-gh--get-search-repos (repo)
"Search for repos with \"gh search repos\" and return a list of items each formatted with properties to pass to consult."
  (let* ((maxnum (format "%s" consult-gh--default-maxnum))
         (repolist  (or (consult-gh--call-process "search" "repos" repo "--limit" maxnum) ""))
         (repos (mapcar (lambda (s) (string-split s "\t")) (split-string repolist "\n"))))
    (remove "" (mapcar (lambda (src) (propertize (consult-gh--output-cleanup (car src)) ':user (car (string-split (car src) "\/")) ':description (cadr src) ':visibility (cadr (cdr src)) ':version (cadr (cdr (cdr src))))) repos)))
    )

(defun consult-gh--browse-url-action ()
"Default action to run on selected itesm in `consult-gh'."
(lambda (cand)
  (browse-url (concat "https://github.com/" (substring cand)))
))

(defun consult-gh--repos-group (cand transform)
"Group the list of item in `consult-gh' by the name of the user"
  (let ((name (car (string-split (substring cand) "\/"))))
           (if transform (substring cand) name)))

(defun consult-gh--narrow (item)
"Create narrowing function for items in `consult-gh' by the first letter of the name of the user/organization."
  (if (stringp item)
    (cons (string-to-char (substring-no-properties item)) (substring-no-properties item))))

(defun consult-gh--annotate ()
"Annotate each repo in `consult-gh' by user, visibility and date."
(lambda (cand)
  ;; (format "%s" cand)
  (if-let ((user (format "%s" (get-text-property 1 :user cand)))
         (visibility (format "%s" (get-text-property 1 :visibility cand)))
         (date (format "%s" (get-text-property 1 :version cand))))

      (progn
        (setq user (propertize user 'face 'consult-gh-user-face)
          visibillity (propertize visibility 'face 'consult-gh-visibility-face)
          date (propertize date 'face 'consult-gh-date-face))
        (format "%s\s\s%s\s\s%s" user visibility date)
     )
    nil)
))

(defun consult-gh--clone-repo (repo targetdir name)
"Clone the repo to targetdir/name directory. It uses \"gh clone repo ...\"."
  (consult-gh--call-process "repo" "clone" (format "%s" repo) (expand-file-name name targetdir))
  (message (format "repo %s was cloned to %s" repo (propertize (expand-file-name name targetdir) 'face 'font-lock-type-face))))

(defun consult-gh-clone-repo (&optional repo targetdir name)
  (interactive)
  (let ((repo (read-string "repo: " repo))
        (targetdir (read-directory-name "target directory: " targetdir))
        (name (read-string "name: " name))
        )
  (consult-gh--clone-repo repo targetdir name)
    ))

(defun consult-gh--clone-repo-action ()
  (lambda (cand)
    (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties cand))))
         (package (car (last (split-string reponame "\/"))))
         )
    (if consult-gh-confirm-before-clone
        (consult-gh-clone-repo reponame consult-gh-default-clone-directory package)
      (consult-gh--clone-repo reponame consult-gh-default-clone-directory package))
    )))

(defun consult-gh--make-source-from-org  (org)
"Create a source for consult from the repos of the organization to use in `consult-gh-orgs'."
                  `(:narrow ,(consult-gh--narrow org)
                    :category 'consult-gh
                    :items  ,(consult-gh--get-repos-of-org org)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-action)
                    :annotate ,(consult-gh--annotate)
                    :defualt t
                    :history t
                    ))

(defun consult-gh--make-source-from-search-repo  (repo)
"Create a source for consult from the search results for repo to use in `consult-gh-search-repos'."
                  `(:narrow ,(consult-gh--narrow repo)
                    :category 'consult-gh
                    :items  ,(consult-gh--get-search-repos repo)
                    :face 'consult-gh-default-face
                    :action ,(funcall consult-gh-action)
                    :annotate ,(consult-gh--annotate)
                    :default t
                    :history t
                    ))

(defun consult-gh-orgs (orgs)
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
                    :group #'consult-gh--repos-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh
                    ))
      )))

(defun consult-gh-default-repos ()
"Show the repos from default organizaitons."
  (interactive)
(consult-gh-orgs consult-gh-default-orgs-list))

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
                    :group #'consult-gh--repos-group
                    :history 'consult-gh--repos-history
                    :category 'consult-gh
                    ))
      (message (concat "consult-gh: " (propertize "no repositories matched your search!" 'face 'warning))))))

(provide 'consult-gh)
