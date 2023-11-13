;;; consult-gh-forge.el --- Magit/Forge Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (consult "0.34") (forge "0.3.3") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

;;; Requirements

(if (featurep 'forge)
    (require 'forge)
  (message "magit/forge is not loaded. Make sure both magit and forge are installed and loaded before loading consult-gh-forge."))
(require 'consult-gh)

;;; Customization Variables

(defcustom consult-gh-forge-timeout-seconds 10
  "Timeout in seconds for forge-visit to load the issue, otherwise revert back to normal viewing in consult-gh."
  :group 'consult-gh
  :type 'integer
  )

;;; Other Variables

(defvar consult-gh-forge--added-repositories (list)
  "List of repositories added to forge's database by `consult-gh-forge--add-topic'.")

;;; Define Backend Functions for `consult-gh-forge'

(defun consult-gh-forge--make-tempdir (repo)
"This function makes a temporary directory for repo in `consult-gh-tempdir' so that magit and forge can operate as intended."
  (let* ((tempdir (expand-file-name (concat repo "/" "forge/") consult-gh-tempdir))
         (gitdir (expand-file-name ".git/" tempdir))
         (_ (make-directory (file-name-directory gitdir) t)))
    tempdir))

(defun consult-gh-forge--add-repository (url)
  "Add a repository to the forge database and only
pull individual topics when the user invokes `forge-pull-topic'. see forge documentation for `forge-add-repository'."
  (if (forge-get-repository url nil 'full)
      nil
    (progn
      (let ((repo (forge-get-repository url nil 'create)))
        (oset repo sparse-p nil)
        (oset repo selective-p t)
        (forge--pull repo nil)
        )
      "created")))


(defun consult-gh-forge--remove-repository (host owner name)
"Removes the forge defined by host owner and name from `forge-database'."
  (closql-delete (forge-get-repository (list host owner name))))

(defun consult-gh-forge--remove-repository-by-url (url)
"Removes the forge defined by url from `forge-database'."
  (let* ((forge-repo (forge-get-repository url))
         (owner (oref forge-repo owner))
         (name (oref forge-repo name))
         (host (oref forge-repo githost)))
    (closql-delete (forge-get-repository (list host owner name)))
    (setq consult-gh-forge--added-repositories (delete url consult-gh-forge--added-repositories))))

(defun consult-gh-forge-remove-added-repositories (&optional urls)
"Removes all the forge repositories added to `forge-database' by `consult-gh-forge--add-repository', which are stored in the list, `conuslt-gh-forge--aded-repositories'."
  (interactive)
  (let ((urls (or urls consult-gh-forge--added-repositories)))
    (mapcar #'consult-gh-forge--remove-repository-by-url urls)))

(defun consult-gh-forge-remove-repository (&optional urls)
"Interactively queries the user the select forge(s) from `consult-gh-forge--added-repositories' list and removes the selecte dones from `forge-database'"
  (interactive)
  (let* ((list (mapcar (lambda (url) (let* ((url-parse (forge--split-url url))
                                            (repo (string-join (cdr url-parse) "/"))
                                            (host (car url-parse)))
                                       (format "%s @%s" repo host))) consult-gh-forge--added-repositories))
         (urls (or urls (completing-read-multiple "Remove Repository from forge db: " list)))
         )
    (message (format "%s" urls))
    (mapcar (lambda (url-parts) (let* ((parts (string-split url-parts " "))
                                 (host (string-trim (car (cdr parts)) "@"))
                                 (owner (car (string-split (car parts) "/")))
                                 (name (cadr (string-split (car parts) "/")))
                                 (url (string-trim (consult-gh--command-to-string "browse" "--repo" (format "%s/%s" owner name) "--no-browser"))))
                            (consult-gh-forge--remove-repository host owner name)
                            (setq consult-gh-forge--added-repositories
                                  (delete url consult-gh-forge--added-repositories))))
            urls)))

(defun consult-gh-forge--pull-topic (url topic)
  "Pull the topic from repository at url using `forge-pull-topic'. see forge documentation for `forge-pull-topic'."
  (let ((repo (forge-get-repository url)))
    (forge--zap-repository-cache repo)
    (forge--pull-topic repo
                       (forge-issue :repository (oref repo id)
                                    :number topic))
    ))

(defun consult-gh-forge--add-topic (url topic)
  "Add a repository to the forge database and only
pull individual topics when the user invokes `forge-pull-topic'. see forge documentation for `forge-add-repository'."
  (cl-letf (((symbol-function #'magit-toplevel)
             (lambda () (consult-gh--make-tempdir (string-join (cdr (forge--split-url url)) "/")))
             ))
    (let ((created (consult-gh-forge--add-repository url))
          (repo (forge-get-repository url)))
      (while (not repo)
        (sit-for 0.0001)
        (setq repo (forge-get-repository url)))
      (consult-gh-forge--pull-topic url topic)
      created
      )))


(defun consult-gh-forge--magit-setup-buffer-internal (mode locked bindings)
"Reimplements `magit-setup-buffer-intenral' in order to avoid making changes to magit settings for the repository in the current working directory."
  (let* ((value (and locked
                     (with-temp-buffer
                       (pcase-dolist (`(,var ,val) bindings)
                         (set (make-local-variable var) val))
                       (let ((major-mode mode))
                         (magit-buffer-value)))))
         (buffer  (magit-get-mode-buffer mode value))
         (section (and buffer (magit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (magit-generate-new-buffer mode value)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (funcall mode)
      (magit-xref-setup #'magit-setup-buffer-internal bindings)
      (setq magit-buffer-gitdir (magit-gitdir))
      (setq magit-buffer-topdir (magit-toplevel))
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        (run-hooks 'magit-create-buffer-hook)))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'magit-setup-buffer-hook)
      (magit-refresh-buffer))
    buffer)
  )

(defmacro consult-gh-forge--magit-setup-buffer (mode &optional locked &rest bindings)
"Reimplements `magit-setup-buffer' in order to avoid making changes to magit settings for the repository in the current working directory."
  (declare (indent 2))
  `(consult-gh-forge--magit-setup-buffer-internal
    ,mode ,locked
    ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                           `(list ',var ,form))
                         bindings))))

(defun consult-gh-forge--topic-setup-buffer (topic)
"Reimplements `forge-setup-buffer' in order to avoid making changes to magit settings for the repository in the current working directory."
  (let* ((repo  (forge-get-repository topic))
         (ident (concat "#"
                        (number-to-string (oref topic number))))
         (owner (oref repo owner))
         (name (oref repo name))
         (buffname  (format "*forge: %s/%s %s*" owner name ident))
         (tempdir (consult-gh-forge--make-tempdir (format "%s/%s" owner name)))
         (magit-generate-buffer-name-function (lambda (_mode _value) buffname))
         (default-directory (or (oref repo worktree)
                                tempdir)))
    (consult-gh-forge--magit-setup-buffer (if (functionp 'forge-issue-mode) (if (forge-issue-p topic) #'forge-issue-mode #'forge-pullreq-mode) #'forge-topic-mode) t
      (forge-buffer-topic topic)
      (forge-buffer-topic-ident ident)))
  )

(defun consult-gh-forge--visit-topic (topic)
"Reimplements `forge-visit-topic' in order to avoid making changes to magit settings for the repository in the current working directory."
  (consult-gh-forge--topic-setup-buffer topic))


;;; Define Functions and Interactive Commands for `consult-gh-forge'

(defun consult-gh-forge--issue-view (repo issue &optional timeout)
  "Adds the repo and issue to forge database by using `consult-gh-forge--add-topic' and Tries to load the issue in forge by `consult-gh-forge--visit-topic' within the timeout limit (in seconds), otherwise reverts back to using `consult-gh--issue-view-action' to open the issue."
  (let* ((repo (string-trim repo))
         (tempdir (consult-gh-forge--make-tempdir repo))
         (default-directory (file-name-parent-directory tempdir))
         (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (id (string-to-number issue))
         (timeout (or timeout consult-gh-forge-timeout-seconds)))

    (cl-letf (((symbol-function #'magit-toplevel)
               `(lambda () ,tempdir))
              ((symbol-function #'magit-gitdir)
               `(lambda () (expand-file-name ".git/" ,tempdir))))
      (let* ((created (consult-gh-forge--add-topic url id))
             (topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
        (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (funcall (consult-gh--issue-view-action) (propertize (format "%s" issue) ':repo repo ':issue issue)))
          (while (not topic)
            (sit-for 0.001)
            (setq topic (ignore-errors (forge-get-topic (forge-get-repository url) id)))
            )
          (if topic
              (consult-gh-forge--visit-topic topic)
            (consult-gh--issue-view repo issue)))
        (when created
          (add-to-list 'consult-gh-forge--added-repositories url))
        ))))

(defun consult-gh-forge--issue-view-action (cand)
  "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in `forge' using `consult-gh-forge--issue-view'."
    (let* ((info (cdr cand))
           (repo (substring-no-properties (plist-get info :repo)))
           (issue (substring-no-properties (format "%s" (plist-get info :issue)))))
      (consult-gh-forge--issue-view repo issue)
      ))

(defun consult-gh-forge--pr-view (repo pr &optional timeout)
  "Adds the repo and issue to forge database by using `consult-gh-forge--add-topic' and Tries to load the issue in forge by `consult-gh-forge--visit-topic' within the timeout limit (in seconds), otherwise reverts back to using `consult-gh--issue-view-action' to open the issue."
  (let* ((repo (string-trim repo))
         (tempdir (consult-gh-forge--make-tempdir repo))
         (default-directory tempdir)
         (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
         (id (string-to-number pr))
         (timeout (or timeout consult-gh-forge-timeout-seconds)))
    (cl-letf (((symbol-function #'magit-toplevel)
               `(lambda () ,tempdir))
              ((symbol-function #'magit-gitdir)
               `(lambda () (expand-file-name ".git/" ,tempdir))))
      (let* ((created (consult-gh-forge--add-topic url id))
             (topic (ignore-errors (forge-get-topic (forge-get-repository url) id))))
        (with-timeout (timeout (message "could not load the topic in forge, reverting back to consult-gh--issue-view!") (funcall (consult-gh--pr-view-action) (propertize (format "%s" issue) ':repo repo ':issue issue)))
          (while (not topic)
            (sit-for 0.001)
            (setq topic (ignore-errors (forge-get-topic (forge-get-repository url) id)))
            )
          (if topic
              (consult-gh-forge--visit-topic topic)
            (consult-gh--pr-view repo pr)))
        (when created
          (add-to-list 'consult-gh-forge--added-repositories url))
        ))))

(defun consult-gh-forge--pr-view-action (cand)
  "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in `forge' using `consult-gh-forge--issue-view'."
    (let* ((info (cdr cand))
           (repo (substring-no-properties (plist-get info :repo)))
           (pr (substring-no-properties (format "%s" (plist-get info :pr)))))
      (consult-gh-forge--pr-view repo pr)
      ))

;;; Provide `consult-gh-forge' module

(provide 'consult-gh-forge)
