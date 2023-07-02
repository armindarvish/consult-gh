;;; consult-gh-forge.el --- Magit/Forge Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.11
;; Package-Requires: ((emacs "27.1") (consult "0.34") (forge "0.3.3") (gh "2.29"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, forges, completion

;;; Commentary:

;;; Code:

(if (featurep 'forge)
    (require 'forge)
  (message "forge is not loaded"))
(require 'consult-gh)
;;; Customization Variables

(defcustom consult-gh-forge-timeout-seconds 10
  "Timeout in seconds for forge-visit to load the issue, otherwise revert back to normal viewing in consult-gh."
  :group 'consult-gh
  :type 'integer
  )

(defvar consult-gh-forge--added-repositories (list)
  "List of repositories added to forge's database by `consult-gh-forge--add-topic'.")

(defun consult-gh-forge--make-tempdir (repo)
  (let* ((tempdir (expand-file-name (concat repo "/" "forge/.git/") consult-gh-tempdir))
         (_ (make-directory (file-name-directory tempdir) t)))
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

(defun consult-gh-forge--remove-repository (url)
  (let* ((forge-repo (forge-get-repository url))
         (owner (oref forge-repo owner))
         (name (oref forge-repo name))
         (host (oref forge-repo githost)))
    (closql-delete (forge-get-repository (list host owner name)))
    ))

(defun consult-gh-forge-remove-repositories (&optionals urls)
  (let ((urls (or urls consult-gh-forge--added-repositories)))
       (mapcar #'consult-gh-forge--remove-repository urls)))

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
  (declare (indent 2))
  `(consult-gh-forge--magit-setup-buffer-internal
    ,mode ,locked
    ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                           `(list ',var ,form))
                         bindings))))

(defun consult-gh-forge--topic-setup-buffer (topic)
  (let* ((repo  (forge-get-repository topic))
         (ident (concat (forge--topic-type-prefix topic)
                        (number-to-string (oref topic number))))
         (owner (oref repo owner))
         (name (oref repo name))
         (buffname  (format "*forge: %s/%s %s*" owner name ident))
         (tempdir (consult-gh-forge--make-tempdir (format "%s/%s" owner name)))
         (magit-generate-buffer-name-function (lambda (_mode _value) buffname))
         (default-directory (or (oref repo worktree)
                                tempdir)))
    (consult-gh-forge--magit-setup-buffer #'forge-topic-mode t
      (forge-buffer-topic topic)
      (forge-buffer-topic-ident ident)))
  )

(defun consult-gh-forge--visit-topic (topic)
  (consult-gh-forge--topic-setup-buffer topic))

(defun consult-gh-forge--issue-view (repo issue &optional timeout)
  "Try to load the issue in forge within the timeout limit, otherwise revert back to running `consult-gh--issue-view-action'."
  (cl-letf (((symbol-function #'magit-toplevel)
             (lambda () (consult-gh-forge--make-tempdir repo)))
            ((symbol-function #'magit-gitdir)
             (lambda () (consult-gh-forge--make-tempdir repo))))
    (let* ((default-directory consult-gh-tempdir)
           (url (string-trim (consult-gh--command-to-string "browse" "--repo" (string-trim repo) "--no-browser")))
           (id (string-to-number issue))
           (timeout (or timeout consult-gh-forge-timeout-seconds))
           (created (consult-gh-forge--add-topic url id))
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
      )))

(defun consult-gh-forge--issue-view-action ()
  "The action function that gets an issue candidate for example from `consult-gh-issue-list' and opens a preview in `forge' using `consult-gh-forge--issue-view'."
  (lambda (cand)
    (let* ((repo (substring (get-text-property 0 :repo cand)))
           (issue (substring (get-text-property 0 :issue cand))))
      (consult-gh-forge--issue-view repo issue)
      )))

(provide 'consult-gh-forge)
