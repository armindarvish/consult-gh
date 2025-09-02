;;; consult-gh-nerd-icons.el --- nerd icons Integration for consult-gh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 3.0
;; Package-Requires: ((emacs "29.4") (nerd-icons "0.1.0") (consult-gh "3.0"))
;; Homepage: https://github.com/armindarvish/consult-gh
;; Keywords: matching, git, repositories, completion

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
;; This package provides nerd-icons integration for consult-gh.
;; (see URL `https://github.com/armindarvish/consult-gh' for more info).

;;; Code:

;;; Requirements
(require 'nerd-icons)
(require 'consult-gh)

;;; Group

(defgroup consult-gh-nerd-icons nil
  "Nerd icons for `consult-gh'."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'consult-gh
  :prefix "consult-gh-nerd-icons-"
  :link '(url-link :tag "GitHub" "https://github.com/armindarvish/consult-gh"))

;;; Customization Variables

(defcustom consult-gh-nerd-icons
  '((repo  . (nerd-icons-octicon "nf-oct-repo" 'consult-gh-repo))
    (issue  . (nerd-icons-codicon "nf-cod-issues" 'consult-gh-issue))
    (pr  . (nerd-icons-mdicon "nf-md-source_pull" 'consult-gh-pr))
    (user  . (nerd-icons-octicon "nf-oct-person" 'consult-gh-user))
    (branch . (nerd-icons-octicon "nf-oct-git_branch" 'consult-gh-branch))
    (label . (nerd-icons-mdicon "nf-md-tag" 'consult-gh-tag))
    (milestone . (nerd-icons-octicon "nf-oct-milestone" 'consult-gh-branch))
    (project . (nerd-icons-mdicon "nf-md-rocket" 'consult-gh-package))
    (tag . (nerd-icons-faicon "nf-fa-barcode" 'consult-gh-pr))
    (star . (nerd-icons-octicon "nf-oct-star" 'consult-gh-highlight-match))
    (topic . (nerd-icons-mdicon "nf-md-focus_field" 'consult-gh-description))
    (dired-dir . (nerd-icons-mdicon "nf-md-folder" 'consult-gh-dired-directory))
    (dired-file . (nerd-icons-mdicon "nf-md-file" 'consult-gh-dired-file))
    (dired-symlink . (nerd-icons-octicon "nf-oct-file_symlink_file" 'consult-gh-dired-symlink))
    (dired-commit . (nerd-icons-mdicon "nf-md-link_variant" 'consult-gh-dired-commit)))
  "Alist of icons for consult-gh categories."
  :group 'consult-gh-nerd-icons
  :type '(alist :key-type symbol
                :value-type (list symbol string symbol)))

(defcustom consult-gh-nerd-faces
  '((dired-dir . consult-gh-dired-directory)
    (dired-file . consult-gh-dired-file)
    (dired-symlink . consult-gh-dired-symlink)
    (dired-commit . consult-gh-dired-commit))
  "Alist of faces for `consult-gh' file categories."
  :group 'consult-gh-nerd-icons
  :type '(alist :key-type symbol
                :value-type (choice (variable :tag "A face variable")
                                    (function :tag "A function that takes name of the file and return a face variable"))))

(defvar consult-gh-repo-icon-default consult-gh-repo-icon
  "Default value of `consult-gh-repo-icon'.")

(defvar consult-gh-issue-icon-default consult-gh-issue-icon
  "Default value of `consult-gh-issue-icon'.")

(defvar consult-gh-pr-icon-default consult-gh-pr-icon
  "Default value of `consult-gh-pr-icon'.")

(defvar consult-gh-user-icon-default consult-gh-user-icon
  "Default value of `consult-gh-user-icon'.")

(defvar consult-gh-branch-icon-default consult-gh-branch-icon
  "Default value of `consult-gh-branch-icon'.")

(defvar consult-gh-label-icon-default consult-gh-label-icon
  "Default value of `consult-gh-label-icon'.")

(defvar consult-gh-milestone-icon-default consult-gh-milestone-icon
  "Default value of `consult-gh-milestone-icon'.")

(defvar consult-gh-project-icon-default consult-gh-project-icon
  "Default value of `consult-gh-project-icon'.")

(defvar consult-gh-star-icon-default consult-gh-star-icon
  "Default value of `consult-gh-star-icon'.")

(defvar consult-gh-tag-icon-default consult-gh-tag-icon
  "Default value of `consult-gh-tag-icon'.")

(defvar consult-gh-topic-icon-default consult-gh-topic-icon
  "Default value of `consult-gh-topic-icon'.")

(defvar consult-gh-dired-dir-icon-default consult-gh-dired-dir-icon
  "Default value of `consult-gh-dired-dir-icon'.")

(defvar consult-gh-dired-file-icon-default consult-gh-dired-file-icon
  "Default value of `consult-gh-dired-file-icon'.")

(defvar consult-gh-dired-symlink-icon-default consult-gh-dired-symlink-icon
  "Default value of `consult-gh-dired-symlink-icon'.")

(defvar consult-gh-dired-commit-icon-default consult-gh-dired-commit-icon
  "Default value of `consult-gh-dired-commit-icon'.")

(defvar consult-gh-dired-dir-face-default consult-gh-dired-dir-face
  "Default value of `consult-gh-dired-dir-face'.")

(defvar consult-gh-dired-file-face-default consult-gh-dired-file-face
  "Default value of `consult-gh-dired-file-face'.")

(defvar consult-gh-dired-symlink-face-default consult-gh-dired-symlink-face
  "Default value of `consult-gh-dired-symlink-face'.")

(defvar consult-gh-dired-commit-face-default consult-gh-dired-commit-face
  "Default value of `consult-gh-dired-commit-face'.")


(defun consult-gh-nerd-icons-get-icon (symbol)
  "Return the icon for SYMBOL."
  (if-let* ((spec (cdr (assoc symbol consult-gh-nerd-icons)))
            (func (car spec))
            (name (cadr spec))
            (face (caddr spec)))
      (if face
          (concat (funcall func name :height 1.0 :face face) " ")
        (concat (funcall func name :height 1.0) " "))
    " "))

(defun consult-gh-nerd-icons-get-face (symbol)
  "Return the face for SYMBOL."
  (let* ((face (cdr-safe (assoc symbol consult-gh-nerd-faces))))
    (if (facep face)
        face
      'default)))

(defun consult-gh-nerd-icons-get-icon-for-dir (name)
  "Return the icon for a directory."
  (if (stringp name)
  (nerd-icons-icon-for-dir name :face 'consult-gh-dired-directory)))


(defun consult-gh-nerd-icons-get-face-for-file (name)
  "Return the face for a file NAME."
  (if (stringp name)
  (let* ((name (file-name-nondirectory name))
         (ext (file-name-extension name))
         (icon (or (and ext
                        (cdr (assoc (downcase ext)
                                    nerd-icons-extension-icon-alist)))
                   (nerd-icons-match-to-alist name nerd-icons-regexp-icon-alist)
                   nerd-icons-default-file-icon))
         (face (or (plist-get icon :face) 'default)))
    face)
  'default))

(defun consult-gh-nerd-icons--mode-on ()
  "Enable `consult-gh-nerd-icons-mode'."
  (setq consult-gh-repo-icon-default consult-gh-repo-icon
        consult-gh-issue-icon-default consult-gh-issue-icon
        consult-gh-pr-icon-default consult-gh-pr-icon
        consult-gh-user-icon-default consult-gh-user-icon
        consult-gh-branch-icon-default consult-gh-branch-icon
        consult-gh-label-icon-default consult-gh-label-icon
        consult-gh-milestone-icon-default consult-gh-milestone-icon
        consult-gh-project-icon-default consult-gh-project-icon
        consult-gh-star-icon-default consult-gh-star-icon
        consult-gh-tag-icon-default consult-gh-tag-icon
        consult-gh-topic-icon-default consult-gh-topic-icon
        consult-gh-dired-dir-icon-default consult-gh-dired-dir-icon
        consult-gh-dired-file-icon-default consult-gh-dired-file-icon
        consult-gh-dired-symlink-icon-default consult-gh-dired-symlink-icon
        consult-gh-dired-commit-icon-default consult-gh-dired-commit-icon
        consult-gh-dired-dir-face-default consult-gh-dired-dir-face
        consult-gh-dired-file-face-default consult-gh-dired-file-face
        consult-gh-dired-symlink-face-default consult-gh-dired-symlink-face
        consult-gh-dired-commit-face-default consult-gh-dired-commit-face
        consult-gh-repo-icon (consult-gh-nerd-icons-get-icon 'repo)
        consult-gh-issue-icon (consult-gh-nerd-icons-get-icon 'issue)
        consult-gh-pr-icon (consult-gh-nerd-icons-get-icon 'pr)
        consult-gh-user-icon (consult-gh-nerd-icons-get-icon 'user)
        consult-gh-branch-icon (consult-gh-nerd-icons-get-icon 'branch)
        consult-gh-label-icon (consult-gh-nerd-icons-get-icon 'label)
        consult-gh-milestone-icon (consult-gh-nerd-icons-get-icon 'milestone)
        consult-gh-project-icon (consult-gh-nerd-icons-get-icon 'project)
        consult-gh-star-icon (consult-gh-nerd-icons-get-icon 'star)
        consult-gh-tag-icon (consult-gh-nerd-icons-get-icon 'tag)
        consult-gh-topic-icon (consult-gh-nerd-icons-get-icon 'topic)
        consult-gh-dired-dir-icon #'consult-gh-nerd-icons-get-icon-for-dir
        consult-gh-dired-file-icon #'nerd-icons-icon-for-file
        consult-gh-dired-symlink-icon (consult-gh-nerd-icons-get-icon 'dired-symlink)
        consult-gh-dired-commit-icon (consult-gh-nerd-icons-get-icon 'dired-commit)
        consult-gh-dired-dir-face (consult-gh-nerd-icons-get-face 'dired-dir)
        consult-gh-dired-file-face (consult-gh-nerd-icons-get-face 'dired-file)
        consult-gh-dired-symlink-face (consult-gh-nerd-icons-get-face 'dired-symlink)
        consult-gh-dired-commit-face (consult-gh-nerd-icons-get-face 'dired-commit)))

(defun consult-gh-nerd-icons--mode-off ()
  "Disable `consult-gh-nerd-icons-mode'."
  (setq consult-gh-repo-icon consult-gh-repo-icon-default
        consult-gh-issue-icon consult-gh-issue-icon-default
        consult-gh-pr-icon consult-gh-pr-icon-default
        consult-gh-user-icon consult-gh-user-icon-default
        consult-gh-branch-icon consult-gh-branch-icon-default
        consult-gh-label-icon consult-gh-label-icon-default
        consult-gh-milestone-icon consult-gh-milestone-icon-default
        consult-gh-project-icon consult-gh-project-icon-default
        consult-gh-star-icon consult-gh-star-icon-default
        consult-gh-tag-icon consult-gh-tag-icon-default
        consult-gh-topic-icon consult-gh-topic-icon-default
        consult-gh-dired-dir-icon consult-gh-dired-dir-icon-default
        consult-gh-dired-file-icon consult-gh-dired-file-icon-default
        consult-gh-dired-symlink-icon consult-gh-dired-symlink-icon-default
        consult-gh-dired-commit-icon consult-gh-dired-commit-icon-default
        consult-gh-dired-dir-face consult-gh-dired-dir-face-default
        consult-gh-dired-file-face consult-gh-dired-file-face-default
        consult-gh-dired-symlink-face consult-gh-dired-symlink-face-default
        consult-gh-dired-commit-face consult-gh-dired-commit-face-default))

;;;###autoload
(define-minor-mode consult-gh-nerd-icons-mode
  "Use`nerd-icons' in `consult-gh'."
  :init-value nil
  :global t
  :group 'consult-gh
  :lighter " consult-gh-nerd-icons"
  (if consult-gh-nerd-icons-mode
      (consult-gh-nerd-icons--mode-on)
    (consult-gh-nerd-icons--mode-off)))


;;; Provide `consult-gh-nerd-icons' module

(provide 'consult-gh-nerd-icons)

;;; consult-gh-nerd-icons.el ends here
