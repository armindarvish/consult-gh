;;; Add embark menus if embark is available
(with-eval-after-load 'embark

(defun consult-gh-embark-get-ssh-link (repo)
  "Get the ssh based link of the repo"
  (kill-new (concat "git@github.com:" (string-trim  (consult-gh--output-cleanup (substring-no-properties repo))) ".git")))

(defun consult-gh-embark-get-https-link (repo)
  "Get the ssh based link of the repo"
  (kill-new (concat "https://github.com/" (string-trim (consult-gh--output-cleanup (substring-no-properties repo))) ".git")))

(defun consult-gh-embark-get-straight-usepackage (repo)
  "Close tab."
  (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties repo))))
         (package (car (last (split-string reponame "\/"))))
         )
    (kill-new (concat "(use-package " package "\n\t:straight (" package " :type git :host github :repo \"" reponame  "\")\n)"))))

(defun consult-gh-embark-clone-repo (repo)
  "Clone the repo at point"
  (let* ((reponame  (consult-gh--output-cleanup (string-trim (substring-no-properties repo))))
         (package (car (last (split-string reponame "\/"))))
         )
    (consult-gh-clone-repo reponame consult-gh-default-clone-directory package)))


(defvar-keymap consult-gh-embark-map
  :doc "Keymap for consult-gh-embark"
  "s" #'consult-gh-embark-get-ssh-link
  "h" #'consult-gh-embark-get-https-link
  "e" #consult-gh-embark-get-straight-usepackage
  "c" #'consult-gh-embark-clone-repo)

(add-to-list 'embark-keymap-alist '(consult-gh . consult-gh-embark-map)))
