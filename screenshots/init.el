;;; init.el --- -*- lexical-binding: t; -*-

(require 'package)
;; Adds the Melpa archive to the list of available repositories
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)
(setq use-package-always-ensure t)
(setq enable-recursive-minibuffers t)

;; Looks
(add-to-list 'default-frame-alist '(background-mode . light))
(add-to-list 'default-frame-alist '(alpha . (95 85)))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode 1)
(scroll-bar-mode -1)
(set-fringe-mode 20)
(setq linum-format "%3d \u2502 ")
(column-number-mode)
(global-display-line-numbers-mode t)
(global-visual-line-mode 1)
(transient-mark-mode 1)
(require 'battery)
(if (and (functionp battery-status-function)
         (equal (cdr (assoc ?L (funcall battery-status-function))) "Battery"))
    (display-battery-mode +1)
  (display-battery-mode -1))
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode +1)
(redraw-frame)
(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono")
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono") nil 'prepend))
(setq standard-mode-line-format mode-line-format)
(setq mode-line-right-align-edge 'right-fringe)

(defface my:modeline-indicator-active
  `((t :foreground "#FFFFFF" :background "#6AE4B9"))
  "The face used for mode-line indicator")

(defface my:modeline-indicator-inactive
  `((t :foreground "#FFFFFF" :background "#6a6a6a"))
  "The face used for mode-line indicator")

(defun my:modeline-indicator ()
  (concat (propertize " " 'face 'my:modeline-indicator-inactive 'width 0.2) " "))

(defvar my:mode-line-front-space "   ")
(setq my:mode-line-front-space " ")
(setq-default mode-line-format
              (list "%e"
                    '(:eval (my:modeline-indicator))
                    'my:mode-line-front-space
                    " "
                    'mode-line-front-space
                    'mode-line-frame-identification
	            'mode-line-buffer-identification
	            " "
	            'mode-line-position
                    'mode-line-format-right-align
                    'mode-line-misc-info
	            'mode-line-modes
                    '(vc-mode vc-mode)
                    'mode-line-front-space
                    "%+"
	            'mode-line-end-spaces))

(defun my:update-modeline (window)
  "Update my mode-line-indicator face in WINDOW dependening on whether the window is selected."
  (with-current-buffer (window-buffer window)
    (let ((rest (cddr mode-line-format)))
      (if (eq window (selected-window))
          (setq-local mode-line-format
                      (append (list (car mode-line-format) '(:eval (concat (propertize " " 'face 'my:modeline-indicator-active 'width 0.2) " "))) rest))
        (setq-local mode-line-format (append (list (car mode-line-format) '(:eval (concat (propertize " " 'face 'my:modeline-indicator-inactive 'width 0.2) " "))) rest))))))

(defun my:battery-status-function ()
  (when (featurep 'nerd-icons)
    (let* ((data (and battery-status-function
                      (functionp battery-status-function)
                      (funcall battery-status-function)))
           (status (cdr (assoc ?L data)))
           (charging? (or (string-equal "AC" status)
                          (string-equal "on-line" status)))
           (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
           (valid-percentage? (and (numberp percentage)
                                   (>= percentage 0)
                                   (<= percentage battery-mode-line-limit)))
           (face (if valid-percentage?
                     (cond (charging? 'font-lock-type-face)
                           ((< percentage battery-load-critical) 'error)
                           ((< percentage 25) 'warning)
                           ((< percentage 95) 'mode-line)
                           (t 'font-lock-type-face))
                   'error))
           (icon (if valid-percentage?
                     (cond
                      ((>= percentage 100)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_100" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 90)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_90" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_90" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 80)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_80" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_80" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 70)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_70" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_70" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 60)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_60" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_60" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 50)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_50" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_50" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 40)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_40" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_40" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 30)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_30" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_30" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 20)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_20" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_20" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      ((>= percentage 10)
                       (if charging?
                           (nerd-icons-mdicon "nf-md-battery_charging_10" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                         (nerd-icons-mdicon "nf-md-battery_10" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
                      (t (if charging?
                             (nerd-icons-mdicon "nf-md-battery_charging_outline" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)
                           (nerd-icons-mdicon "nf-md-battery_outline" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face))))
                   (nerd-icons-mdicon "nf-md-battery_alert" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face face)))
           (text (if valid-percentage? (format "%d%s" percentage "% ") ""))
           (help-echo (if (and battery-echo-area-format data valid-percentage?)
                          (battery-format battery-echo-area-format data)
                        "Battery status not available"))
           (res (propertize (concat icon (propertize text 'face face)) 'help-echo help-echo)))
      (setq battery-mode-line-string res))))

(defun my:modeline-override-battery ()
  (if display-battery-mode
      (progn
        (advice-add #'battery-update :override #'my:battery-status-function)
        (and (bound-and-true-p display-battery-mode) (battery-update)))
    (progn
      (advice-remove #'battery-update #'my:battery-status-function))))

(add-hook 'display-battery-mode-hook 'my:modeline-override-battery)

(defun my:vc-mode-line (&optional file backend &rest args)
  "Set `vc-mode' to display type of version control for FILE.

The value is set in the current buffer, which should be the buffer
visiting FILE.
If BACKEND is passed use it as the VC backend when computing the result."
  (when (featurep 'nerd-icons)
    (let* ((file (or file (buffer-file-name)))
           (backend (or backend (vc-backend file))))
      (cond
       ((not backend)
        (setq vc-mode nil))
       ((null vc-display-status)
        (setq vc-mode (concat " " (symbol-name backend))))
       (t
        (let* ((state (vc-state file backend))
               (icon (cond
                      ((memq state '(edited added))
                       (nerd-icons-devicon "nf-dev-git_compare" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face 'font-lock-type-face))
                      ((eq state 'needs-merge)
                       (nerd-icons-devicon "nf-dev-git_merge" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face 'font-lock-type-face))
                      ((eq state 'needs-update)
                       (nerd-icons-devicon "nf-dev-git_pull_request" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face 'font-lock-type-face))
                      ((memq state '(removed conflict unregistered))
                       (nerd-icons-octicon "nf-oct-alert" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face 'error))
                      (t (nerd-icons-devicon "nf-dev-git_branch" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face 'font-lock-type-face))))
               (str (if vc-display-status
                        (substring (vc-call-backend backend 'mode-line-string file) (+ (if (eq backend 'Hg) 2 3) 1))
                      ""))
               (face (cond
                      ((eq state 'needs-update)
                       'warning)
                      ((memq state '(removed conflict unregistered))
                       'error)
                      (t 'font-lock-type-face)))
               (text (propertize (if (length> str 12)
                                     (concat
                                      (substring str 0 8) "...")
                                   str)
                                 'face face)))
          (setq vc-mode (concat
	                 " "
                         (propertize (concat icon " " text)
	                             'mouse-face 'mode-line-highlight
	                             'help-echo
	                             (concat (or str
			                         (format "File under the %s version control system"
				                         backend))
		                             "\nmouse-1: Version Control menu")
	                             'local-map vc-mode-line-map)))))))))

(when (featurep 'nerd-icons)
  (add-hook 'find-file-hook #'my:vc-mode-line)
  (add-hook 'after-save-hook #'my:vc-mode-line)
  (advice-add #'vc-refresh-state :after #'my:vc-mode-line))

(use-package moody
  :ensure t
  :init
  (defun my:moody-enable ()
    (interactive)
    (moody-replace-vc-mode)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-eldoc-minibuffer-message-function)
    (moody-redisplay t))

  (defun my:moody-disable ()
    (interactive)
    (moody-replace-vc-mode t)
    (moody-replace-mode-line-buffer-identification t)
    (moody-replace-eldoc-minibuffer-message-function t)
    (moody-redisplay t))
  :config
  (my:moody-enable))

(use-package minions
  :after nerd-icons
  :custom
  (minions-mode-line-lighter (nerd-icons-codicon "nf-cod-gear" :height 1.2 :v-adjust 0.0 :width 1.2 :h-adjust 0.0 :face 'mode-line))
  (minions-mode-line-delimiters nil)
  :config
  (defun my:minions-mode-line-modes ()
    (setq minions-mode-line-modes
          (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
            (list (propertize "%[" 'help-echo recursive-edit-help-echo)
                  '(:eval (car minions-mode-line-delimiters))
                  `(:propertize ("" mode-name)
                                face '(bold mode-line-emphasis)
                                help-echo "Major mode
mouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes"
                                mouse-face mode-line-highlight
                                local-map ,mode-line-major-mode-keymap)
                  '("" mode-line-process)
                  (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                              'mouse-face 'mode-line-highlight
                              'local-map (make-mode-line-mouse-map
                                          'mouse-2 #'mode-line-widen))
                  `(:propertize ("" (:eval (minions--prominent-modes)))
                                mouse-face mode-line-highlight
                                help-echo "Minor mode
mouse-1: Display minor mode menu
mouse-2: Show help for minor mode
mouse-3: Toggle minor modes"
                                local-map ,mode-line-minor-mode-keymap)
                  '(:eval (and (not (member minions-mode-line-lighter '("" nil))) " "))
                  '(:eval (propertize minions-mode-line-lighter
                                      'face minions-mode-line-face
                                      'mouse-face 'mode-line-highlight
                                      'help-echo "Minions
mouse-1: Display minor modes menu"
                                      'local-map minions-mode-line-minor-modes-map))
                  '(:eval (cdr minions-mode-line-delimiters))
                  (propertize "%]" 'help-echo recursive-edit-help-echo)
                  " "))))

  (minions-mode +1)
  (my:minions-mode-line-modes)

  (defun my:minions-refresh ()
    (interactive)
    (if minions-mode
        (progn
          (minions-mode -1)
          (minions-mode +1)
          (ignore-errors (funcall major-mode))
          (my:minions-mode-line-modes)))))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-disable-other-themes t)
  (setq modus-vivendi-deuteranopia-palette-overrides
        '((bg-mode-line-active "#232635")
          (blue            "#00ffb4")
          (blue-warmer     "#00A8B0")
          (blue-cooler     "#00ff8a")
          (blue-faint      "#00ff86")
          (blue-intense    "#00ffb8")
          (bg-blue-intense    "#00ffaa")
          (bg-blue-subtle     "#00ffea")
          (bg-blue-nuanced    "#00ff8a")
          (bg-graph-blue-0    "#00ffc3")
          (bg-graph-blue-1    "#00ffbb")
          (bg-completion       "#3D4750")
          (bg-yellow-intense  "#820153")
          (border-mode-line-active    nil)
          (border-mode-line-inactive  nil)))

  (load-theme 'modus-vivendi-deuteranopia t))

(use-package zoom
  :config
  (zoom-mode +1))

;; Behavior

(use-package savehist
  :init
  (savehist-mode 1))


;; better options for switiching windows and buffers
(use-package ace-window
  :ensure t
  :init
  (setq aw-scope 'frame ; limit to single frame
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-SPC SPC" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "#FFFFFF"))

(use-package pulsar
  :custom
  (pulsar-pulse-functions '(ace-window
                            backward-page
                            consult--jump
                            recenter-top-bottom
                            move-to-window-line-top-bottom
                            reposition-window
                            bookmark-jump
                            other-window
                            delete-window
                            delete-other-windows
                            forward-page
                            goto-char
                            scroll-up-command
                            scroll-down-command
                            next-buffer
                            previous-buffer
                            windmove-right
                            windmove-left
                            windmove-up
                            windmove-down
                            windmove-swap-states-right
                            windmove-swap-states-left
                            windmove-swap-states-up
                            windmove-swap-states-down
                            tab-new
                            tab-close
                            tab-next
                            handle-switch-frame
                            org-next-visible-heading
                            org-previous-visible-heading
                            org-forward-heading-same-level
                            org-backward-heading-same-level
                            outline-backward-same-level
                            outline-forward-same-level
                            outline-next-visible-heading
                            outline-previous-visible-heading
                            outline-up-heading
                            recenter
                            switch-to-buffer
                            switch-to-buffer-other-window
                            switch-to-buffer-other-tab
                            switch-to-buffer-other-frame
                            consult-gh--files-view-action))
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode t))

(which-key-mode +1)
(require 'org)
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("C-l" . exit-minibuffer)
              ("<backtab>" . vertico-next-group)

              (:map minibuffer-local-map
                    ("M-j" . next-history-element)
                    ("M-k" . previous-history-element)
                    ("M-i" . completion-at-point)))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :config

  (set-face-attribute 'vertico-current nil :foreground "#FABFC5" :background "#3D4750")
  (setq read-extended-command-predicate #'command-completion-default-include-p)


  ;; add arrow prefix
  (defun minibuffer-format-candidate (orig cand prefix suffix index _start)
    (let ((prefix (if (= vertico--index index)
                      (concat " " prefix)
                    (concat "  " prefix))))
      (funcall orig cand prefix suffix index _start)))

  (advice-add #'vertico--format-candidate
              :around #'minibuffer-format-candidate))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :demand t
  :config

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  (defun +orderless-consult-dispatch (word _index _total)
    (cond

     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))


  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (command (styles orderless+initialism))
                                        (variable (styles orderless+initialism))
                                        (symbol (styles orderless+initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch))

  (add-to-list 'completion-styles 'orderless))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia--ellipsis "…"
        marginalia-align 'left
        marginalia-align-offset -1))

(use-package consult
  :ensure nil
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("C-s" . consult-line)
   ("C-M-l" . consult-imenu)
   ("C-M-j" . consult-buffer)
   :map minibuffer-local-map
   ("C-r" . consult-history)
   ("C-s" . previous-history-element))
  :config
  (consult-preview-at-point-mode)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "C-o")

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package  embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (global-set-key [remap describe-bindings] #'embark-bindings)


  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))


(use-package magit
  :demand t
  :commands (magit magit-status)
  :init
  (setq forge-add-default-sections t)
  (setq forge-add-default-bindings t)
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (setq magit-repository-directories '(("~/projects" . 1)))
  (setq git-commit-summary-max-length 50)
  (add-hook 'magit-mode-hook 'hl-line-mode)
  (add-to-list 'magit-status-headers-hook #'magit-insert-user-header))

(use-package forge
  :after magit
  :init
  (setq forge-add-default-sections t)
  (setq forge-add-default-bindings t))

(use-package org
  :ensure nil
  :hook
  (org-mode . org-indent-mode)
  :custom
  (org-support-shift-select 'always)
  (org-return-follows-link t)
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-image-actual-width (/ (display-pixel-width) 10))
  (org-startup-with-inline-images t)
  (org-display-inline-images t)
  (org-display-remote-inline-images 'cache)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-block-startup nil)
  (org-startup-folded 'content)
  (org-cycle-separator-lines 0)
  (org-capture-bookmark nil)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-highlight-latex-and-related '(native latex scripts entities))
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-special-ctrl-a/e t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((shell . t)
     (emacs-lisp . t)
     (lisp . t)))

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'split-window-right
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-tab-acts-natively t
        org-src-fontify-natively t))

(use-package org-modern
  :ensure t
  :custom
  (org-pretty-entities t)
  (org-modern-todo nil)
  (org-modern-table nil)
  (org-modern-hide-stars nil)
  (org-modern-star '("◉" "○" "◈" "◇" "▣" "□" "⬢" "⬡"))
  (org-modern-list '((43 . "⏵") (45 . "–") (42 . "❖")))
  (org-modern-timestamp t)
  :hook
  (org-mode . org-modern-mode))

(use-package visual-fill-column
  :ensure t
  :init
  (pcase system-type
    ('darwin
     (setq visual-fill-column-width 110
           visual-fill-column-center-text t))
    (_
     (setq visual-fill-column-width 90
           visual-fill-column-center-text t)))
  (visual-fill-column-mode 1)
  :hook (org-mode . visual-fill-column-mode))

(use-package consult-gh
  :ensure t
  :after embark-consult
  :custom
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-preview-mode nil)
  (consult-gh-preview-major-mode 'org-mode)
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-default-interactive-command #'consult-gh-transient)
  (consult-gh-group-dashboard-by :reason)
  :config
  (consult-gh-enable-default-keybindings)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list))

(use-package consult-gh-embark
  :config
  (consult-gh-embark-mode +1))

(use-package consult-gh-forge
  :custom
  (consult-gh-forge-timeout-seconds 20))

(use-package consult-gh-with-pr-review)

(use-package keycast
  :defer t
  :commands (keycast-mode-line-mode)
  :custom
  (keycast-mode-line-insert-after (or (car (member 'mode-line-format-right-align mode-line-format))
                                      (car (member 'mode-line-position mode-line-format))
                                      (car (member 'minions-mode-line-modes mode-line-format))
                                      (car (last mode-line-format))))
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-format " | %k%c%R | ")
  :config
  (set-face-attribute 'keycast-key nil :background "#00868c" :foreground "#000000" :box '(:line-width -3 :style flat-button))
  (keycast-mode-line-mode +1))

;;; init.el ends here
