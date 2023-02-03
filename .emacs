(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; BOOTSTRAP `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; COMPLETION SYSTEM: vertico, orderless, marginalia, consult, embark

(use-package vertico
  :ensure t
  :config
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "»" 'face 'vertico-current) " ") cand)))  
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t :defer t
  :custom
  (orderless-matching-styles
   '(orderless-regexp orderless-literal orderless-initialism))
  (completion-styles '(orderless))
  (completion-category-defaults nil))


(use-package consult
  :ensure t :defer t
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g e" . consult-error)
  ("M-s w" . consult-line-at-point)
  ("M-s g" . consult-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s F" . consult-find)
  ("M-s u" . consult-focus-lines)
  ("C-x / k" . consult-keep-lines)
  ("M-X" . consult-mode-command)
  ("C-x B" . consult-buffer)
  (:map minibuffer-local-map ("M-r" . consult-history))
  :init
  ;; @FIXME: Disable `consult-completion-in-region' buggy in (e)shell-mode (tramp), minibuffer (compile command)
  ;; (setq completion-in-region-function
  ;;       (lambda (&rest args)
  ;;         (apply (if (and (fboundp 'vertico-mode) vertico-mode)
  ;;                    #'consult-completion-in-region
  ;;                  #'completion--in-region) args)))
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  :config
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format
        consult-preview-key (kbd "C-l"))
  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history)
  (defun consult-thing-at-point ()
    "Return a string that corresponds to the current thing at point."
    (substring-no-properties
     (cond
      ((use-region-p)
       (let* ((beg (region-beginning))
              (end (region-end))
              (eol (save-excursion (goto-char beg) (line-end-position))))
         (buffer-substring-no-properties beg (min end eol))))
      ((thing-at-point 'url))
      ((let ((s (thing-at-point 'symbol)))
         (and (stringp s)
              (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
                  (match-string 1 s)
                s))))
      ((looking-at "(+\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
       (match-string-no-properties 1))
      (t ""))))
  (defun consult-line-at-point()
    (interactive)
    (let ((thing (consult-thing-at-point))
          (consult-preview-key 'any))
      (when (use-region-p)
        (deactivate-mark))
      (consult-line (regexp-quote thing)))))

(use-package embark
  :ensure t :defer t
  :bind ("C-c /" . embark-act)
  (:map minibuffer-local-map ("M-o" . embark-act))
  (:map embark-general-map ("/" . embark-chroot))
  (:map embark-region-map ("M-&" . async-shell-from-region))
  (:map embark-file-map
        ("s" . embark-run-shell)
        ("t" . embark-run-term)
        ("T" . embark-run-vterm)
        ("v" .  magit-status-setup-buffer)
        ("+" . embark-make-directory)
        ("x" . consult-file-externally)))

(use-package embark-consult
  :ensure t :defer t
  :init
  (with-eval-after-load 'consult
    (with-eval-after-load 'embark
      (require 'embark-consult))))

(use-package multiple-cursors
  :ensure t :defer t
  :bind
  ("C-c c a" . mc/mark-all-like-this)
  ("C-c c n" . mc/mark-next-like-this)
  ("C-c c p" . mc/mark-previous-like-this)
  ("C-c c l" . mc/edit-lines)
  ("C-c c r" . mc/mark-all-in-region))

;; (use-package shell-command+
;;   :ensure t :defer t
;;   :init (global-set-key (kbd "M-!") #'shell-command+))

;;
(use-package xclip ;; -- don't use xsel
  :ensure t :defer t
  :init
  (add-hook 'tty-setup-hook
            (lambda()(require 'xclip nil t)
              (ignore-errors (xclip-mode)))))

;;; VERSION CONTROL: git-gutter, magit, git-link
(use-package magit
  :ensure t :defer t
  :bind ("C-x g" . magit-status))
  
;;; DISPLAY
;; highlight cursor
(use-package beacon
  :ensure t :defer t
  :hook (after-init . beacon-mode))

(use-package dracula-theme
  :ensure t
  :init (load-theme 'dracula t))

(defun doom-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))


;;PROGRAMING
(use-package json-mode
  :ensure t)

(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	      ("C-c C-f" . json-mode-beautify)))

(defun develop-docker()
  "Docker tools."
  (interactive)
  (package-installs 'dockerfile-mode 'docker 'docker-tramp 'docker-compose-mode))
(use-package docker :defer t
  :config (setq docker-run-async-with-buffer-function #'docker-run-async-with-buffer-shell))


;;; COMPLETION CODE: corfu, yasnippet, eglot, dumb-jump
(use-package corfu
  :ensure t :defer t
  :init (global-corfu-mode)
  :bind
  (:map corfu-map
        ("M-m" . corfu-move-to-minibuffer)
        ("TAB" . corfu-complete-common-or-next) ;; Use TAB for cycling, default is `corfu-complete'.
        ([tab] . corfu-complete-common-or-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :config
  (unless (display-graphic-p)
    (use-package corfu-terminal
      :ensure t :defer t
      :init (add-hook 'corfu-mode-hook #'corfu-terminal-mode)))
  (setq completion-cycle-threshold 3
        corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 2
        corfu-preselect-first nil
        corfu-history-mode t)
  (defvar-local corfu-common-old nil)
  (defun corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate (@minad/corfu#170)."
    (interactive)
    (if (= corfu--total 1)
        (if (not (thing-at-point 'filename))
            (progn
              (corfu--goto 1)
              (corfu-insert))))
    (let* ((input (car corfu--input))
           (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
           (pt (length str))
           (common (try-completion str corfu--candidates)))
      (if (and (> pt 0)
               (stringp common)
               (not (string= str common)))
          (insert (substring common pt))
        (if (equal common corfu-common-old)
            (corfu-next)))
      (setq-local corfu-common-old common)))
  (put 'corfu-complete-common-or-next 'completion-predicate #'ignore)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (defun corfu-move-to-minibuffer ()
    "Move completion to minibuffer instead of corfu."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data))))


;; ;; Buildsystem
;; ;; docker
;; (use-package docker :defer t)
;; (use-package dockerfile-mode :defer t)

;; insert,wrap,unwrap,rewrap
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t))
  :bind (:map smartparens-mode-map
	      ("C-M-f" . 'sp-forward-sexp)
              ("C-M-b" . 'sp-backward-sexp)))

;; highlight parens
(use-package rainbow-delimiters
  :ensure t :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package dumb-jump
;;   :bind (("C-M-g" . dumb-jump-go)
;; 	 ("C-M-p" . dumb-jump-back)
;;          ("C-M-q" . dumb-jump-quick-look)))

;; AUTOCOMPLETE
;; Eglot is a client to Language Server Protocol servers.
(use-package eglot
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server" "--stdio"))))

;;; TOOLS: move-text avy
(use-package move-text
  :ensure t :defer t
  :bind
  ("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))

(use-package avy
  :ensure t :defer t
  :config
  (setq avy-all-windows nil
        avy-background t)
  :bind
  ("M-g a" . avy-goto-char)
  ("M-g l" . avy-goto-line))

(use-package vundo
        :ensure t :defer t
        :init (global-set-key (kbd "C-x u") #'vundo)
        :config (define-key vundo-mode-map (kbd "q") #'vundo-confirm))                        

;;; org, alert
(use-package org-alert
  :ensure t)
(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)


;;; CUSTOMIZE
(defun add-to-hooks (func &rest hooks)
  "Add FUNC to mutil HOOKS."
  (dolist (hook hooks) (add-hook hook func)))
;; enable whitespace-mode
(add-to-hooks 'whitespace-mode
              'prog-mode-hook 'org-mode-hook
              'markdown-mode-hook 'yaml-mode-hook
              'dockerfile-mode-hook)

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-x / o") 'org-agenda)
(global-set-key (kbd "C-x 2") 'split-window-vertically-last-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-last-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(corfu-terminal corfu shell-command+ multiple-cursors restclient vundo rainbow-delimiters magit markdown-mode dracula-theme beacon orderless project marginalia vertico use-package docbook)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Unbind unneeded keys

(defun split-window-vertically-last-buffer (prefix)
  "Slip window vertically.
- PREIFIX default(1) is switch to last buffer"
  (interactive "p")
  (split-window-vertically) (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(defun split-window-horizontally-last-buffer (prefix)
  "Split window horizontally
- PREFIX default(1) is switch to last buffer"
  (interactive "p")
  (split-window-horizontally) (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(defun today(&optional prefix)
  "Go to daily."
  (interactive "P")
  (let ((file-format "~/worklogs/%s/%s.org")
	(today (if prefix (org-read-date)
		 (format-time-string "%Y-%m-%d" (current-time)))))
    (find-file (format file-format today today))))
    
    
(mapc (lambda (x) (add-to-list 'org-agenda-files x))
      (append
       (directory-files-recursively "~/worklogs" ".org$")))
