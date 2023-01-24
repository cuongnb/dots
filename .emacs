;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)

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

;; Buildsystem
;; docker
(use-package docker :defer t)
(use-package dockerfile-mode :defer t)

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

(global-set-key (kbd "M-o") 'mode-line-other-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(vundo rainbow-delimiters magit markdown-mode dracula-theme beacon orderless project marginalia vertico use-package docbook)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Unbind unneeded keys
