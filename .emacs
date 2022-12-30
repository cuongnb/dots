(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


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

;;; TOOLS

(use-package magit
  :ensure t :defer t)
  
;;; DISPLAY
;; highlight cursor
(use-package beacon
  :ensure t :defer t
  :hook (after-init . beacon-mode))

(use-package rainbow-delimiters
  :ensure t :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(rainbow-delimiters magit markdown-mode dracula-theme beacon orderless project marginalia vertico use-package docbook)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
