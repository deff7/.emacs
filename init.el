(toggle-frame-fullscreen)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-auto-revert-mode t)           ; Auto-update buffer if file has changed on disk
(fset 'yes-or-no-p 'y-or-n-p)         ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                   ; Show closing parens by default
(setq enable-recursive-minibuffers t) ; So I can use M-x in other minibuffers for example
 
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Buffers
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-c f") 'next-buffer)

;; Eshell
(global-set-key (kbd "C-x e") 'eshell)

;; Dired
(setq dired-dwim-target t)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/org/next-step.org" "~/org/inbox.org" "~/org/todo.org")))
 '(package-selected-packages
   (quote
    (yaml-mode haskell-mode org-roam wgrep projectile magit company which-key counsel lsp-mode go-mode use-package linum-relative ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r")
  (setq wgrep-change-readonly-file t))

(use-package magit
  :ensure t)

;; Languages

(use-package yaml-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports"))

(use-package haskell-mode
  :ensure t)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :ensure t
  :hook (go-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.8)
  (which-key-mode))


;; Ivy Swiper Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("M-y" . counsel-yank-pop)
	 ("<f1> f" . counsel-describe-function) 
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)      
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)      
	 ("<f2> j" . counsel-set-variable)))
	
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  :bind (("C-x b" . ivy-switch-buffer)	      
	 ("C-c v" . ivy-push-view)	      
	 ("C-c V" . ivy-pop-view)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)
	 ("M-s ." . swiper-isearch-thing-at-point)))

(use-package projectile
  :pin melpa-stable
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (require 'ivy)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/Repos/" "~/Repos/haskell/" "~/org/"))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; Autocomplete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

(global-set-key (kbd "M-i") 'imenu)

(use-package linum-relative
  :ensure t
  :init
  (global-display-line-numbers-mode)
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-global-mode))

(use-package org
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (setq org-directory "~/org/")
  (let ((inbox-file (concat org-directory "inbox.org"))
	(til-file (concat org-directory "til.org")))
    (setq org-default-notes-file inbox-file)
    (setq org-capture-templates
	  `(("t" "TODO" entry (file+headline ,inbox-file "Tasks")
	     "* TODO %?\n  %i\n  %a")
	    ("l" "Today I learned" entry (file ,til-file)
	     "* %?\n  %i")))))

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org-roam/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
