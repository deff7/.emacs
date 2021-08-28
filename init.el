(toggle-frame-fullscreen)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)

(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; (setenv "PATH"
;; 	(concat "/usr/local/bin" ":"
;; 	        "/Users/deff/go/bin" ":"
;; 		(getenv "PATH")))
;; (setq exec-path (append exec-path '("/usr/local/bin" "/Users/deff/go/bin")))

;; Cmd -> Meta
(setq mac-command-modifier 'meta)

;; Auto-update buffers if file has changed on disk
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Do it with dired
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Autosave by xah
(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t))
(if (version< emacs-version "27")
    (add-hook 'focus-out-hook 'xah-save-all-unsaved)
  (setq after-focus-change-function 'xah-save-all-unsaved))

;; Don't create lockfiles.
(setq create-lockfiles nil)

(fset 'yes-or-no-p 'y-or-n-p)         ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                   ; Show closing parens by default
(electric-pair-mode 1)
(setq enable-recursive-minibuffers t) ; So I can use M-x in other minibuffers for example

;; Increase minibuffer font size
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 1.5))))
 
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
 '(org-agenda-files '("~/org/next-step.org" "~/org/inbox.org" "~/org/todo.org"))
 '(package-selected-packages
   '(org-bullets smartparens avy cider clojure-mode flycheck-pos-tip flycheck elixir-mode exec-path-from-shell exec-path-from-shel restclient org-drill org-fc lsp-haskell haskell-lsp sly yasnippet-snippets yasnippet yaml-mode haskell-mode org-roam wgrep projectile magit company which-key counsel lsp-mode go-mode use-package linum-relative ##)))
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

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package wgrep
  :ensure t
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

(use-package elixir-mode
  :ensure t
  :hook (elixir-mode . lsp-deferred))

(use-package lsp-haskell
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  ;; TODO: refactor
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :ensure t
  :hook (go-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
   :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.8)
  (which-key-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :init (flycheck-pos-tip-mode))

;; Ivy Swiper Counsel
(use-package counsel
  :ensure t
  :config
  (setq counsel-rg-base-command `("rg"
				  "--max-columns" "240"
				  "--hidden"
				  "--with-filename"
				  "--no-heading"
				  "--line-number"
				  "--color" "never"
				  "%s"))
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("M-s r" . counsel-rg)
	 ("M-y" . counsel-yank-pop)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)      
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("<f2> j" . counsel-set-variable)
	 ("M-i" . counsel-imenu)))
	
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
  (setq projectile-project-search-path '("~/dev/work"
					 "~/dev/keyboard"
					 "~/dev/haskell"
					 "~/dev/ml"
					 "~/dev/elixir"
                                         "~/dev/clojure"))
  (projectile-add-known-project "~/org")
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; Autocomplete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  ;; Make navigation in the autocomplete dialog more intuitive
  :bind (:map company-active-map
              (("C-n" . company-select-next)
               ("C-p" . company-select-previous))))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; Navigation

(use-package avy
  :ensure t
  :bind (("C-;" . 'avy-goto-char-timer)))

(use-package org
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-directory "~/org/")
  (let ((inbox-file (concat org-directory "inbox.org"))
	(til-file (concat org-directory "til.org")))
    (setq org-default-notes-file inbox-file)
    (setq org-capture-templates
	  `(("t" "TODO" entry (file+headline ,inbox-file "Tasks")
	     "* TODO %?\n  %i")
	    ("T" "TODO annotated" entry (file+headline ,inbox-file "Tasks")
	     "* TODO %?\n  %i\n  %a")
	    ("l" "Today I learned" entry (file ,til-file)
	     "* %?\n  %i")))
    (setq org-refile-targets '((nil :maxlevel . 1)
			       (org-agenda-files :maxlevel . 1)
			       ("someday.org" :maxlevel . 1)))
    (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
    ))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

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
              (("C-c n I" . org-roam-insert-immediate))
	      (("C-c n t" . org-roam-tag-add))
	      (("C-c n r" . org-roam-random-note))))

(use-package org-drill
  :ensure t)

(put 'scroll-left 'disabled nil)

(use-package restclient
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  :init
  (smartparens-global-mode)
  :bind (:map smartparens-mode-map
              (("C-M-f" . sp-forward-sexp)
               ("C-M-b" . sp-backward-sexp)
               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . sp-previous-sexp)
               ("C-<down>" . sp-down-sexp)
               ("C-<up>"   . sp-up-sexp)
               ("C-<right>" . sp-forward-slurp-sexp)
               ("C-<left>" . sp-backward-slurp-sexp)
               ("M-<right>" . sp-forward-barf-sexp)
               ("M-<left>" . sp-backward-barf-sexp)

               ("C-M-w" . sp-copy-sexp)
               ("C-M-k" . sp-kill-sexp)

               ;; Wrap / unwrap
               ("M-[" . sp-backward-unwrap-sexp)
               ("M-]" . sp-unwrap-sexp)

               ("C-c (" . sp-wrap-round)
               ("C-c [" . sp-wrap-square)
               ("C-c {" . sp-wrap-curly))))

