(defvar bootstrap-version)
(let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	  (url-retrieve-synchronously
	  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	  'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("org"   . "https://orgmode.org/elpa/")
			   ("elpa"  . "https://elpa.gnu.org/packages/")))

(setq user-full-name "Nina DePalma"
      user-mail-address "ninadepalma@gmail.com"
	work-comp "WIN-HS95ZD3"
	home-comp "DESKTOP-HP02IG5")

(customize-set-variable 'inhibit-startup-screen t) ; no splash screen on start
(tool-bar-mode -1)   ; no tool bar with icons
(scroll-bar-mode -1) ; no scroll bars

(add-to-list 'initial-frame-alist '(left . 1)) ; initial frame on left
(add-to-list 'default-frame-alist '(fullscreen . fullheight)) ; every frame full height
(add-to-list 'default-frame-alist '(width . 90)) ; 90 columns wide

;; smoother and nicer scrolling
(setq scroll-margin 10
      scroll-step 1
      next-line-add-newlines nil
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; define how i want line/column numbers displayed in any modes of my choosing...
(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1)
  (column-number-mode 1)
  )

;; ...and set the modes of my choosing
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'org-mode-hook 'my-display-numbers-hook)


(global-hi-lock-mode 1) ; highlights text that matches regular expressions
(global-hl-line-mode 1) ; highlight current row

(set-frame-font "Hack 9" nil t)

;; hide minor modes
(use-package rich-minority
  :straight t
  :config
  (rich-minority-mode 1)
  (setf rm-blacklist ""))

(global-auto-revert-mode t)   ; update buffers automatically when underlying files are changed externally

(fset 'yes-or-no-p 'y-or-n-p) ; prompt me for 'y/n' instead of 'yes/no'
(delete-selection-mode 1)     ; delete selected text when typing

(if (fboundp #'save-place-mode)  ; remember line number
    (save-place-mode +1)
  (setq-default save-place t))

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; delete trailing whitespaces

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq default-directory "~/")
(setq command-line-default-directory "~/")

(straight-use-package 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package move-text
  :straight t
  :config
  (move-text-default-bindings))

(global-set-key (kbd "M-u") 'upcase-dwim)   ;; Alt+u upcase
(global-set-key (kbd "M-l") 'downcase-dwim) ;; Alt-l lowercase

(when (string= (system-name) work-comp)
  (load-theme 'tsdh-light)

  (set-face-attribute 'mode-line nil :background "SlateGray1")
  (set-face-attribute 'mode-line-inactive nil :background "grey93")

  (custom-set-faces
   '(org-block-begin-line
      ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF" :extend t))))
       '(org-block
         ((t (:background "#EFF0F1" :extend t))))
       '(org-block-end-line
         ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF" :extend t))))
       )
  )

(when (string= (system-name) home-comp)
  (straight-use-package 'color-theme-sanityinc-tomorrow)
  (load-theme 'sanityinc-tomorrow-night t))

;; org block preferences tbd...

;;;; below are my preferences for 'modus operandi tinted'
;; (use-package modus-themes
;;   :straight t
;;   :config

;; (setq modus-themes-org-blocks 'gray-background) ; helps code blocks stand out

;; (setq modus-themes-common-palette-overrides
;;       '((bg-mode-line-active bg-blue-intense) ; colorful mode line
;;         (fg-mode-line-active fg-main)
;;         (border-mode-line-active blue-intense)))

;; (load-theme 'modus-operandi t)

;; (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package neotree
  :straight t
  :config
  (setq neo-window-width 27
        neo-autorefresh t
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-window-fixed-size nil
        neo-vc-integration nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-mode-line-type 'none
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-hidden-regexp-list '("venv" "\\.pyc$" "~$" "\\.git" "__pycache__" ".DS_Store"))
  (global-set-key (kbd "C-B") 'neotree-toggle))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; Show icons in dired mode.
(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  )

;; integrate projectile with counsel
(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode 1))

;; use ivy in projectile
(setq projectile-completion-system 'ivy)

(use-package ivy
  :diminish
  :straight t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (setq ivy-re-builders-alist
  	'((swiper . ivy--regex-plus)
  	  (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for swiper

  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; add information to ivy buffers
(use-package ivy-rich
  :straight t
  :after all-the-icons-ivy-rich
  :init (ivy-rich-mode 1)
  :config
					;(ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev ;; abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
  	ivy-virtual-abbreviate 'abbrev
  	)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; add icons to info-rich ivy buffers
(use-package all-the-icons-ivy-rich
  :straight t
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode 1))

(use-package swiper
  :straight t
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper))

(use-package counsel
  :straight t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(straight-use-package 'smex)  ;; show recent commands first with counsel-M-x
(straight-use-package 'flx)   ;; enable fuzzy matching
(straight-use-package 'avy)   ;; enable avy for quick navigation

(use-package helm
  :straight t
  :diminish)

(global-set-key (kbd "C-c h") 'helm-mini)

(helm-mode 1)

(use-package smartparens
  :straight t
  :config
  ;; Remove ' and  from pairing
  ;;(sp-pair "'" nil :actions :rem)
  ;;(sp-pair "`" "'" :actions :rem)
  (smartparens-global-mode 1))

(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode t))

(when (string= (system-name) work-comp)
  (setq python-shell-interpreter "C:\\Users\\ndepalma\\AppData\\Local\\Programs\\Python\\Python310\\python.exe"))

(when (string= (system-name) home-comp)
  (setq python-shell-interpreter "C:\\Users\\ninad\\AppData\\Local\\Programs\\Python\\Python310\\python.exe"))

(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode t))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode) ; permanently enable syntax checking with Flycheck

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-python-pylint-executable "C:\\Users\\ndepalma\\AppData\\Local\\Programs\\Python\\Python310\\Scripts\\pylint.exe")
	    (setq flycheck-pylintrc (substitute-in-file-name "C:\\Users\\ndepalma\\.pylintrc"))))

(straight-use-package 'yaml-mode)

(add-hook 'yaml-mode-hook
  #'(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package magit
  :straight t
  :bind
  (("C-c g"     . 'magit-status)
   ("C-c C-p"   . 'magit-push)))

(use-package org
  :straight t)

(setq org-ellipsis " ↴") ; change fold/unfold symbol

(use-package org-bullets ; nicer org bullets
  :straight t)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-hide-leading-stars t)

;; global todo statuses
(setq org-todo-keywords
     '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "REMOVED")
	 (sequence "DEV" "TEST" "PROD" "DONE"))) ; I use org for work

(setq org-log-done t) ; log time when task marked done

;; global keybindings so I can use these anywhere in emacs
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; capture templates
(setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/work/dash.org" "========================= Unsorted TODOs =========================")
	   "* TODO %?")
	("l" "Todo with link" entry (file+headline "~/org/work/dash.org" "========================= Unsorted TODOs =========================")
	 "* TODO %?\n  %i\n %a\n")
	  ("j" "Journal" entry (file+datetree "~/org/life/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))

;; automatically tangle files when saved; keeps init.el up to date
;; ty https://systemcrafters.net/emacs-from-scratch/configure-everything-with-org-babel/
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/nina.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

(straight-use-package 'markdown-mode)
(straight-use-package 'olivetti)
(add-hook 'markdown-mode-hook (lambda ()
				(buffer-face-set '(:family "iA Writer Duo S"))
				(setq line-spacing 0.5)
				(olivetti-mode t)
				(setq olivetti-style 'fancy)
				(olivetti-set-width 75)
				))

(straight-use-package 'go-translate)

(setq gts-translate-list '(("en" "de") ("en" "hu") ("en" "ru")))

(straight-use-package 'nov)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("c:/Users/ndepalma/org/work/dash.org"))
 '(package-selected-packages '(org-books go-translate elpy use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
