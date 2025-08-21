(eval-when-compile (require 'use-package))

;; (let ((default-directory "/Users/edward/.emacs.d/"))
;;   (normal-top-level-add-subdirs-to-load-path))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package window-numbering
  :demand t
  :straight (window-numbering :type git :host github :repo "nschum/window-numbering.el")
  :config (window-numbering-mode t))

(use-package line-comment-banner
  :straight (line-comment-banner :type git :host github :repo "emacsattic/line-comment-banner")
  :config
  (global-set-key (kbd "C-;") 'line-comment-banner))


(setenv "PATH"
	(concat "/usr/local/bin:"
            (getenv "PATH")))
(setenv "PATH"
	(concat "/Library/TeX/texbin:"
                (getenv "PATH")))


(use-package cython-mode
  :straight t)

(use-package python
  :straight t
  :init
  (add-hook 'python-mode-hook #'electric-pair-mode))

(use-package python-x
  :straight t)

(python-x-setup)
(autoload 'python-x "python-x" "Python-x-mode" t)

(use-package counsel
  :straight t
  :bind (
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x b" . ivy-switch-buffer)
         ("M-r" . counsel-ag)
         ("C-x C-d" . counsel-dired)
         ("C-x d" . counsel-dired)
         ("C-s" . swiper-isearch)
         )
  :diminish
  :config
  (global-set-key [remap org-set-tags-command] #'counsel-org-tag))


(use-package ivy
  :straight t
  :demand t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist '(
                                (t . ivy--regex-ignore-order)
                                ))
  (setq ivy-height 10)
  (setq counsel-find-file-at-point t)
  (setq ivy-count-format "(%d/%d) "))


(use-package swiper
  :straight t
  :bind(("M-C-s" . swiper)))

(use-package ivy-hydra
  :straight t)

(use-package virtualenvwrapper
  :straight t
  :config
  (venv-initialize-interactive-shells)
  (setq venv-location '("/Users/edward/venvs/py3/"
                        "/Users/edward/venvs/py2/")))



; emacs
(setq inhibit-startup-message   t)   ; Don't want any startup message
(blink-cursor-mode 0)
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving
(setq create-lockfiles          nil) ; Don't want any lockfiles
(menu-bar-mode 0)		     ; get rid of menu-bar
(tool-bar-mode 0)	       ; get rid of tool-bar
(show-paren-mode 1)	       ; enable highlighting of matched parens
(column-number-mode 1)	       ; column numbers
(setq-default indent-tabs-mode nil)	; use only spaces and no tabs
(setq delete-by-moving-to-trash t)
;; (set-frame-font "Inconsolata 16" nil t)
;; (set-frame-font "Monaco 15" nil t)
;; (set-frame-font "SF Mono 15" nil t)
(set-frame-font "Fira Mono 15" nil t)

(global-set-key (kbd "C-c q") 'auto-fill-mode)
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     ;; (paredit-mode t)
	     (outline-minor-mode)
	     (make-local-variable 'outline-regexp)
	     (setq outline-regexp "[;\f]+")))

;; https://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            ;; This would override `fill-column' if it's an integer.
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)



;; uniquify
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style
        'post-forward uniquify-separator ":"))

; UTF-8 enconding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; spelling
;; (setq ispell-program-name (executable-find "hunspell"))
;; (require 'ispell)
;; (setq ispell-program-name "/usr/local/bin/hunspell")
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))



; Insert Date
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
 two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "[%Y.%m.%d]")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

; text mode
(add-hook 'text-mode-hook
	  '(lambda ()
	     (setq outline-minor-mode t)
	     (make-local-variable 'outline-regexp)
         (electric-pair-mode t)))


;; ;; latex hook
;; (load "auctex.el" nil t t)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (setq outline-minor-mode t)
	    (make-local-variable 'outline-regexp)
            (electric-pair-mode t)
	    ;; (TeX-fold-mode 1)
	    (set (make-variable-buffer-local 'TeX-electric-math)
		 (cons "$" "$"))
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
(setq LaTeX-math-mode t)

; c++
(add-hook 'c++-mode-hook
          '(lambda ()
             (hs-minor-mode)
             (electric-pair-mode t)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'c-mode-hook
          '(lambda ()
             (hs-minor-mode)
             (electric-pair-mode t)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;; (require 'cmake-mode)

; color-theme
(load-theme 'wombat t)

; ess
;(add-to-list 'load-path "/Users/ez/.emacs.d/ESS/lisp/")


(use-package julia-mode
  :straight t
  :config
  (setq inferior-julia-program-name "/Users/edward/.juliaup/bin/julia"))
;; (setq inferior-R-program "/usr/local/bin/R")



;; (setenv "LD_LIBRARY_PATH" "/usr/local/Cellar/arrayfire/3.8.0/lib")
;; (setq inferior-julia-program "/Applications/Julia-1.6.app/Contents/Resources/julia/bin/julia")
(add-hook 'ess-julia-mode-hook
	  '(lambda ()
	     (outline-minor-mode)
             (hs-minor-mode)
	     (make-local-variable 'outline-regexp)
	     (setq outline-regexp "[#\f]+")
             (electric-pair-mode t)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


(use-package ess
  :straight (ess :type git :files ("lisp/*.el" "doc/ess.texi" ("etc" "etc/*") ("obsolete" "lisp/obsolete/*") (:exclude "etc/other") "ess-pkg.el") :host github :repo "emacs-ess/ESS")
  :init
  (add-hook 'ess-mode-hook #'electric-pair-mode)
  :config
  (setq inferior-R-args "--no-restore-history --no-save ")
  (setq ess-eval-visibly-p nil) ; otherwise C-c C-r (eval region) takes forever
  (setq ess-ask-for-ess-directory nil) ; otherwise prompted each time you start R
  (setq ess-r-package-auto-set-evaluation-env nil)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))


(use-package org :straight (:type built-in))

(use-package nodejs-repl
  :straight t
  :init
  (add-hook 'js-mode-hook
            (lambda ()
              (define-key js-mode-map (kbd "C-c C-n") 'nodejs-repl-send-line)
              (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
              (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
              (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

(use-package json-mode
  :straight t
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))


(use-package yaml-mode
  :straight t
  :init
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (electric-pair-mode t)
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)
               (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


(use-package poly-R
  :straight t)

(use-package quarto-mode
  :straight t)

(use-package poly-markdown
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.[jJ]md" . poly-markdown-mode))
  (add-to-list 'polymode-mode-name-override-alist '(julia . ess-julia))
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-quarto-mode)))

;; (require 'js-mode)
;; (add-hook 'js-mode-hook #'electric-pair-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; (require 'typescript-mode)
;; (add-hook 'typescript-mode-hook
;;           '(lambda ()
;;              (electric-pair-mode t)))

;; electric pair mode -- ensure single quote
(defvar web-electric-pairs-single-quote '((?\' . ?\') ) "Electric pairs for web-mode, single quote.")
(defvar web-electric-pairs-angle-bracket '((?\< . ?\>) ) "Electric pairs for web-mode, angle bracket.")
(defun web-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs-single-quote))
  (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs-angle-bracket)))


(use-package web-mode
  :straight t
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; short circuit js mode and just do everything in jsx-mode
              (electric-pair-mode t)
              (web-add-electric-pairs)
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type))))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte?\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-closing t))




; compile
(global-set-key [f9] 'compile)

(defun compile-in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given
directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'compile)))
(global-set-key [f8] 'compile-in-directory)

;;; make
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Nothing is fucked here, Dude.")
         (run-with-timer 1 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t
         (message "Compilation exited abnormally: %s" string))))

(setq compilation-finish-functions 'compile-autoclose)

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  (yas-reload-all))


;; c
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))

(use-package stan-mode
  :straight t
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))
