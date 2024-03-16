(let ((default-directory "/Users/edward/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; package.el
;; (require 'package)
;; (package-initialize)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ;("marmalade" . "https://marmalade-repo.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
;; (package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (unless (package-installed-p 'python-x)
;;   (package-refresh-contents)
;;   (package-install 'python-x))
;; (unless (package-installed-p 'cython-mode)
;;   (package-refresh-contents)
;;   (package-install 'cython-mode))

;; ;; needs dash and s; install with package-install
;; (unless (package-installed-p 'virtualenvwrapper)
;;   (package-install 'virtualenvwrapper))
;; (unless (package-installed-p 'julia-mode)
;; (unless (package-installed-p 'web-mode)
;;   (package-install 'web-mode))
;;   (package-install 'julia-mode))
;; (unless (package-installed-p 'ess)
;;   (package-install 'ess))
;; (unless (package-installed-p 'poly-R)
;;   (package-install 'poly-R))
;; (mapc
;;  (lambda (p)
;;    (unless (package-installed-p p)
;;      (package-install p)))
;;  '(stan-mode stan-snippets))
;; (unless (package-installed-p 'json-mode)
;;   (package-install 'json-mode))


;; ido
;; (require 'ido)
;; (ido-mode t)

;; window-numbering
(require 'window-numbering)
(window-numbering-mode t)

;;;;;;;;;;;;;;;;;;;;;;;; line-comment-banner ;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'line-comment-banner "line-comment-banner" nil t)
(global-set-key (kbd "C-;") 'line-comment-banner)

; paredit
;; (require 'paredit)
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)

(setenv "PATH"
	(concat "/usr/local/bin:"
            (getenv "PATH")))
(setenv "PATH"
	(concat "/Library/TeX/texbin:"
                (getenv "PATH")))

;; ; clojure
;; (unless (package-installed-p 'cider)
;;   (package-install 'cider))
;; (require 'cider)
;; (setq cider-repl-use-clojure-font-lock t)
;; (setq nrepl-hide-special-buffers t)
;; (setq cider-popup-stacktraces nil)
;; (add-hook 'cider-mode-hook
;; 	  '(lambda ()
;;              (cider-turn-on-eldoc-mode)
;; 	     (outline-minor-mode)
;;              (paredit-mode t)
;; 	     (make-local-variable 'outline-regexp)
;; 	     (setq outline-regexp "[;\f]+")))

;; (add-hook 'clojure-mode-hook 'paredit-mode)
;; (add-hook 'nrepl-connected-hook 'paredit-mode)


; python
;; in case want ipython interpreter
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt"
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;  "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;  "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; (defun python-shell-send-region-or-line ()
;;   "Call `python-shell-send-region' with selected region or current line (if none selected)."
;;   (interactive)
;;   (if (and transient-mark-mode mark-active)
;;       (python-shell-send-region (point) (mark))
;;     (python-shell-send-region (point-at-bol) (point-at-eol)))
;;   (next-line))

;; (autoload 'folding-mode          "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)


(require 'python)
(require 'cython-mode)



(python-x-setup)
(autoload 'python-x "python-x" "Python-x-mode" t)
(add-hook 'python-mode-hook
	  '(lambda ()
	     (make-local-variable 'outline-regexp)
	     (python-x-setup)
	     (setq outline-regexp "[#\f]+")
             (electric-pair-mode t)
             (local-set-key "\C-c\C-z" 'run-python)
	     (define-key python-mode-map (kbd "C-c C-p") 'python-shell-send-paragraph-and-step)
	     (define-key python-mode-map (kbd "C-c M-p") 'python-shell-send-paragraph)
             ;; (local-set-key "\C-c\C-p" 'python-shell-send-paragraph-and-step)
             ;; (local-set-key "\C-c\M-p" 'python-shell-send-paragraph)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'inferior-python-mode-hook
          '(lambda ()
             (electric-pair-mode t)))

;; virtualenvwrapper
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location '("/Users/edward/venvs/py3/"
                      "/Users/edward/venvs/py2/"))

; emacs
(setq inhibit-startup-message   t)   ; Don't want any startup message
(require 'icomplete)
(blink-cursor-mode 0)
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving
(menu-bar-mode 1)		     ; get rid of menu-bar
(tool-bar-mode 0)	       ; get rid of tool-bar
(show-paren-mode 1)	       ; enable highlighting of matched parens
(column-number-mode 1)	       ; column numbers
(setq-default indent-tabs-mode nil)	; use only spaces and no tabs
(setq delete-by-moving-to-trash t)

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
(require 'uniquify)
(setq
 uniquify-buffer-name-style
 'post-forward uniquify-separator ":")

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

; narrowing
;; (put 'narrow-to-region 'disabled nil)

; WindMove
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down)

; text mode
(add-hook 'text-mode-hook
	  '(lambda ()
	     (setq outline-minor-mode t)
	     (make-local-variable 'outline-regexp)
         (electric-pair-mode t)))


; latex
;; RefTeX work properly
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; (setq-default TeX-master nil)

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

;; ;; pdf mode
;; (setq TeX-PDF-mode t)
;; (setq TeX-view-program-list '(("Preview" "open %o")))
;; (setq TeX-view-program-selection '((output-pdf "Preview")))

; outline
;; Outline-minor-mode key map
;; (define-prefix-command 'cm-map nil "outline-")
;; ;;; HIDE
;; (define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
;; (define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
;; (define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
;; (define-key cm-map "a" 'outline-show-all)  ; Show (expand) everything
;; (define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
;; (define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
;; ;; (define-key cm-map "o" 'hide-other)        ; Hide other branches
;; ;; (define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
;; ;; (define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
;; ;; (define-key cm-map "e" 'show-entry)        ; Show this heading's body
;; ;; (define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
;; (define-key cm-map "u" 'outline-up-heading)                ; Up
;; (define-key cm-map "n" 'outline-next-visible-heading)      ; Next
;; (define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
;; ;; (define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
;; ;; (define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
;; (global-set-key "\M-p" cm-map)

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

; hs
;; HideShow new key binding
;; (global-set-key (kbd "C-c l") 'hs-hide-level)
;; (global-set-key (kbd "C-c h") 'hs-hide-all)
;; (global-set-key (kbd "C-c c") 'hs-toggle-hiding)


; java
;; (add-hook 'java-mode-hook
;;           '(lambda ()
;;              (hs-minor-mode)
;;              (electric-pair-mode t)))


; color-theme
(load-theme 'wombat t)

; ess
;(add-to-list 'load-path "/Users/ez/.emacs.d/ESS/lisp/")


(require 'julia-mode)
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


(load "ess-autoloads")
(require 'ess-site)
(setq inferior-R-args "--no-restore-history --no-save ")
(add-hook 'ess-mode-hook
	  '(lambda ()
	     (outline-minor-mode)
             (hs-minor-mode)
	     (make-local-variable 'outline-regexp)
	     (setq outline-regexp "[#\f]+")
             (electric-pair-mode t)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)
             (setq ess-r-package-auto-set-evaluation-env nil)))
(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise prompted each time you start R


;; (add-hook 'julia-mode-hook
;; 	  '(lambda ()
;; 	     (outline-minor-mode)
;;              (hs-minor-mode)
;; 	     (make-local-variable 'outline-regexp)
;; 	     (setq outline-regexp "[#\f]+")
;;              (electric-pair-mode t)
;;              (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(require 'org)

; sort words
;; (defun sort-words (reverse beg end)
;;   "Sort words in region alphabetically, in REVERSE if negative.
;;     Prefixed with negative \\[universal-argument], sorts in reverse.

;;     The variable `sort-fold-case' determines whether alphabetic case
;;     affects the sort order.

;;     See `sort-regexp-fields'."
;;   (interactive "*P\nr")
;;   (sort-regexp-fields reverse "\\w+" "\\&" beg end))

; node.js jade
;; requires M-x package-install RET js2-mode RET
;; (require 'sws-mode)
;; (require 'jade-mode)
;; (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
;; (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
;; (add-hook 'jade-mode-hook
;;           '(lambda ()
;;              (electric-pair-mode t)))
;; (add-hook 'sws-mode-hook
;;           '(lambda ()
;;              (electric-pair-mode t)))

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
;; (setq js2-indent-switch-body t)

;; (require 'nodejs-repl)
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (define-key js-mode-map (kbd "C-c C-n") 'nodejs-repl-send-last-sexp)
;;             (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
;;             (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
;;             (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

;json

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (electric-pair-mode t)
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))




;;; polymode

(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(add-to-list 'auto-mode-alist '("\\.[jJ]md" . poly-markdown-mode))
(add-to-list 'polymode-mode-name-override-alist '(julia . ess-julia))

;; (folding-add-to-marks-list 'markdown-mode "#{{{" "#}}}" nil t)




;;; javascript
(add-hook 'js-mode-hook
          '(lambda ()
             (electric-pair-mode t)))
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

;; web-mode
(require 'web-mode)

(setq web-mode-enable-auto-closing t)
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


;;; ya snippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(yas-reload-all)


;; browse kill ring
(require 'browse-kill-ring)
