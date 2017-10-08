;; DO NOT PUT PASSWORDS IN THIS FILE
;; FOR THE LOVE OF GOD

;; EXPERIMENTAL ERC-SASL SUPPORT

(add-to-list 'load-path "/home/chris/.emacs.d/load")
(require 'erc)
(require 'erc-sasl)
(add-to-list 'erc-sasl-server-regexp-list "irc\\.freenode\\.net")

;; PACKAGES

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; MAIL


;; DOC-VIEW

;; open rtf files with doc-view-mode
(setq auto-mode-alist
      (append
       '(("\\.rtf\\'" . doc-view-mode))
       auto-mode-alist))

;; Redefine doc-view function to allow reading RTFs in Emacs
(defun doc-view-odf->pdf-converter-ted (odf callback)
  (doc-view-start-process
   "odf->pdf"
   doc-view-odf->pdf-converter-program
   (list odf (expand-file-name (concat (file-name-base odf) ".pdf") (doc-view--current-cache-dir)))
   callback))

;; HOOKS

(add-hook 'find-file-hook 'auto-insert)
(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "/") 'find-dired)))

;; ORG-MODE

(setq org-refile-targets ;; Allows entries to be refiled to subheadings 3 deep
      '((nil . (:maxlevel . 3))))
(setq org-refile-use-outline-path t) ;; List subheadings hierarchically
(setq org-outline-path-complete-in-steps t) ;; Don't flood the completion window

(setq org-capture-templates ;; Generic TODO-adding template
      '(("a" "TODO task format" entry
         (file "~/notes.org")
         "* TODO %?
SCHEDULED: %U
DEADLINE: %^t")))

(setq org-agenda-include-diary t)
(setq org-agenda-custom-commands
      '(("c" "TODOs + weekly"
	 ((agenda "")
	  (todo)))))
(setq org-default-notes-file "~/notes.org")
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
(setq org-todo-keyword-faces '(("CANCELLED" . "grey")))
(setq org-enforce-todo-dependencies t)
(setq org-log-done 'time) ;; Add a timestamp a task is marked DONE


;; UTILITY ;;

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t       ; use versioned backups
 auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t))
 auto-revert-interval 5)

(defun cjm-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%m%d%y")))

(add-to-list 'write-file-functions 'delete-trailing-whitespace)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode)
(show-paren-mode)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; BINDINGS ;;

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c s") 'shell)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-set-key (kbd "C-c d") 'cjm-insert-timestamp)
