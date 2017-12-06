;;; init.el --- Init file for Emacs                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Mazzullo

;; Author: Chris Mazzullo <chris.mazzullo@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.q

;;; Commentary:

;; DO NOT PUT PASSWORDS IN THIS FILE
;; FOR THE LOVE OF GOD

;;; Code:

;; PACKAGES ;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/local/share/emacs/site-lisp")
(package-initialize) 			; removes the need for `require`s


;; UTILITY ;;

(setq user-full-name "Chris Mazzullo"
      default-directory "~/"
      create-lockfiles nil
      auto-revert-interval 1
      auto-revert-remote-files t
      auto-revert-use-notify nil
      inhibit-startup-screen t
      eshell-banner-message ""
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t))
      auto-revert-interval 5)

(ido-mode)
(ido-everywhere t)
(setq ido-use-url-at-point t
      ido-use-filename-at-point 'guess)
(global-prettify-symbols-mode)
(winner-mode) ; Allow undo-ing window operations
(display-time-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Consolas-12"))
(column-number-mode)
(show-paren-mode)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-hook 'proced-mode-hook ; Use proced as a `top` replacement
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "/") 'find-dired)))
	  '(lambda () (proced-toggle-auto-update 1)))
(add-hook 'prog-mode-hook 'linum-mode)


;; BINDINGS ;;

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-z") 'nil) ; God I hate this binding


;; PYTHON ;;

(require 'python)
(setq python-skeleton-autoinsert t)

(setq python-shell-interpreter "python3.5")
(add-to-list 'python-shell-completion-native-disabled-interpreters "python3.5")
;; (setq python-shell-completion-native-enable nil) ; windows hack

;; Virtual Envs subsection
(venv-initialize-interactive-shells) ;; interactive shell support
(venv-initialize-eshell)
(setq venv-location "~/venvs/")
(setq python-shell-unbuffered nil) ; Necessary to avoid warnings in Windows

;; Django subsection
;; Needed b/c our Django template files have the `.html` extension instead of `.djhtml`
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html?\\'")))

;; TODO: Move most of this section into sass-mode.el
;; SASS-MODE ;;

;; (add-to-list 'load-path "~/projects/sass-mode")
;; (require 'sass-mode)


;; ;; Some handy skeletons to make our life less miserable 💀
;; (define-skeleton sas-data
;;   "Automagically make a `data` block" "" \n
;;   "data " (skeleton-read "Dataset name: ") ";" \n
;;   _ \n
;;   "run;" > \n)
;; (define-abbrev sass-mode-abbrev-table "data" "" 'sas-data)

;; (define-skeleton sas-print
;;   "Automagically make a `proc print` block" "" \n
;;   "proc print data=" (skeleton-read "Dataset name: ")
;;   (let ((where (skeleton-read "Where statement: "))) ; Optional `where` statement
;;     (if (string-empty-p where) nil
;;       (concat " (where=(" where "))"))) ";" \n
;;   "var " _ ";" \n
;;   "run;" > \n)
;; (define-abbrev sass-mode-abbrev-table "print" "" 'sas-print)

;; (define-skeleton sas-freq
;;   "Automagically make a `proc freq`" "" \n
;;   "proc freq data=" (skeleton-read "Use Dataset: ") ";" \n
;;   "title4 '" (skeleton-read "Title: ")"';" \n
;;   "tables " _ " / missing;" \n
;;  "run;" > \n)
;; (define-abbrev sass-mode-abbrev-table "freq" "" 'sas-freq)

;; (define-skeleton sas-tabulate
;;   "Automagically make a `proc tabulate`" "" \n
;;   "proc tabulate data=" (skeleton-read "Use Dataset: ") " missing;" \n
;;   "title4 '" (skeleton-read "Title: ")"';" \n
;;   "class " _ ";" \n
;;   "table all , all ;" \n
;;   "keylabel n=' ';" \n
;;   "format ;" \n
;;  "run;" > \n)

;; (eval-after-load 'autoinsert		; Header definitions
;;   '(define-auto-insert '("\\.sas\\'" . "SAS skeleton")
;;      '("Purpose: " "/*" ?\n ?\n
;;        "AUTHOR:    " "Chris Mazzullo" ?\n
;;        "DATE:      " (format-time-string "%e %B %Y.") ?\n
;;        "LOCATION:  " (buffer-file-name) ?\n ?\n
;;        "PURPOSE:   " str ?\n
;;        "*/" ?\n ?\n
;;        "title1 \"" (skeleton-read "Title: ") "\";" ?\n
;;        "title2 \"" (buffer-file-name) "\";" ?\n
;;        "title3 \" \";" ?\n ?\n
;;        "%let progdate=jan17." (format-time-string "%m%d%y") ";" ?\n ?\n
;;        "*** INPUT FILES ************************************************************************************;" ?\n ?\n _ ?\n ?\n
;;        "*** FORMATS ****************************************************************************************;" ?\n ?\n
;;        "options nonotes;" ?\n ?\n ?\n
;;        "options notes;" ?\n ?\n
;;        "*** DATA SETS **************************************************************************************;" ?\n ?\n ?\n ?\n
;;        "*** TESTS ******************************************************************************************;" ?\n ?\n
;;        "%macro assert(cond);" \n
;;        "  if ~(&cond.) then" \n
;;        "    putlog "ERROR: Assertion (%superq(cond)) is FALSE.";" \n
;;        "%mend assert;" \n
;;        "data TEST;" ?\n
;;        "run;" ?\n ?\n ?\n
;;        "*** OUTPUTS ****************************************************************************************;" ?\n ?\n
;;        "options nonotes;" ?\n
;;        "ods path(prepend) work.template(update);" ?\n
;;        "%include \"/prj/plcoims/study_wide/program_tools/styles/styles.sas\";" ?\n
;;        "options nodate center orientation=portrait linesize=120;" ?\n
;;        "ods rtf file='" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
;;        ".rtf' bodytitle style=style1;" ?\n
;;        "options notes;" ?\n ?\n
;;        "ods rtf close;" ?\n ?\n )))

;; (define-abbrev sass-mode-abbrev-table "tab" "" 'sas-tabulate)
;; (add-hook 'sass-mode-hook 'auto-insert)
;; (add-hook 'sass-output-mode-hook 'view-mode)
;; (add-hook 'sass-output-mode-hook 'auto-revert-mode)


;; HOOKS

(add-to-list 'write-file-functions 'delete-trailing-whitespace)
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "/") 'find-dired)))


;; ORG-MODE ;;

(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "RAW")
      org-agenda-include-diary t
      org-agenda-custom-commands '(("c" "TODOs + weekly" ((agenda "") (todo))))
      org-agenda-files '("~/notes.org")
      org-default-notes-file "~/notes.org"
      org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED"))
      org-todo-keyword-faces '(("CANCELLED" . "grey"))
      org-enforce-todo-dependencies t
      org-log-done 'time ;; Add a timestamp a task is marked DONE
      org-agenda-files '("~/notes.org")
      org-src-fontify-natively t
      org-return-follows-link t
      org-refile-targets  '((nil . (:maxlevel . 3))) ;;Allows entries to be refiled to subheadings 3 deep
      org-refile-use-outline-path t ;; List subheadings hierarchically
      org-outline-path-complete-in-steps t ;; Don't flood the completion window
      org-capture-templates ;; Generic TODO-adding template
      '(("a" "TODO task format" entry (file "~/notes.org") "* TODO %? SCHEDULED: %U DEADLINE: %^t")))


;; OCAML ;;
(load "/home/chris/.opam/4.05.0/share/emacs/site-lisp/tuareg-site-file")


;; C ;;
;; Prettification
(add-hook 'c-mode-hook
	  (lambda ()
	    (push '("!=" . ?≠) prettify-symbols-alist)
	    (push '("<=" . ?≤) prettify-symbols-alist)
	    (push '(">=" . ?≥) prettify-symbols-alist)
	    (push '("->" . ?→) prettify-symbols-alist)))
