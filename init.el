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
(package-initialize) ; removes the need for most `require`s

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/local/share/emacs/site-lisp")
(setq package-selected-packages '(elpy
				  flycheck-pyflakes
				  virtualenvwrapper
				  projectile
				  golden-ratio
				  magit
				  web-mode
				  php-mode))


;; UTILITY ;;

(setq user-full-name "Chris Mazzullo"
      default-directory "~/"
      create-lockfiles nil
      inhibit-startup-screen t
      eshell-banner-message ""
      ido-auto-merge-work-directories-length -1
      ido-use-filename-at-point 'guess  ; Auto-ffap
      inhibit-eol-conversion nil)

(require 'zone)
(zone-when-idle 900) ; Literally necessary

(menu-bar-mode -1) ; Clean up UI
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ido-mode)
(ido-everywhere)
(golden-ratio-mode)
(projectile-mode)
(column-number-mode)
(show-paren-mode)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; Always treat .m as a matlab extension


;; BACKUP ;;

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))


;; BINDINGS ;;

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-z") 'nil) ; God I hate this binding
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; PYTHON ;;

(setq python-skeleton-autoinsert t)
(elpy-enable)
(setq whitespace-style '(face tabs lines-tail)) ; highlight long lines

;; Virtual Envs subsection
(venv-initialize-interactive-shells) ;; interactive shell support
(venv-initialize-eshell)
(setq venv-location "~/venvs/")
(setq python-shell-unbuffered nil) ; Necessary to avoid warnings in Windows

;; Django subsection
(defun django-shell ()
  "Run a django shell buffer for the current project"
  (interactive)
  (let ((project-root (projectile-project-root)))
    (run-python (concat "python -i " project-root "manage.py shell") nil t)))

(defun django-runserver ()
  "Run a django local server for the current project"
  (interactive)
  (let ((project-root (projectile-project-root)))
    (run-python (concat "python " project-root "manage.py runserver") nil t)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Needed b/c our Django template files have the `.html` extension instead of `.djhtml`:
(setq web-mode-engines-alist '(("django" . "\\.html?\\'")))


;; HOOKS

(add-to-list 'write-file-functions 'delete-trailing-whitespace)
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'proced-mode '(lambda () (proced-toggle-auto-update t)))

;; ORG-MODE ;;

(setq org-startup-indented t
      org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "RAW")
      org-agenda-include-diary t
      org-agenda-custom-commands '(("c" "TODOs + weekly" ((agenda "") (todo))))
      org-agenda-files '("~/notes.org")
      org-default-notes-file "~/notes.org"
      org-todo-keywords '((sequence "TODO" "DEFERRED" "|" "DONE" "CANCELLED"))
      org-todo-keyword-faces '(("CANCELLED" . "grey")
			       ("DEFERRED" . "bold"))
      org-enforce-todo-dependencies t
      org-log-done 'time ;; Add a timestamp a task is marked DONE
      org-agenda-files '("~/notes.org")
      org-src-fontify-natively t
      org-return-follows-link t
      org-refile-targets  '((nil . (:maxlevel . 3))) ;;Allows entries to be refiled to subheadings 3 deep
      org-refile-use-outline-path t ;; List subheadings hierarchically
      org-outline-path-complete-in-steps t ;; Don't flood the completion window
      org-capture-templates '(("a" "TODO task format" entry (file "~/notes.org") "* TODO %? SCHEDULED: %U DEADLINE: %^t")))
