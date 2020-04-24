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
(setq package-selected-packages '(elpy
				  emmet-mode
				  flycheck-pyflakes
				  virtualenvwrapper
				  projectile
				  golden-ratio
				  magit
				  web-mode
				  php-mode
				  nix-mode))
(package-initialize) ; removes the need for most `require`s
(unless package-archive-contents
  (package-refresh-contents))


;; UTILITY ;;

(setq user-full-name "Chris Mazzullo"
      default-directory "~/"
      create-lockfiles nil
      inhibit-startup-screen t
      eshell-banner-message ""
      ido-auto-merge-work-directories-length -1 ; disable auto directory switching in IDO
      ido-use-filename-at-point 'guess  ; Auto-ffap
      inhibit-eol-conversion nil
      search-default-mode t) ; Default to regex search

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

;; I define a minor mode to keep all my key bindings in because this
;; prevents other modes from clobbering them (e.g. diff-mode would
;; otherwise clobber my M-o rebinding)
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-o") 'other-window) ; Much more convenient window-switching
    (define-key map (kbd "C-c r") 'revert-buffer)
    (define-key map (kbd "C-c l") 'org-store-link)
    (define-key map (kbd "C-c c") 'org-capture)
    (define-key map (kbd "C-c a") 'org-agenda)
    (define-key map (kbd "C-c b") 'org-iswitchb)
    (define-key map (kbd "C-z") 'nil) ; God I hate this binding
    (define-key map (kbd "C-x g") 'magit-status)
    (define-key map (kbd "C-x M-g") 'magit-dispatch-popup)
    (define-key map (kbd "C-x C-b") 'ibuffer) ; Ibuffer is a straight upgrade from stock buffer-list
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(if (eq system-type 'windows-nt) ; Use eshell on windows systems, normal shell on Linux
    (global-set-key (kbd "C-c s") 'eshell)
  (global-set-key (kbd "C-c s") 'shell)
  (setenv "PAGER" "cat"))


;; MAGIT ;;

;; Pop over to file in another window when looking at a magit diff
(setq magit-display-file-buffer-function 'magit-display-file-buffer-other-window)


;; JAVASCRIPT ;;

(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-jsx-mode))


;; PYTHON ;;

(setq python-skeleton-autoinsert t)
(setq whitespace-style '(face tabs lines-tail)) ; highlight long lines

(elpy-enable)
(setq elpy-rpc-timeout 2) ; increase timeout (seconds) for our slow computer
(add-hook 'elpy-mode-hook
	  (lambda ()
	    (define-key elpy-mode-map (kbd "M-q") 'elpy-black-fix-code)))

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
  (let ((server-buffer "django-server")
	 (cmd (concat "python " (projectile-project-root) "manage.py runserver")))
    (display-buffer (python-shell-make-comint cmd server-buffer t nil))))


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Needed b/c our Django template files have the `.html` extension instead of `.djhtml`:
(setq web-mode-engines-alist '(("django" . "\\.html?\\'")))


;; HOOKS

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

; Clean trailing whitespace before saving:
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(add-hook 'python-mode-hook 'whitespace-mode)

; Makes proced-mode auto-update like "top":
(add-hook 'proced-mode '(lambda () (proced-toggle-auto-update t)))


;; ORG-MODE ;;

(setq org-default-notes-file "~/notes.org"
      org-agenda-files '(org-default-notes-file)
      org-capture-templates '(("a" "TODO task format" entry (file org-default-notes-file) "* TODO %? SCHEDULED: %U DEADLINE: %^t"))
      org-startup-indented t
      org-agenda-include-diary t
      org-agenda-custom-commands '(("c" "TODOs + weekly" ((agenda "") (todo))))
      org-todo-keywords '((sequence "TODO(t)" "STUCK(s)" "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces '(("CANCELLED" . "slategrey")
			       ("STUCK" . "black"))
      org-enforce-todo-dependencies t
      org-log-done 'time ; Add a timestamp a task is marked DONE
      org-src-fontify-natively t ; Syntax highlighting in source code blocks
      org-return-follows-link t
      org-refile-targets  '((nil . (:maxlevel . 3))) ; Allows entries to be refiled to subheadings 3 deep
      org-refile-use-outline-path t ; List subheadings hierarchically
      org-outline-path-complete-in-steps t ; Don't flood the completion window
      org-src-window-setup 'current-window  ;; edit source blocks in the same window
      org-src-preserve-indentation t  ;; prevents source blocks from indenting their content
      org-file-apps '((auto-mode . emacs)) ;; Always open org links in emacs (not firefox)
      org-startup-truncated nil)
