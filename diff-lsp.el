;;; diff-lsp.el --- A package for configuring & using diff-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Chris Hipple

;; Author: Chris Hipple (github.com/C-Hipple)
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; diff-lsp https://github.com/C-Hipple/diff-lsp is a language server which is
;; a middleware language server enabling multiple servers to provide diagnostics
;; to all of the hunks in diffs.

;;; Code:

;; Initial version is just for getting the lsp setup and configured.
(define-derived-mode diff-test-mode markdown-mode
  "diff-test-mode"
  )

(add-to-list 'auto-mode-alist '("\\.diff-test\\'" . diff-test-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(diff-test-mode . "diff-lsp"))

  (add-to-list 'lsp-language-id-configuration
               '(magit-status-mode . "diff-lsp"))

  (add-to-list 'lsp-language-id-configuration
               '(magit-mode . "diff-lsp"))

  (add-to-list 'lsp-language-id-configuration
               '(code-reviewmode . "diff-lsp"))


  ;; (add-to-list 'lsp-language-id-configuration
  ;;              '(magit-status-mode . "diff-lsp"))

  ;; (add-to-list 'lsp-language-id-configuration
  ;;              '(emacs-lisp-mode . "diff-lsp"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "diff-lsp")
                    :activation-fn (lsp-activate-on "diff-lsp")
                    :server-id 'diff-lsp))
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(magit-status-mode . ("diff-lsp"))
               `(magit-mode . ("diff-lsp"))
               )
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               ;;`(diff-test-mode . ("lsp-example"))
               `(diff-test-mode . ("diff-lsp")))
  )
(defun insertln (ln)
  (insert ln)
  (insert "\n")
  )

(defun dlsp--buffer-to-file (filename)
  (delete-file filename)
  (let* (
         (contents (buffer-string))
         (current_filename (buffer-name))
         (project_name (projectile-project-name))
         (root (projectile-project-root))
         ;; (uri (eglot--uri-to-path current_filename))
         )
    (with-temp-buffer
      (insertln (concat "Project: " current_filename))
      (insertln (concat "Root: " root))
      (insertln (concat "Buffer: " project_name))
      (insertln (concat "Type: " "magit-status"))
      (insert contents)
      (write-file filename)
      )
    )
  )

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "diff-lsp")
                  :activation-fn (lsp-activate-on "diff-lsp")
                  :server-id 'diff-lsp)
 )

(defun test-dlsp-fun ()
  (interactive)
  (dlsp--buffer-to-file "~/diff-lsp/diff-lsp-status.diff-test")
  )

(defun diff-lsp--tail-logs (pipe-cmd)
  ;; Simple helper to immediately tail the logs when debugging
  (interactive "sPipeCommand:")
  (let ((command "tail -f ~/.diff-lsp.log"))
    (when (not (string= pipe-cmd "")) (setq command (concat command " | " pipe-cmd)))
    (compile command)))



;; f l for files - logs, i guess
(define-key evil-normal-state-map (kbd ", f l") 'diff-lsp--tail-logs)

(provide 'diff-lsp)
;;; test.el ends here
