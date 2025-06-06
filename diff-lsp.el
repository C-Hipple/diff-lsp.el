;;; diff-lsp.el --- A package for configuring & using diff-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Chris Hipple

;; Author: Chris Hipple (github.com/C-Hipple)
;; Keywords: lisp
;; Version: 0.0.3

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

(defvar diff-lsp-tempfile-name "~/.diff-lsp-tempfile"
  ;; TODO: Make this proper temp file directory /tmp/.diff-lsp ?
  "the tempfile which syncs the emphermal buffer with diff-lsp")

;; Initial version is just for getting the lsp setup and configured.

(defun diff-lsp--valid-buffer ()
  "checks the major mode to see if we should apply diff-lsp overrides"
  (not (null (member major-mode '(code-review-mode magit-status-mode)))))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(diff-test-mode . "diff-lsp"))

  ;; TODO: Magit support.
  ;; Will involve adjusting advices will new diff-lsp--apply-overrides? helper)
  ;; (add-to-list 'lsp-language-id-configuration
  ;;              '(magit-status-mode . "diff-lsp"))

  (add-to-list 'lsp-language-id-configuration
               '(magit-status-mode . "diff-lsp"))

  (add-to-list 'lsp-language-id-configuration
               '(code-review-mode . "diff-lsp"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "diff-lsp")
                    :activation-fn (lsp-activate-on "diff-lsp")
                    :server-id 'diff-lsp))
  )

(defun diff-lsp--buffer-to-temp-file (filename)
  (delete-file filename)
  (let ((contents (buffer-string))
        (current_filename (buffer-name))
        (project_name (projectile-project-name))
        (root (projectile-project-root))
        (buffer-type (replace-regexp-in-string "-mode" "" (prin1-to-string major-mode))))
    (with-temp-buffer
      (insert "Project:" ?\s current_filename ?\n)
      (insert "Root:" ?\s root ?\n)
      (insert "Buffer:" ?\s project_name ?\n)
      (insert "Type:" ?\s buffer-type ?\n)
      (insert contents)
      (write-file filename))))

;; (remove-hook 'code-review-mode-hook 'diff-lsp--buffer-to-temp-file)
;; (add-hook 'code-review-mode-hook 'diff-lsp--buffer-to-temp-file)

;;;###autoload
(defun diff-lsp--tail-logs (pipe-cmd)
  ;; Simple helper to immediately tail the logs when debugging
  (interactive "sPipeCommand:")
  (let ((command "tail -f ~/.diff-lsp.log"))
    (when (not (string= pipe-cmd "")) (setq command (concat command " | " pipe-cmd)))
    (compile command)))

;; f l for files - logs, i guess
(define-key evil-normal-state-map (kbd ", f l") 'diff-lsp--tail-logs)


;; Below are a series of advice patches which support an LSP client for a buffer not visiting a file.
(defun diff-lsp--entrypoint (orig-fn &rest args)
  "patch function which sets up diff lsp before starting the lsp"
  (diff-lsp--buffer-to-temp-file diff-lsp-tempfile-name)
  (apply orig-fn args))

(advice-add 'lsp :around #'diff-lsp--entrypoint)

(defun diff-lsp--lsp-f-same? (orig-fn &rest args)
  "Override for lsp-f-same? which just returns true since we don't have real files to compare"
  (if (diff-lsp--valid-buffer)
      t
    (apply orig-fn args)))

(advice-add 'lsp-f-same? :around #'diff-lsp--lsp-f-same?)


;; (defun diff-lsp--patch-calculate-root (&optional x y)
;;   (projectile-project-root))

(defun diff-lsp--calculate-root (orig-fn &rest args)
  (if (diff-lsp--valid-buffer)
      ;; (apply #'diff-lsp--patch-calculate-root args)
      (projectile-project-root)
    (apply orig-fn args)))

(advice-add 'lsp--calculate-root :around #'diff-lsp--calculate-root)

(defun diff-lsp--client-priority (orig-fn &rest args)
  "Needing this patched suggests I'm missing something in the language setup, probably something in filtering available servers types.  Must be like a nil or something which means that un-assigned but defined server types are eligible for this?"
  (if (diff-lsp--valid-buffer)
      ;; Some really high number.
      10
    (apply orig-fn args)))

(advice-add 'lsp--client-priority :around #'diff-lsp--client-priority)

(defun diff-lsp--buffer-file-name (orig-fn &rest args)
  (if (diff-lsp--valid-buffer)
      diff-lsp-tempfile-name
    (apply orig-fn args)))

(advice-add 'buffer-file-name :around #'diff-lsp--buffer-file-name)

(defun diff-lsp--dap--after-open (orig-fn &rest args)
  (if (diff-lsp--valid-buffer)
      (message "Skipping setting up dap for diff-lsp tempfile.")
    (apply orig-fn args)))

(advice-add 'dap--after-open :around #'diff-lsp--dap--after-open)

(defun diff-lsp--cur-line(orig-fn &rest args)
  "Wrapper which offsets the line to account for the extra lines we added above to communicate with diff-lsp."
  (if (diff-lsp--valid-buffer)
      ;; we add 3 here, since it's usually -1 to accoutn for 1 index of editor, but we add 4 lines
      ;; in the buffer to temp file.
      (+ (line-number-at-pos) 3)
    (apply orig-fn args)))

(advice-add 'lsp--cur-line :around #'diff-lsp--cur-line)

(defun diff-lsp--text-document-did-close (orig-fn &rest args)
  (if (diff-lsp--valid-buffer)
      (progn
        (message (mapconcat #'prin1-to-string args))
        (message "Skipping textDocument/didClose for diff-lsp tempfile."))
    (apply orig-fn args)))

(advice-add 'lsp--text-document-did-close :around #'diff-lsp--text-document-did-close)
;; (advice-remove 'lsp--text-document-did-close #'diff-lsp--text-document-did-close)

(defun diff-lsp--disconnect (orig-fn &rest args)
  (if (diff-lsp--valid-buffer)
      (message "Skipping lsp-disconnect for diff-lsp tempfile.")
    (apply orig-fn args)))

(advice-add 'lsp-disconnect :around #'diff-lsp--disconnect)

;; I DON't think i actually need this one
(defun diff-lsp--after-set-visited-file-name (orig-fn &rest args)
  (if (diff-lsp--valid-buffer)
      (message "Skipping lsp--after-set-visited-file-name for diff-lsp tempfile.")
    (apply orig-fn args)))

(advice-add 'lsp--after-set-visited-file-name :around #'diff-lsp--after-set-visited-file-name)

(provide 'diff-lsp)
;;; diff-lsp.el ends here
