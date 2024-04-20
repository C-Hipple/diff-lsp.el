;;; diff-lsp.el -- A package for configuring & using diff-lsp -* lexical-binding: t; -*-

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

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "diff-lsp")
                    :activation-fn (lsp-activate-on "diff-lsp")
                    :server-id 'diff-lsp)
   )
  )

(provide 'diff-lsp)
;;; test.el ends here
