(define-derived-mode prbody-mode markdown-mode
  "prbody-mode")

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(prbody-mode . "lsp-example")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "lsp-example")
                  :activation-fn (lsp-activate-on "lsp-example")
                  :server-id 'lsp-example))

(add-to-list 'auto-mode-alist '("\\.prbody\\'" . prbody-mode))


(defun test-dlsp-fun ()
  (interactive)
  (diff-lsp--buffer-to-temp-file "~/.diff-lsp-tempfile"))


(defun diff-lsp-clear-buffers ()
  (interactive)
  (kill-buffer "*diff-lsp*")
  (kill-buffer "*diff-lsp::stderr*"))


(define-key evil-normal-state-map (kbd ", d k") 'diff-lsp-clear-buffers)
(define-key code-review-mode-map (kbd ", d k") 'diff-lsp-clear-buffers)
