(define-derived-mode diff-test-mode markdown-mode
  "diff-test-mode")

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "diff-lsp")
                  :activation-fn (lsp-activate-on "diff-lsp")
                  :server-id 'diff-lsp))

(add-to-list 'auto-mode-alist '("\\.diff-test\\'" . diff-test-mode))


(defun test-dlsp-fun ()
  (interactive)
  (diff-lsp--buffer-to-temp-file "~/.diff-lsp-tempfile"))


(defun diff-lsp-clear-buffers ()
  (interactive)
  (kill-buffer "*diff-lsp*")
  (kill-buffer "*diff-lsp::stderr*"))


(define-key evil-normal-state-map (kbd ", d k") 'diff-lsp-clear-buffers)
