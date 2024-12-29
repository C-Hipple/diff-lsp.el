(defun diff-file-name ()
  (message "setting diff-file-name")
  "test6.diff-test"
  )

;; This function is analogous to the standard "lsp"
;; heavily edited so i don't break other stuff
(defun diff-lsp-entrypoint (&optional arg)

  "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start."
  (interactive "P")

  (lsp--require-packages)
  (message "starting")
  ;; (print lsp-clients)
  (when (projectile-project-name) ;; used to be buffer-file-name
    (let (clients
          (matching-clients (lsp--filter-clients
                             (-andfn #'lsp--supports-buffer?
                                     #'lsp--server-binary-present?))))

      ;; (prin1 (concat "matching clients: " clients))  ;; prints empty
      (cond
       (matching-clients
        ;; (message "in cond") ;; yeah this hits
        (when (setq lsp--buffer-workspaces
                    (or
                     ;; Don't open as library file if file is part of a project.
                     ;;(not (lsp-find-session-folder (lsp-session) "test6.diff-test"))
                     (diff-lsp--try-open-in-library-workspace)

                     ;; (lsp--try-project-root-workspaces (equal arg '(4))
                     ;;                                   (and arg (not (equal arg 1))))

                     ))
          (message "Enabling LSP mode for buffer")
          (lsp-mode 1)
          (when lsp-auto-configure (lsp--auto-configure))
          (setq lsp-buffer-uri (lsp--buffer-uri))
          (lsp--info "Connected to %s."
                     (apply 'concat (--map (format "[%s %s]"
                                                   (lsp--workspace-print it)
                                                   (lsp--workspace-root it))
                                           lsp--buffer-workspaces)))))
       ;; look for servers which are currently being downloaded.
       ((setq clients (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                   #'lsp--client-download-in-progress?)))
        (message "set the clients!")
        (lsp--info "There are language server(%s) installation in progress.
The server(s) will be started in the buffer when it has finished."
                   (-map #'lsp--client-server-id clients))
        (seq-do (lambda (client)
                  (cl-pushnew (current-buffer) (lsp--client-buffers client)))
                clients))
       ;; look for servers to install
       ((setq clients (lsp--filter-clients
                       (-andfn #'lsp--supports-buffer?
                               (-const lsp-enable-suggest-server-download)
                               #'lsp--client-download-server-fn
                               (-not #'lsp--client-download-in-progress?))))
        (message (concat "clients: " clients))
        (let ((client (lsp--completing-read
                       (concat "Unable to find installed server supporting this file. "
                               "The following servers could be installed automatically: ")
                       clients
                       (-compose #'symbol-name #'lsp--client-server-id)
                       nil
                       t)))
          (cl-pushnew (current-buffer) (lsp--client-buffers client))
          (lsp--install-server-internal client)))
       ;; ignore other warnings
       ((not lsp-warn-no-matched-clients)
        nil)
       ;; automatic installation disabled
       ((setq clients (unless matching-clients
                        (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                     #'lsp--client-download-server-fn
                                                     (-not (-const lsp-enable-suggest-server-download))
                                                     (-not #'lsp--server-binary-present?)))))
        (lsp--warn "The following servers support current file but automatic download is disabled: %s
\(If you have already installed the server check *lsp-log*)."
                   (mapconcat (lambda (client)
                                (symbol-name (lsp--client-server-id client)))
                              clients
                              " ")))
       ;; no clients present
       ((setq clients (unless matching-clients
                        (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                     (-not #'lsp--server-binary-present?)))))
        (lsp--warn "The following servers support current file but do not have automatic installation: %s
You may find the installation instructions at https://emacs-lsp.github.io/lsp-mode/page/languages.
\(If you have already installed the server check *lsp-log*)."
                   (mapconcat (lambda (client)
                                (symbol-name (lsp--client-server-id client)))
                              clients
                              " ")))
       ;; no matches
       ((-> #'lsp--supports-buffer? lsp--filter-clients not)
        (lsp--error "There are no language servers supporting current mode `%s' registered with `lsp-mode'.
This issue might be caused by:
1. The language you are trying to use does not have built-in support in `lsp-mode'. You must install the required support manually. Examples of this are `lsp-java' or `lsp-metals'.
2. The language server that you expect to run is not configured to run for major mode `%s'. You may check that by checking the `:major-modes' that are passed to `lsp-register-client'.
3. `lsp-mode' doesn't have any integration for the language behind `%s'. Refer to https://emacs-lsp.github.io/lsp-mode/page/languages and https://langserver.org/ .
4. You are over `tramp'. In this case follow https://emacs-lsp.github.io/lsp-mode/page/remote/.
5. You have disabled the `lsp-mode' clients for that file. (Check `lsp-enabled-clients' and `lsp-disabled-clients').
You can customize `lsp-warn-no-matched-clients' to disable this message."
                    major-mode major-mode major-mode))))))

(defun lsp--supports-buffer? (client)
  (message "calling supports buffer")
  1
  )

(define-key evil-normal-state-map (kbd ", d s") 'diff-lsp-entrypoint)


(defun lsp--filter-clients (pred)
  (->> lsp-clients hash-table-values (-filter pred)))

(defun lsp--find-clients ()
  "Find clients which can handle current buffer."
  ("Starting find clients")
  (-when-let (matching-clients (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                            #'lsp--server-binary-present?)))
    (message "Found the following clients for %s: %s" ;; was lsp-log
             (diff-file-name)
             (s-join ", "
                     (-map (lambda (client)
                             (format "(server-id %s, priority %s)"
                                     (lsp--client-server-id client)
                                     (lsp--client-priority client)))
                           matching-clients)))
    (-let* (((add-on-clients main-clients) (-separate #'lsp--client-add-on? matching-clients))
            (selected-clients (if-let ((main-client (and main-clients
                                                         (--max-by (> (lsp--client-priority it)
                                                                      (lsp--client-priority other))
                                                                   main-clients))))
                                  (cons main-client add-on-clients)
                                add-on-clients)))
      (lsp-log "The following clients were selected based on priority: %s"
               (s-join ", "
                       (-map (lambda (client)
                               (format "(server-id %s, priority %s)"
                                       (lsp--client-server-id client)
                                       (lsp--client-priority client)))
                             selected-clients)))
      selected-clients)))


(defun diff-lsp--try-open-in-library-workspace ()
  "Try opening current file as library file in any of the active workspace.
The library folders are defined by each client for each of the active workspace."
  (message "starting try open in workspace")
  ;; (print (lsp-session)) ;; prints a lot, looks like it finds the session
  ;; (message (concat "library folder:" (library-folder)))
  (print "workspaces: ")
  (print (lsp--session-workspaces (lsp-session)))
  (when-let ((workspace (->> (lsp-session)
                             (lsp--session-workspaces)
                             ;; Sort the last active workspaces first as they are more likely to be
                             ;; the correct ones, especially when jumping to a definition.
                             (-sort (lambda (a _b)
                                      (-contains? lsp--last-active-workspaces a)))
                             (--first
                              (and (-> it lsp--workspace-client lsp--supports-buffer?)
                                   (when-let ((library-folders-fn
                                               (-> it lsp--workspace-client lsp--client-library-folders-fn)))
                                     (-first (lambda (library-folder)
                                               (lsp-f-ancestor-of? library-folder (diff-file-name)))
                                             (funcall library-folders-fn it))))))))
    (message "in end of try-open")
    (lsp--open-in-workspace workspace)
    (view-mode t)
    (lsp--info "Opening read-only library file %s." (diff-file-name)
               (list workspace))))


(defun lsp-f-same? (path-a path-b)
  "Return t if PATH-A and PATH-B are references to the same file.
Symlinks are not followed."
  t
  )

;; (when (and (f-exists? path-a)
;;            (f-exists? path-b))
;;   (equal
;;    (lsp-f-canonical (directory-file-name (f-expand path-a)))
;;    (lsp-f-canonical (directory-file-name (f-expand path-b))))))
