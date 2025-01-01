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
  (message "\n\n\nstarting\n\n\n")
  ;; (print lsp-clients)
  (setq arg t)
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
                     ;; (not (lsp-find-session-folder (lsp-session) "test6.diff-test"))
                     (diff-lsp--try-open-in-library-workspace)

                     (lsp--try-project-root-workspaces (equal arg '(4))
                                                       (and arg (not (equal arg 1))))

                     ))
          (message "Enabling LSP mode for buffer")
          (lsp-mode 1)
          (when lsp-auto-configure (lsp--auto-configure))
          (setq lsp-buffer-uri (lsp--buffer-uri))
          (message "Connected to %s." ;; was lsp-info
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
  (message "Starting find clients")
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
      (message "The following clients were selected based on priority: %s" ;; was lsp-log not message
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
  (print "end workspaces: ")
  (when-let ((workspace (->> (lsp-session)
                             (lsp--session-workspaces)
                             ;; Sort the last active workspaces first as they are more likely to be
                             ;; the correct ones, especially when jumping to a definition.
                             (-sort (lambda (a _b)
                                      (-contains? lsp--last-active-workspaces a)))
                             (--first
                              ;; (message (concat "checking: " workspace))
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

(defun lsp-resolve-final-command (command &optional test?)
  "Resolve final function COMMAND."
  (message (concat "Tryihng to resolve teh command: " command))

  (let* ((command (lsp-resolve-value command))
         (command (cl-etypecase command
                    (list
                     (cl-assert (seq-every-p (apply-partially #'stringp) command) nil
                                "Invalid command list")
                     command)
                    (string (list command)))))
    (if (and (file-remote-p default-directory) (not test?))
        (list shell-file-name "-c"
              (string-join (cons "stty raw > /dev/null;"
                                 (mapcar #'shell-quote-argument command))
                           " "))
      command)))

(defun lsp-f-canonical (file-name)
  "Return the canonical FILE-NAME, without a trailing slash."
  (if (not file-name)
      ;; (message "Overriding filename")
      (setq file-name "~/lsp-example/main.go")
    )
  (message (concat "Canonicallizing the file-name: " file-name))
  (directory-file-name (expand-file-name file-name)))


;; TODO rename to diff-lsp--start-workspace and update caller
(defun lsp--start-workspace (session client-template root &optional initialization-options)
  "Create new workspace for CLIENT-TEMPLATE with project root ROOT.
INITIALIZATION-OPTIONS are passed to initialize function.
SESSION is the active session."
  (lsp--spinner-start)
  (-let* ((default-directory root)
          (client (copy-lsp--client client-template))
          (workspace (make-lsp--workspace
                      :root root
                      :client client
                      :status 'starting
                      :buffers (list (lsp-current-buffer))
                      :host-root (file-remote-p root)))
          ((&lsp-cln 'server-id 'environment-fn 'new-connection 'custom-capabilities
                     'multi-root 'initialized-fn) client)
          ((proc . cmd-proc) (funcall
                              (or (plist-get new-connection :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (lsp--create-filter-function workspace)
                              (apply-partially #'lsp--process-sentinel workspace)
                              (format "%s" server-id)
                              environment-fn
                              workspace))
          (workspace-folders (gethash server-id (lsp-session-server-id->folders session))))
    (setf (lsp--workspace-proc workspace) proc
          (lsp--workspace-cmd-proc workspace) cmd-proc)

    ;; update (lsp-session-folder->servers) depending on whether we are starting
    ;; multi/single folder workspace
    (mapc (lambda (project-root)
            (->> session
                 (lsp-session-folder->servers)
                 (gethash project-root)
                 (cl-pushnew workspace)))
          (or workspace-folders (list root)))

    (with-lsp-workspace workspace
      (run-hooks 'lsp-before-initialize-hook)
      (lsp-request-async
       "initialize"
       (append
        (list :processId (unless (file-remote-p (buffer-file-name))
                           (emacs-pid))
              :rootPath (lsp-file-local-name (expand-file-name root))
              :clientInfo (list :name "emacs"
                                :version (emacs-version))
              :rootUri (lsp--path-to-uri root)
              :capabilities (lsp--client-capabilities custom-capabilities)
              :initializationOptions initialization-options
              :workDoneToken "1")
        (when lsp-server-trace
          (list :trace lsp-server-trace))
        (when multi-root
          (->> workspace-folders
               (-distinct)
               (-map (lambda (folder)
                       (list :uri (lsp--path-to-uri folder)
                             :name (f-filename folder))))
               (apply 'vector)
               (list :workspaceFolders))))
       (-lambda ((&InitializeResult :capabilities))
         ;; we know that Rust Analyzer will send {} which will be parsed as null
         ;; when using plists
         (when (equal 'rust-analyzer server-id)
           (-> capabilities
               (lsp:server-capabilities-text-document-sync?)
               (lsp:set-text-document-sync-options-save? t)))

         (setf (lsp--workspace-server-capabilities workspace) capabilities
               (lsp--workspace-status workspace) 'initialized)

         (with-lsp-workspace workspace
           (lsp-notify "initialized" lsp--empty-ht))

         (when initialized-fn (funcall initialized-fn workspace))

         (cl-callf2 -filter #'lsp-buffer-live-p (lsp--workspace-buffers workspace))
         (->> workspace
              (lsp--workspace-buffers)
              (mapc (lambda (buffer)
                      (lsp-with-current-buffer buffer
                        (lsp--open-in-workspace workspace)))))

         (with-lsp-workspace workspace
           (run-hooks 'lsp-after-initialize-hook))
         (lsp--info "%s initialized successfully in folders: %s"
                    (lsp--workspace-print workspace)
                    (lsp-workspace-folders workspace)))
       :mode 'detached))
    workspace))



;; doing path to URI for: /Users/rain/lsp-example/main.go
(defun lsp--path-to-uri (path)
  "Convert PATH to a uri."
  (message (concat "doing path to URI for: " path))
  (if (not path)
      (setq path "/Users/rain/lsp-example/test6.diff-test")
    )
  (if-let ((uri-fn (->> (lsp-workspaces)
                        (-keep (-compose #'lsp--client-path->uri-fn #'lsp--workspace-client))
                        (cl-first))))
      (funcall uri-fn path)
    (lsp--path-to-uri-1 path)))






(defun diff-lsp--set-priority (server priority)
  (setf (lsp--client-priority (gethash server lsp-clients)) priority))

(defun diff-lsp--get-priority (server)
  (lsp--client-priority (gethash server lsp-clients)))



(diff-lsp--set-priority 'gdscript -1)
(diff-lsp--set-priority 'gdscript-tramp -1)
(diff-lsp--set-priority 'diff-lsp-tramp -1)
(diff-lsp--get-priority 'gdscript-tramp)
