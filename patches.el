;;; ~/.doom.d/patches.el -*- lexical-binding: t; -*-

;; CHANGE: Redefine evil-delete-buffer to not also close the window
(evil-define-command evil-delete-buffer (buffer &optional bang)
  "Deletes a buffer."
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process) (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    ;; if the buffer which was initiated by emacsclient,
    ;; call `server-edit' from server.el to avoid
    ;; "Buffer still has clients" message
    (if (and (fboundp 'server-edit)
             (boundp 'server-buffer-clients)
             server-buffer-clients)
        (server-edit)
      (kill-buffer nil))))

;; Changed unstage from u to x
(after! magit
;;;###autoload (autoload 'magit-dispatch "magit" nil t)
  (transient-define-prefix magit-dispatch ()
    "Invoke a Magit command from a list of available commands."
    :info-manual "(magit)Top"
    ["Transient and dwim commands"
     ;; → bound in magit-mode-map or magit-section-mode-map
     ;; ↓ bound below
     [("A" "Apply"          magit-cherry-pick)
      ;; a                  ↓
      ("b" "Branch"         magit-branch)
      ("B" "Bisect"         magit-bisect)
      ("c" "Commit"         magit-commit)
      ("C" "Clone"          magit-clone)
      ("d" "Diff"           magit-diff)
      ("D" "Diff (change)"  magit-diff-refresh)
      ("e" "Ediff (dwim)"   magit-ediff-dwim)
      ("E" "Ediff"          magit-ediff)
      ("f" "Fetch"          magit-fetch)
      ("F" "Pull"           magit-pull)
      ;; g                  ↓
      ;; G                → magit-refresh-all
      ("h" "Help"           magit-info)
      ("H" "Section info"   magit-describe-section :if-derived magit-mode)]
     [("i" "Ignore"         magit-gitignore)
      ("I" "Init"           magit-init)
      ("j" "Jump to section"magit-status-jump  :if-mode     magit-status-mode)
      ("j" "Display status" magit-status-quick :if-not-mode magit-status-mode)
      ("J" "Display buffer" magit-display-repository-buffer)
      ;; k                  ↓
      ;; K                → magit-file-untrack
      ("l" "Log"            magit-log)
      ("L" "Log (change)"   magit-log-refresh)
      ("m" "Merge"          magit-merge)
      ("M" "Remote"         magit-remote)
      ;; n                → magit-section-forward
      ;; N       reserved → forge-dispatch
      ("o" "Submodule"      magit-submodule)
      ("O" "Subtree"        magit-subtree)
      ;; p                → magit-section-backward
      ("P" "Push"           magit-push)
      ;; q                → magit-mode-bury-buffer
      ("Q" "Command"        magit-git-command)]
     [("r" "Rebase"         magit-rebase)
      ;; R                → magit-file-rename
      ;; s                  ↓
      ;; S                  ↓
      ("t" "Tag"            magit-tag)
      ("T" "Note"           magit-notes)
      ;; u                  ↓
      ;; U                  ↓
      ;; v                  ↓
      ("V" "Revert"         magit-revert)
      ("w" "Apply patches"  magit-am)
      ("W" "Format patches" magit-patch)
      ;; x                → magit-reset-quickly
      ("X" "Reset"          magit-reset)
      ("y" "Show Refs"      magit-show-refs)
      ("Y" "Cherries"       magit-cherry)
      ("z" "Stash"          magit-stash)
      ("Z" "Worktree"       magit-worktree)
      ("!" "Run"            magit-run)]]
    ["Applying changes"
     :if-derived magit-mode
     [("a" "Apply"          magit-apply)
      ("v" "Reverse"        magit-reverse)
      ("k" "Discard"        magit-discard)]
     [("s" "Stage"          magit-stage)
      ("x" "Unstage"        magit-unstage)]
     [("S" "Stage all"      magit-stage-modified)
      ("X" "Unstage all"    magit-unstage-all)]]
    ["Essential commands"
     :if-derived magit-mode
     [("g" "       refresh current buffer"   magit-refresh)
      ("q" "       bury current buffer"      magit-mode-bury-buffer)
      ("<tab>" "   toggle section at point"  magit-section-toggle)
      ("<return>" "visit thing at point"     magit-visit-thing)]
     [("C-x m"    "show all key bindings"    describe-mode)
      ("C-x i"    "show Info manual"         magit-info)]]))

(after! clojure-mode
  ;; CHANGE: Indent docstrings relative to their parent forms
  (defun clojure-indent-line ()
    "Indent current line as Clojure code."
    (if (clojure-in-docstring-p)
        (save-excursion
          (let ((indent (save-excursion
                          (progn (search-backward "\"") (1+ (current-column))))))
            (beginning-of-line)
            (looking-at "^\\s-*")
            (replace-match (make-string indent ? )))))
    (lisp-indent-line))

  ;; CHANGE: Lots of changes to font locking
  (setq clojure-font-lock-keywords
        (eval-when-compile
          (let ((clojure--num-regexp "-?[0-9]+\\(\\.[0-9]+\\)?"))
            `( ;; Top-level variable definition
              ;; (,(concat "(\\(?:" clojure--sym-regexp "/\\)?\\("
              (,(concat "(\\(\\(?:" clojure--sym-regexp "/\\)?"
                        (regexp-opt '("def" "def-" "defconst" "defonce" "defs"))
                        ;; variable declarations
                        "\\)\\>"
                        ;; Any whitespace
                        "[ \r\n\t]*"
                        ;; Possibly type or metadata
                        "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                        "\\(\\sw+\\)?")
               (1 font-lock-keyword-face)
               (2 font-lock-variable-name-face nil t))
              ;; Type definition
              (,(concat "(\\(?:clojure.core/\\)?\\("
                        (regexp-opt '("defstruct" "deftype" "defprotocol"
                                      "defrecord" "deftype-"))
                        ;; type declarations
                        "\\)\\>"
                        ;; Any whitespace
                        "[ \r\n\t]*"
                        ;; Possibly type or metadata
                        "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                        "\\(\\sw+\\)?")
               (1 font-lock-keyword-face)
               (2 font-lock-type-face nil t))
              ;; Function definition (anything that starts with def and is not
              ;; listed above)
              (,(concat "(\\(\\(?:" clojure--sym-regexp "/\\)?"
                        "def[^ \r\n\t]*\\)"
                        ;; Function declarations
                        "\\>"
                        ;; Any whitespace
                        "[ \r\n\t]*"
                        ;; Possibly type or metadata
                        "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                        (concat "\\(" clojure--sym-regexp "\\)?"))
               (1 font-lock-keyword-face)
               (2 font-lock-function-name-face nil t))
              ;; (fn name? args ...)
              (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                        ;; Possibly type
                        "\\(?:#?^\\sw+[ \t]*\\)?"
                        ;; Possibly name
                        "\\(\\sw+\\)?" )
               (1 font-lock-keyword-face)
               (2 font-lock-function-name-face nil t))
              ;; lambda arguments - %, %&, %1, %2, etc
              ("\\<%[&1-9]?" (0 font-lock-variable-name-face))
              ;; Special forms
              (,(concat
                 "("
                 (regexp-opt
                  '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
                    "recur" "throw" "try" "catch" "finally"
                    "set!" "new" "."
                    "monitor-enter" "monitor-exit" "quote") t)
                 "\\>")
               1 font-lock-keyword-face)
              ;; Built-in binding and flow of control forms
              (,(concat
                 "(\\(?:clojure.core/\\)?"
                 (regexp-opt
                  '("letfn" "case" "cond" "cond->" "cond->>" "condp"
                    "for" "when" "when-not" "when-let" "when-first" "when-some"
                    "if-let" "if-not" "if-some"
                    ".." "->" "->>" "as->" "doto" "and" "or"
                    "dosync" "doseq" "dotimes" "dorun" "doall"
                    "ns" "in-ns"
                    "with-open" "with-local-vars" "binding"
                    "with-redefs" "with-redefs-fn"
                    "declare") t)
                 "\\>")
               1 font-lock-keyword-face)
              ;; Macros similar to let, when, and while
              ;; (,(rx symbol-start
              ;;       (or "let" "when" "while") "-"
              ;;       (1+ (or (syntax word) (syntax symbol)))
              ;;       symbol-end)
              ;;  0 font-lock-keyword-face)
              (,(concat
                 "\\<"
                 (regexp-opt
                  '("*1" "*2" "*3" "*agent*"
                    "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
                    "*command-line-args*" "*compile-files*"
                    "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
                    "*e" "*err*" "*file*" "*flush-on-newline*"
                    "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
                    "*print-dup*" "*print-length*" "*print-level*"
                    "*print-meta*" "*print-readably*"
                    "*read-eval*" "*source-path*"
                    "*unchecked-math*"
                    "*use-context-classloader*" "*warn-on-reflection*")
                  t)
                 "\\>")
               0 font-lock-builtin-face)
              ;; Dynamic variables - *something* or @*something*
              (,(concat "\\(?:\\<\\|/\\)@?\\(\\*" clojure--sym-regexp "\\*\\)\\>")
               1 font-lock-variable-name-face)
              ;; Global constants - nil, true, false
              (,(concat
                 "\\<"
                 (regexp-opt
                  '("true" "false" "nil") t)
                 "\\>")
               0 'clojure-number-face)
              ;; Character literals - \1, \a, \newline, \u0000
              ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'clojure-character-face)

              ;; namespace definitions: (ns foo.bar)
              (,(concat "(\\<ns\\>[ \r\n\t]*"
                        ;; Possibly metadata, shorthand and/or longhand
                        "\\(?:\\^?\\(?:{[^}]+}\\|:[^ \r\n\t]+[ \r\n\t]\\)[ \r\n\t]*\\)*"
                        ;; namespace
                        "\\(" clojure--sym-regexp "\\)")
               (1 font-lock-type-face))

              ;; TODO dedupe the code for matching of keywords, type-hints and unmatched symbols

              ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
              ;; :a/1 is not possible, but :1/a and :1 are.
              (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--keyword-sym-regexp "?\\)\\(/\\)\\(" clojure--keyword-sym-regexp "\\)")
               (1 'clojure-keyword-face)
               (2 font-lock-type-face)
               ;; (2 'clojure-keyword-face)
               (3 'default)
               (4 'clojure-keyword-face))
              (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--keyword-sym-regexp "\\)")
               (1 'clojure-keyword-face)
               (2 'clojure-keyword-face))

              ;; type-hints: #^oneword
              (,(concat "\\(#?\\^\\)\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
               (1 'default)
               (2 font-lock-type-face)
               (3 'default)
               (4 'default))
              (,(concat "\\(#?\\^\\)\\(" clojure--sym-regexp "\\)")
               (1 'default)
               (2 font-lock-type-face))

              ;; clojure symbols not matched by the previous regexps; influences CIDER's
              ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
              ;; (,(concat "\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
              ;;  (1 font-lock-type-face)
              ;;  ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
              ;;  ;; CDSH seems to kick in only for functions and variables referenced w/o
              ;;  ;; writing their namespaces.
              ;;  (2 nil)
              ;;  (3 clojure-pink-face))
              ;; (,(concat "\\(" clojure--sym-regexp "\\)")
              ;;  ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
              ;;  (1 nil))

              ;; #_ and (comment ...) macros.
              (clojure--search-comment-macro 1 font-lock-comment-face t)
              ;; Highlight `code` marks, just like `elisp'.
              (,(rx "`" (group-n 1 (optional "#'")
                                 (+ (or (syntax symbol) (syntax word)))) "`")
               (1 'font-lock-constant-face prepend))
              ;; Highlight [[var]] comments
              (,(rx "[[" (group-n 1 (optional "#'")
                                  (+ (or (syntax symbol) (syntax word)))) "]]")
               (1 'font-lock-constant-face prepend))
              ;; Highlight escaped characters in strings.
              (clojure-font-lock-escaped-chars 0 'bold prepend)
              ;; Highlight grouping constructs in regular expressions
              (clojure-font-lock-regexp-groups
               (1 'font-lock-regexp-grouping-construct prepend))
              ;; ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" 0 'clojure-number-face)
              (,(rx (and symbol-start
                         (? "-")
                         digit
                         (*? any)
                         symbol-end))
               0 'clojure-number-face)
              ("[~@'`#]" 0 'clojure-quote-face))))))

(after! cider
  ;; CHANGE: Fix (def v (a-fn ...)) not font locking a-fn
  (defun cider--parse-and-apply-locals (end &optional outer-locals)
    "Figure out local variables between point and END.
A list of these variables is set as the `cider-locals' text property over
the code where they are in scope.
Optional argument OUTER-LOCALS is used to specify local variables defined
before point."
    (while (search-forward-regexp "(\\(ns\\_>\\|def\\|fn\\|for\\b\\|loop\\b\\|with-\\|do[a-z]+\\|\\([a-z]+-\\)?let\\b\\)"
                                  end 'noerror)
      (goto-char (match-beginning 0))
      (let ((sym (match-string 1))
            (beg (match-beginning 0))
            whole-sym
            (sexp-end (save-excursion
                        (or (ignore-errors (forward-sexp 1)
                                           (point))
                            end))))
        ;; #1324: Don't do dynamic font-lock in `ns' forms, they are special
        ;; macros where nothing is evaluated, so we'd get a lot of false
        ;; positives.
        (if (equal sym "ns")
            (add-text-properties (point) sexp-end '(cider-block-dynamic-font-lock t))
          (forward-char 1)
          (forward-sexp 1)
          (setq whole-sym (buffer-substring-no-properties (1+ beg) (point)))
          (let ((locals (append outer-locals
                                (if (or (string= whole-sym "def")
                                        (string= whole-sym "defconst"))
                                    '()
                                  (pcase sym
                                    ((or "fn" "def" "") (cider--read-locals-from-arglist))
                                    (_ (cider--read-locals-from-bindings-vector)))))))
            (add-text-properties (point) sexp-end (list 'cider-locals locals))
            (clojure-forward-logical-sexp 1)
            (cider--parse-and-apply-locals sexp-end locals)))
        (goto-char sexp-end))))

  ;; CHANGE: Fix docstring lines starting with spaces showing in docview
  (defun cider-docview-render-info (buffer info)
    "Emit into BUFFER formatted INFO for the Clojure or Java symbol."
    (let* ((ns      (nrepl-dict-get info "ns"))
           (name    (nrepl-dict-get info "name"))
           (added   (nrepl-dict-get info "added"))
           (depr    (nrepl-dict-get info "deprecated"))
           (macro   (nrepl-dict-get info "macro"))
           (special (nrepl-dict-get info "special-form"))
           (forms   (when-let* ((str (nrepl-dict-get info "forms-str")))
                      (split-string str "\n")))
           (args    (when-let* ((str (nrepl-dict-get info "arglists-str")))
                      (split-string str "\n")))
           (doc     (replace-regexp-in-string
                     "^ *" "  "
                     (or (nrepl-dict-get info "doc")
                         "Not documented.")))
           (url     (nrepl-dict-get info "url"))
           (class   (nrepl-dict-get info "class"))
           (member  (nrepl-dict-get info "member"))
           (javadoc (nrepl-dict-get info "javadoc"))
           (super   (nrepl-dict-get info "super"))
           (ifaces  (nrepl-dict-get info "interfaces"))
           (spec    (nrepl-dict-get info "spec"))
           (clj-name  (if ns (concat ns "/" name) name))
           (java-name (if member (concat class "/" member) class))
           (see-also (nrepl-dict-get info "see-also")))
      (cider--help-setup-xref (list #'cider-doc-lookup (format "%s/%s" ns name)) nil buffer)
      (with-current-buffer buffer
        (cl-flet ((emit (text &optional face)
                        (insert (if face
                                    (propertize text 'font-lock-face face)
                                  text)
                                "\n")))
          (emit (if class java-name clj-name) 'font-lock-function-name-face)
          (when super
            (emit (concat "   Extends: " (cider-font-lock-as 'java-mode super))))
          (when ifaces
            (emit (concat "Implements: " (cider-font-lock-as 'java-mode (car ifaces))))
            (dolist (iface (cdr ifaces))
              (emit (concat "            "(cider-font-lock-as 'java-mode iface)))))
          (when (or super ifaces)
            (insert "\n"))
          (when-let* ((forms (or forms args)))
            (dolist (form forms)
              (insert " ")
              (emit (cider-font-lock-as-clojure form))))
          (when special
            (emit "Special Form" 'font-lock-keyword-face))
          (when macro
            (emit "Macro" 'font-lock-variable-name-face))
          (when added
            (emit (concat "Added in " added) 'font-lock-comment-face))
          (when depr
            (emit (concat "Deprecated in " depr) 'font-lock-keyword-face))
          (if class
              (cider-docview-render-java-doc (current-buffer) doc)
            (emit doc))
          (when url
            (insert "\n  Please see ")
            (insert-text-button url
                                'url url
                                'follow-link t
                                'action (lambda (x)
                                          (browse-url (button-get x 'url))))
            (insert "\n"))
          (when javadoc
            (insert "\n\nFor additional documentation, see the ")
            (insert-text-button "Javadoc"
                                'url javadoc
                                'follow-link t
                                'action (lambda (x)
                                          (browse-url (button-get x 'url))))
            (insert ".\n"))
          (insert "\n")
          (when spec
            (emit "Spec:" 'font-lock-function-name-face)
            (insert (cider-browse-spec--pprint-indented spec))
            (insert "\n\n")
            (insert-text-button "Browse spec"
                                'follow-link t
                                'action (lambda (_)
                                          (cider-browse-spec (format "%s/%s" ns name))))
            (insert "\n\n"))
          (if cider-docview-file
              (progn
                (insert (propertize (if class java-name clj-name)
                                    'font-lock-face 'font-lock-function-name-face)
                        " is defined in ")
                (insert-text-button (cider--abbreviate-file-protocol cider-docview-file)
                                    'follow-link t
                                    'action (lambda (_x)
                                              (cider-docview-source)))
                (insert "."))
            (insert "Definition location unavailable."))
          (when see-also
            (insert "\n\n Also see: ")
            (mapc (lambda (ns-sym)
                    (let* ((ns-sym-split (split-string ns-sym "/"))
                           (see-also-ns (car ns-sym-split))
                           (see-also-sym (cadr ns-sym-split))
                           ;; if the var belongs to the same namespace,
                           ;; we omit the namespace to save some screen space
                           (symbol (if (equal ns see-also-ns) see-also-sym ns-sym)))
                      (insert-text-button symbol
                                          'type 'help-xref
                                          'help-function (apply-partially #'cider-doc-lookup symbol)))
                    (insert " "))
                  see-also))
          (cider--doc-make-xrefs)
          (let ((beg (point-min))
                (end (point-max)))
            (nrepl-dict-map (lambda (k v)
                              (put-text-property beg end k v))
                            info)))
        (current-buffer)))))

(provide 'patches)
