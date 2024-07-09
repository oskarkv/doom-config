;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(add-load-path! "/home/oskar/.doom.d/")
(require 'tsexp)
(require 'utils)
(require 'patches)
(require 'esexp)
(require 'seq)
(require 's)
(require 'cl-indent)
(require 'dash)
(require 'dap-swi-prolog)
(require 'undo-tree)
(require 'fill-as-markdown)
(global-undo-tree-mode)

;; RET vs <return>:
;; Emacs considers RET (\n) and <return> as distinct events. GUI Emacs emits
;; <return> and falls back on RET. Terminal Emacs only emits RET. So when
;; binding keys your best bet is to bind them both. Same for TAB and <tab> (and
;; C-i) [tab] = "<tab>" and is used when I press tab.

(setq undo-tree-history-directory-alist '(("." . "~/.undo-tree/")))

(setq  org-odt-preferred-output-format "docx")
;; Name and email address
(setq user-full-name "Oskar Kvist"
      user-mail-address "oskar.kvist@gmail.com")

(evil-set-undo-system 'undo-tree)
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-molokai)
(load-theme 'doom-molokai t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Leader and localleader keys
(setq doom-leader-key ","
      doom-localleader-key ", m")

(setq fancy-splash-image "~/.doom.d/doom.png"
      +doom-dashboard-banner-padding '(0 . 1))

;; Disable automatically inserting closing parens, quotes, etc.
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; Fix TAB shadowing C-i
;; (when (daemonp)
;;   (add-to-list 'load-path "~/.doom.d")
;;   (require 'edit-server)
;;   (edit-server-start)
;;   (add-hook 'edit-server-start-hook 'evil-insert-state)
;;   (add-hook 'edit-server-start-hook (fn (auto-fill-mode -1))))

(defun ok-bind-tab-to-TAB (keymap)
  "Bind <tab> (regular tab) to what TAB (C-i) was bound to in KEYMAP,
unless <tab> was already bound, then unbinds TAB."
  (unless (lookup-key keymap (kbd "<tab>"))
    (define-key keymap (kbd "<tab>") (lookup-key keymap (kbd "TAB"))))
  (define-key keymap (kbd "TAB") nil))

(defun ok-fix-tab-fn (mode)
  "MODE can be a mode or a list of (mode keymap)."
  (-let (((mode map) (if (listp mode) mode (list mode))))
    `(after! ,mode
       (ok-bind-tab-to-TAB
        (or ,map
            (symbol-value (intern-soft (concat (symbol-name ',mode)
                                               "-mode-map"))))))))

(defmacro ok-fix-tab (&rest modes)
  "Binds whatever TAB was bound to to <tab> in the mode's keymap. A mode in
MODES can be a symbol or a list consisting of a symbol and a boolean. If the
boolean is non-nil, also unbinds TAB in that mode."
  (declare (indent 0))
  `(progn ,@(seq-map #'ok-fix-tab-fn modes)))

(ok-fix-tab
  org
  (ivy ivy-minibuffer-map)
  evil-org)

;;; Faces

(defface rainbow-delimiters-depth-10-face '((t (:foreground "#CC00EE"))) "")
(defface rainbow-delimiters-depth-11-face '((t (:foreground "#9933FF"))) "")
(defface clojure-quote-face '((t (:foreground "#ff0088"))) "")
(defface clojure-number-face '((t (:foreground "#d419ff"))) "")
(setq rainbow-delimiters-max-face-count 11)
(custom-set-faces!
  ;; '(font-lock-keyword-face :foreground "#fb2874")
  ;; '(org-headline-done :foreground "#ffffff")
  ;; I had these set before:
  ;; linum
  ;; mode-line
  ;; clojure-interop-method-face
  ;; clojure-pink-face
  '(clojure-keyword-face :foreground "#89f")
  '(font-lock-function-name-face :foreground "#b6ef3e")
  ;; font-lock-keyword-face
  ;; racket-selfeval-face
  '(show-paren-match :background "#333355" :foreground nil)
  '(font-lock-comment-face :foreground "#777777")
  '(font-lock-doc-face :foreground "#8888aa")
  '(hl-fill-column-face :background "#773333")
  '(highlight-numbers-number :foreground "#d419ff" :weight normal)
  '(lsp-face-highlight-textual :background "#202020" :foreground "#fff"
    :weight bold)
  '(org-level-1 :inherit default :foreground "#33dd33")
  '(org-level-2 :inherit default :foreground "#FFb030")
  '(org-level-3 :inherit default :foreground "#FF44FF")
  '(org-level-4 :inherit default :foreground "#00FFFF")
  '(org-level-5 :inherit default :foreground "#FFFF55")
  '(org-level-6 :inherit default :foreground "#FF9966")
  '(org-level-7 :inherit default :foreground "#FFaaBB")
  '(org-level-8 :inherit default :foreground "#ff3333")
  '(rainbow-delimiters-depth-1-face :foreground "#CCCCCC")
  '(rainbow-delimiters-depth-2-face :foreground "#33FF33")
  '(rainbow-delimiters-depth-3-face :foreground "#008800")
  '(rainbow-delimiters-depth-4-face :foreground "#00FFFF")
  '(rainbow-delimiters-depth-5-face :foreground "#0066CC")
  '(rainbow-delimiters-depth-6-face :foreground "#DDFF11")
  '(rainbow-delimiters-depth-7-face :foreground "#FF6000")
  '(rainbow-delimiters-depth-8-face :foreground "#EE0000")
  '(rainbow-delimiters-depth-9-face :foreground "#FF88BB"))
;; '(rainbow-delimiters-depth-10-face :foreground "#CC00EE")
;; '(rainbow-delimiters-depth-11-face :foreground "#9933FF")

;; Disable buggy scrolling when searching
(dolist (fn '(evil-visualstar/begin-search-forward
              evil-visualstar/begin-search-backward
              evil-ex-search-word-forward
              evil-ex-search-word-backward
              evil-ex-search-next
              evil-ex-search-previous
              evil-ex-search-forward
              evil-ex-search-backward))
  (advice-remove fn #'doom-preserve-window-position-a))

;; Don't pop up completion menu after idling in insert mode.
(setq company-idle-delay nil)

;;; Some operators

(defmacro -ok-call-f (pattern replacement)
  `(progn
     (setq end (f beg end ,pattern ,replacement))))

(cl-flet* ((f (beg end pattern replacement)
             (let ((lines (count-lines beg end)))
               (evil-ex-substitute
                beg end
                `(,pattern t t)
                `(replace-eval-replacement . ,replacement))
               (setq end (progn
                           (goto-char beg)
                           (dotimes (i lines) (search-forward "\n"))
                           (point)))
               end))
           (remove-some
             (beg end)
             (-ok-call-f " +," ",")
             (-ok-call-f " +/ +" "/")
             (-ok-call-f " +\\." ".")
             (-ok-call-f "\( +" "(")
             (-ok-call-f "\\[ +" "[")
             (-ok-call-f " +\)" ")")
             (-ok-call-f " +\\]" "]")
             (-ok-call-f " +" " ")
             end))
  (evil-define-operator ok-remove-spaces (beg end)
    :repeat nil
    :move-point nil
    (save-excursion
      (setq end (remove-some beg end))
      (dotimes (i 2)
        (-ok-call-f "\\(\\w\\) +\\(\\w\\)" "\\1\\2"))
      (-ok-call-f ",\\([^ ]\\)" ", \\1")
      (-ok-call-f " +\(" "(")))
  (evil-define-operator ok-remove-some-spaces (beg end)
    :repeat nil
    :move-point nil
    (save-excursion
      (remove-some beg end))))

(defun ok-to-snum ()
  (interactive)
  (let ((n (number-at-point)))
    (delete-forward-char (length (str n)))
    (insert (str (s-repeat n "s(") "0" (s-repeat n ")")))))


(setq webpaste-providers-alist
      '(("ix.io"
         :uri "http://ix.io/"
         :post-field "f:1"
         :lang-uri-separator "/"
         :lang-overrides ((emacs-lisp-mode . "elisp")
                          (nix-mode . "nix"))
         :success-lambda webpaste--providers-success-returned-string)

        ("paste.rs"
         :uri "https://paste.rs"
         :post-field nil
         :success-lambda webpaste--providers-success-returned-string)

        ("dpaste.com"
         :uri "http://dpaste.com/api/v2/"
         :post-data (("title" . "")
                     ("poster" . "")
                     ("expiry_days" . 10))
         :post-field "content"
         :post-lang-field-name "syntax"
         :lang-overrides ((emacs-lisp-mode . "clojure"))
         :success-lambda webpaste--providers-success-location-header)

        ("dpaste.org"
         :uri "https://dpaste.org/api/"
         :post-data (("expires" . 864000))
         :post-field "content"
         :post-lang-field-name "lexer"
         :lang-overrides ((emacs-lisp-mode . "clojure"))
         :success-lambda webpaste--providers-success-returned-string)

        ("paste.mozilla.org"
         :uri "https://paste.mozilla.org/api/"
         :post-data (("expires" . 864000))
         :post-field "content"
         :post-lang-field-name "lexer"
         :lang-overrides ((emacs-lisp-mode . "clojure"))
         :success-lambda webpaste--providers-success-returned-string)

        ("paste.ubuntu.com"
         :uri "https://paste.ubuntu.com/"
         :post-data (("poster" . "webpaste"))  ;; the poster is required
         :post-field "content"
         :post-lang-field-name "syntax"
         :lang-overrides ((emacs-lisp-mode . "emacs"))
         :success-lambda webpaste--providers-success-response-url)

        ("gist.github.com"
         :uri "https://api.github.com/gists"
         :post-field nil
         :post-field-lambda (lambda ()
                              (cl-function
                               (lambda (&key text &allow-other-keys)
                                 (let ((filename (if (buffer-file-name)
                                                     (file-name-nondirectory (buffer-file-name))
                                                   "file.txt")))
                                   (json-encode `(("description" . "Pasted from Emacs with webpaste.el")
                                                  ("public" . "false")
                                                  ("files" .
                                                   ((,filename .
                                                               (("content" . ,text)))))))))))
         :success-lambda (lambda ()
                           (cl-function
                            (lambda (&key data &allow-other-keys)
                              (when data
                                (webpaste--return-url
                                 (cdr (assoc 'html_url (json-read-from-string data)))))))))

        ("bpa.st"
         :uri "https://bpa.st/api/v1/paste"
         :post-data (("expiry" . "10day"))
         :post-field-lambda webpaste--providers-pinnwand-request
         :lang-overrides ((emacs-lisp-mode . "emacs"))
         :success-lambda webpaste--providers-pinnwand-success)))


(evil-define-operator ok-evil-webpaste (beg end)
  :repeat nil
  :move-point nil
  (webpaste-paste-region beg end))

(evil-define-operator ok-evil-reddit-yank (beg end type register yank-handler)
  "Yanks text and adds 4 spaces in front of every line. Works
with evil."
  (evil-yank beg end type register yank-handler)
  (kill-new (replace-regexp-in-string "^" "    " (car kill-ring)) t))

(defun ok-mode-to-language (m)
  "Given an Emacs mode M, returns the language name in markdown
that should be used for code from the mode."
  (let ((mode-alist '((emacs-lisp-mode . "lisp"))))
    (or (cdr (assoc m mode-alist))
        (ok-string-drop-at-end (str m) 5))))


(evil-define-operator lwjgl-to-clj
  (beg end type register yank-handler)
  (println beg end)
  (evil-delete beg end type register yank-handler)
  (->> (car kill-ring)
       (s-replace "_" "-")
       (s-replace-regexp "/GL-" "")
       (s-replace-regexp "/gl" "")
       (s-replace "GL33" "gl/")
       insert)
  (goto-char beg)
  (toggle-case-fold-search)
  (while (re-search-forward "gl/" end t)
    (forward-char)
    (when (s-lowercase? (char-to-string (char-after)))
      (-let (((beg end) (evil-inner-word)))
        (evil-delete beg end)
        (insert (s-dashed-words (car kill-ring))))))
  (toggle-case-fold-search))

(evil-define-operator ok-evil-three-backticks-yank
  (beg end type register yank-handler)
  "Yanks text and adds three backticks around it, as well as a
language after the first backticks, depending on the Emacs mode.
Works with evil."
  :move-point nil
  :repeat nil
  (evil-yank beg end type register yank-handler)
  (kill-new (concat "```" "\n"
                    (car kill-ring)
                    "```")
            t))

(evil-define-motion ok-move-down-15-lines ()
  :type line
  (evil-next-visual-line 15))

(evil-define-motion ok-move-up-15-lines ()
  :type line
  (evil-previous-visual-line 15))

(evil-define-motion ok-move-down-5-lines ()
  :type line
  (evil-next-visual-line 5))

(evil-define-motion ok-move-up-5-lines ()
  :type line
  (evil-previous-visual-line 5))

(defun clean-ns-more (&rest args)
  "Clean up a Clojure ns form more."
  (-let* (((beg end) (esexp-true-toplevel-positions)))
    (while (re-search-forward "(:\\(require\\|use\\|import\\)" end t)
      (re-search-forward " ")
      (when (not (= (char-before (1- (point))) ?\n))
        (backward-char)
        (insert "\n")))
    (goto-char beg)
    ;; wrap naked words in libs in []
    (when (re-search-forward ":require" end t)
      (-let* (((beg end) (esexp-true-form-positions)))
        (while (re-search-forward "^\\s-*[a-z]" end t)
          (backward-char)
          (insert "[")
          (re-search-forward "\\b)*$")
          (while (= ?\) (char-before))
            (backward-char))
          (insert "]")
          (setq end (+ 2 end)))))
    (goto-char beg)
    (when (re-search-forward ":import" end t)
      (-let* (((beg end) (esexp-true-toplevel-positions)))
        (while (re-search-forward "^\\s-*[a-z]" end t)
          (backward-char)
          (insert "(")
          (re-search-forward "$")
          (insert ")")
          (re-search-backward "\\.")
          (replace-match " ")
          (setq end (+ 2 end)))))
    (goto-char beg)
    (when (re-search-forward ":import" end t)
      (-let* (((beg end) (esexp-true-toplevel-positions)))
        (while (re-search-forward "\\[" end t)
          (replace-match "(")
          (re-search-forward "\\]")
          (replace-match ")"))))
    (indent-region beg end)))

(evil-define-command ok-run-q-macro (count)
  (interactive "<c>")
  :repeat nil
  (evil-execute-macro count (evil-get-register ?q)))

(defun ok-clean-ns (&rest args)
  (interactive)
  (let ((utils? (save-excursion
                  (goto-char (point-min))
                  (or
                   (re-search-forward "game.utils :refer :all" nil t)
                   (re-search-forward ":use game.utils" nil t)
                   (re-search-forward "oskarkv.utils :refer :all" nil t)
                   (re-search-forward ":use oskarkv.utils" nil t)))))
    (cljr--ensure-op-supported "clean-ns")
    (cider-eval-ns-form)
    (cljr--clean-ns nil nil)
    (when utils?
      (save-excursion
        (goto-char (point-min))
        (re-search-forward ":require" nil t)
        (insert "[game.utils :refer :all]"))
      (cljr--clean-ns nil t))))


;;; Settings

;; Buffer stuff, sigh...

;; Add buffers you want to be able to switch to in this function. There is also
;; doom-mark-as-real-buffer-h that can be used, but this seems easier.
(defun ok-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (doom-real-buffer-p buf)
      ;; Makes *scratch* a buffer to switch to
      ;; (eq buf (doom-fallback-buffer))
      (-some? (lambda (x) (s-starts-with? x (buffer-name buf)))
              (list
               "*Python"
               "*cider-repl"))))

(defun ok-set-buffer-frame-predicate (&rest args)
  (setcdr (assoc 'buffer-predicate default-frame-alist)
          'ok-buffer-frame-predicate))

(advice-add 'doom-init-ui-h :after #'ok-set-buffer-frame-predicate)

;; To be able to switch to all buffers, empty this list.
;; (setq-default doom-unreal-buffer-functions
;; '(minibufferp doom-special-buffer-p doom-non-file-visiting-buffer-p))

;; (defadvice message (after message-tail activate)
;;   "Goto point max after a message."
;;   (with-current-buffer "*Messages*"
;;     (goto-char (point-max))
;;     (walk-windows
;;      (lambda (window)
;;        (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
;;            (set-window-point window (point-max))))
;;      nil
;;      t)))

(defadvice eval-region (after eval-region-message activate)
  (message "evaled region"))

;; Make which-key show help for Vim operators
(setq which-key-show-operator-state-maps nil)

;; Disable using cljfmt on save and getting error about it
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode
        sql-mode
        tex-mode
        latex-mode
        clojure-mode
        cider-mode
        python-mode))

;; (setq rcirc-server-alist
;;       '(("irc.freenode.net"
;;          :channels ("#rcirc" "#emacs" "#evil-mode")))
;;       rcirc-default-nick "tufflax"
;;       rcirc-authinfo
;;       (list (seq-concatenate
;;              'list
;;              '("freenode" nickserv "tufflax")
;;              (list (ok-get-string-from-file "~/.doom.d/.freenode-pass")))))

(setq webpaste-provider-priority
      '("dpaste.com" "gist.github.com" "paste.mozilla.org" "dpaste.org"
        "ix.io" "paste.pound-python.org"))

;; Avoid double \\ in re-builder
(setq reb-re-syntax 'string)

(setq +evil-want-o/O-to-continue-comments nil)

(after! evil-org
  ;; Cycle between 3 states on a heading (folded, subheadings, all)
  (setq org-tab-first-hook
        (delete '+org-cycle-only-current-subtree-h org-tab-first-hook)))

(after! git
  (setq git-commit-summary-max-length 50))

(after! clj-refactor
  (apply #'general-define-key
         :keymaps 'ok-clj-refactor-map
         (-reduce-from (-lambda (list (key fn))
                         (cons key (cons fn list)))
                       nil
                       cljr--all-helpers))

  (map! :map ok-clj-refactor-map
        "nn" (cmd (cljr--clean-ns nil :no-prune))
        "cn" 'ok-clean-ns)

  ;; (advice-add 'cljr--clean-ns :after #'clean-ns-more)
  )

(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'fci-mode)

(after! fill-column-indicator
  (setq fci-rule-color "#602020")
  (setq fci-rule-width 5))

(after! magit
  (defun magit-diff-master ()
    "Diff with master"
    (interactive)
    (apply #'magit-diff-range "main" (magit-diff-arguments)))

  (defun magit-diff-dev ()
    "Diff with dev"
    (interactive)
    (apply #'magit-diff-range "dev" (magit-diff-arguments)))

  (transient-define-prefix magit-diff ()
    "Show changes between different versions."
    :man-page "git-diff"
    :class 'magit-diff-prefix
    ["Limit arguments"
     (magit:--)
     (magit-diff:--ignore-submodules)
     ("-b" "Ignore whitespace changes"      ("-b" "--ignore-space-change"))
     ("-w" "Ignore all whitespace"          ("-w" "--ignore-all-space"))
     (5 "-D" "Omit preimage for deletes"    ("-D" "--irreversible-delete"))]
    ["Context arguments"
     (magit-diff:-U)
     ("-W" "Show surrounding functions"     ("-W" "--function-context"))]
    ["Tune arguments"
     (magit-diff:--diff-algorithm)
     (magit-diff:-M)
     (magit-diff:-C)
     ("-x" "Disallow external diff drivers" "--no-ext-diff")
     ("-s" "Show stats"                     "--stat")
     ("=g" "Show signature"                 "--show-signature")
     (5 magit-diff:--color-moved)
     (5 magit-diff:--color-moved-ws)]
    ["Actions"
     [("d" "Dwim"          magit-diff-dwim)
      ("r" "Diff range"    magit-diff-range)
      ("p" "Diff paths"    magit-diff-paths)]
     [("u" "Diff unstaged" magit-diff-unstaged)
      ("s" "Diff staged"   magit-diff-staged)
      ("w" "Diff worktree" magit-diff-working-tree)]
     [("c" "Show commit"   magit-show-commit)
      ("t" "Show stash"    magit-stash-show)
      ("m" "Diff with master" magit-diff-master)
      ("v" "Diff with dev" magit-diff-dev)]])

  (setq magit-diff-refine-hunk nil)
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))

(setq-default
 ;;; Scrolling
 scroll-margin 5
 ;;; Backups
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 2
 version-control t
 ;;; Other
 evil-auto-indent t
 ;; ag-highlight-search t
 abbrev-mode t
 evil-cross-lines t
 evil-ex-substitute-global t
 column-number-mode t
 indent-tabs-mode nil
 inhibit-startup-screen t
 tab-width 4
 python-guess-indent nil
 python-fill-docstring-style 'pep-257-nn
 python-indent-guess-indent-offset nil
 ;; disabled because it's unnecessary for formatting clojure comments as
 ;; markdown
 markdown-enable-math nil)

(after! ace-window
  (setq aw-scope 'global))

(define-abbrev-table 'global-abbrev-table
  '(
    ;; ("tex" "t.ex.")
    ("dvs" "d.v.s.")
    ("osv" "o.s.v.")
    ("pga" "p.g.a.")
    ))

(add-hook! 'minibuffer-setup-hook (abbrev-mode -1))

;; Define modes for file extensions.
(add-to-list 'auto-mode-alist '("\\.joke\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
;; Don't ask before quitting
(setq confirm-kill-emacs nil)

;; Make cider-find-var be considered a jump in evil.
(evil-add-command-properties #'cider-find-var :jump t)

;; Makes lines wrap instead of going off screen
(global-visual-line-mode)

;; Make *format-all-errors* use visual-line-mode, it doesn't respect the global
;; setting.
(defadvice! wrap-errors (&rest _)
  :after #'format-all--show-or-hide-errors
  (with-current-buffer (get-buffer "*format-all-errors*")
    (visual-line-mode +1)))

;; Makes evil snipe (t, f, s) search in whole buffer instead of just the current
;; line
(setq evil-snipe-scope 'buffer)
(setq evil-snipe-repeat-scope 'buffer)

;; Start frame X columns wide
(add-to-list 'default-frame-alist '(width . 91))

(after! org
  ;; (remove-hook! 'org-tab-first-hook #'+org-yas-expand-maybe-h)
  (setq org-clock-clocked-in-display nil
        org-M-RET-may-split-line nil
        org-export-with-smart-quotes nil
        org-export-with-sub-superscripts nil
        org-enforce-todo-dependencies nil
        org-enforce-todo-checkbox-dependencies nil
        org-fontify-done-headline nil
        org-todo-keywords '((sequence
                             "TODO(t)" "INPR(p)" "WAIT(w)" "|" "DONE(d)"))
        org-todo-keyword-faces '(("TODO" . "#f00") ("INPR" . "#fff")
                                 ("WAIT" . "#33f") ("DONE" . "#0f0")))
  (add-hook! 'org-mode-hook
    (setq paragraph-separate "[ 	\f]*$"
          paragraph-start "\f\\|[ 	]*$")
    (fci-mode -1)
    ;; global-visual-line-mode isn't enough for org, apparently
    (visual-line-mode 1))

  (setq-hook! 'org-mode-hook evil-auto-indent nil)

  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t
           ("pdflatex"))
          ("T1" "fontenc" t
           ("pdflatex"))
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "amssymb" t)
          ("" "capt-of" nil)
          ("colorlinks=true" "hyperref" nil))))

(after! ox-latex
  (add-to-list 'org-latex-classes
               `("myclass"
                 ,(str "\\documentclass[11pt]{article}\n"
                       "\\addtolength{\\parskip}{\\baselineskip}\n"
                       "\\setlength{\\parindent}{0 em}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "myclass"))

;;; States

(add-to-list 'evil-emacs-state-modes 'term-mode)
(delete 'term-mode evil-insert-state-modes)
(add-hook 'cider-inspector-mode-hook 'evil-emacs-state)
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;;; Misc

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode-disable)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode-disable)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook! 'text-mode-hook (setq fill-column 64))
(add-hook! 'term-mode-hook (yas-minor-mode -1) (setq yas-dont-activate t))

(add-hook! 'text-mode-hook #'olivetti-mode)
(add-hook! 'text-mode-hook #'auto-fill-mode)
(add-hook! 'text-mode-hook (setq olivetti-body-width (+ fill-column 12)))
(add-hook! 'html-mode-hook (setq fill-column 80))
;; (add-hook! 'org-mode-hook #'olivetti-mode)
;; (add-hook! 'org-mode-hook #'auto-fill-mode)
;; (add-hook! 'org-mode-hook (setq olivetti-body-width (+ fill-column 12)))
;; (add-hook! 'markdown-mode-hook #'olivetti-mode)
;; (add-hook! 'markdown-mode-hook #'auto-fill-mode)
;; (add-hook! 'markdown-mode-hook (setq olivetti-body-width (+ fill-column 12)))

;;; Babashka and Joker

(defun ok-clojure-mode-fn ()
  "If we are in a joker or babashka file, activate clojure-mode."
  (if (or (string-match-p "env joker" (ok-read-first-line))
          (string-match-p "env bb" (ok-read-first-line)))
      (clojure-mode)))

(add-hook 'find-file-hook #'ok-clojure-mode-fn)

(after! ace-window
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h)))

(after! clojure-mode
  ;; A bug in my version, fixed in later versions (the symbol-end was missing)
  (setq clojure-namespace-regexp
        (rx line-start "(" (? "clojure.core/") (or "in-ns" "ns" "ns+")
            symbol-end))
  ;; LSP
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (add-hook 'clojurescript-mode-hook #'lsp-deferred)

  (add-hook! 'clojure-mode-hook (highlight-numbers-mode -1))
  (setq clojure-indent-style :always-align
        clojure-docstring-fill-column 72)

  (setq-default clojure-docstring-fill-prefix-width 3)

  ;; Docstring positions in def forms
  (put 'defconst 'clojure-doc-string-elt 2)
  (put 'def- 'clojure-doc-string-elt 2)
  (put 'defmacro- 'clojure-doc-string-elt 2)
  (put 'defmacro! 'clojure-doc-string-elt 2)

  (define-clojure-indent
    (-> 1)
    (->$ 1)
    (->> 1)
    (->>$ 1)
    (add-event-handler 3)
    (are 0)
    (as-> 1)
    (call-update-fns 2)
    (call-update-fns* 2)
    (cond* 0)
    (cond->$ 1)
    (cond->>$ 1)
    (condf 1)
    (condp* 2)
    (defs 0)
    (deftype- '(2 nil nil (:defn)))
    (defnav '(:defn nil nil :defn))
    (do-every-ms 2)
    (do1 0)
    (docstring-fix 0)
    (dosync 0)
    (dotimes* 1)
    (error-printing-future :defn)
    (event-handler 1)
    (fx-run-later 0)
    (if-lets 1)
    (ignore-exception 0)
    (macrolet '(1 (:defn)))
    (templatelet '(1 (:defn)))
    (templatelet-rm-nils '(1 (:defn)))
    (once-only 1)
    (recursive-path 2)
    (some-> :defn)
    (some->> :defn)
    (start-new-thread 1)
    (symbol-macrolet 1)
    (take-at-least-ms 1)
    (try :defn)
    (unqualify-syms 1)
    (when-lets 1)
    (while-let 1)
    (with-gensyms 1)))

(after! cider
  (set-popup-rules!
    '(("^\\*cider-error*" :ignore t)
      ("^\\*cider-repl" :ignore t)
      ("^\\*cider-repl-history" :vslot 2 :ttl nil)))
  (setq-default
   cider-repl-result-prefix ""
   cider-font-lock-reader-conditionals nil
   cider-pprint-fn 'fipp
   cider-repl-use-pretty-printing t
   cider-font-lock-dynamically t
   cider-prompt-for-symbol nil))

(defvar ok-clj-refactor-map (make-sparse-keymap))
(defvar -visual-inside-keymap (lookup-key evil-visual-state-map "i"))
(defvar -operator-inside-keymap (lookup-key evil-operator-state-map "i"))

(defmacro ok-eval-form-builder (sender)
  `(lambda ()
     (interactive)
     (-let* (((b e) (esexp-true-form-positions)))
       (,sender b e))))

(defalias 'ok-eval-form (ok-eval-form-builder eval-region))

(defalias 'ok-cider-eval-form (ok-eval-form-builder cider-eval-region))

(defun ok-cider-eval (form-string)
  (cider-interactive-eval form-string nil nil (cider--nrepl-pr-request-map)))

(defun ok-before-doctring-p ()
  (save-excursion
    (re-search-forward "[^\s-]")
    (clojure-in-docstring-p)))

(defun ok-clojure-docstring-indent ()
  "Returns how many columns more than the usual 2 the docstring at point is
indented."
  (save-excursion
    (paredit-backward-up)
    (- (point) (bol))))

(defun ok-clojure-comment-indent ()
  (save-excursion
    (evil-execute-macro 1 "^")
    ;; -3 to adjust for the
    (- (point) (bol))))

(defun ok-clojure-fill-paragraph ()
  (interactive)
  (save-excursion
    (if (ok-before-doctring-p)
        (let ((fill-column (- clojure-docstring-fill-column
                              (ok-clojure-docstring-indent) 1)))
          (re-search-forward "[^\s-]")
          (ok-fill-docstring-as-markdown))
      (if (or (clojure--in-comment-p)
              (progn (goto-char (bol)) (looking-at-p "\s*;")))
          (let ((fill-column (- clojure-docstring-fill-column
                                (ok-clojure-comment-indent) 3)))
            (ok-fill-comment-as-markdown))
        (clojure-fill-paragraph)))
    (when (ignore-errors (paredit-backward-up))
      (when (> (- (point) (bol)) 2)
        (forward-char)
        (evil-execute-macro 1 "=as")))))

(defun ok-rebind-in-all-maps* (start end exclude-list to from)
  (mapatoms (lambda (sym)
              (if (and (boundp sym)
                       (not (seq-contains exclude-list sym))
                       (keymapp (symbol-value sym))
                       (s-ends-with-p end (symbol-name sym))
                       (s-starts-with-p start (symbol-name sym)))
                  (let* ((km (symbol-value sym)))
                    (define-key km (kbd to)
                                (or (and from (lookup-key km (kbd from)))
                                    nil)))))
            obarray))

(defun ok-rebind-in-all-maps (start end exclude-list &rest to-froms)
  (apply #'ok-rebind-in-all-maps* start end exclude-list (seq-take to-froms 2))
  (if (not (seq-empty-p (seq-drop to-froms 2)))
      (apply #'ok-rebind-in-all-maps start end exclude-list
             (seq-drop to-froms 2))))

(defun ok-wrap-word-in-backticks ()
  (interactive)
  (save-excursion
    (let* ((sep "[][(){}[:space:]\n\r,\"]"))
      (re-search-backward sep)
      (forward-char)
      (insert "`")
      (re-search-forward sep)
      (backward-char)
      (insert "`"))))

(map! :map lsp-signature-mode-map
      :n "M-p" nil
      "M-p" nil)

(after! yasnippet
  (map! :map yas-minor-mode-map
        :i "s-<tab>" yas-maybe-expand
        )
  (add-hook 'yas-minor-mode-hook
            (fn (yas-activate-extra-mode 'fundamental-mode))))

(map! :i "C-§" (cmd (insert "§"))
      :e "C-," 'evil-exit-emacs-state
      :o "l" -operator-inside-keymap
      :o "i" nil
      :o "o" (cmd (forward-evil-symbol))
      :o "e" 'evil-next-line
      :o "u" 'evil-previous-line
      :ir "ESC" 'doom/escape
      :ir "<escape>" 'doom/escape
      :i [tab] 'complete-symbol
      :i "§" 'evil-normal-state
      :v "§" 'evil-exit-visual-state
      :i "C-e" 'doom/delete-backward-word
      :i "C-<tab>" (cmd (insert "\t"))
      :r "§" 'evil-normal-state
      :nmvi "TAB" nil
      "C-i" 'end-of-line
      "C-n" 'beginning-of-line
      "C-s" 'save-buffer
      "C-ä" 'other-window
      "C-å" 'ace-window
      "M-c" 'evil-snipe-repeat-reverse
      "M-v" 'evil-snipe-repeat
      "M-f" 'switch-to-prev-buffer
      "M-p" 'switch-to-next-buffer
      "M-w" 'er/expand-region
      :nv "gB" 'ok-remove-some-spaces
      :nv "gb" 'ok-yank-without-filling
      :nv "gh" 'ok-evil-webpaste
      :nv "go" '+evil:yank-unindented
      ;; Makes gs work with a motion command afterwards
      :nvm "gs" nil
      ;; OLD: Last I checked, gs didn't work with motion afterwards without the m
      ;; below. But I remember something else being wrong with m, but not
      ;; exactly what.
      ;;
      ;; NEW: It didn't work to do gse (down 2 lines) with m, but it worked
      ;; after remvoing m.
      :nv "gs" 'ok-evil-three-backticks-yank
      :nvm "gy" 'ok-evil-reddit-yank
      :nvm "gq" 'ok-org-quote-block
      :n "§" (cmd (evil-ex-nohighlight) (evil-force-normal-state))
      :n "C-n" nil
      :n "u" nil
      :n "e" nil
      :n "j" nil
      :n "J" nil
      :n "M" nil
      :n "n" nil
      :n "N" nil
      :n "h" nil
      :n "H" nil
      :n "i" nil
      :n "I" nil
      :n "SPC" nil
      :n "zx" 'evil-delete-buffer
      :n "k" 'undo
      :n "E" 'ok-evil-join
      :n "Å" 'newline-and-indent
      :n "l" 'evil-insert
      :n "L" 'evil-insert-line
      :n "C-x" 'evil-numbers/inc-at-pt
      :n "C-z" 'evil-numbers/dec-at-pt
      :n "M-t" #'ok-run-q-macro
      :m "SPC" nil
      :m "k" nil
      :m "E" nil
      :m "l" nil
      :m "L" nil
      :m "u" 'evil-previous-visual-line
      :m "e" 'evil-next-visual-line
      :m "C-u" 'ok-move-up-15-lines
      :m "C-e" 'ok-move-down-15-lines
      :m "j" 'evil-forward-word-end
      :m "J" 'evil-forward-WORD-end
      :m "n" 'evil-backward-char
      :m "h" 'evil-ex-search-next
      :m "H" 'evil-ex-search-previous
      :m "i" 'evil-forward-char
      :m "å" 'ace-window
      :m "ä" 'evil-window-next
      :m "Ä" 'evil-window-prev
      :m "C-q" 'evil-visual-block
      :m "<C-i>" nil
      ;; :m "C-y" 'evil-jump-backward
      ;; :m "C-o" 'evil-jump-forward
      :m "C-y" 'better-jumper-jump-backward
      :m "C-o" 'better-jumper-jump-forward
      :n "M-y" 'goto-last-change
      :n "M-o" 'goto-last-change-reverse
      :m "C-," 'evil-emacs-state
      :v "u" nil
      :v "l" -visual-inside-keymap
      :v "i" nil
      :v "L" 'evil-insert
      :v "A" 'evil-append
      :prefix "SPC"
      :n "v" 'browse-url-at-point
      :nv "gl" 'magit-log-buffer-file
      :n "gb" 'magit-blame-addition
      :n "q" 'ok-wrap-word-in-backticks
      :nv "f" 'fill-paragraph)

(map! :map cider-popup-buffer-mode-map
      :n "q" 'cider-popup-buffer-quit)

(map! :map makefile-mode-map
      "M-p" nil)

(map! :after company
      :map company-active-map
      "C-e" 'company-select-next
      "C-u" 'company-select-previous)

(map! :map comint-mode-map
      "," nil
      "M-f" nil
      "M-p" nil)

(map! :map help-mode-map
      :n "i" nil)

(defun ok-hy-eval-toplevel ()
  (interactive)
  (save-excursion
    (-let* (((b e) (esexp-true-toplevel-positions)))
      (goto-char (1- e))
      (hy-shell-eval-current-form))))

(map! :after hy-mode
      :map hy-mode-map
      :prefix "SPC"
      :n "ef" 'hy-shell-eval-current-form
      :n "eb" 'hy-shell-eval-buffer
      :n "et" 'ok-hy-eval-toplevel
      :n "dd" 'hy-describe-thing-at-point
      :n "jj" 'run-hy)

(setq hy-shell--interpreter-args
      '("--spy"))

(after! lsp-mode
  (setq lsp-lens-enable nil)
  (add-hook! 'lsp-mode-hook (lsp-ui-mode 0))
  ;; Otherwise lsp broke regular indentation with =
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'jedi)
  (add-to-list 'lsp-enabled-clients 'clojure-lsp)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("clojure-lsp"))
    :activation-fn (lsp-activate-on "clojure")
    :server-id 'clojure-lsp)))

(after! sly
  (advice-add 'sly-show-description
              :after (fn (ok-switch-to-window "*sly-description*"))))

;; (defmacro ok-sly-switch-window-after (command)
;;   `(cmd (call-interactively #',command)
;;      (call-interactively (cmd (ok-switch-to-window "*sly-description*")))))
;;      ;; (run-at-time 0.1 nil #'ok-switch-to-window "*sly-description*")))

(map! :after sly
      :map sly-mode-map
      :prefix "SPC"
      :n "et" 'sly-eval-defun
      :v "er" 'sly-eval-region
      :n "eb" 'sly-eval-buffer
      :n "ct" 'sly-compile-defun
      :n "cb" 'sly-compile-and-load-file
      :n "sw" 'sly-edit-definition
      :n "dd" 'sly-documentation
      :n "df" 'sly-describe-function
      :n "ds" 'sly-describe-symbol
      :n "dh" 'sly-hyperspec-lookup
      :n "dm" 'hyperspec-lookup-reader-macro
      :n "do" 'hyperspec-lookup-format
      :n "aa" 'sly-apropos
      :n "sr" 'sly-who-calls
      ;; :n "ww" 'sly-calls-who
      :n "mm" 'sly-macroexpand-1
      :n "ma" 'sly-macroexpand-all
      :n "ej" 'sly
      :n "rr" (cmd (other-window 1) (call-interactively #'sly-mrepl))
      :map sly-editing-mode-map
      :n "M-p" nil)

(after! python
  (require 'dap-python)
  (require 'elpy)
  (setq dap-python-debugger 'debugpy)
  (add-hook! 'python-mode-hook
    (setenv "PAGER" "cat")
    (require 'lsp-jedi)
    (tree-sitter-mode)
    (ok-set-jedi-extra-paths)
    (lsp)
    (setq-local dap-python-executable (with-venv (executable-find "python")))
    (lsp-ui-mode 0))
  ;; Don't format with LSP formatter
  (setq-hook! 'python-mode-hook +format-with-lsp nil))

;; Defined in esexp
(map! :map evil-outer-text-objects-map
      "l" 'ok-python-thing-text-object
      :map evil-inner-text-objects-map
      "l" 'ok-python-thing-text-object)

(defun ok-python-insert-breakpoint ()
  (interactive)
  (evil-open-below 1)
  (evil-normal-state)
  ;; (insert "breakpoint()")
  (insert "import pdb; pdb.set_trace()")
  (save-buffer)
  (evil-indent (point-at-bol) (point-at-eol)))

(setq elpy-test-runner 'elpy-test-pytest-runner
      elpy-test-compilation-function 'ok-test-function)

(map! :map latex-mode-map
      :prefix "SPC"
      :n "et" (cmd (shell-command "make")))

(map! :after python
      :map (python-mode-map
            inferior-python-mode-map)
      :n "SPC v" 'tsexp-test
      :n "M-i" 'tsexp-transpose-atom-forward
      :n "M-n" 'tsexp-transpose-atom-backward
      :n "C-M-i" 'tsexp-transpose-container-forward
      :n "C-M-n" 'tsexp-transpose-container-backward
      :n "M-I" 'tsexp-transpose-ccontainer-forward
      :n "M-N" 'tsexp-transpose-ccontainer-backward
      :n "M-u" 'tsexp-raise-atom
      :n "C-M-u" 'tsexp-raise-container
      :n "M-U" 'tsexp-raise-ccontainer
      :prefix "SPC"
      :n "pp" 'ok-python-import-pprint
      :n "m" '+make/run
      :n "a" 'tsexp-wrap-element-string-beg
      :n "A" 'tsexp-wrap-element-string-beg
      :n "l" 'tsexp-wrap-form-parens-beg
      :n "L" 'tsexp-wrap-form-parens-end
      :n "w" 'tsexp-wrap-element-parens-beg
      :n "W" 'tsexp-wrap-element-parens-end
      :n "e[" 'tsexp-wrap-element-brackets-beg
      :n "e]" 'tsexp-wrap-element-brackets-end
      :n "e{" 'tsexp-wrap-element-braces-beg
      :n "e}" 'tsexp-wrap-element-braces-end
      :n "[" 'tsexp-wrap-form-brackets-beg
      :n "]" 'tsexp-wrap-form-brackets-end
      :n "{" 'tsexp-wrap-form-braces-beg
      :n "}" 'tsexp-wrap-form-braces-end
      :n "f" 'python-fill-paragraph
      :n "ps" 'projectile-run-eshell
      :n "tk" (cmd (shell-command "pkill -f \"sleep 10000\""))
      ;; :n "cf" '+format/buffer
      :n "cf" (cmd (save-buffer) (ok-python-isort buffer-file-name)
                (save-buffer) (ok-python-black)
                ;; Load the buffer from file, like with :e
                (evil-edit nil))
      :n "tt" 'elpy-test-pytest-runner
      :n "sw" 'lsp-find-definition
      :n "sr" 'lsp-find-references
      :n "r" (cmd (call-interactively #'lsp-rename)
               (ok-save-buffers-matching "\\.py$"))
      :n "dd" 'lsp-describe-thing-at-point
      :n "ca" 'lsp-execute-code-action
      :n "cr" 'lsp-rename
      :n "jj" 'run-python
      :n "ed" 'ok-python-insert-breakpoint
      ;; :n "ed" (cmd (call-interactively #'dap-breakpoint-add)
      ;;           (call-interactively #'dap-debug))
      :n "eb" 'python-shell-send-buffer
      :n "ef" 'python-shell-send-defun
      :n "et"  (cmd (python-shell-send-region (point-at-bol) (point-at-eol)))
      :v "et" 'python-shell-send-region)

(map! :map minibuffer-local-map
      "C-n" 'evil-backward-word-begin
      "C-i" 'evil-forward-word-end
      "M-n" 'doom/delete-backward-word)

(defun open-prolog-history ()
  (interactive)
  (evil-window-vsplit)
  (evil-window-next nil)
  (switch-to-buffer "*ediprolog-history*")
  (goto-char (point-max))
  (evil-window-prev nil))

(defun ok-delete-ediprolog-consult ()
  (interactive)
  (delete-window (get-buffer-window "*ediprolog-consult*")))

(defun ediprolog-kill-process ()
  (interactive)
  (save-window-excursion
    (shell-command "pkill --signal 9 -f /usr/bin/prolog")))

(defun -ediprolog-ensure-no-trace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward "?- trace," (+ (point) 12) t)
        (delete-backward-char 7))))

(defun -ediprolog-ensure-trace ()
  (save-excursion
    (beginning-of-line)
    (if (search-forward "?- " (+ (point) 10) t)
        (insert "trace, ")
      (message "Not on a query!"))))

(defun ediprolog-toggle-trace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward "?- trace," (+ (point) 12) t)
        (delete-backward-char 7)
      (-ediprolog-ensure-trace))))

(defun ediprolog-debug-query ()
  (interactive)
  (ediprolog-toggle-trace)
  (ediprolog-query))

(defun ediprolog-eval-query ()
  (interactive)
  (-ediprolog-ensure-no-trace)
  (ediprolog-dwim))

(after! ediprolog
  (setq ediprolog-system 'swi
        ediprolog-program "/usr/bin/prolog"))

(defun ok-fix-art-of-prolog-paste ()
  (interactive)
  (evil-execute-macro
   1
   (kmacro-lambda-form [?v ?a ?p
                           ?: ?s ?/ ?  ?/ return
                           ?g ?v ?: ?s ?/ ?I ?/ ?| ?/ return
                           ?g ?v ?: ?s ?/ ?! ?/ ?| ?/ return
                           ?g ?v ?: ?s ?/ ?- ?/ ?: ?- return
                           ?g ?v ?=] 0 "%d")))

;; (map! :after prolog
;;       :map prolog-inferior-mode-map
;;       :i "C-SPC" (cmd (insert ";") (comint-send-input)))
;;       :map prolog-mode-map
;;       :prefix "SPC"
;;       :n "t" 'ediprolog-toggle-trace
;;       :n "o" 'ediprolog-toplevel
;;       :n "c" 'ediprolog-consult
;;       :n "k" 'ediprolog-kill-process
;;       :n "r" 'ediprolog-remove-interactions
;;       :n "s" 'ok-to-snum
;;       :n "et" 'ediprolog-eval-query
;;       :n "ed" 'ediprolog-debug-query
;;       :n "eb" 'ediprolog-consult
;;       :n "h" 'open-prolog-history
;;       :n "a" 'ok-fix-art-of-prolog-paste
;;       :n "d" 'ok-delete-ediprolog-consult)

(defun ok-prolog-predicate-at-point ()
  (interactive)
  (save-excursion
    (let* ((beg (progn (evil-backward-WORD-begin) (point)))
           (end (progn (evil-jump-item) (point)))
           (open (progn (evil-jump-item) (point)))
           (name (buffer-substring-no-properties beg open))
           (args (buffer-substring-no-properties open (1+ end))))
      (str name "/" (length (read args))))))

(defun ok-prolog-send-string (s)
  (let ((s (str s "\n")))
    ;; Inserts it for the user to see.
    (prolog-process-insert-string (get-process "prolog") s)
    ;; Actually sends it.
    (process-send-string "prolog" s)))

(defun ok-prolog-apropos ()
  (interactive)
  (ok-prolog-send-string (str "apropos(" (read-string "Search for: ") ").")))

(defun ok-prolog-help ()
  (interactive)
  (ok-prolog-send-string (str "help(" (read-string "Help for: ") ").")))

(map! :after prolog
      :map prolog-inferior-mode-map
      :i "C-SPC" (cmd (ok-prolog-send-string ";"))
      :n "C-f" (cmd (process-send-string "prolog" " \n"))
      :map prolog-mode-map
      :prefix "SPC"
      :n "jj" 'run-prolog
      :n "ds" (cmd (ok-prolog-send-string
                    (str "spy(" (ok-prolog-predicate-at-point) ").")))
      :n "dn" (cmd (ok-prolog-send-string "nospyall."))
      :n "eb" 'prolog-consult-buffer
      :map (prolog-inferior-mode-map prolog-mode-map)
      :n "dt" 'prolog-trace-on
      :n "do" 'prolog-trace-off
      :n "dd" 'ok-prolog-help
      :n "da" 'ok-prolog-apropos)

(map! :after markdown-mode
      :map markdown-mode-map
      "M-p" nil)

(defun ok-org-fix-blank-lines ()
  (interactive)
  (goto-char 1)
  (while (re-search-forward "^\*+\s" nil t)
    (goto-char (match-beginning 0))
    (insert "\n")
    (forward-char)
    (goto-char (1+ (eol)))
    (insert "\n"))
  (goto-char 1)
  (while (re-search-forward "#\\+BEGIN_SRC" nil t)
    (goto-char (match-beginning 0))
    (insert "\n")
    (re-search-forward "#\\+END_SRC" nil t)
    (forward-char)
    (insert "\n"))
  (replace-regexp-in-region "\n\n\n+" "\n\n" 1)
  (goto-char 1)
  (while (looking-at-p "$")
    (kill-whole-line)))

(defun ok-org-clean-buffer ()
  (interactive)
  (save-excursion
    (ok-org-fill-buffer-excluding-code-blocks)
    (ok-org-fix-blank-lines)))

(map! :after org
      :map org-mode-map
      ;; motion state
      :m "M-r" 'outline-next-visible-heading
      :m "M-w" 'outline-previous-visible-heading
      :m "M-s" 'org-forward-heading-same-level
      :m "M-a" 'org-backward-heading-same-level
      :m "M-q" (cmd (ignore-errors (outline-up-heading 1)))
      ;; normal state
      :n "C-M-n" 'org-shiftmetaleft
      :n "C-M-i" 'org-shiftmetaright
      :n "C-M-u" 'org-shiftmetaup
      :n "C-M-e" 'org-shiftmetadown
      :n "M-n" 'org-metaleft
      :n "M-i" 'org-metaright
      :n "M-u" 'org-metaup
      :n "M-e" 'org-metadown
      :n "C-n" 'org-shiftleft
      :n "C-i" 'org-shiftright
      :i [tab] 'org-cycle
      :prefix "SPC"
      :n "<tab>" (cmd (org-cycle '(64)))
      :n "v" (cmd (org-insert-todo-heading-respect-content) (evil-append 1))
      :n "b" (cmd (org-insert-heading-respect-content) (evil-append 1))
      :n "p" (cmd (org-insert-todo-heading '(4)) (evil-append 1))
      :n "g" (cmd (org-insert-heading) (evil-append 1))
      :n "t" (cmd (org-insert-todo-subheading '(4)) (evil-append 1))
      :n "d" (cmd (org-insert-subheading nil) (evil-append 1))
      :n "n" 'org-narrow-to-subtree
      :n "w" 'widen
      :n "a" 'org-sparse-tree
      :n "m" 'org-refile
      :n "f" 'org-fill-paragraph
      :n "o" 'org-open-at-point
      :n "F" 'ok-org-clean-buffer
      :n "l" (cmd
               (setq current-prefix-arg '(4)) ; C-u
               (call-interactively 'org-insert-link))
      :n "u" 'org-insert-link
      :n "e" 'org-edit-special
      :n "i" '(lambda (s) (interactive "sCustom ID: ")
                (org-set-property "CUSTOM_ID" s))
      :v "t" 'org-table-create-or-convert-from-region
      :map org-src-mode-map
      :n "ce" 'org-edit-src-exit
      :n "cc" 'org-edit-src-abort
      )

(defun ok-make-link-description (link desc)
  (seq-elt (seq-reverse (s-split "[/:.]" link)) 1))

(setq org-link-make-description-function #'ok-make-link-description)

(map! :after evil-org
      :map evil-org-mode-map
      :i "C-h" nil
      ;; :i "<return>" 'org-return
      :i "<return>" 'newline-and-indent
      :vo "ie" nil
      :vo "iE" nil
      :vo "ir" nil
      :vo "iR" nil
      :vo "i" nil
      :vo "le" #'evil-org-inner-object
      :vo "lE" #'evil-org-inner-element
      :vo "lr" #'evil-org-inner-greater-element
      :vo "lR" #'evil-org-inner-subtree
      :nv "TAB" nil
      :prefix "SPC"
      :nvm "c" 'ok-org-quote-block)

(map! :after ivy
      :map (ivy-minibuffer-map ivy-switch-buffer-map)
      "<C-backspace>" nil
      "M-DEL" nil
      "C-u" 'ivy-previous-line
      "C-e" 'ivy-next-line
      "C-S-e" 'ivy-scroll-up-command
      "C-S-u" 'ivy-scroll-down-command
      "RET" 'ivy-alt-done
      "<return>" 'ivy-alt-done
      "C-<return>" 'ivy-immediate-done
      "C-RET" 'ivy-immediate-done
      "!" (cmd (insert "\\!")))

(map! :map key-translation-map
      "ESC" (kbd "C-g"))

;; Because evil-join has advice and is buggy.
(evil-define-operator ok-evil-join (beg end)
  "Join the selected lines."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (join-line 1))))

(evil-define-operator ok-org-quote-block (beg end type register yank-handler)
  :type line
  :repeat nil
  :move-point nil
  (goto-char end)
  (insert "#+END_QUOTE")
  (goto-char beg)
  (insert "#+BEGIN_QUOTE\n"))

(evil-define-operator ok-yank-without-filling (beg end type register yank-handler)
  :type line
  :repeat nil
  :move-point nil
  (evil-yank beg end type register yank-handler)
  (kill-new (->> (car kill-ring)
                 (replace-regexp-in-string "\n\n" "#@#@#@")
                 (replace-regexp-in-string "\n" " ")
                 (replace-regexp-in-string "#@#@#@" "\n\n")
                 s-trim)
            t))

;; Apparently C-x is a common prefix, and many modes use it.
;; Shadow all of them for now.
(map! :map general-override-mode-map
      :n "C-x" 'evil-numbers/inc-at-pt)

(defun ok-list-buffers ()
  (interactive)
  (list-buffers)
  (ok-switch-to-window "*Buffer List*")
  ;; This map! doesn't work if just by itself for some reason.
  ;; And it has to be after (list-buffers)
  (map! :map Buffer-menu-mode-map
        :n "u" 'evil-previous-visual-line
        :n "n" 'evil-backward-char
        :n "i" 'evil-forward-char
        :n "e" 'evil-next-visual-line
        :n "C-u" 'ok-move-up-5-lines
        :n "C-e" 'ok-move-down-5-lines
        :n "a" 'Buffer-menu-save
        :n "k" 'Buffer-menu-unmark
        :n "K" 'Buffer-menu-unmark-all
        :n "gg" 'evil-goto-first-line
        :n "r" 'Buffer-menu-toggle-read-only
        :n "%" 'Buffer-menu-toggle-read-only
        :n "o" 'Buffer-menu-other-window
        :n "f" 'Buffer-menu-switch-other-window))

(map! :leader
      ;; Apparently, one should bind this here, not in the file-map
      :desc "Find file in project" "fp" 'counsel-projectile-find-file
      :desc "Open config" "v" (cmd (find-file "~/.doom.d/config.el"))
      :desc "webpaste buffer" "h" 'webpaste-paste-buffer
      :desc "list buffers" "l" 'ok-list-buffers
      "e" 'eval-expression
      "k" 'org-capture
      :desc "Seach proj. for symbol at point"
      "#" '+default/search-project-for-symbol-at-point
      "*" nil
      "X" nil
      ";" nil)

(map! :map doom-leader-buffer-map
      "b" 'switch-to-buffer
      "l" 'ok-list-buffers)

(after! magit
  (ok-rebind-in-all-maps
   "magit" "-map"
   '(magit-popup-mode-map
     magit-popup-help-mode-map)
   "x" "u"
   "X" "U"
   "g" nil
   "G" nil
   "u" nil
   "U" nil
   "M-f" nil
   "M-p" nil))

(defun magit-add-current-branch-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(add-hook! 'git-rebase-mode-hook
  (map!
   :map git-rebase-mode-map
   :n "u" 'previous-line
   :n "e" 'next-line
   "M-u" 'git-rebase-move-line-up
   "M-e" 'git-rebase-move-line-down
   "a" 'git-rebase-edit
   "d" 'git-rebase-kill-line
   "p" 'git-rebase-pick
   "q" 'with-editor-cancel
   :n "k" 'git-rebase-undo))

(map! :map doom-leader-map
      "g M-y" 'magit-add-current-branch-to-kill-ring)

(map! :after magit
      :map magit-blob-mode-map
      "n" nil
      :map magit-log-mode-map
      "u" 'previous-line
      "C-u" (cmd (previous-line 10))
      "C-e" (cmd (next-line 10))
      :map (magit-mode-map
            git-rebase-mode-map
            magit-log-select-mode-map
            magit-log-mode-map)
      "," doom-leader-map
      "å" 'ace-window
      "ä" 'evil-window-next
      "Ä" 'evil-window-prev
      :n "u" 'previous-line
      :n "e" 'next-line
      :v "u" 'evil-previous-visual-line
      :v "e" 'evil-next-visual-line
      :n "t" 'git-rebase-edit
      :n "M-u" 'git-rebase-move-line-up
      :n "M-e" 'git-rebase-move-line-down
      :map magit-blame-mode-map
      :n "<return>" 'magit-show-commit
      :map (magit-status-mode-map magit-mode-map)
      "<tab>" 'magit-section-toggle
      "§" 'keyboard-quit
      "<f2>" 'magit-refresh-all
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line
      ":" 'evil-ex
      "/" 'evil-ex-search-forward
      "?" 'evil-ex-search-backward
      "M-h" 'magit-dispatch-popup
      "h" 'evil-ex-search-next
      "H" 'evil-ex-search-previous
      "e" 'next-line
      "u" 'previous-line
      :n "i" 'evil-forward-char
      :n "n" 'evil-backward-char
      "C-u" 'magit-section-backward
      :nv "C-u" 'magit-section-backward
      "C-e" 'magit-section-forward
      "C-n" 'magit-section-backward-sibling
      "C-i" 'magit-section-forward-sibling
      "M-u" 'magit-previous-line
      "M-e" 'magit-next-line
      "M-C-e" 'move-down-15-lines
      "M-C-u" 'move-up-15-lines
      "C-q" 'set-mark-command
      "C-r" 'magit-reset
      "x" 'magit-unstage
      :nv "x" 'magit-unstage
      :nv "X" 'magit-unstage-all
      "X" 'magit-unstage-all
      :nv "C-d" 'magit-delete-thing
      :n "gz" 'magit-jump-to-stashes
      :n "z" 'magit-stash)

(map! :after cider
      :map (cider-repl-mode-map cider-stacktrace-mode-map)
      "M-p" nil
      "M-f" nil)

(map! :map dired-mode-map
      :n "e" 'next-line
      :n "u" 'previous-line
      :n "q" nil)

(defun opengl-lookup ()
  (interactive)
  (let* ((sym (str (symbol-at-point)))
         (split (s-split "/" sym))
         (name (car (if (= 2 (length split))
                        (cdr split)
                      split))))
    (->> name
         ok-kebab-to-camel-case
         ((lambda (x) (if (not (s-starts-with? "gl" x))
                          (str "gl" (s-upcase-first-letter x))
                        x)))
         ((lambda (x) (if (s-starts-with? "glUniform" x)
                          "glUniform"
                        x)))
         ;; (str "https://www.google.com/search?q=site%3Adocs.gl%2Fgl3+")
         (str "https://docs.gl/gl3/")
         browse-url)))

;; (defun ok-clojure-refresh ()
;;   (interactive)
;;   (-> "(do (require 'refresh) (refresh/better-refresh))" ok-cider-eval))

(defun ok-clojure-refresh ()
  (interactive)
  (let* ((root (projectile-project-root))
         (files (directory-files-recursively root ".*clj$"))
         (files (-filter (lambda (x) (s-contains? "/src/" x)) files))
         (files (mapcar (lambda (name) (cadr (s-split "/src/" name))) files))
         (files (mapcar (lambda (name) (s-replace-all '(("/" . ".") (".clj" . "") ("_" . "-")) name))
                        files)))
    (-> (str "(do "
             "(require '[clojure.tools.namespace.repl :refer [refresh]])"
             "(require '[oskarkv.utils :as u])"
             ;; "(doseq [sym (keys (ns-publics (find-ns 'oskarkv.utils)))
             ;;          nams '" files "]"
             ;; "  (u/ignore-exception"
             ;; "    (ns-unmap (find-ns name) sym)))"
             "(doseq [sym (keys (ns-publics (find-ns 'game.utils)))
                      nams '" files "]"
             "  (u/ignore-exception"
             "    (ns-unmap (find-ns name) sym)))"
             "(refresh))")
        ok-cider-eval)
    ))

(map! :after (:or clojure-mode cider)
      :map (clojure-mode-map cider-repl-mode-map)
      :prefix "SPC"
      :n "aa" 'cider-apropos
      :n "ad" 'cider-apropos-documentation
      :n "al" 'clojure-align
      :n "as" 'cider-apropos-documentation-select
      :n "c" ok-clj-refactor-map
      :n "da" 'cider-clojuredocs-web
      :n "db" 'cider-browse-ns
      :n "dc" 'cider-javadoc
      :n "dd" 'cider-doc
      :n "dg" (cmd cider-grimoire)
      :n "dw" 'opengl-lookup
      :n "eb" 'cider-load-buffer
      :n "ed" 'cider-debug-defun-at-point
      :n "ef" 'ok-cider-eval-form
      :n "en" 'cider-eval-ns-form
      :n "et" 'cider-eval-defun-at-point
      :n "er" 'ok-clojure-refresh
      :n "gn" 'cider-find-ns
      :n "gr" 'cider-find-resource
      :n "jb" 'cider-jack-in-clj&cljs
      :n "jj" 'cider-jack-in
      :n "js" 'cider-jack-in-cljs
      :n "m" 'esexp-cider-macroexpand
      :n "ra" 'clojure-unwind-all
      :n "rb" 'clojure-thread-all-but-last
      :n "rc" 'cider-repl-clear-buffer
      :n "rf" 'clojure-thread-first-all
      :n "rl" 'clojure-thread-last-all
      :n "rnr" (cmd (cider-switch-to-repl-buffer t) (sleep-for 0 100) (cider-set-repl-type 'cljs))
      :n "rns" 'cider-repl-set-ns
      :n "rr" 'cider-switch-to-repl-buffer
      :n "rt" 'clojure-thread
      :n "ru" 'clojure-unwind
      :n "sw" 'cider-find-var
      :n "sr" 'lsp-rename
      :n "sf" '+lookup/references
      :n "so" 'cider-clojuredocs-web
      :n "tn" 'cider-test-run-ns-tests
      :n "tp" 'cider-test-run-project-tests
      :n "tt" 'cider-test-run-test
      :v "er" 'cider-eval-region
      :v "et" 'cider-eval-region)

(defvar my-smerge-mode-map (make-sparse-keymap))

(map! :map my-smerge-mode-map
      "u" 'smerge-keep-upper
      "e" 'smerge-keep-lower
      "a" 'smerge-keep-all
      "n" 'smerge-next
      "p" 'smerge-prev
      "d" 'smerge-ediff
      "r" 'smerge-refine)

(map! :map (prog-mode-map
            cider-repl-mode-map)
      :n "(" 'esexp-backward-paren
      :n ")" 'esexp-forward-paren
      :n "M-i" 'esexp-transpose-sexps
      :n "M-n" (cmd (esexp-transpose-sexps -1))
      :n "C-M-i" (cmd (esexp-transpose-forms 1))
      :n "C-M-n" (cmd (esexp-transpose-forms -1))
      :n "M-e" 'paredit-splice-sexp
      :n "M-u" 'paredit-raise-sexp
      :n "C-M-u" 'esexp-raise-form
      :n "C-i" 'esexp-forward-slurp-sexp
      ;; If <tab> is not bound then tab calls C-i
      :n "<tab>" (cmd)
      :n "C-n" 'esexp-backward-slurp-sexp
      :n "C-m" 'esexp-backward-barf-sexp
      :n "<return>" (cmd)
      :n "C-ä" 'esexp-forward-barf-sexp
      :n "M-l" 'paredit-backward-up
      :n "M-q" 'paredit-backward-up
      :prefix "SPC"
      :n "u" my-smerge-mode-map
      :n "i" 'esexp-insert-at-end
      :n "n" 'esexp-insert-at-head
      :n "l" 'esexp-wrap-form-parens-beg
      :n "L" 'esexp-wrap-form-parens-end
      :n "w" 'esexp-wrap-element-parens-beg
      :n "W" 'esexp-wrap-element-parens-end
      :n "e[" 'esexp-wrap-element-brackets-beg
      :n "e]" 'esexp-wrap-element-brackets-end
      :n "e{" 'esexp-wrap-element-braces-beg
      :n "e}" 'esexp-wrap-element-braces-end
      :n "[" 'esexp-wrap-form-brackets-beg
      :n "]" 'esexp-wrap-form-brackets-end
      :n "{" 'esexp-wrap-form-braces-beg
      :n "}" 'esexp-wrap-form-braces-end
      :map clojure-mode-map
      :n "f" 'ok-clojure-fill-paragraph)

(map! :map (emacs-lisp-mode-map lisp-interaction-mode-map)
      :prefix "SPC"
      :n "dd" 'describe-symbol
      :n "dv" 'describe-variable
      :n "df" 'describe-function
      :n "et" 'eval-defun
      :n "sw" 'evil-goto-definition
      :n "ed" (cmd (eval-defun t))
      :n "eb" 'eval-buffer
      :n "ef" 'ok-eval-form
      :n "es" 'eval-last-sexp
      :n "m" 'macrostep-expand
      :v "et" 'eval-region)

(section-comment "Old keybindings I haven't look at yet"
  ;; These modes were emacs state modes
  (general-define-key
   :keymaps '(debugger-mode-map help-mode-map completion-list-mode-map)
   "C-s" nil
   ;;"C-n" nil
   ;;"C-i" nil
   "SPC" nil
   )

  ;; (defmacro magit-undefine-key (&rest binds)
  ;;   `(general-define-key
  ;;     :keymaps ',
  ;;     (let ((lst nil))
  ;;       (mapatoms (lambda (sym)
  ;;                   (if (and (boundp sym)
  ;;                            (keymapp (symbol-value sym))
  ;;                            (s-ends-with-p "section-map" (symbol-name sym))
  ;;                            (s-starts-with-p "magit" (symbol-name sym)))
  ;;                       (push sym lst)))
  ;;                 obarray)
  ;;       lst)
  ;;     ,@binds))

  ;; (magit-undefine-key
  ;;  "u" nil)

  ;; (general-evil-define-key
  ;;     'normal '(magit-mode-map
  ;;               magit-status-mode-map)
  ;;   "SPC" magit-status-mode-map)

  (general-evil-define-key
      'normal '(cider-stacktrace-mode-map
                cider-inspector-mode-map
                cider-docview-mode-map)
    "q" 'cider-popup-buffer-quit-function)

  (general-define-key
   :keymaps 'cider-stacktrace-mode-map
   :prefix "SPC"
   "j" 'cider-stacktrace-toggle-java
   "r" 'cider-stacktrace-toggle-repl
   "t" 'cider-stacktrace-toggle-tooling
   "c" 'cider-stacktrace-toggle-clj
   "d" 'cider-stacktrace-toggle-duplicates
   "a" 'cider-stacktrace-toggle-all
   )

  ;; Needed because of problem with evil not updating keymaps properly
  (add-hook 'macrostep-mode-hook 'evil-normalize-keymaps)

  (general-evil-define-key
      'normal macrostep-keymap
    "q" (cmd (macrostep-collapse-all) (fci-mode 1)))





 ;;; Racket

  (general-evil-define-key
      'normal '(racket-mode-map
                racket-repl-mode-map)
    :prefix "SPC"
    "et" 'racket-send-definition
    ;; "eb" 'racket-run
    "eb" (cmd (racket-run) (run-at-time 1 nil #'fci-mode 1))
    "dd" 'racket-describe
    "ef" 'ok-racket-eval-form
    "mm" 'racket-expand-definition
    "ma" 'racket-expand-again
    "ms" 'macro-stepper
    )

  (general-evil-define-key
      'normal 'racket-describe-mode-map
    "q" 'quit-window
    ))

(section-comment "Old customization"
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(cider-cljs-lein-repl
     "(do (require 'cljs.repl.node)
         (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))"))
  )

(section-comment "Crap from old config"
  ;; For racket
  (defun macro-stepper ()
    (interactive)
    (-let* (((beg end) (esexp-true-toplevel-positions))
            (text (buffer-substring-no-properties beg end)))
      (comint-send-string (get-buffer-process "*racket repl*")
                          (concat "(expand/step #'" text ")\n"))))

  (advice-add 'cider-popup-buffer
              :before (lambda (buffer &rest args)
                        (when (get-buffer buffer)
                          (evil-delete-buffer buffer))))

  (advice-add 'cider-doc
              :before (lambda (&rest args)
                        (let ((b "*cider-doc*"))
                          (when (get-buffer b)
                            (evil-delete-buffer b)))))

  (defun display-buffer-min (buffer alist)
    (if (= (length (window-list)) 1)
        (display-buffer-pop-up-window buffer nil)
      (display-buffer-use-some-window buffer nil)))

  (setq display-buffer-alist
        '(("\\*Backtrace\\*" . ((display-buffer-min))))))
