;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(require 'utils)
(require 'patches)
(require 'esexp)
(require 'seq)
(require 's)
(require 'cl-indent)
(require 'dash)

;; Name and email address
(setq user-full-name "Oskar Kvist"
      user-mail-address "oskar.kvist@gmail.com")

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
(setq doom-theme 'doom-molokai)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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

(dolist (fn '(switch-to-next-buffer switch-to-prev-buffer))
  (advice-remove fn #'doom-run-switch-to-next-prev-buffer-hooks-a))

;;; Fix TAB shadowing C-i

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
            (symbol-value (intern-soft (concat (symbol-name ',mode) "-mode-map"))))))))

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
(setq rainbow-delimiters-max-face-count 11)
(custom-set-faces!
  '(clojure-quote-face :foreground "#ff0088")
  '(clojure-number-face :foreground "#d419ff")
  '(highlight-numbers-number :foreground "#d419ff" :weight normal)
  '(org-level-1 :inherit default :foreground "#33ff33")
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

(evil-define-operator ok-evil-webpaste (beg end)
  :repeat nil
  :move-point nil
  (webpaste-paste-region beg end))

(evil-define-operator ok-evil-reddit-yank (beg end type register yank-handler)
  "Yanks text and adds 4 spaces in front of every line. Works with evil."
  (evil-yank beg end type register yank-handler)
  (kill-new (replace-regexp-in-string "^" "    " (car kill-ring)) t))

(defun ok-mode-to-language (m)
  "Given a Emacs mode M, returns the language name in markdown that
  should be used for code from the mode."
  (let ((mode-alist '((emacs-lisp-mode . "lisp"))))
    (or (cdr (assoc m mode-alist))
        (ok-string-drop-at-end (str m) 5))))

(evil-define-operator ok-evil-three-backticks-yank (beg end type register yank-handler)
  "Yanks text and adds three backticks around it, as well as a
  language after the first backticks, depending on the Emacs
  mode. Works with evil."
  (evil-yank beg end type register yank-handler)
  (kill-new (concat "```" (ok-mode-to-language major-mode) "\n"
                    (car kill-ring)
                    "```")
            t))

(evil-define-motion ok-move-down-15-lines ()
  :type line
  (evil-next-visual-line 15))

(evil-define-motion ok-move-up-15-lines ()
  :type line
  (evil-previous-visual-line 15))

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

;;; Settings

;; Make which-key show help for Vim operators
(setq which-key-show-operator-state-maps nil)

(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#rcirc" "#emacs" "#evil-mode")))
      rcirc-default-nick "tufflax"
      rcirc-authinfo
      (list (seq-concatenate
             'list
             '("freenode" nickserv "tufflax")
             (list (ok-get-string-from-file "~/.doom.d/.freenode-pass")))))

(setq webpaste-provider-priority
      '("dpaste.com" "gist.github.com" "paste.mozilla.org" "dpaste.org"
        "ix.io" "paste.pound-python.org"))

;; Avoid double \\ in re-builder
(setq reb-re-syntax 'string)

;; Disable automatically inserting closing parens, quotes, etc.
(after! smartparens
  (remove-hook! 'minibuffer-setup-hook
    #'doom-init-smartparens-in-minibuffer-maybe-h)
  (smartparens-global-mode -1))

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

  (general-define-key
   :keymaps 'ok-clj-refactor-map
   "nn" (cmd (cljr--clean-ns nil :no-prune)))

  (advice-add 'cljr--clean-ns :after #'clean-ns-more))

(after! magit
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
 ;; ag-highlight-search t
 abbrev-mode t
 evil-cross-lines t
 evil-ex-substitute-global t
 column-number-mode t
 indent-tabs-mode nil
 inhibit-startup-screen t
 tab-width 4)

;; Define modes for file extensions.
(add-to-list 'auto-mode-alist '("\\.joke\\'" . clojure-mode))

;; Don't ask before quitting
(setq confirm-kill-emacs nil)

;; Make cider-find-var be considered a jump in evil.
(evil-add-command-properties #'cider-find-var :jump t)

;; Makes lines wrap instead of going off screen
(global-visual-line-mode)

;; Makes evil snipe (t, f, s) search in whole buffer instead of just the current
;; line
(setq evil-snipe-scope 'buffer)

;; Start frame 91 columns wide
(add-to-list 'default-frame-alist '(width . 81))

(after! org
  (remove-hook! 'org-tab-first-hook #'+org-yas-expand-maybe-h)
  (setq org-clock-clocked-in-display nil
        org-M-RET-may-split-line nil
        org-todo-keywords '((sequence "TODO(t)" "INPR(p)" "WAIT(w)" "|" "DONE(d)")))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq paragraph-separate "[ 	\f]*$"
                    paragraph-start "\f\\|[ 	]*$"))))

;;; States

(add-to-list 'evil-emacs-state-modes 'term-mode)
(delete 'term-mode evil-insert-state-modes)
(add-hook 'cider-inspector-mode-hook 'evil-emacs-state)
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;;; Misc

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook
          (fn (setq fill-column 60)))
(add-hook 'term-mode-hook (fn (yas-minor-mode -1)
                              (setq yas-dont-activate t)))

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
  (add-hook 'clojure-mode-hook (fn (highlight-numbers-mode -1)))
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
    (defs 0)
    (deftype- '(2 nil nil (:defn)))
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

(defun ok-clojure-fill-paragraph ()
  (interactive)
  (if (ok-before-doctring-p)
      (save-excursion
        (re-search-forward "[^\s-]")
        (clojure-fill-paragraph))
    (clojure-fill-paragraph)))

(defun ok-hy-eval-toplevel ()
  (interactive)
  (save-excursion
    (-let* (((b e) (esexp-true-toplevel-positions)))
      (goto-char (1- e))
      (hy-shell-eval-current-form))))

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
      (apply #'ok-rebind-in-all-maps start end exclude-list (seq-drop to-froms 2))))

(defun ok-wrap-word-in-backticks ()
  (interactive)
  (save-excursion
    (let* ((sep "[][(){}[:space:]\n\r.,\"]"))
      (re-search-backward sep)
      (forward-char)
      (insert "`")
      (re-search-forward sep)
      (backward-char)
      (insert "`"))))

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
      :n "l" 'org-insert-link
      :n "e" 'org-edit-special
      :n "i" '(lambda (s) (interactive "sCustom ID: ") (org-set-property "CUSTOM_ID" s))
      :v "t" 'org-table-create-or-convert-from-region
      :map org-src-mode-map
      :n "ce" 'org-edit-src-exit
      :n "cc" 'org-edit-src-abort)

(map! :map (evil-normal-state-map
            evil-visual-state-map
            evil-motion-state-map
            evil-insert-state-map)
      "TAB" nil)

(map! :after evil-org
      :map evil-org-mode-map
      :vo "ie" nil
      :vo "iE" nil
      :vo "ir" nil
      :vo "iR" nil
      :vo "i" nil
      :vo "le" #'evil-org-inner-object
      :vo "lE" #'evil-org-inner-element
      :vo "lr" #'evil-org-inner-greater-element
      :vo "lR" #'evil-org-inner-subtree
      :nv "TAB" nil)

(map! :after ivy
      :map ivy-minibuffer-map
      "C-u" 'ivy-previous-line
      "C-e" 'ivy-next-line
      "C-S-e" 'ivy-scroll-up-command
      "C-S-u" 'ivy-scroll-down-command
      "RET" 'ivy-alt-done
      "C-RET" 'ivy-done
      "!" (cmd (insert "\\!")))

(map! :map key-translation-map
      "ESC" (kbd "C-g"))

(map! :map evil-emacs-state-map
      "C-," 'evil-exit-emacs-state)

(map! :map global-map
      "C-å" 'ace-window
      "C-ä" 'other-window
      "C-s" 'save-buffer
      "M-f" 'switch-to-prev-buffer
      "M-p" 'switch-to-next-buffer
      "C-u" 'previous-line
      "C-e" 'next-line
      "C-i" 'end-of-line
      "C-n" 'beginning-of-line
      "M-w" 'er/expand-region)

;; Makes tab not call yas-expand
(map! :i [tab] 'complete-symbol)

(map! :map evil-insert-state-map
      "§" 'evil-normal-state
      "C-<tab>" (cmd (insert "\t")))

(map! :map evil-replace-state-map
      "§" 'evil-normal-state)

(map! :map evil-normal-state-map
      ",h" 'webpaste-paste-buffer
      ",e" 'eval-expression
      "§" (cmd (evil-ex-nohighlight) (evil-force-normal-state))
      "C-n" nil
      "u" nil
      "e" nil
      "j" nil
      "J" nil
      "M" nil
      "n" nil
      "N" nil
      "h" nil
      "H" nil
      "i" nil
      "I" nil
      "SPC" nil
      "k" 'undo
      "E" 'evil-join
      "Å" 'newline-and-indent
      "l" 'evil-insert
      "L" 'evil-insert-line
      "M-y" 'goto-last-change
      "M-o" 'goto-last-change-reverse
      "C-x" 'evil-numbers/inc-at-pt
      "C-z" 'evil-numbers/dec-at-pt
      "M-t" #'ok-run-q-macro
      :prefix "SPC"
      "dk" 'describe-key
      "db" 'describe-bindings)

;; Apparently C-x is a common prefix, and many modes use it.
;; Shadow all of them for now.
(map! :map general-override-mode-map
      :n "C-x" 'evil-numbers/inc-at-pt)

(map! :map evil-motion-state-map
      "SPC" nil
      "k" nil
      "E" nil
      "l" nil
      "L" nil
      "u" 'evil-previous-visual-line
      "e" 'evil-next-visual-line
      "j" 'evil-forward-word-end
      "J" 'evil-forward-WORD-end
      "C-u" 'ok-move-up-15-lines
      "C-e" 'ok-move-down-15-lines
      "n" 'evil-backward-char
      "h" 'evil-ex-search-next
      "H" 'evil-ex-search-previous
      "i" 'evil-forward-char
      "å" 'ace-window
      "ä" 'other-window
      "Ä" (cmd (other-window -1))
      "C-q" 'evil-visual-block
      "C-y" 'evil-jump-backward
      "C-o" 'evil-jump-forward
      "C-," 'evil-emacs-state)

(map! :map evil-visual-state-map
      "§" 'evil-exit-visual-state
      "u" nil
      "l" -visual-inside-keymap
      "i" nil
      "L" 'evil-insert
      "A" 'evil-append)

(map! :map doom-leader-map
      "h" nil
      "k" 'org-capture)

(map! :map doom-leader-buffer-map
      "b" 'switch-to-buffer)

(map! :map (evil-normal-state-map evil-visual-state-map)
      "gh" 'ok-evil-webpaste
      "gs" 'ok-evil-three-backticks-yank
      "gy" 'ok-evil-reddit-yank
      "go" '+evil:yank-unindented
      :prefix "SPC"
      "f" 'fill-paragraph
      :prefix ","
      "v" (cmd (find-file "~/.doom.d/config.el")))

(map! :map evil-operator-state-map
      "l" -operator-inside-keymap
      "i" nil
      "e" 'evil-next-line
      "u" 'evil-previous-line)

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

(map! :after magit
      :map magit-log-mode-map
      "u" 'previous-line
      "C-u" (cmd (previous-line 10))
      "C-e" (cmd (next-line 10))
      :map git-rebase-mode-map
      "a" 'git-rebase-edit
      "c" 'git-rebase-kill-line
      "p" 'git-rebase-pick
      "q" 'with-editor-cancel
      "k" 'git-rebase-undo
      :map (magit-mode-map
            git-rebase-mode-map
            magit-log-select-mode-map
            magit-log-mode-map)
      "å" 'ace-window
      "ä" 'other-window
      "Ä" (cmd (other-window -1))
      "u" 'previous-line
      "e" 'next-line
      :map magit-status-mode-map
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
      "C-e" 'magit-section-forward
      "C-u" 'magit-section-backward
      "C-i" 'magit-section-forward-sibling
      "C-n" 'magit-section-backward-sibling
      "M-u" 'magit-previous-line
      "M-e" 'magit-next-line
      "M-C-e" 'move-down-15-lines
      "M-C-u" 'move-up-15-lines
      "C-q" 'set-mark-command
      "C-r" 'magit-reset
      "x" 'magit-unstage
      "X" 'magit-unstage-all
      :nv "C-d" 'magit-delete-thing)

(map! :after (:or clojure-mode cider)
      :map (clojure-mode-map cider-repl-mode-map)
      :prefix "SPC"
      :n "aa" 'cider-apropos
      :n "ad" 'cider-apropos-documentation
      :n "al" 'clojure-align
      :n "as" 'cider-apropos-documentation-select
      :n "c" ok-clj-refactor-map
      :n "dc" 'cider-javadoc
      :n "dd" 'cider-doc
      :n "dg" 'cider-grimoire
      :n "dw" 'cider-grimoire-web
      :n "eb" (cmd (cider-load-buffer) (run-at-time 1 nil #'fci-mode 1))
      :n "ed" 'cider-debug-defun-at-point
      :n "ef" 'ok-cider-eval-form
      :n "en" 'cider-eval-ns-form
      :n "et" 'cider-eval-defun-at-point
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
      :n "rnr" (cmd (cider-switch-to-repl-buffer t))
      :n "rns" 'cider-repl-set-ns
      :n "rr" 'cider-switch-to-repl-buffer
      :n "rt" 'clojure-thread
      :n "ru" 'clojure-unwind
      :n "sw" 'cider-find-var
      :n "tn" 'cider-toggle-trace-ns
      :n "tv" 'cider-toggle-trace-var
      :v "er" 'cider-eval-region
      :v "et" 'cider-eval-region)

(map! :map prog-mode-map
      ;; :after (:or utils racket clojure hy)
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
      :n "C-n" 'esexp-backward-slurp-sexp
      :n "C-m" 'esexp-backward-barf-sexp
      :n "C-ä" 'esexp-forward-barf-sexp
      :n "M-l" 'paredit-backward-up
      :n "M-q" 'paredit-backward-up
      :prefix "SPC"
      :n "q" 'esexp-wrap-word-in-backticks
      :n "f" 'ok-clojure-fill-paragraph
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
      :n "}" 'esexp-wrap-form-braces-end)

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
      :n "m" 'macrostep-expand)

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


  ;;; Additional keys, not as set in stone


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

  (general-define-key
   :keymaps '(cider-stacktrace-mode-map
              cider-repl-mode-map
              comint-mode-map
              slime-repl-mode-map
              ag-mode-map)
   "," nil
   "M-f" nil
   "M-p" nil
   )


  ;; THIS MIGHT BE WRONG, DUNNO THE NAME OF THE PACKAGE
  (map! :after slime
   :map slime-mode-map
    :prefix "SPC"
    :n "et" 'slime-eval-defun
    :v "er" 'slime-eval-region
    :n "eb" 'slime-eval-buffer
    :n "ct" 'slime-compile-defun
    :n "cb" 'slime-compile-and-load-file
    :n "sw" 'slime-edit-definition
    :n "dd" 'slime-describe-function
    :n "ds" 'slime-describe-symbol
    :n "aa" 'slime-apropos
    :n "dh" 'slime-hyperspec-lookup
    :n "dm" 'hyperspec-lookup-reader-macro
    :n "df" 'hyperspec-lookup-format
    :n "wc" 'slime-who-calls
    :n "ww" 'slime-calls-who
    :n "mm" 'slime-macroexpand-1
    :n "ma" 'slime-macroexpand-all
    :n "ej" 'slime
    :n "rr" (cmd (other-window 1) (slime-repl)))

  (map! :after hy-mode
      :map hy-mode-map
    :prefix "SPC"
    :n "ef" 'hy-shell-eval-current-form
    :n "en" 'hy-shell-eval-buffer
    :n "et" 'ok-hy-eval-toplevel)

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
    )

(section-comment "Old customization"

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 113 :width normal))))
   '(clojure-interop-method-face ((t (:inherit font-lock-type-face))))
   '(clojure-keyword-face ((t (:foreground "#ae81ff"))))
   '(clojure-pink-face ((t (:foreground "#ff0088"))))
   '(font-lock-keyword-face ((t (:foreground "#ff2233"))))
   ;;'(font-lock-doc-face ((t (:foreground "#858175"))))
   '(linum ((t (:background "#272822" :foreground "#8F908A" :underline nil :height 113))))
   '(mode-line ((((class color) (min-colors 257)) (:inverse-video unspecified :underline unspecified :foreground "#F8F8F0" :background "#49483E" :box (:line-width 1 :color "#64645E" :style unspecified))) (((class color) (min-colors 89)) (:inverse-video unspecified :underline unspecified :foreground "#F5F5F5" :background "#1B1E1C" :box (:line-width 1 :color "#474747" :style unspecified)))))
   '(racket-selfeval-face ((t (:foreground "#5cf")))))

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(cider-cljs-lein-repl "(do (require 'cljs.repl.node)
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
        '(("\\*Backtrace\\*" . ((display-buffer-min)))))

  (use-package rainbow-delimiters
    :ensure t
    :config
    (defmacro rainbow-delimiters--define-depth-faces ()
      (let ((faces '())
            (light-colors ["#707183" "#7388d6" "#909183" "#709870" "#907373"
                           "#6276ba" "#858580" "#80a880" "#887070"])
            (dark-colors ["grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
                          "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"]))
        (dotimes (i 2)
          (push `(defface ,(intern (format "rainbow-delimiters-depth-%d-face" (+ 10 i)))
                   '((((class color) (background light)) :foreground ,(aref light-colors i))
                     (((class color) (background dark)) :foreground ,(aref dark-colors i)))
                   ,(format "nested delimiter face, depth %d." (+ 10 i))
                   :group 'rainbow-delimiters-faces)
                faces))
        `(progn ,@faces)))
    (rainbow-delimiters--define-depth-faces)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'org-mode-hook #'rainbow-delimiters-mode-disable)
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'text-mode-hook #'rainbow-delimiters-mode))

  (use-package orgtbl-aggregate
    :ensure t)

  (use-package magit
    :ensure t
    :config
    (add-hook 'git-commit-mode-hook
              (lambda ()
                (setq fill-column 72)
                (turn-on-auto-fill))))

  (use-package cider
    :load-path "~/code/cider"
    :ensure t
    :config
                                        ;(setq cider-latest-middleware-version "0.21.2-snapshot")
    (emacs-mode-in-mode cider--debug-mode-hook cider--debug-mode)
    (setq-default
     cider-font-lock-reader-conditionals nil
     cider-pprint-fn 'fipp
     cider-repl-use-pretty-printing t
     cider-font-lock-dynamically t
     cider-prompt-for-symbol nil))

  (use-package fill-column-indicator
    :config
    (setq fci-rule-color "#502727")
    (setq fci-rule-width 4)
    (setq fci-always-use-textual-rule nil)
    (add-hook 'clojure-mode-hook 'fci-mode)
    (add-hook 'hy-mode-hook 'fci-mode)))
