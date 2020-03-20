  ;; (advice-add 'cider-popup-buffer
  ;;             :before (lambda (buffer &rest args)
  ;;                       (when (get-buffer buffer)
  ;;                         (evil-delete-buffer buffer))))

  ;; (advice-add 'cider-doc
  ;;             :before (lambda (&rest args)
  ;;                       (let ((b "*cider-doc*"))
  ;;                         (when (get-buffer b)
  ;;                           (evil-delete-buffer b)))))

  ;; (defun display-buffer-min (buffer alist)
  ;;   (if (= (length (window-list)) 1)
  ;;       (display-buffer-pop-up-window buffer nil)
  ;;     (display-buffer-use-some-window buffer nil)))

  ;; (setq display-buffer-alist
  ;;       '(("\\*Backtrace\\*" . ((display-buffer-min)))))

  ;; (use-package rainbow-delimiters
  ;;   :ensure t
  ;;   :config
  ;;   (defmacro rainbow-delimiters--define-depth-faces ()
  ;;     (let ((faces '())
  ;;           (light-colors ["#707183" "#7388d6" "#909183" "#709870" "#907373"
  ;;                          "#6276ba" "#858580" "#80a880" "#887070"])
  ;;           (dark-colors ["grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
  ;;                         "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"]))
  ;;       (dotimes (i 2)
  ;;         (push `(defface ,(intern (format "rainbow-delimiters-depth-%d-face" (+ 10 i)))
  ;;                  '((((class color) (background light)) :foreground ,(aref light-colors i))
  ;;                    (((class color) (background dark)) :foreground ,(aref dark-colors i)))
  ;;                  ,(format "nested delimiter face, depth %d." (+ 10 i))
  ;;                  :group 'rainbow-delimiters-faces)
  ;;               faces))
  ;;       `(progn ,@faces)))
  ;;   (rainbow-delimiters--define-depth-faces)
  ;;   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  ;;   (add-hook 'org-mode-hook #'rainbow-delimiters-mode-disable)
  ;;   (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  ;;   (add-hook 'text-mode-hook #'rainbow-delimiters-mode))

  ;; (use-package orgtbl-aggregate
  ;;   :ensure t)

  ;; (use-package ok
  ;;   :load-path "~/.emacs.d/lisp/ok/")

  ;; (use-package org
  ;;   ;; only this from archive
  ;;   :pin "org"
  ;;   :init
  ;;   (setq-default
  ;;    org-startup-indented t
  ;;    org-todo-keywords '((sequence "todo" "in-progress" "done")))
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (add-hook 'org-src-mode-hook 'evil-normalize-keymaps)
  ;;     (add-hook 'org-mode-hook
  ;;               (lambda () (setq fill-column 70)))))

  ;; (use-package webpaste
  ;;   :ensure t)

  ;; (use-package magit
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'git-commit-mode-hook
  ;;             (lambda ()
  ;;               (setq fill-column 72)
  ;;               (turn-on-auto-fill))))

  ;; (load "~/.emacs.d/elpa/lv.el")

  ;; (use-package clj-refactor
  ;;   :ensure t
  ;;   :config
  ;;   (apply #'general-define-key
  ;;          :keymaps 'ok-clj-refactor-map
  ;;          (-reduce-from (-lambda (list (key fn))
  ;;                          (cons key (cons fn list)))
  ;;                        nil
  ;;                        cljr--all-helpers))

  ;;   (general-define-key
  ;;    :keymaps 'ok-clj-refactor-map
  ;;    "nn" (cmd (cljr--clean-ns nil :no-prune)))

  ;;   (defun clean-ns-more (&rest args)
  ;;     (-let* (((beg end) (ok-true-toplevel-positions)))
  ;;       (while (re-search-forward "(:\\(require\\|use\\|import\\)" end t)
  ;;         (re-search-forward " ")
  ;;         (when (not (= (char-before (1- (point))) ?\n))
  ;;           (backward-char)
  ;;           (insert "\n")))
  ;;       (goto-char beg)
  ;;       ;; wrap naked words in libs in []
  ;;       (when (re-search-forward ":require" end t)
  ;;         (-let* (((beg end) (ok-true-form-positions)))
  ;;           (while (re-search-forward "^\\s-*[a-z]" end t)
  ;;             (backward-char)
  ;;             (insert "[")
  ;;             (re-search-forward "\\b)*$")
  ;;             (while (= ?\) (char-before))
  ;;               (backward-char))
  ;;             (insert "]")
  ;;             (setq end (+ 2 end)))))
  ;;       (goto-char beg)
  ;;       (when (re-search-forward ":import" end t)
  ;;         (-let* (((beg end) (ok-true-toplevel-positions)))
  ;;           (while (re-search-forward "^\\s-*[a-z]" end t)
  ;;             (backward-char)
  ;;             (insert "(")
  ;;             (re-search-forward "$")
  ;;             (insert ")")
  ;;             (re-search-backward "\\.")
  ;;             (replace-match " ")
  ;;             (setq end (+ 2 end)))))
  ;;       (goto-char beg)
  ;;       (when (re-search-forward ":import" end t)
  ;;         (-let* (((beg end) (ok-true-toplevel-positions)))
  ;;           (while (re-search-forward "\\[" end t)
  ;;             (replace-match "(")
  ;;             (re-search-forward "\\]")
  ;;             (replace-match ")"))))
  ;;       (indent-region beg end)))

  ;;   (advice-add 'cljr--clean-ns :after #'clean-ns-more))

  ;; (use-package cider
  ;;   :load-path "~/code/cider"
  ;;   :ensure t
  ;;   :config
  ;;   ;(setq cider-latest-middleware-version "0.21.2-snapshot")
  ;;   (emacs-mode-in-mode cider--debug-mode-hook cider--debug-mode)
  ;;   (setq-default
  ;;    cider-font-lock-reader-conditionals nil
  ;;    cider-pprint-fn 'fipp
  ;;    cider-repl-use-pretty-printing t
  ;;    cider-font-lock-dynamically t
  ;;    cider-prompt-for-symbol nil))

;; (use-package fill-column-indicator
;;   :config
;;   (setq fci-rule-color "#502727")
;;   (setq fci-rule-width 4)
;;   (setq fci-always-use-textual-rule nil)
;;   (add-hook 'clojure-mode-hook 'fci-mode)
;;   (add-hook 'hy-mode-hook 'fci-mode))

(defmacro cmd (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro fn (&rest body)
  `(lambda (&rest args) ,@body))

(defun ok-hours-and-mins (mins)
  (let* ((mins (if (neg? mins) (+ (* 60 24) mins) mins))
         (hours (str (/ mins 60)))
         (mins (str (mod mins 60))))
    (concat hours ":" (if (< (length mins) 2) "0" "") mins)))

;; (HAVE NOT TESTED YET (the intern-soft part))
(defmacro emacs-mode-in-mode (hook mode-var)
  "Given a mode name, make Emacs start that mode in Emacs mode instead of Evil
mode."
  `(add-hook (intern-soft (concat (symbol-name ,mode-var) "-hook"))
             (cmd (if ,mode-var
                      (evil-emacs-state)
                    (evil-exit-emacs-state)))))

(defun ok-parse-time (time-string)
  "Given an HH:MM string, returns the total minutes that TIME-STRING
represents."
  (let* ((split (split-string time-string ":"))
         (nums (mapcar #'string-to-number split)))
    (+ (* 60 (first nums)) (second nums))))

(defun ok-time-sub (t1 t2)
  "Returns the difference in minutes between two HH:MM strings."
  (apply #'- (mapcar #'ok-parse-time (list t1 t2))))

(defun ok-read-first-line ()
  "Returns the first line of current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((b (point))
          (e (progn (end-of-line) (point))))
      (buffer-substring-no-properties b e))))

(evil-define-operator ok-evil-reddit-yank (beg end type register yank-handler)
  "Yanks text and adds 4 spaces in front of every line. Works with evil."
  (evil-yank beg end type register yank-handler)
  (kill-new (replace-regexp-in-string "^" "    " (car kill-ring)) t))

(defun ok-string-drop-at-end (s n)
  "Drops the last N characters from the string S."
  (substring s 0 (- (length s) n)))

(defun ok-mode-to-language (m)
  "Given a Emacs mode M, returns the language name in markdown that
  should be used for code from the mode."
  (let ((mode-alist '((emacs-lisp-mode . "lisp"))))
    (or (cdr (assoc m mode-alist))
        (string-drop-at-end (str m) 5))))

(evil-define-operator ok-evil-three-backticks-yank (beg end type register yank-handler)
  "Yanks text and adds three backticks around it, as well as a
  language after the first backticks, depending on the Emacs
  mode. Works with evil."
  (evil-yank beg end type register yank-handler)
  (kill-new (concat "```" (ok-mode-to-language major-mode) "\n"
                    (car kill-ring)
                    "```")
            t))

;; Avoid double \\ in re-builder
;; (setq reb-re-syntax 'string)

;; Disable automatically inserting closing parens, quotes, etc.
(after! smartparens
  (smartparens-global-mode -1))

(after! org
  (setq org-clock-clocked-in-display nil
        org-M-RET-may-split-line nil))

(after! evil-org
  ;; Cycle between 3 states on a heading (folded, subheadings, all)
  (setq org-tab-first-hook (delete '+org-cycle-only-current-subtree-h org-tab-first-hook)))

(after! git
 (setq git-commit-summary-max-length 50))

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
 ;; debug-on-error nil
 abbrev-mode t
 ;; debug-on-signal t
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

;; Make ~cider-find-var~ be considered a jump in evil.
(evil-add-command-properties #'cider-find-var :jump t)

;; Makes lines wrap instead of going off screen
(global-visual-line-mode)

;; Makes evil snipe (t, f, s) search in whole buffer instead of just the current line
(setq evil-snipe-scope 'buffer)

;; Start frame 91 columns wide
(add-to-list 'default-frame-alist '(width . 91))

(after! org
  ;; Original paragraph-separate and -start in org mode,
  ;; works weirdl. Saved here if I wanna try to edit it
  ;; sometimes.
  (defvar ok-org-paragraph-separate
    (concat "^\\(?:"
            ;; Headlines, inlinetasks.
            org-outline-regexp "\\|"
            ;; Footnote definitions.
            "\\[fn:[-_[:word:]]+\\]" "\\|"
            ;; Diary sexps.
            "%%(" "\\|"
            "[ \t]*\\(?:"
            ;; Empty lines.
            "$" "\\|"
            ;; Tables (any type).
            "|" "\\|"
            "\\+\\(?:-+\\+\\)+[ \t]*$" "\\|"
            ;; Comments, keyword-like or block-like constructs.
            ;; Blocks and keywords with dual values need to be
            ;; double-checked.
            "#\\(?: \\|$\\|\\+\\(?:"
            "BEGIN_\\S-+" "\\|"
            "\\S-+\\(?:\\[.*\\]\\)?:[ \t]*\\)\\)"
            "\\|"
            ;; Drawers (any type) and fixed-width areas.  Drawers
            ;; need to be double-checked.
            ":\\(?: \\|$\\|[-_[:word:]]+:[ \t]*$\\)" "\\|"
            ;; Horizontal rules.
            "-\\{5,\\}[ \t]*$" "\\|"
            ;; LaTeX environments.
            "\\\\begin{\\([A-Za-z0-9*]+\\)}" "\\|"
            ;; Clock lines.
            (regexp-quote org-clock-string) "\\|"
            ;; Lists.
            (let ((term (pcase org-plain-list-ordered-item-terminator
                          (?\) ")") (?. "\\.") (_ "[.)]")))
                  (alpha (and org-list-allow-alphabetical "\\|[A-Za-z]")))
              (concat "\\(?:[-+*]\\|\\(?:[0-9]+" alpha "\\)" term "\\)"
                      "\\(?:[ \t]\\|$\\)"))
            "\\)\\)"))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq paragraph-separate "[ 	\f]*$" ;;ok-org-paragraph-separate
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

(defvar -visual-inside-keymap (lookup-key evil-visual-state-map "i"))
(defvar -operator-inside-keymap (lookup-key evil-operator-state-map "i"))
;; (defvar -evil-g-map (lookup-key evil-normal-state-map "g"))
;; (defvar -evil-org-mode-inside-map (lookup-key evil-org-mode-map "i"))

(after! ivy
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-u" 'ivy-previous-line
   "C-e" 'ivy-next-line
   "C-S-e" 'ivy-scroll-up-command
   "C-S-u" 'ivy-scroll-down-command
   "RET" 'ivy-alt-done
   "C-RET" 'ivy-done
   ))

(map!
 :after evil-org
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
 )

(evil-define-motion ok-move-down-15-lines ()
  :type line
  (evil-next-visual-line 15))

(evil-define-motion ok-move-up-15-lines ()
  :type line
  (evil-previous-visual-line 15))

(general-define-key
 :keymaps 'key-translation-map
 "ESC" (kbd "C-g"))

(general-define-key
 :keymaps 'global-map
 "C-å" 'ace-window
 "C-ä" 'other-window
 "C-s" 'save-buffer
 "M-f" 'switch-to-prev-buffer
 "M-p" 'switch-to-next-buffer
 "C-u" 'previous-line
 "C-e" 'next-line
 "C-i" 'end-of-line
 "C-n" 'beginning-of-line
 "M-w" 'er/expand-region
 "<tab>" 'complete-symbol
 )

(general-define-key
 :keymaps 'evil-normal-state-map
 "§" (cmd (evil-ex-nohighlight) (evil-force-normal-state)))

(general-define-key
 :keymaps 'evil-insert-state-map
 "§" 'evil-normal-state
 "C-<tab>" (cmd (insert "\t")))

(general-define-key
 :keymaps 'evil-visual-state-map
 "§" 'evil-exit-visual-state)

(general-define-key
 :keymaps 'evil-replace-state-map
 "§" 'evil-normal-state)

(general-define-key
 :keymaps 'evil-normal-state-map
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
 )

(general-define-key
 :keymaps 'evil-motion-state-map
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
 )

(general-define-key
 :keymaps 'evil-visual-state-map
 "u" nil
 "l" -visual-inside-keymap
 "i" nil
 "L" 'evil-insert
 "A" 'evil-append
 )

(general-define-key
 :keymaps 'evil-operator-state-map
 "l" -operator-inside-keymap
 "i" nil
 "e" 'evil-next-line
 "u" 'evil-previous-line
 )

(general-define-key
 :keymaps '(evil-normal-state-map
            evil-visual-state-map)
 :prefix ","
 "v" (cmd (find-file "~/.doom.d/emacs-config.org"))
 )

(setq fancy-splash-image "~/.doom.d/doom.png")
