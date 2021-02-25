;;; amdl.el -*- lexical-binding: t; -*-

(defvar amdl-mode-hook nil)

(defvar amdl-mode-map
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for AMDL major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.amdl\\'" . amdl-mode))

(defface amdl-normal-face '((t (:inherit 'default))) "")
(defface amdl-annotation-face '((t (:inherit 'default :foreground "#0ef"))) "")
(defface amdl-operator-face '((t (:inherit 'default :foreground "#f0f"))) "")
(defface amdl-number-face '((t (:inherit 'default :foreground "#66f"))) "")
(defface amdl-def-face '((t (:inherit 'default :foreground "#9f0"))) "")
(defface amdl-keyword-face '((t (:inherit 'default :foreground "#f10"))) "")
(defface amdl-var-face '((t (:inherit 'default :foreground "#f70"))) "")

(defvar keywords '("rules"
                   "state"
                   "globals"
                   "values"
                   "features"
                   "var"
                   "lists"
                   "acglists"))

(defvar amdl-font-lock-keywords
  (eval-when-compile
    `(
      (,(concat "\\<event\\>")
       (0 'amdl-var-face))
      (,(concat "\\<" (regexp-opt keywords) "\\>")
       (0 'amdl-keyword-face))
      ;; annotations
      (,(concat "@\\sw+")
       (0 'amdl-annotation-face))
      ;; true false
      (,(concat "\\<" (regexp-opt '("true" "false") t) "\\>")
       (0 'amdl-number-face))
      ;; numbers
      (,(concat "\\<[0-9]+\\(?:\\.[0-9]+\\)?\\>")
       (0 'amdl-number-face))
      ;; durations
      (,(concat "\\<[0-9]+[Mdhms]\\>")
       (0 'amdl-number-face))
      ;; definition
      (,(concat "^\\(\\sw+\\)\\.\\(\\sw+\\)\\(:\\)")
       (1 'amdl-keyword-face)
       (2 'amdl-def-face)
       (3 'amdl-normal-face))
      ;; variables
      (,(concat (regexp-opt '("="
                              "+" "-" "*" "/"
                              ".."
                              "==" "!=" "<" "<=" ">" ">="
                              "!" "&&" "||"
                              "~#" "!#"
                              "==#" "!=#" "<#" "<=#" ">#" ">=#"
                              "?" ":"
                              "~?" ;; switch
                              "??" ;; default value
                              "~" ;; exists
                              "~=" ;; regex match
                              "~:" ;; regex replace
                              )))
       (0 'amdl-operator-face))
      )))

(defvar amdl-comment-regexp "[ \t]*//")

;; For comments
(defun amdl-find-next-lines-indent ()
  (save-excursion
    (while (looking-at amdl-comment-regexp)
      (forward-line 1))
    (beginning-of-line)
    (let ((bol (point)))
      (ok-skip-whitespace-forward)
      (- (point) bol))))

(defun amdl-find-paren-indent (start)
  (save-excursion
    (goto-char start)
    (1+ (current-column))))

(defvar def-line (concat (concat "[ \t]*" (regexp-opt keywords) "\\.\\sw+:")))

(defun amdl-indent-line-amount ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; comment
     ((looking-at "[ \t]*//") (save-excursion
                                (forward-line 1)
                                (amdl-indent-line-amount)))
     ;; annotation
     ((looking-at "[ \t]*@") 0)
     ;; definition
     ((looking-at def-line) 0)
     ;; body
     (t (let ((start (second (syntax-ppss))))
          (cond (start (amdl-find-paren-indent start))
                (t 2)))))))

(defun amdl-indent-line ()
  (interactive)
  (indent-line-to (amdl-indent-line-amount)))

(defvar amdl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for amdl-mode")

(defun amdl-mode ()
  "Major mode for editing ARIC Model Definition Language files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table amdl-mode-syntax-table)
  (use-local-map amdl-mode-map)
  (setq font-lock-defaults
        '(amdl-font-lock-keywords
          nil nil))
  (setq indent-line-function 'amdl-indent-line)
  (setq major-mode 'amdl-mode)
  (setq mode-name "AMDL")
  (run-hooks 'amdl-mode-hook))

(provide 'amdl-mode)
