;;; amdl.el -*- lexical-binding: t; -*-

(defvar amdl-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.amdl\\'" . amdl-mode))

(defface amdl-normal-face '((t :inherit default)) "")
(defface amdl-annotation-face '((t :foreground "#0ff")) "")
(defface amdl-operator-face '((t :foreground "#f0f")) "")
(defface amdl-number-face '((t :foreground "#07f")) "")
(defface amdl-def-face '((t :foreground "#9f0")) "")
(defface amdl-keyword-face '((t :foreground "#f10")) "")
(defface amdl-var-face '((t :foreground "#f70")) "")
(defface amdl-preprocessor-thing-face '((t :foreground "#FC0")) "")
(defface amdl-preprocessor-directive-face '((t :foreground "#090")) "")

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
      ;; preprocessor directive
      (,(concat "#\\w+")
       (0 'amdl-preprocessor-directive-face))
      (,(concat "\\$([_a-zA-Z][_a-zA-Z0-9]*)")
       (0 'amdl-preprocessor-thing-face))
      (,(concat "\\<[_A-Z][_A-Z0-9]*\\>")
       (0 'amdl-preprocessor-thing-face))
      (,(concat "\\$[_a-zA-Z][_a-zA-Z0-9]*")
       (0 'amdl-preprocessor-thing-face))
      ;; definition
      (,(concat "[ \t]*\\(" (regexp-opt keywords) "\\)\\.\\(\\sw+\\)"
                "\\(?:\\[[^\s\n]+\\]\\)?" "\\(:\\)")
       (1 'amdl-keyword-face)
       (2 'amdl-def-face)
       (3 'amdl-normal-face))
      (,(concat "\\<" (regexp-opt keywords) "\\>")
       (0 'amdl-keyword-face))
      (,(concat "\\<\\(event\\)\\.")
       (1 'amdl-var-face))
      ;; annotations
      (,(concat "@\\sw+")
       (0 'amdl-annotation-face))
      ;; true false
      (,(concat "\\<" (regexp-opt '("true" "false") t) "\\>")
       (0 'amdl-number-face))
      ;; durations
      (,(concat "\\<[0-9]+[Mdhms]\\>")
       (0 'amdl-number-face))
      ;; numbers
      (,(concat "\\<[0-9]+\\(\\.[0-9]+\\)?")
       (0 'amdl-number-face))
      ;; operators
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

;;; Indentation

(defvar amdl-comment-regexp "[ \t]*//")

(defvar def-line
  (concat "[ \t]*" (regexp-opt keywords)
          "\\.[[:alnum:]()\\$]+\\(\\[.*?\\]\\)?:$"))

(defun amdl-next-line-indent ()
  (save-excursion
    (while (looking-at amdl-comment-regexp)
      (forward-line 1))
    (beginning-of-line)
    (let ((bol (point)))
      (ok-skip-whitespace-forward)
      (- (point) bol))))

(defun amdl-pos-indent (pos)
  (save-excursion
    (goto-char pos)
    (1+ (current-column))))

(defun amdl-pos-line-indent (pos)
  (save-excursion
    (goto-char pos)
    (current-indentation)))

(defun amdl-prev-line-end ()
  (save-excursion
    (next-line -1)
    (end-of-line)
    (1- (point))))

(defun amdl-pos-ends-line? (pos)
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (= (1+ pos) (point))))

(defun amdl-prev-line-indent ()
  (save-excursion
    (next-line -1)
    (current-indentation)))

(defun amdl-pos-in-prev-line? (pos)
  (save-excursion
    (next-line -1)
    (beginning-of-line)
    (>= pos (point))))

(defun amdl-closing-paren-first? ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^ \t]")
    (seq-contains-p (list ?\) ?\] ?\}) (char-before))))

(defun amdl-opening-parens-last-line ()
  (save-excursion
    (next-line -1)
    (beginning-of-line)
    (length (nth 9 (syntax-ppss)))))

(defun amdl-looking-at-def-line? ()
  (or (looking-at def-line) (looking-at "[ \t]*#[a-z]")))

(defun amdl-in-def? (def-string)
  (save-excursion
    (beginning-of-line)
    (when (not (looking-at (str "[ \t]*#" def-string)))
      (end-of-line)
      (when-let ((def (save-excursion
                        (search-backward (str "#" def-string) nil t)))
                 (end (or (save-excursion
                            (search-backward (str "#end_" def-string) nil t))
                          0)))
        (> def end)))))

(defun amdl-indent-if-body (indent)
  (if (or (amdl-in-def? "defmacro") (amdl-in-def? "defm"))
      (+ 2 indent)
    indent))

(defun amdl-indent-if-body-and-0 (indent)
  (if (= 0 indent) (amdl-indent-if-body 0) indent))

(defun amdl-indent-line-amount ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((state (syntax-ppss))
           ;; start of innermost opening paren
           (start (cadr state))
           (opens (length (nth 9 state))))
      (cond
       ;; comment
       ((and (= 0 opens) (looking-at "[ \t]*//"))
        (save-excursion
          (forward-line 1)
          (amdl-indent-if-body-and-0
           (amdl-indent-line-amount))))
       ;; annotation
       ((and (= 0 opens) (looking-at "[ \t]*@")) (amdl-indent-if-body 0))
       ;; definition
       ((and (= 0 opens) (amdl-looking-at-def-line?)) (amdl-indent-if-body 0))
       ;; first line after opening paren as last char
       ((and start (amdl-pos-ends-line? start) (amdl-pos-in-prev-line? start))
        (min (+ 2 (amdl-prev-line-indent))
             (amdl-pos-indent start)))
       ;; opening paren in prev line but not last
       ((and start (amdl-pos-in-prev-line? start))
        (amdl-pos-indent start))
       ;; first line after def
       ((save-excursion (next-line -1)
                        (while (progn (beginning-of-line)
                                      (looking-at "[ \t]*//"))
                          (next-line -1))
                        (beginning-of-line)
                        (looking-at def-line))
        (amdl-indent-if-body 2))
       ;; closing paren
       ((amdl-closing-paren-first?)
        (if (amdl-pos-ends-line? start)
            (amdl-pos-line-indent start)
          (1- (amdl-pos-indent start))))
       ((< opens (amdl-opening-parens-last-line))
        (amdl-indent-if-body
         (if (> (amdl-prev-line-indent) 0) (+ 2 opens) opens)))
       (t (amdl-indent-if-body-and-0 (amdl-prev-line-indent)))))))

(defun amdl-indent-line ()
  (indent-line-to (amdl-indent-line-amount)))

(defvar amdl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for amdl-mode")

(define-derived-mode amdl-mode java-mode "AMDL"
  (add-hook! 'after-save-hook
             #'delete-trailing-whitespace)
  (setq-local dabbrev-case-fold-search nil)
  (setq-local paragraph-start "\f\\|[ 	]*$")
  (setq-local indent-line-function 'amdl-indent-line)
  (setq-local indent-region-function nil)
  (setq-local font-lock-defaults '(amdl-font-lock-keywords nil nil)))

(map! :map java-mode-map
      "(" nil
      ")" nil
      "[" nil
      "]" nil
      "{" nil
      "}" nil
      ":" nil
      "," nil
      ";" nil
      "/" nil)

(map! :map amdl-mode-map
      :prefix "SPC"
      :n "f" (cmd
               (let ((fill-column 70))
                 (call-interactively #'fill-paragraph))))

(after! amdl-mode
  (add-hook! 'amdl-mode-hook (highlight-numbers-mode -1)))

(provide 'amdl-mode)
