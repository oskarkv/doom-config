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

(defvar amdl-definition-keywords '("rules" "state" "values" "var"))

(defvar amdl-font-lock-keywords
  (eval-when-compile
    `(
      ;; preprocessor directive
      (,(concat "#\\w+")
       (0 'amdl-preprocessor-directive-face))
      (,(concat "#}$")
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

(defvar amdl-def-line
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
(defvar amdl-def-line
  (concat "^[ \t]*" (regexp-opt amdl-definition-keywords)
          "\\.[[:alnum:]()\\$]+\\(\\[.*?\\]\\)?:$"))

(defvar amdl-dec-line
  (rx bol (* whitespace)
      (or (and "#" (any "a-z"))
          "#}"
          "~~~"
          (and "@" (any "a-z")))))

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

(defun amdl-defmacro-opening? (pos)
  (save-excursion
    (and
     (progn (goto-char pos) (s-ends-with? ") {" (ok-line-as-string)))
     (not (s-starts-with? "#defmacro" (ok-line-as-string)))
     (progn (next-line -1) (s-starts-with? "#defmacro" (ok-line-as-string))))))

(defun amdl-pos-line-indent (pos)
  (save-excursion
    (goto-char pos)
    (if (amdl-defmacro-opening? pos)
        (progn (next-line -1) (current-indentation))
      (current-indentation))))

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
(defun amdl-def-closing? ()
  (save-excursion
    (beginning-of-line)
    (looking-at "\s*#\}$")))

(defun amdl-under-amdl-def? ()
  (save-excursion
    (beginning-of-line)
    (if (or (looking-at amdl-def-line) (looking-at amdl-dec-line))
        nil
      (let ((start (cadr (syntax-ppss))))
        (let* ((hit (save-excursion (re-search-backward amdl-def-line nil t)))
               (else (or (re-search-backward amdl-dec-line hit t) 0)))
          (when hit
            (goto-char hit)
            (end-of-line))
          (cond ((and hit (not start)) (> hit else))
                ((or (not hit) (< hit else)) nil)
                (t (> (point) start))))))))

(defun amdl-indent-line-amount ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; Om jag får fel med inhibit-modification-hooks, så kanske det är fel på
    ;; detta, d.v.s. jag måste anropa syntax-pps-flush-cache
    (let* ((state (syntax-ppss))
           ;; start of innermost opening paren
           (start (cadr state))
           (opens (length (nth 9 state))))
      (cond
       ;;; Git confilct, maybe use this instead of code below
       ;; comment
       ;; ((and (= 0 opens) (looking-at "[ \t]*//"))
       ;;  (save-excursion
       ;;    (forward-line 1)
       ;;    (amdl-indent-if-body-and-0
       ;;     (amdl-indent-line-amount))))
       ;; ;; annotation
       ;; ((and (= 0 opens) (looking-at "[ \t]*@")) (amdl-indent-if-body 0))
       ;; ;; definition
       ;; ((and (= 0 opens) (amdl-looking-at-def-line?)) (amdl-indent-if-body 0))
       ;; ;; first line after opening paren as last char
       ;; ((and start (amdl-pos-ends-line? start) (amdl-pos-in-prev-line? start))
       ;;  (min (+ 2 (amdl-prev-line-indent))
       ;;       (amdl-pos-indent start)))
       ;; ;; opening paren in prev line but not last
       ;; ((and start (amdl-pos-in-prev-line? start))
       ;;  (amdl-pos-indent start))
       ;; ;; first line after def
       ;; ((save-excursion (next-line -1)
       ;;                  (while (progn (beginning-of-line)
       ;;                                (looking-at "[ \t]*//"))
       ;;                    (next-line -1))
       ;;                  (beginning-of-line)
       ;;                  (looking-at def-line))
       ;;  (amdl-indent-if-body 2))
       ;; ;; closing paren
       ;; ((amdl-closing-paren-first?)
       ;;  (if (amdl-pos-ends-line? start)
       ;;      (amdl-pos-line-indent start)
       ;;    (1- (amdl-pos-indent start))))
       ;; ((< opens (amdl-opening-parens-last-line))
       ;;  (amdl-indent-if-body
       ;;   (if (> (amdl-prev-line-indent) 0) (+ 2 opens) opens)))
       ;; (t (amdl-indent-if-body-and-0 (amdl-prev-line-indent)))))))
       ((looking-at "[ \t]*//")
        (save-excursion
          (while (or (ok-blank-line?)
                     (looking-at "[ \t]*//"))
            (forward-line 1))
          (amdl-indent-line-amount)))
       ;; line is #}
       ((amdl-def-closing?)
        (amdl-pos-line-indent start))
       ;; closing paren first in line
       ((amdl-closing-paren-first?)
        (if (amdl-pos-ends-line? start)
            (amdl-pos-line-indent start)
          (- (amdl-pos-indent start) 1)))
       ;; directly under amdl definition
       ((amdl-under-amdl-def?)
        (save-excursion
          (re-search-backward amdl-def-line)
          (+ 2 (amdl-pos-line-indent (point)))))
       ;; opening parent last in line
       ((and start (amdl-pos-ends-line? start))
        (+ 2 (amdl-pos-line-indent start)))
       ;; opening paren not last in line
       (start
        (amdl-pos-indent start))
       (t 0)))))

(defun amdl-indent-line ()
  (indent-line-to (amdl-indent-line-amount)))


;; Starta level efter indent, 2 spaces = en level.
;; En #defmacro eller #defm ökar level, end_ minskar
;; ok-blank-line?
;; FIXME inte klar hehe
(defun amdl-indent-region (beg end)
  (let ((level 0))))

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
