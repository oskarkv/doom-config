;;; ~/.doom.d/utils.el -*- lexical-binding: t; -*-
(require 'dash)
(require 'seq)

(defun range (start &optional end step)
  (unless end
    (setq end start
      start 0))
  (number-sequence start (1- end) step))

(defun println (&rest args)
  (let ((x (s-join " " (mapcar #'str args))))
    (print x)
    x))

(defun printit (&rest xs)
  (apply #'println xs)
  (car (last xs)))

(defmacro pm (form)
  `(printit ',form "=" ,form))

(defmacro time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro with-gensyms (syms &rest body)
  (declare (indent 1))
  `(let (,@(seq-map (lambda (sym) (list sym '(cl-gensym))) syms))
     ,@body))

(defmacro section (&rest code)
  "Macro just for code folding purposes; ignores the first argument."
  (declare (indent 1))
  `(progn ,@(cdr code)))

(defmacro section-comment (&rest _)
  "Macro for code folding and commenting; does nothing."
  (declare (indent 1))
  nil)

(defun pos? (n)
  (> n 0))

(defun neg? (n)
  (< n 0))

(defun zero? (n)
  (= 0 n))

(defalias 'as-> '-as->)
(defalias 'some-> '-some->)
(defalias 'some->> '-some->>)

(defmacro case (expr &rest cases)
  (with-gensyms (result)
  `(let ((,result ,expr))
     (cond ,@(mapcar (-lambda ((a b))
                       (if (listp a)
                           (list `(cl-member ,result ',a :test #'equal) b)
                           (list `(equal ,result ',a) b)))
                     cases)))))

(defun ok-line-as-string ()
  (s-trim (thing-at-point 'line t)))

(defmacro cond-> (expr &rest clauses)
  ;; (assert (evenp (length clauses))
  ;;         t "cond-> requires an odd number of args")
  (let* ((g (gensym))
         (steps (seq-map (-lambda ((test step))
                           `(if ,test (-> ,g ,step) ,g))
                         (seq-partition clauses 2))))
    `(let* ((,g ,expr)
            ,@(seq-partition (-interleave (-repeat (length clauses) g)
                                          steps)
                             2))
       ,g)))

(defmacro cond->> (expr &rest clauses)
  ;; (assert (evenp (length clauses))
  ;;         t "cond->> requires an odd number of args")
  (let* ((g (gensym))
         (steps (seq-map (-lambda ((test step))
                           `(if ,test (->> ,g ,step) ,g))
                         (seq-partition clauses 2))))
    `(let* ((,g ,expr)
            ,@(seq-partition (-interleave (-repeat (length clauses) g)
                                          steps)
                             2))
       ,g)))

(defun ok-blank-line? (&optional point)
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun ok-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ok-read-lines (file)
  "Return a list of lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defmacro cmd (&rest body)
  "Wraps BODY in an interactive lambda."
  (declare (indent defun))
  `(lambda () (interactive) ,@body))

(defmacro fn (&rest body)
  "Wraps BODY in a lambda."
  (declare (indent defun))
  `(lambda (&rest args) ,@body))

(defmacro update-place (place fn &rest args)
  `(setf ,place (funcall ,fn ,place ,@args)))

(defun sort-by (key-fn comp seq)
  (sort seq (lambda (a b)
              (funcall comp (funcall key-fn a) (funcall key-fn b)))))

(defun str (&rest args)
  (with-output-to-string
    (mapc 'princ args)))

(defmacro for (bindings body)
  "E.g. (for ((a some-list) (b some-list)) (list a b))"
  (declare (indent 1))
  (letrec ((make-dolists (lambda (body &optional var-list &rest more)
                           (if var-list
                               `(dolist ,var-list
                                  ,(apply make-dolists body more))
                             body))))
    (with-gensyms [acc]
      `(let ((,acc nil))
         ,(apply make-dolists `(setq ,acc (cons ,body ,acc))
                 bindings)
         (reverse ,acc)))))

(defmacro do-while (test &rest body)
  "Evaluates BODY then tests TEST, if non-nil do another iteration."
  (declare (indent 1))
  `(while
       (progn
         ,@body
         ,test)))

(defmacro condp (pred expr &rest clauses)
  "Takes a binary predicate, an expression, and a set of clauses.
Each clause takes the form:

 (test-expr result-expr)

For each clause, (pred test-expr expr) is evaluated. If it returns
non-nil, the clause is a match, and the result-expr is returned."
  (declare (indent 2))
  (with-gensyms [p e]
    `(let ((,p (function ,pred))
           (,e ,expr))
       (cond ,@(seq-map
                (-lambda ((test result))
                  (list (list 'funcall p test e) result))
                clauses)))))

(defun ok-hours-and-mins (mins)
  (let* ((mins (if (neg? mins) (+ (* 60 24) mins) mins))
         (hours (str (/ mins 60)))
         (mins (str (mod mins 60))))
    (concat hours ":" (if (< (length mins) 2) "0" "") mins)))

(defun ok-num-to-circles (n)
  (let* ((n (string-to-number (s-replace ":" "." n)))
         (f (floor n))
         (d (/ (* (- n f) 10) 6)))
    (str (s-repeat f "●")
         (condp <= d
           (0.749 "◕")
           (0.499 "◑")
           (0.249 "◔")
           (0 "")))))

;; (HAVE NOT TESTED YET (the intern-soft part))
(defmacro ok-emacs-mode-in-mode (mode-var)
  "Given a mode name, make Emacs start that mode in Emacs mode instead of Evil
mode."
  `(add-hook (intern-soft (concat (symbol-name ,mode-var) "-hook"))
    (cmd (if ,mode-var
             (evil-emacs-state)
           (evil-exit-emacs-state)))))

(defun ok-symbol (&rest args)
  (intern (apply #'concat (mapcar #'str args))))

(defun ok-parse-time (time-string)
  "Given an HH:MM string, returns the total minutes that TIME-STRING
represents."
  (let* ((split (split-string time-string ":"))
         (nums (mapcar #'string-to-number split)))
    (+ (* 60 (car nums)) (cadr nums))))

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

(defun ok-string-drop-at-end (s n)
  "Drops the last N characters from the string S."
  (substring s 0 (- (length s) n)))

(defun ok-contains-whitespace? (char-or-string &optional including-newlines)
  "Returns non-nil if CHAR-OR-STRING contains whitespace. Only
space and \t are considered withespace, unless INCLUDING-NEWLINES
is non-nil, then also \r and \n are considered whitespace."
  (let ((s (if (characterp char-or-string)
               (char-to-string char-or-string)
             char-or-string)))
    (string-match (if including-newlines "[ \t\r\n]" "[ \t]") s)))

(defun ok-skip-whitespace-forward (&optional including-newlines)
  (while (-some-> (char-after) (ok-contains-whitespace? including-newlines))
    (forward-char)))

(defun ok-skip-whitespace-backward (&optional including-newlines)
  (while (-some-> (char-before) (ok-contains-whitespace? including-newlines))
    (backward-char)))

(defun ok-save-buffers (buffers)
  (dolist (b buffers)
    (with-current-buffer b
      (save-buffer))))

(defun ok-save-buffers-matching (regex)
  (let* ((buffers (cl-remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-file-name buffer)))
                       (and name (s-matches? regex name))))
                   (buffer-list))))
    (ok-save-buffers buffers)))

(defun ok-switch-to-window (name)
  (select-window (get-buffer-window name)))

(defmacro ok-projectile-run-in-root (&rest code)
  `(projectile-with-default-dir
       (projectile-ensure-project (projectile-project-root))
     ,@code))

(defun ok-project-path-contains? (string)
  (ignore-errors
    (s-contains-p string (projectile-project-root))))

(defun ok-shell-command-in-root (command)
  (ok-projectile-run-in-root
   (apply #'start-process command (str "*" command "*")
          (s-split-words command))))

(defun ok-kebab-to-camel-case (string)
  (s-replace-regexp "-[^\s]" (lambda (x) (s-upcase (s-right 1 x))) string))


(defun s-upcase-first-letter (s)
  "Convert S first character to upper case."
  (declare (side-effect-free t))
  (concat (upcase (substring s 0 1)) (substring s 1)))

(defun ok-org-fill-buffer-excluding-code-blocks ()
  "Fill the buffer, excluding code blocks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (start end)
      (setq start (point))
      (while (re-search-forward "#\\+BEGIN_SRC" nil t)
        (goto-char (bol))
        (setq end (1- (point)))
        (evil-visual-select start end)
        (org-fill-paragraph nil t)
        (when (re-search-forward "#\\+END_SRC" nil t)
          (setq start (1+ (point)))))
      (evil-visual-select start (point-max))
      (org-fill-paragraph nil t))))

(defmacro save-mode-excursion (&rest body)
  (declare (indent 0))
  (with-gensyms (m)
    `(let ((,m major-mode))
       ,@body
       (funcall ,m))))

(provide 'utils)
