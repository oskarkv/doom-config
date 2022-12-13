;;; ~/.doom.d/utils.el -*- lexical-binding: t; -*-
(require 'dash)
(require 'seq)

(defun println (&rest args)
  (print (s-join " " (mapcar #'str args))))

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

(defalias 'as-> '-as->)
(defalias 'some-> '-some->)
(defalias 'some->> '-some->>)

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

;; (HAVE NOT TESTED YET (the intern-soft part))
(defmacro ok-emacs-mode-in-mode (mode-var)
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

(provide 'utils)
