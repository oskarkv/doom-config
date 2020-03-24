;;; -*- lexical-binding: t -*-
(require 'utils)
(require 'evil)
(require 'paredit)
(require 'seq)
(require 'dash)

;; (general-evil-define-key
;;     'normal
;;     :keymaps global-map
;;     "ö" (cmd (princ (apply #'count-lines (-cons-to-list (evilnc-get-comment-bounds))))))

(defvar esexp-element-separator-re "[][[:space:]\n\r(){}]")

(defvar esexp-paren-re "[][(){}]")

(defun esexp--modify-bounds-fn (pos f1 f2)
  (when pos
    (cons (funcall f1 (car pos)) (cons (funcall f2 (cadr pos)) (cddr pos)))))

(defun esexp-shrink-bounds (pos)
  "Make POS contract by one at both ends."
  (esexp--modify-bounds-fn pos #'1+ #'1-))

(defun esexp-widen-bounds (pos)
  "Make POS expand by one at both ends."
  (esexp--modify-bounds-fn pos #'1- #'1+))

(defun esexp-select-paren (open close point &optional end-point)
  "Return the positions (beg end ...) of the closest pair of OPEN and CLOSE
delimiters that encloses POINT."
  (evil-select-paren open close point (or end-point point) 'inclusive 1 t))

(defun esexp-expand-around (pos &optional expand-past-newlines)
  "Expand into the whitespace around pos. If whitespace after pos, use
that. Else use whitespace before pos."
  (save-excursion
    (-let* (((beg end) pos))
      (goto-char end)
      (ok-skip-whitespace-forward expand-past-newlines)
      (if (= (point) end)
          (progn
            (goto-char beg)
            (ok-skip-whitespace-backward expand-past-newlines)
            (setf (car pos) (point)))
        (setf (cadr pos) (point)))
      pos)))

(defun esexp-include-macro-char (pos-beg)
  "If the current selection POS-BEG starts with a macro character (#) then
modify POS-BEG to include it."
  (if (and (char-before pos-beg)
           (= (char-before pos-beg) ?#))
      (1- pos-beg)
    pos-beg))

(defun esexp-exclude-macro-char (pos-beg around)
  (if (and (not around)
           (= (char-after pos-beg) ?#))
      (1+ pos-beg)
    pos-beg))

(defun esexp-correct-bounds (shrink include-whitespace include-newlines pos)
  "Modify POS; if SHRINK is non-nil, shrink POS by one on both sides, then
include whitespace (before or after) if INCLUDE-WHITESPACE is non-nil."
  (ignore-errors
    (setf (car pos) (esexp-exclude-macro-char (car pos) (not shrink)))
    (let* ((pos2 (if shrink (esexp-shrink-bounds pos) pos)))
      (if include-whitespace (esexp-expand-around pos2 include-newlines) pos2))))

(defun esexp--stuff-before-comment ()
  "Returns t if the current line does not start with a comment (after any
leading whitespace)."
    (save-excursion
      (beginning-of-line)
      (not (looking-at (concat "[ \t]*" comment-start)))))

(defun esexp--in-comment ()
  (save-excursion
    (goto-char (point-at-eol))
    (evil-in-comment-p)))

(defun esexp--normal-comment-positions (around)
  "Returns the positions of a normal comment (not a comment after code)."
  ;; We have already determined that there is nothing but whitespace before the
  ;; comment
  (when (esexp--in-comment)
    (save-excursion
      (-let (((beg end) (-cons-to-list (evilnc-get-comment-bounds))))
        (goto-char beg)
        (when (not around)
          (setq beg (re-search-forward (concat "\\s-*" comment-start "+"))))
        (list beg end)))))

(defun esexp--eol-comment-positions (around)
  "Return comment positions when comment is after a line of code."
  (save-excursion
    (let* ((eol (point-at-eol)))
      (beginning-of-line)
      (if around
          (progn
            (re-search-forward (concat "\\s-*" comment-start "+"))
            (goto-char (match-beginning 0)))
        (re-search-forward (concat comment-start "+\\s-*")))
      (when (< (point) eol)
        (list (point) eol)))))

(defun esexp-comment-positions (around)
  (or (if (esexp--stuff-before-comment)
          (esexp--eol-comment-positions around)
        (esexp--normal-comment-positions around))
      '(nil nil)))

(defun esexp-beginning-of-comment ()
  (goto-char (car (esexp-comment-positions))))

(defun esexp-end-of-comment ()
  (goto-char (cadr (esexp-comment-positions))))

(put 'esexp-comment 'end-op 'esexp-end-of-comment)
(put 'esexp-comment 'beginning-op 'esexp-beginning-of-comment)

(evil-define-text-object esexp-around-comment (count &optional beg end type)
  (evil-select-an-object 'esexp-comment beg end type count t))

(evil-define-text-object esexp-inner-comment (count &optional beg end type)
  (evil-select-inner-object 'esexp-comment beg end type count t))

(defun esexp-true-string-positions ()
  (let ((pos (ignore-errors (bounds-of-evil-string-at-point))))
    (when pos
      (setf (car pos) (esexp-include-macro-char (car pos)))
      (-cons-to-list pos))))

(defun esexp-string-positions (around)
  (-let* ((pos (esexp-true-string-positions)))
    (esexp-correct-bounds (not around) nil nil pos)))

(defun esexp-true-form-positions ()
  (let* ((p (point))
         (fn (lambda (open close)
               (ignore-errors
                 (esexp-select-paren open close p))))
         (parens (funcall fn ?\( ?\)))
         (brackets (funcall fn ?\[ ?\]))
         (braces (funcall fn ?\{ ?\}))
         (pos (->> (seq-filter #'identity (list parens brackets braces))
                   (sort-by #'car #'>)
                   car)))
    (setf (car pos) (esexp-include-macro-char (car pos)))
    pos))

(defun esexp-form-positions (around)
  (esexp-correct-bounds (not around) nil nil (esexp-true-form-positions)))

(defun esexp-true-toplevel-positions ()
  (ignore-errors
    (-let* ((pos (esexp-select-paren ?\( ?\) (point)))
            ((beg end) pos))
      (condition-case nil
          (while t
            (-let* ((new-pos (esexp-select-paren ?\( ?\) beg end))
                    ((nb ne) new-pos))
              (setq beg nb end ne)))
        (error nil))
      (setf (car pos) beg)
      (setf (cadr pos) end)
      pos)))

(defun esexp-toplevel-positions (around)
  (esexp-correct-bounds (not around) nil nil (esexp-true-toplevel-positions)))

(defun esexp-beginning-of-element ()
  (re-search-backward esexp-element-separator-re)
  (forward-char))

(defun esexp-end-of-element ()
  (condition-case nil
      (progn
        (re-search-forward esexp-element-separator-re)
        (backward-char))
    (error (re-search-forward "\\'"))))

(put 'esexp-element 'end-op 'esexp-end-of-element)

(put 'esexp-element 'beginning-op 'esexp-beginning-of-element)

(defun esexp-true-element-positions ()
  (cond ((bounds-of-evil-string-at-point)
         (esexp-true-string-positions))
        ((seq-contains '(?\( ?\) ?\[ ?\] ?\{ ?\}) (char-after))
         (esexp-true-form-positions))
        (t (-cons-to-list (bounds-of-thing-at-point 'esexp-element)))))

(defun esexp-element-positions (around)
  (esexp-correct-bounds nil around nil (esexp-true-element-positions)))

(defun esexp-wrap-positions (open close pos-fn insert-where)
  (ignore-errors
    (-let* (((beg end) (funcall pos-fn)))
      (goto-char end)
      (insert close)
      (goto-char beg)
      (insert open)
      (cond ((equal insert-where 'beg)
             (goto-char beg)
             (evil-append 1))
            ((equal insert-where 'end)
             (goto-char end)
             (evil-append 1))))))

(defun esexp-define-wrapper-form (thing wrappers where)
  (-let* (((open close)
           (condp equal wrappers
             ('parens '("(" ")"))
             ('brackets '("[" "]"))
             ('braces '("{" "}")))))
    `(defun ,(intern (str "esexp-wrap-" thing "-" wrappers "-" where)) ()
       (interactive)
       ,(list
         'esexp-wrap-positions
         open close
         `#',(intern (str "esexp-true-" thing "-positions"))
         `',where))))

(defmacro esexp-define-wrappers ()
  `(progn
     ,@(seq-map
        (-lambda ((thing wrappers where))
          (esexp-define-wrapper-form thing wrappers where))
        (for ((thing '("form" "string" "toplevel" "element"))
              (wrapper '(parens brackets braces))
              (place '(beg end)))
          (list thing wrapper place)))))

;; Define wrappers esexp-wrap-thing-type-place, e.g. esexp-wrap-form-parens-beg for
;; all combinations found in esexp-define-wrappers macro.
(esexp-define-wrappers)

(defun esexp-define-text-object-form (modifier name val)
  `(evil-define-text-object
     ,(intern (concat "esexp-" modifier "-" name))
     (count &optional beg end type)
     :extend-selection t
     ,(list (intern (concat "esexp-" name "-positions")) val)))

(defmacro esexp-define-text-objects (&rest key-name-pairs)
  `(progn
     ,@(seq-mapcat
        (-lambda ((key name))
          (list
           (esexp-define-text-object-form "around" name t)
           (esexp-define-text-object-form "inner" name nil)
           `(define-key evil-outer-text-objects-map
              (kbd ,key) ',(intern (concat "esexp-around-" name)))
           `(define-key evil-inner-text-objects-map
              (kbd ,key) ',(intern (concat "esexp-inner-" name)))))
        (seq-partition key-name-pairs 2))))

;; arstarstart arstart arstar
;; arst rstrst rstrs

(esexp-define-text-objects
 "f" "form"
 "s" "string"
 ;; "c" "comment"
 "t" "toplevel"
 "e" "element")

(define-key evil-inner-text-objects-map
  (kbd "d") #'+evil:defun-txtobj)
(define-key evil-outer-text-objects-map
  (kbd "d") #'+evil:defun-txtobj)

(defun esexp-insert-at-head ()
  (interactive)
  (ignore-errors
    (-let* (((beg) (esexp-form-positions t)))
      (goto-char beg)
      (evil-append 1))))

(defun esexp-insert-at-end ()
  (interactive)
  (ignore-errors
    (-let* (((_ end) (esexp-form-positions nil)))
      (goto-char end)
      (evil-insert 1))))

(defun esexp-raise-form ()
  (interactive)
  (ignore-errors
    (-let* (((beg) (esexp-form-positions t)))
      (goto-char beg)
      (paredit-raise-sexp))))

(defun esexp--transpose-sexps-no-error (&optional arg)
  (interactive "p")
  (let ((p (point)))
    (condition-case nil
        (transpose-sexps (or arg 1))
      (scan-error (goto-char p)))))

;; The two different cases of moving back point, in the two esexp-transpose fns
;; are necessary, because the point moves strangely if there is an error, so we
;; have to go back to the absolute position. But if we transposed, we should go
;; back relatively.
(defun esexp-make-transpose-things-fn (thing-fn)
  (lambda (&optional arg)
    (interactive "p")
    (-let* ((p (point))
            ((beg end) (funcall thing-fn))
            (diff (- end p)))
      (goto-char end)
      (esexp--transpose-sexps-no-error (or arg 1))
      (backward-char diff))))

(defalias 'esexp-transpose-sexps
  (esexp-make-transpose-things-fn #'esexp-true-element-positions))

(defalias 'esexp-transpose-forms
  (esexp-make-transpose-things-fn #'esexp-true-form-positions))

(defun esexp-escaped-p (&optional pos)
  (= ?\\ (char-syntax (char-before pos))))

(defun esexp-in-string-p (&optional pos)
  (save-excursion
    (goto-char (or pos (point)))
    (nth 3 (syntax-ppss))))

(defun esexp-in-string-or-comment (&optional pos)
  (save-excursion
    (goto-char (or pos (point)))
    (or (esexp-in-string-p)
        (evil-in-comment-p))))

(defun esexp-not-real-paren-p (&optional pos)
  (or (esexp-in-string-or-comment pos)
      (esexp-escaped-p pos)))

(defun esexp-real-paren-p (&optional pos)
  (not (esexp-not-real-paren-p pos)))

(defun esexp-forward-paren ()
  (interactive)
  (do-while (esexp-not-real-paren-p)
    (forward-char)
    (re-search-forward esexp-paren-re)
    (backward-char)))

(defun esexp-backward-paren ()
  (interactive)
  (do-while (esexp-not-real-paren-p)
    (re-search-backward esexp-paren-re)))

(defun esexp-opening-p (char)
  (seq-contains '(?\( ?\{ ?\[) char))

(defun esexp-matching-paren (char)
  (condp = char
    (?\( ?\))
    (?\) ?\()
    (?\[ ?\])
    (?\] ?\[)
    (?\{ ?\})
    (?\} ?\{)))

(defun esexp-matching-paren-pos (&optional pos)
  (interactive)
  (save-excursion
    (goto-char (or pos (point)))
    (let* ((c (char-after))
           (opening (esexp-opening-p c))
           (dir (if opening 1 -1))
           (stack nil)
           (last-pos -1))
      (forward-char)
      (when (esexp-matching-paren c)
        (push c stack)
        (catch 'return
          (while t
            (when (not opening) (backward-char))
            (re-search-forward esexp-paren-re nil t dir)
            (when (not opening) (forward-char))
            (let ((p (point)))
              ;; If (= last-pos p) then we are stuck at the eof/bof.
              (when (= last-pos p) (throw 'return nil))
              (setq last-pos p)
              (when (esexp-real-paren-p (1- p))
                (let* ((cc (char-before))
                       (top (car stack)))
                  (cond ((= top (esexp-matching-paren cc))
                         (pop stack))
                        ((equal opening (esexp-opening-p cc))
                         (push cc stack))
                        (t (throw 'return nil)))
                  (if (equal stack nil)
                      (throw 'return (1- p))))))))))))

(defun esexp-advice-fn (delete beg end type &optional register yank-handler)
  (print type)
  (print (list beg end))
  (funcall delete beg end type register yank-handler))

;;(advice-add 'evil-delete :around 'advice-fn)
;;(advice-remove 'evil-delete 'advice-fn)

(defun esexp--slurp-or-barf-fn (f stay-at-end)
  "Make a slurp or barf function using the paredit function F, but that does not
slurp or barf in strings (instead works on the surrounding form) and ignores
errors if slurping of barfing can't happen. If STAY-AT-END is t, move point to
the end of the form while running F, useful for slurping or barfing at front. If
STAY-AT-END is nil, move to front instead."
  (lambda (&optional num)
    (interactive "P")
    (ignore-errors
      (save-excursion
        (-let* (((beg end) (esexp-true-form-positions))
                (stay-at (if stay-at-end (1- end) (1+ beg))))
          (goto-char stay-at)
          (funcall f num))))))

(defalias 'esexp-forward-slurp-sexp
  (esexp--slurp-or-barf-fn #'paredit-forward-slurp-sexp nil))

(defalias 'esexp-forward-barf-sexp
  (esexp--slurp-or-barf-fn #'paredit-forward-barf-sexp nil))

(defalias 'esexp-backward-slurp-sexp
  (esexp--slurp-or-barf-fn #'paredit-backward-slurp-sexp t))

(defalias 'esexp-backward-barf-sexp
  (esexp--slurp-or-barf-fn #'paredit-backward-barf-sexp t))

(after! cider
  (defun esexp-cider-macroexpand ()
    (interactive)
    (save-excursion
      (-let (((beg end) (esexp-true-form-positions)))
        (goto-char end)
        (cider-macroexpand-1)))))

(provide 'esexp)
