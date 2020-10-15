;;; -*- lexical-binding: t -*-
(require 'dash)
(require 'evil)
(require 'evil-nerd-commenter)
(require 'paredit)
(require 'seq)
(require 'smartparens)
(require 'utils)

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

;; This function seems buggy if expand-past-newlines is t,
;; but it's not used for now. Use
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

(defun esexp-expand-lines-up (pos)
  (save-excursion
    (-let (((beg end) pos))
      (goto-char beg)
      (do-while (ok-blank-line?)
        (evil-previous-line))
      (evil-next-line)
      (beginning-of-line)
      (list (point) end))))

(defun esexp-expand-lines-down (pos)
  (save-excursion
    (-let (((beg end) pos))
      (goto-char end)
      (while (ok-blank-line?)
        (evil-next-line))
      (beginning-of-line)
      (list beg (point)))))

(defun esexp-expand-around-lines (pos &optional only-if-surrounded-by-space)
  (save-excursion
    (-let (((beg end) pos)
           blank-above blank-below)
      (goto-char beg)
      (evil-previous-line)
      (setq blank-above (ok-blank-line?))
      (goto-char end)
      (setq blank-below (ok-blank-line?))
      (if (or (and blank-below blank-above) (not only-if-surrounded-by-space))
          (if blank-below
              (esexp-expand-lines-down pos)
            (esexp-expand-lines-up pos))
        pos))))

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
      (if include-whitespace
          (esexp-expand-around pos2 include-newlines)
        pos2))))

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
  (-let (((beg end) (esexp-correct-bounds
                     (not around) nil nil (esexp-true-toplevel-positions))))
    (when around (update-place end #'1+))
    (cond-> (list beg end)
            around (esexp-expand-around-lines nil))))

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
        (beginning-of-line)
        (setq beg (point))
        (if around
            (list beg end)
          (progn
            (goto-char beg)
            (setq beg (re-search-forward
                       (concat "\\s-*" comment-start "+[ \t]+")))
            (update-place end #'1-)
            (list beg end)))))))

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
        (cond-> (-let (((beg end) (esexp--normal-comment-positions around)))
                  (when (> (count-lines beg end) 1)
                    (update-place end #'1+))
                  (list beg end))
                around (esexp-expand-around-lines t)))
      '(nil nil)))

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

;; Define wrappers esexp-wrap-thing-type-place, e.g. esexp-wrap-form-parens-beg
;; for all combinations found in esexp-define-wrappers macro.
(esexp-define-wrappers)

(defun esexp-define-text-object-form (modifier name val type)
  `(evil-define-text-object
     ,(intern (concat "esexp-" modifier "-" name))
     (count &optional beg end type)
     :extend-selection t
     ,@(when type (list :type type))
     ,(list (intern (concat "esexp-" name "-positions")) val)))

(defmacro esexp-define-text-objects (&rest key-name-type-lists)
  `(progn
     ,@(seq-mapcat
        (-lambda ((key name around-type inner-type))
          (list
           (esexp-define-text-object-form "around" name t around-type)
           (esexp-define-text-object-form "inner" name nil inner-type)
           `(define-key evil-outer-text-objects-map
              (kbd ,key) ',(intern (concat "esexp-around-" name)))
           `(define-key evil-inner-text-objects-map
              (kbd ,key) ',(intern (concat "esexp-inner-" name)))))
        key-name-type-lists)))

(esexp-define-text-objects
 ("f" "form")
 ("s" "string")
 ("c" "comment" line nil)
 ("t" "toplevel" line nil)
 ("e" "element"))

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

;;; Non-Lisp things

;; FIXME: Make C-things be not just calls, but strings and lists too.

(defun ok-string-bounds ()
  "Get the bounds of string or comment the point is in."
  (save-excursion
    (let ((bounds (sp-get-quoted-string-bounds)))
      (when (and (not bounds) (= (char-after) ?\"))
        (forward-char)
        (setq bounds (sp-get-quoted-string-bounds)))
      (when bounds
        (-cons-to-list bounds)))))

(defun ok-c-thing-bounds (separator-regex)
  "Finds the bounds of a C thing at point. SEPARATOR-REGEX should
match what does not count as part of the C thing."
  (or
   (ok-string-bounds)
   (save-excursion
     (let (beg)
       (cl-flet ((on-paren? (which)
                            (-contains? (if (eq which 'closing)
                                            (list ?\) ?\] ?\})
                                          (list ?\( ?\[ ?\{))
                                        (char-after))))
         ;; If we start on a closing paren, bounds include the whole call
         (if (on-paren? 'closing)
             (evil-jump-item))
         ;; If we are on opening paren, bounds also include the whole call
         (if (on-paren? 'opening)
             ;; If no identifier before the paren, start here
             (if (string-match-p separator-regex (string (char-before)))
                 (setq beg (point))
               (backward-char)))
         ;; If there is an identifier, search backward will succeed
         (search-backward-regexp separator-regex)
         (forward-char)
         (if (null beg) (setq beg (point)))
         (search-forward-regexp separator-regex)
         (backward-char)
         (if (on-paren? 'opening)
             (evil-jump-item)
           (backward-char))
         (forward-char)
         (list beg (point)))))))

(defun ok-jump-to-next-c-thing (separator-regex dir)
  "Move point to the next C thing. If DIR is positive, move jump
forward, otherwise backward."
  (-let (((beg end) (ok-c-thing-bounds separator-regex)))
    (goto-char (if (pos? dir) end beg))
    (search-forward-regexp "[][[:alnum:]\_(){}\'\"]" nil t dir)
    (if (pos? dir) (backward-char))))

(defun ok-c-thing-jump-allowed (separator-regex dir)
  "Check if jumping is allowed. Jumping is not allowed if we are
at the last thing inside parentheses and we are trying to move
foward (DIR is positive), or vice versa."
  (save-excursion
    (ok-jump-to-next-c-thing separator-regex dir)
    (not (s-contains? (string (char-after)) (if (pos? dir) ")]}" "([{")))))

(defun ok-next-c-thing-bounds (separator-regex dir)
  "Returns the bounds of the next C thing in DIR direction."
  (save-excursion
    (ok-jump-to-next-c-thing separator-regex dir)
    (ok-c-thing-bounds separator-regex)))

(defun ok-swap-text (bounds bounds2)
  "Swap the the positions of the pieces of text in BOUNDS and
BOUNDS2. Preserves the position of point relative to the word it
is in."
  ;; bounds must come before bounds2
  (if (> (car bounds) (car bounds2))
      (ok-swap-text bounds2 bounds)
    (-let* ((old-point (point))
            ((beg end) bounds)
            ((beg2 end2) bounds2)
            (len (- end beg))
            (len2 (- end2 beg2))
            ;; delete last text first to not mess up bounds
            (text2 (delete-and-extract-region beg2 end2))
            (text (delete-and-extract-region beg end)))
      (goto-char (- beg2 len))
      (insert text)
      (goto-char beg)
      (insert text2)
      ;; fix point position
      (if (<= beg old-point end)
          (goto-char (+ beg2 (- len2 len) (- old-point beg)))
        (goto-char (+ beg (- old-point beg2)))))))

(defun ok-transpose-c-things (separator-regex dir)
  "Transposes C things."
  (ok-swap-text
   (ok-c-thing-bounds separator-regex)
   (ok-next-c-thing-bounds separator-regex dir)))

(defvar ok-python-separator-regex "[^[:alnum:]\.\_]")

(defun ok-python-transpose-thing (dir)
  (interactive)
  (if (ok-c-thing-jump-allowed ok-python-separator-regex dir)
      (ok-transpose-c-things ok-python-separator-regex dir)))

(defun ok-python-move-up-len (&optional bigger)
  "Returns the distance point moves if going backwards up through
a paren. If BIGGER is non-nil, go through parens that are stacked
together."
  (save-excursion
    (let ((old-point (point)))
      (sp-backward-up-sexp)
      (while (and bigger (s-contains? (string (char-before)) "([{"))
        (backward-char))
      (- old-point (point)))))

(defun ok-python-transpose-big-thing (dir &optional bigger)
  "Like ok-python-transpose-thing but considers the wrapping call
or list to be the thing to transpose."
  (interactive)
  (let ((distance (ok-python-move-up-len bigger)))
    (backward-char distance)
    (if (ok-c-thing-jump-allowed ok-python-separator-regex dir)
        (ok-transpose-c-things ok-python-separator-regex dir))
    (forward-char distance)))

(evil-define-text-object ok-python-thing-text-object
  (count &optional beg end type)
  (ok-c-thing-bounds ok-python-separator-regex))

(provide 'esexp)
