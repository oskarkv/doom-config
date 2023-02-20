;;; tsexp.el -*- lexical-binding: t; -*-
(require 'tsc)
(require 's)
(require 'seq)
(require 'utils)
(require 'tree-sitter)

(defvar tsexp-atom-at-point-goes-up-to
  '(list_splat_pattern
    list_splat
    dictionary_splat_pattern
    dictionary_splat
    typed_parameter))

;; Siblings to children of these will be looked for outside of them too.
(defvar tsexp-siblings-outside
  '(binary_operator
    named_expression ; only :=
    assignment
    boolean_operator
    comparison_operator))

;; These are considered as having no siblings, so elements can't be transposed.
(defvar tsexp-no-siblings
  '(function_definition
    class_definition))


(defvar tsexp-containers-only-when-topmost
  '(binary_operator
    boolean_operator
    comparison_operator))

(defvar tsexp-containers
  '(assignment
    not_operator
    named_expression ; only :=
    parenthesized_expression
    call
    list
    tuple
    dictionary
    pair
    set
    class_definition
    function_definition
    if_statement
    while_statement
    for_statement
    try_statement
    nonlocal_statement
    global_statement
    block
    with_statement
    finally_clause
    except_clause
    import_statement
    import_from_statement))

(defun remove-& (coll)
  (seq-filter (lambda (x) (not (s-starts-with? "&" (str x)))) coll))

(defun to-name (x)
  (if (tsc-node-p x)
      (list (tsc-node-type x) (tsc-node-text x))
    x))

(defmacro pdefun (name args &rest body)
  `(defun ,name ,args
     (interactive)
     (apply #'println "calling" ',name "args:" (mapcar #'to-name (list ,@(remove-& args))))
     (let ((result (progn ,@body)))
       (println "result from" ',name ":" (to-name result))
       result)))

(defun tsexp-get-root-node ()
  (tsc-root-node tree-sitter-tree))

(defun tsexp-get-parent (node)
  (when node (tsc-get-parent node)))

(defun tsexp-get-first-named-child (node)
  (tsc-get-nth-child node 0))

(defun tsexp-get-last-named-child (node)
  (tsc-get-nth-named-child node (1- (tsc-count-named-children node))))

(defun tsexp-named? (node)
  (symbolp (tsc-node-type node)))

(defun tsexp-node-type-is? (node kind)
  (equal (tsc-node-type node) kind))

(defun tsexp-node-in? (node coll)
  (when node
    (-contains? coll (tsc-node-type node))))

(defun tsexp-up-if-not-named (node)
  (if (tsexp-named? node) node (tsexp-get-parent node)))

(defun tsexp-count-named-siblings (node)
  (tsc-count-named-children (tsexp-get-parent node)))

(defun tsexp-same-bounds-as-parent? (node)
  (when node
    (when-let ((p (tsexp-get-parent node)))
      (equal (tsc-node-position-range node)
             (tsc-node-position-range p)))))

(defun tsexp-skip-to-last-invisible-node (node)
  (while (tsexp-same-bounds-as-parent? node)
    (setq node (tsexp-get-parent node)))
  node)

;; FIXME questionable
(defun tsexp-up-to-atom (node)
  (let ((current-node node))
    (while (and current-node
                (not (tsexp-node-in? current-node
                                     tsexp-atom-at-point-goes-up-to)))
      (setq current-node (tsexp-get-parent current-node)))
    (if current-node current-node node)))

;; FIXME questionable
(defun tsexp-atom-at-point ()
  (tsexp-skip-to-last-invisible-node
   (tsexp-up-to-atom
    (tsexp-up-if-not-named
     (tree-sitter-node-at-point)))))

(defun tsexp-up-while-same-type (node)
  (let ((type (tsc-node-type node))
        (parent (tsexp-get-parent node)))
    (if (and parent (equal (tsc-node-type parent) type))
        (tsexp-up-while-same-type parent)
      node)))

(defun tsexp-is-container? (node)
  (if (tsexp-node-in? node tsexp-containers-only-when-topmost)
      (equal node (tsexp-up-while-same-type node))
    (tsexp-node-in? node tsexp-containers)))

(defun tsexp-parent-container (node &optional levels)
  (dotimes (i (or levels 1))
    (setq node (tsexp-get-parent node))
    (while (and node (not (tsexp-is-container? node)))
      (setq node (tsexp-get-parent node))))
  (if (pos? levels)
      (tsexp-skip-to-last-invisible-node node)
    node))

(defun tsexp-container-at-point (&optinonal levels)
  (tsexp-parent-container (tsexp-atom-at-point) levels))

(defun tsexp-node-bounds (node)
  (when node
    (let ((pos (tsc-node-position-range node)))
      (list (car pos) (cdr pos)))))

(defun tsexp-swap-text (bounds bounds2)
  "Swap the the positions of the pieces of text in BOUNDS and
BOUNDS2. Preserves the position of point relative to the word it
is in."
  ;; bounds must come before bounds2
  (if (> (car bounds) (car bounds2))
      (tsexp-swap-text bounds2 bounds)
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

(defun tsexp-raise-text (small-bounds big-bounds)
  (when (and small-bounds big-bounds)
    (-let* ((old-point (point))
            ((beg end) small-bounds)
            ((big-beg big-end) big-bounds)
            (len (- end beg))
            (len2 (- big-end big-beg))
            (remains (buffer-substring-no-properties beg end)))
      (delete-region big-beg big-end)
      (goto-char big-beg)
      (insert remains)
      ;; fix point position
      (goto-char (+ (- old-point beg) big-beg)))))

(defun tsexp-get-node-or-child (node dir)
  (if (tsexp-node-in? node tsexp-siblings-outside)
      (tsexp-get-node-or-child
       (funcall (if (pos? dir)
                    #'tsexp-get-first-named-child
                  #'tsexp-get-last-named-child)
                node)
       dir)
    node))

;; Could perhaps just seach forward in the buffer if in non-container.
(defun tsexp--get-sibling (node dir)
  "Gets a sibling of node, but one should also call
tsexp-get-node-or-child on the result."
  (if-let ((f (if (pos? dir)
                  #'tsc-get-next-named-sibling
                #'tsc-get-prev-named-sibling))
           (sib (funcall f node)))
      sib
    (when-let ((parent (tsexp-get-parent node))
               (_ (tsexp-node-in? parent tsexp-siblings-outside)))
      (tsexp-get-sibling parent dir))))

(defun tsexp-get-sibling (node dir)
  (when (not (tsexp-node-in? (tsexp-get-parent node) tsexp-no-siblings))
    (tsexp-get-node-or-child (tsexp--get-sibling node dir) dir)))

(defun tsexp-transpose (node dir)
  (when-let ((_ node)
             (sib (tsexp-get-sibling node dir)))
    (tsexp-swap-text
     (tsexp-node-bounds node)
     (tsexp-node-bounds sib))))

(defun tsexp-transpose-atom-forward ()
  (interactive)
  (tsexp-transpose
   (tsexp-parent-container (tsexp-atom-at-point) 0) 1))

(defmacro tsexp-def-transpose-cmd (name dir)
  (let ((levels (cl-case name
                  ('atom 0)
                  ('container 1)
                  ('ccontainer 2)))
        (dir-number (if (equal dir 'forward) 1 -1)))
    `(defun ,(ok-symbol 'tsexp-transpose- name '- dir) ()
       (interactive)
       (,#'tsexp-transpose
        (tsexp-parent-container (tsexp-atom-at-point) ,levels) ,dir-number))))

(tsexp-def-transpose-cmd atom forward)
(tsexp-def-transpose-cmd atom backward)
(tsexp-def-transpose-cmd container forward)
(tsexp-def-transpose-cmd container backward)
(tsexp-def-transpose-cmd ccontainer forward)
(tsexp-def-transpose-cmd ccontainer backward)

(defmacro tsexp-def-raise-cmd (name level)
  `(defun ,(ok-symbol 'tsexp-raise- name) ()
     (interactive)
     (let ((node (tsexp-atom-at-point)))
       (tsexp-raise-text
        (tsexp-node-bounds (tsexp-parent-container node ,level))
        (tsexp-node-bounds (tsexp-parent-container node ,(1+ level)))))))

(tsexp-def-raise-cmd atom 0)
(tsexp-def-raise-cmd container 1)
(tsexp-def-raise-cmd ccontainer 2)

(defun tsexp-wrap-text (bounds what place)
  (-let* ((open what)
          (close (case open
                       ("\"" "\"")
                       ("(" ")")
                       ("[" "]")
                       ("{" "}")))
          ((beg end) bounds))
    (goto-char end)
    (insert close)
    (goto-char beg)
    (insert open)
    (goto-char
     (if (equal 'beg place) (+ beg (if (equal open "(") 0 1)) (1+ end)))
    (evil-insert-state)))

(defmacro tsexp-def-wrap-cmd (level opening place)
  (let ((to-wrap (case level
                       (0 'element)
                       (1 'form)))
        (in-what (case opening
                       ("\"" 'string)
                       ("(" 'parens)
                       ("[" 'brackets)
                       ("{" 'braces))))
    `(defun ,(ok-symbol 'tsexp-wrap- to-wrap '- in-what '- place) ()
       (interactive)
       (tsexp-wrap-text (tsexp-node-bounds
                         (tsexp-parent-container (tsexp-atom-at-point) ,level))
                        ,opening ',place))))

(tsexp-def-wrap-cmd 0 "\"" beg)
(tsexp-def-wrap-cmd 0 "(" beg)
(tsexp-def-wrap-cmd 0 "[" beg)
(tsexp-def-wrap-cmd 0 "{" beg)
(tsexp-def-wrap-cmd 1 "\"" beg)
(tsexp-def-wrap-cmd 1 "(" beg)
(tsexp-def-wrap-cmd 1 "[" beg)
(tsexp-def-wrap-cmd 1 "{" beg)
(tsexp-def-wrap-cmd 0 "\"" end)
(tsexp-def-wrap-cmd 0 "(" end)
(tsexp-def-wrap-cmd 0 "[" end)
(tsexp-def-wrap-cmd 0 "{" end)
(tsexp-def-wrap-cmd 1 "\"" end)
(tsexp-def-wrap-cmd 1 "(" end)
(tsexp-def-wrap-cmd 1 "[" end)
(tsexp-def-wrap-cmd 1 "{" end)

(provide 'tsexp)
