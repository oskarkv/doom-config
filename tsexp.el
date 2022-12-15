;;; tsexp.el -*- lexical-binding: t; -*-
(require 'tsc)
(require 'tree-sitter)

(defvar tsexp-atom-at-point-goes-up-to
  '(list_splat_pattern
    list_splat
    dictionary_splat_pattern
    dictionary_splat
    typed_parameter))

(defvar tsexp-invisible-nodes
  '(expression_statement
    block
    type))

(defvar tsexp-containers
  '(assignment
    named_expression ; only :=
    parenthesized_expression
    call
    list
    tuple
    dictionary
    set
    class_definition
    function_definition
    if_statement
    while_statement
    try_statement
    with_statement
    finally_clause
    except_clause))

(defun tsexp-named? (node)
  (symbolp (tsc-node-type node)))

(defun tsexp-node-is? (node kind)
  (equal (tsc-node-type node) kind))

(defun tsexp-node-in? (node coll)
  (-contains? coll (tsc-node-type node)))

(defun tsexp-up-if-not-named (node)
  (if (tsexp-named? node) node (tsc-get-parent node)))

(defun tsexp-count-named-siblings (node)
  (tsc-count-named-children (tsc-get-parent node)))

(defun tsexp-invisible-node-with-one-child? (node)
  (and (tsexp-node-in? node tsexp-invisible-nodes)
       (= (tsc-count-named-children node) 1)))

(defun tsexp-skip-to-last-invisible-node (node)
  (while (tsexp-invisible-node-with-one-child? (tsc-get-parent node))
    (setq node (tsc-get-parent node)))
  node)

(defun tsexp-up-to-atom (node)
  (if (tsexp-node-in? (tsc-get-parent node) tsexp-atom-at-point-goes-up-to)
      (tsc-get-parent node)
    node))

(defun tsexp-atom-at-point ()
  (tsexp-skip-to-last-invisible-node
   (tsexp-up-to-atom
    (tree-sitter-node-at-point))))

(defun tsexp-is-container? (node)
  (tsexp-node-in? node tsexp-containers))

;; (defun tsexp-is-container? (node)
;;   (> (tsc-count-children node) 1))

(defun tsexp-parent-container (node &optional levels)
  (dotimes (i (or levels 1))
    (setq node (tsc-get-parent node))
    (while (not (tsexp-is-container? node))
      (setq node (tsc-get-parent node))))
  (tsexp-skip-to-last-invisible-node node))

(defun tsexp-container-at-point (&optinonal levels)
  (tsexp-parent-container (tsexp-atom-at-point) levels))

(defun tsexp-node-bounds (node)
  (let ((pos (tsc-node-position-range node)))
    (list (car pos) (cdr pos))))

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
    (goto-char (+ (- old-point beg) big-beg))))

(defmacro tsexp-def-get-sibling-fn (word)
`(defun ,(ok-symbol "tsexp-get-" word "-sibling") (node)
  (let ((sib (,(ok-symbol "tsc-get-" word "-named-sibling") node)))
    (if sib
        sib
      (let ((parent (tsc-get-parent node)))
        (if (tsexp-node-is? parent 'binary_operator)
            (,(ok-symbol "tsexp-get-" word "-sibling") parent)))))))

(tsexp-def-get-sibling-fn next) ; tsexp-get-next-sibling
(tsexp-def-get-sibling-fn prev) ; tsexp-get-prev-sibling

(defmacro tsexp-def-transpose-fn (next dir)
  `(defun ,(ok-symbol 'tsexp-transpose- dir) (node)
     (when-let ((sib (,(ok-symbol 'tsexp-get- next '-sibling) node)))
       (tsexp-swap-text
        (tsexp-node-bounds node)
        (tsexp-node-bounds sib)))))

(tsexp-def-transpose-fn next forward) ; tsexp-transpose-forward
(tsexp-def-transpose-fn prev backward) ; tsexp-transpose-backward

(defmacro tsexp-def-transpose-cmd (name levels dir)
  `(defun ,(ok-symbol 'tsexp-transpose- name '- dir) ()
     (interactive)
     (,(ok-symbol 'tsexp-transpose- dir)
      (tsexp-parent-container (tsexp-atom-at-point) ,levels))))

(tsexp-def-transpose-cmd atom 0 forward)
(tsexp-def-transpose-cmd atom 0 backward)
(tsexp-def-transpose-cmd container 1 forward)
(tsexp-def-transpose-cmd container 1 backward)
(tsexp-def-transpose-cmd ccontainer 2 forward)
(tsexp-def-transpose-cmd ccontainer 2 backward)

(provide 'tsexp)
