;;; tsexp.el -*- lexical-binding: t; -*-
(require 'tsc)
(require 'tree-sitter)

(defvar tsexp-atom-at-point-goes-up-to
  '(list_splat_pattern
    dictionary_splat_pattern
    typed_parameter))

(defvar tsexp-containers
  '(assignment
    named_expression ; only :=
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
    with_statement))

(defun tsexp-named? (node)
  (symbolp (tsc-node-type node)))

(defun tsexp-node-is? (node kind)
  (equal (tsc-node-type node) kind))

(defun tsexp-node-in? (node coll)
  (-contains? coll (tsc-node-type node)))

(defun tsexp-up-if-not-named (node)
  (if (tsexp-named? node) node (tsc-get-parent node)))

(defun tsexp-atom-at-point ()
  (tsexp-up-if-not-named (tree-sitter-node-at-point)))

(defun tsexp-is-container? (node)
  (or (tsexp-node-in? node tsexp-containers)
      (and (tsexp-named? node)
           (> (tsc-count-children node) 1))))

(defun tsexp-parent-container (node &optional levels)
  (dotimes (i (or levels 1))
    (setq node (tsc-get-parent node))
    (while (not (tsexp-is-container? node))
      (setq node (tsc-get-parent node))))
  node)

(defun tsexp-count-container-siblings (node)
  (1- (tsc-count-named-children (tsexp-parent-container node))))
