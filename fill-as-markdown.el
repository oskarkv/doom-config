;;; fill-as-markdown.el -*- lexical-binding: t; -*-
(require 'utils)
(require 'org)

(defun ok-narrow-to-comment ()
  "Narrow the buffer to the current multi-line comment block."
  (interactive)
  (save-excursion
    (let (start end)
      (goto-char (bol))
      (while (looking-at "\s*;")
        (forward-line -1))
      (forward-line 1)
      (setq start (point))
      (while (looking-at "\s*;")
        (forward-line 1))
      (backward-char)
      (setq end (point))
      (narrow-to-region start end))))

(defun ok-narrow-to-docstring ()
  (interactive)
  (save-excursion
    (let (start)
      (paredit-backward-up)
      (goto-char (bol))
      (setq start (point))
      (re-search-forward "\"" nil t)
      (re-search-forward "\"" nil t)
      (narrow-to-region start (point)))))

(defun ok--remove-all-regex-and-return-first (regex)
  (interactive)
  (save-excursion
    (let (prefix)
      (goto-char (point-min))
      (save-excursion
        (re-search-forward regex nil t)
        (setq prefix (buffer-substring-no-properties (bol) (point))))
      ;; Because empty lines in a comment starts with ";;" not ";; "
      (if (s-contains? ";" prefix)
          (while (re-search-forward regex nil t)
            (replace-match ""))
        ;; But we have to delete a fixed amount of space from docstring indents
        (progn
          (delete-char (length prefix))
          (while (zero? (forward-line 1))
            ;; Don't try to delete prefix from empty lines.
            (if (> (eol) (point))
                (ignore-errors (delete-char (length prefix)))))))
      prefix)))

(defun ok-remove-docstring-indent ()
  (interactive)
  (save-excursion
    (re-search-backward "\"" nil t)
    (delete-char 1)
    (insert " ")
    (ok--remove-all-regex-and-return-first "^\s+")))

(defun ok-remove-comment-characters ()
  (interactive)
  (ok--remove-all-regex-and-return-first
   (concat "^\s*" (regexp-quote comment-start) "+" " ?")))

(defun ok-add-before-every-line (string &optional skip)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line (or skip 0))
    (insert string)
    (while (zero? (progn (forward-line 1)
                         (save-excursion (forward-line 1))))
      (insert string))))

(defun ok-find-changed-indent ()
  (interactive)
  (let ((indent (ok-current-line-indent)))
    ;; Find new indent
    (while (and (= (forward-line 1) 0)
                (= (ok-current-line-indent) indent)))
    ;; If more indent than previous indent
    (if (> (ok-current-line-indent) indent)
        ;; Look at previous line, might be a list start
        (if (save-excursion
              (forward-line -1)
              (looking-at "-\\|+\\|*\\|[[:digit:]]+\\."))
            ;; Doesn't count, find next indent change
            (ok-find-changed-indent)
          (point))
      ;; If this line is a list start, doesn't count either
      (if (looking-at "-\\|+\\|*\\|[[:digit:]]+\\.")
          (ok-find-changed-indent)
        (point)))))

(defun ok-insert-paragraph-breakers ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (ok-find-changed-indent)
      (when (not (= (point) (point-max)))
        (insert "\nBREAKER\n\n")))))

(defun ok-remove-paragraph-breakers ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\nBREAKER\n\n" nil t)
      (replace-match ""))))

(defun ok-replace-md-with-org ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^*" nil t)
      (replace-match "+"))
    (goto-char (point-min))
    (while (re-search-forward "^#+" nil t)
      (-let (((start end) (match-data)))
        (replace-match (s-repeat (- end start) "*"))))))

(defun ok-replace-org-with-md ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+" nil t)
      (-let (((start end) (match-data)))
        (replace-match (s-repeat (- end start) "#"))))
    (goto-char (point-min))
    (while (re-search-forward "^+" nil t)
      (replace-match "*"))))

;; Took this from org-mode and removed widening the buffer.
(defun ok-adaptive-fill-function ()
  "Compute a fill prefix for the current line.
Return fill prefix, as a string, or nil if current line isn't
meant to be filled.  For convenience, if `adaptive-fill-regexp'
matches in paragraphs or comments, use it."
  (unless (org-at-heading-p)
    (let* ((p (line-beginning-position))
           (element (save-excursion
                      (forward-line 0)
                      (org-element-at-point)))
           (type (org-element-type element))
           (post-affiliated (org-element-post-affiliated element)))
      (unless (< p post-affiliated)
        (cl-case type
          (comment
           (save-excursion
             (forward-line 0)
             (looking-at "[ \t]*")
             (concat (match-string 0) "# ")))
          (footnote-definition "")
          ((item plain-list)
           (make-string (org-list-item-body-column post-affiliated) ?\s))
          (paragraph
           ;; Fill prefix is usually the same as the current line,
           ;; unless the paragraph is at the beginning of an item.
           (let ((parent (org-element-parent element)))
             (save-excursion
               (forward-line 0)
               (cond ((org-element-type-p parent 'item)
                      (make-string (org-list-item-body-column
                                    (org-element-begin parent))
                                   ?\s))
                     ((and adaptive-fill-regexp
                           ;; Locally disable
                           ;; `adaptive-fill-function' to let
                           ;; `fill-context-prefix' handle
                           ;; `adaptive-fill-regexp' variable.
                           (let (adaptive-fill-function)
                             (fill-context-prefix
                              post-affiliated
                              (org-element-end element)))))
                     ((looking-at "[ \t]+") (match-string 0))
                     (t  "")))))
          (comment-block
           ;; Only fill contents if P is within block boundaries.
           (let* ((cbeg (save-excursion (goto-char post-affiliated)
                                        (forward-line)
                                        (point)))
                  (cend (save-excursion
                          (goto-char (org-element-end element))
                          (skip-chars-backward " \r\t\n")
                          (line-beginning-position))))
             (when (and (>= p cbeg) (< p cend))
               (if (save-excursion (forward-line 0) (looking-at "[ \t]+"))
                   (match-string 0)
                 "")))))))))

(defun ok--fill-buffer-as-markdown ()
  (save-excursion
    (save-mode-excursion
      ;; fill-column changes when mode is switched, so save it before switching,
      ;; and use the one we had.
      (let ((fc fill-column))
        ;; Messes up buffer if enabled, adding a sort of uneditable indentation.
        (let ((org-startup-indented nil))
          (org-mode)
          ;; Insert newline at end, because filling doesn't work properly
          ;; otherwise.
          (goto-char (point-max))
          (insert "\n")
          ;; Fill buffer
          (ok-replace-md-with-org)
          (ok-insert-paragraph-breakers)
          (let ((fill-column fc)
                ;; org-mode decides sometimes that there should be a ";; "
                ;; prefix.
                (adaptive-fill-function (lambda (&rest args)
                                          (let ((result (ok-adaptive-fill-function)))
                                            (if (s-starts-with? ";" result)
                                                ""
                                              result)))))
            (fill-region (point-min) (point-max)))
          (ok-remove-paragraph-breakers)
          (ok-replace-org-with-md)
          (markdown-mode)
          (markdown-cleanup-list-numbers)
          ;; Remove extra newline
          (goto-char (1- (point-max)))
          (delete-char 1))))))

(defun ok-fill-comment-as-markdown ()
  "Fill the current comment as if it were a markdown-mode buffer."
  (interactive)
  (save-window-excursion
    (save-restriction
      (save-excursion
        (let (prefix)
          (ok-narrow-to-comment)
          (setq prefix (ok-remove-comment-characters))
          (ok--fill-buffer-as-markdown)
          (ok-add-before-every-line prefix))))))

(defun ok-fill-docstring-as-markdown ()
  "Fill the current comment as if it were a markdown-mode buffer."
  (interactive)
  (save-window-excursion
    (save-restriction
      (save-excursion
        (let (prefix)
          (ok-narrow-to-docstring)
          (setq prefix (ok-remove-docstring-indent))
          (ok--fill-buffer-as-markdown)
          (ok-add-before-every-line prefix)
          ;; Adds back the " that was removed by ok-remove-docstring-indent
          (goto-char (point-min))
          (re-search-forward "[[:graph:]]" nil t)
          (backward-char 2)
          (delete-char 1)
          (insert "\""))))))

(provide 'fill-as-markdown)
