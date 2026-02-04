;; (defvar tree-jumper-buffer-positions '())

(defconst tree-jumper-ts-node-types '(preproc_include
				      string_content
				      identifier
				      type_identifier
				      function_declarator
				      qualified_identifier
				      compound_statement
				      argument_list
				      field_expression
				      field_identifier))

(defun tree-jumper-get-buffer-positions ()
  (let ((parser (tsc-make-parser)) (buffer-positions nil))
    (tsc-set-language parser (tree-sitter-require 'cpp))
    (let* ((source-code (buffer-string))
	   (tree (tsc-parse-string parser source-code))
	   (root (tsc-root-node tree)))
      (tsc-traverse-do ([type depth named-p start-byte] root)
	(when (and named-p (member type tree-jumper-ts-node-types))
	  (add-to-list 'buffer-positions start-byte t))))
    buffer-positions))

(defun tree-jumper-filter-pos-current-window (positions)
  )

(defun tree-jumper-hints-overlay()
  (interactive)
   (dolist (p (tree-jumper-get-buffer-positions))
     (let* ((ov (make-overlay p (1+ p))))
       (overlay-put ov 'before-string "d"))))

(defun tree-jumper-test-positions ()
  (interactive)
  (dolist (p (tree-jumper-get-buffer-positions))
    (insert (format "%S" p) " $\n")))

;; (debug-on-entry 'tree-jumper-test-positions)
;; (cancel-debug-on-entry 'tree-jumper-test-positions)
