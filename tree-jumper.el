(require 'cl-lib)

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

(defvar tree-jumper-hint-hash-table (make-hash-table))
(defvar tree-jumper-hint-list nil)

;; we need to regenerate the positions every time, because the code may have
;; changes since the last call TODO: detect if the buffer has been
;; updated or re-read from the disk?
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

(defun tree-jumper-filter-pos-window (positions)
  ;; (window-start)
  ;; (window-end)
  )

(defun tree-jumper-hints-overlay ()
  (interactive)
  (let ((n 0))
    (dolist (p (tree-jumper-get-buffer-positions))
      (let* ((ov (make-overlay p (1+ p))) (ov-list '()))
	(overlay-put ov 'before-string ())
	ov-list))))

;; helper function to form hint lists
(defun tree-jumper-hint-string (hint-characters digit-3 digit-2 digit-1)
  (let ((nonzero nil) (char-list nil))
    (when digit-3
      (add-to-list 'char-list (nth digit-3 hint-characters) t))
    (when (and digit-2 (null char-list))
      (add-to-list 'char-list (nth digit-2 hint-characters) t))
    (add-to-list 'char-list (nth digit-1 hint-characters) t)
    (concat char-list)))


;; create a hash table for hint strings
(defun tree-jumper-init-hint-hash-table ()
  (clrhash tree-jumper-hint-hash-table)
  (setq tree-jumper-hint-list nil)

  (let* ((hint-characters '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
	 (hc-len (length hint-characters))
	 (n 0))
    ;; 9**3 should be enough for everything (all buffers if we want to
    ;; jump cross-buffer
    (while (< n (expt hc-len 3))
      ;; counting digits from the least significant to most significant
      ;; use square here and record all powers into local variables for reuse
      (let* ((digit-3 (/ n hc-len))
	     (digit-3-remainder (mod n hc-len))
	     (digit-2 (/ digit-3-remainder hc-len))
	     (digit-1 (mod digit-3-remainder hc-len))
	     (hint-str (tree-jumper-hint-string hint-characters
						digit-3
						digit-2
						digit-1)))
	(add-to-list 'tree-jumper-hint-list hint-str t)
	(puthash hint-str n tree-jumper-hint-hash-table))
      (print n)
      (cl-incf n))))



(defun tree-jumper-test-positions ()
  (interactive)
  (dolist (p (tree-jumper-get-buffer-positions))
    (insert (format "%S" p) " $\n")))

;; (debug-on-entry 'tree-jumper-test-positions)
;; (cancel-debug-on-entry 'tree-jumper-test-positions)
(progn
  (tree-jumper-init-hint-hash-table)
  (print (hash-table-count tree-jumper-hint-hash-table))
  ;; (print tree-jumper-hint-list)
  )
