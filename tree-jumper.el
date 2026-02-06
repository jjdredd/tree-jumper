(require 'cl-lib)

(defface highlight
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'basic-faces)

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

;; XXX TODO
;; once positions are filtered, there will be need for less hint strings
(defun tree-jumper-filter-pos-window (positions)
  ;; (window-start)
  ;; (window-end)
  )

(defun tree-jumper-hints-overlay ()
  (interactive)
  (let ((n 0)
	(ov-list '()))
    (cl-dolist (p (tree-jumper-get-buffer-positions))
      (let* ((ov (make-overlay p (1+ p))))
	(overlay-put ov 'face 'highlight)
	(overlay-put ov 'display (nth n tree-jumper-hint-list))
	(add-to-list 'ov-list ov)
	(cl-incf n)
	(when (>= n (length tree-jumper-hint-list))
	  (cl-return ov-list))))
    ov-list))

;; helper function to form hint lists
(defun tree-jumper-hint-string (hint-characters digit-3 digit-2 digit-1)
  (let ((char-list nil))
    (when digit-3
      (push digit-3 char-list))
    (when digit-2
      (push digit-2 char-list))
    (push digit-1 char-list)
    (concat (reverse char-list))))

;; XXX: Instead,use a triple loop to create the hash table and list of hint strings!!!

(defun tree-jumper-add-hint (n digit-1 &optional digit-2 digit-3)
  (let ((hint-str (tree-jumper-hint-string hint-characters
					       digit-3
					       digit-2
					       digit-1)))
	(add-to-list 'tree-jumper-hint-list hint-str t)
	(puthash hint-str n tree-jumper-hint-hash-table)))

;; create a hash table for hint strings
(defun tree-jumper-init-hint-hash-table ()
  (clrhash tree-jumper-hint-hash-table)
  (setq tree-jumper-hint-list nil)

  (let* ((hint-characters '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
	 (hint-characters-ex (cons nil hint-characters))
	 (n 0)
	 (digit-3 nil)
	 (digit-2 nil)
	 (digit-1 nil))
    (dolist (digit-1 hint-characters)
      (tree-jumper-add-hint n digit-1)
      (cl-incf n))

    (dolist (digit-2 hint-characters)
      (dolist (digit-1 hint-characters)
	(tree-jumper-add-hint n digit-1 digit-2)
	(cl-incf n)))

    (dolist (digit-3 hint-characters)
      (dolist (digit-2 hint-characters)
	(dolist (digit-1 hint-characters)
	  (tree-jumper-add-hint n digit-1 digit-2 digit-3)
	  (cl-incf n))))))

(defun tree-jumper-test-positions ()
  (interactive)
  (dolist (p (tree-jumper-get-buffer-positions))
    (insert (format "%S" p) " $\n")))

;; (defun print-list-to-buffer (list-to-print buffer-name)
;;   "Prints each element of LIST-TO-PRINT to BUFFER-NAME, each on a new line."
;;   (with-current-buffer (generate-new-buffer buffer-name) ; Create and switch to a new buffer
;;     (dolist (elm list-to-print)
;;       (insert elm)  ; Print the element as a string
;;       (insert "\n")) ; Insert a newline character
;;     (switch-to-buffer (current-buffer)))) ; Switch the display to the new buffer

;; (debug-on-entry 'tree-jumper-test-positions)
;; (cancel-debug-on-entry 'tree-jumper-test-positions)
(progn
  (tree-jumper-init-hint-hash-table)
  (print (hash-table-count tree-jumper-hint-hash-table))
  (print (length tree-jumper-hint-list)))
