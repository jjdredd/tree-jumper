(require 'cl-lib)
(require 'treesit)

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

(defface tree-jumper-face-sg
  '((t (:foreground "white" :background "#f86bf3")))
  "Face used for leading chars.")

(defcustom tree-jumper-escape-chars '(?\e ?\C-g)
  "List of characters that quit tree-jumper during `read-char'."
  :type 'list)

(defconst tree-jumper-ts-node-types '(
				      "preproc_include"
				      "string_content"
				      "preproc_ifdef"
				      "identifier"
				      "declaration"
				      "type_identifier"
				      "init_declarator"
				      ;; "qualified_identifier"
				      "namespace_identifier"
				      "call_expression"
				      "argument_list"
				      "storage_class_specifier"
				      "function_declarator"
				      "parameter_list"
				      "compound_statement"
				      "null"
				      "number_literal"
				      "type_identifier"
				      "this"
				      "field_identifier"
				      "false"
				      "true"
				      ))

(defvar tree-jumper-hint-hash-table (make-hash-table :test #'equal))
(defvar tree-jumper-hint-list nil)

(defvar tree-jumper-buffer-positions nil)

(defun tree-jumper-register-node (node offset)
  (when (member (treesit-node-type node) tree-jumper-ts-node-types)
    (add-to-list 'tree-jumper-buffer-positions (+ (treesit-node-start node) offset) t))
  (cl-dolist (child-node (treesit-node-children node t))
    (tree-jumper-register-node child-node offset)))

;; XXX: probably no need for this anymore
;; we need to regenerate the positions every time, because the code may have
;; changes since the last call TODO: detect if the buffer has been
;; updated or re-read from the disk?
(defun tree-jumper-get-buffer-positions (start end)
  (let* ((source-code (buffer-substring-no-properties start  end))
	 (st-root (treesit-parse-string source-code 'cpp)))
    (cl-dolist (node (treesit-node-children st-root t))
      (tree-jumper-register-node node start))))

;; XXX TODO
;; use all emacs tree-sitter function, not tsc

(defun tree-jumper-hints-overlay ()
  (interactive)
  (let ((n 0)
	(ov-list '()))
    (setq tree-jumper-buffer-positions nil)
    (tree-jumper-get-buffer-positions (window-start) (window-end))
    (cl-dolist (p tree-jumper-buffer-positions)
      (let* ((ov (make-overlay (- p 1) p)))
	;; (overlay-put ov 'face 'highlight)
	(overlay-put ov 'before-string (propertize (nth n tree-jumper-hint-list) 'face 'tree-jumper-face-sg))
	(add-to-list 'ov-list ov)
	(cl-incf n)
	(when (>= n (length tree-jumper-hint-list))
	  (cl-return ov-list))))
    ov-list))

;; helper function to form hint lists
(defun tree-jumper-hint-string (digit-3 digit-2 digit-1)
  (let ((char-list nil))
    (when digit-3
      (push digit-3 char-list))
    (when digit-2
      (push digit-2 char-list))
    (push digit-1 char-list)
    (concat (reverse char-list))))

(defun tree-jumper-add-hint (n digit-1 &optional digit-2 digit-3)
  (let ((hint-str (tree-jumper-hint-string digit-3
					   digit-2
					   digit-1)))
	(add-to-list 'tree-jumper-hint-list hint-str t)
	(puthash hint-str n tree-jumper-hint-hash-table)))


;; Generate the hint string so that it's unambiguous
(defun tree-jumper-init-hint-hash-table ()
  (clrhash tree-jumper-hint-hash-table)
  (setq tree-jumper-hint-list nil)

  (let* ((hint-characters '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
	 (hint-characters-ex (cons nil hint-characters))
	 (n 0)
	 (digit-3 nil)
	 (digit-2 nil)
	 (digit-1 nil))

    ;; Quick fix: make key combibations unambiguious by
    ;; allowing only length 3
    ;; (dolist (digit-1 hint-characters)
    ;;   (tree-jumper-add-hint n digit-1)
    ;;   (cl-incf n))

    ;; (dolist (digit-2 hint-characters)
    ;;   (dolist (digit-1 hint-characters)
    ;; 	(tree-jumper-add-hint n digit-1 digit-2)
    ;; 	(cl-incf n)))

    (dolist (digit-3 hint-characters)
      (dolist (digit-2 hint-characters)
	(dolist (digit-1 hint-characters)
	  (tree-jumper-add-hint n digit-1 digit-2 digit-3)
	  (cl-incf n))))))

(defun tree-jumper-collect-input()
  (let ((n 0) (char-list nil))
    (while (< n 3)
      (let ((current-char (read-char)))
	(when (or (memq current-char tree-jumper-escape-chars)
		  (mouse-event-p current-char))
	  (throw 'exit nil))
	(push current-char char-list)
      (cl-incf n)))
    (concat (reverse char-list))))

(defun tree-jumper-remove-overlays (ovl-list)
  (dolist (ovl ovl-list)
    (delete-overlay ovl)))

(defun tree-jumper-start-interaction ()
  (interactive)
  (let ((ovl-list (tree-jumper-hints-overlay)))
    (catch 'exit
      (let ((user-input (tree-jumper-collect-input)))
	(goto-char (- (nth (gethash user-input tree-jumper-hint-hash-table) tree-jumper-buffer-positions) 1))))
    (tree-jumper-remove-overlays ovl-list)))

(defun tree-jumper-test-positions ()
  (interactive)
  (dolist (p (tree-jumper-get-buffer-positions (window-start) (window-end)))
    (insert (format "%S" p) " $\n")))

(progn
  (tree-jumper-init-hint-hash-table)
  (print (hash-table-count tree-jumper-hint-hash-table))
  (print (length tree-jumper-hint-list)))

; (global-set-key (kbd "M-g M-t") 'tree-jumper-start-interaction)

;; 1. DONE refactor tree-sitter functions to use modern
;; 2. make unambiguous suggesitons see avy and https://en.wikipedia.org/wiki/De_Bruijn_sequence
;; 3. implement overlay removal
;; 4. more intelligent node filtering?
;; 5. DONE (automatically by treesit) handle non-ascii characters correctly
;; 6. keyboard control: research
;; 7. Check if (treesit-available-p)
;; 8. Check if (treesit-language-available-p ‘lang)

;; (defun tree-jumper-test-print-children (node level)
;;   (cl-dolist (child-node (treesit-node-children node t))
;;     (when (member (treesit-node-type child-node) tree-jumper-ts-node-types)
;;       (insert (make-string level ?\s) (format "%S: %d - %d"
;; 					      (treesit-node-type child-node)
;; 					      (treesit-node-start child-node)
;; 					      (treesit-node-end child-node)) "\n"))
;;     (tree-jumper-test-print-children child-node (1+ level))))


;; (defun tree-jumper-test-ts ()
;;   (interactive)    
;;   (let ((buffer-positions nil))
;;     (let* ((st-root (treesit-parse-string (buffer-string) 'cpp)))
;;       (switch-to-buffer-other-window "*test*")
;;       (erase-buffer)
;;       (tree-jumper-test-print-children st-root 0)
;;       (other-window 1)
;;       )))


