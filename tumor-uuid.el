(require 'cl-macs)
(require 'pcase)
(require 'macroexp)

(defmacro tumor-module (&rest body)
  "Wrapper around all tumor-specific functionality."
  (unless (memq :tumor-version-history body)    
    (macroexp-progn
     (loop
      while body collect
      (pcase (pop body)
	(`(:tumor-version-history . ,history)
	 (pop body)
	 `(setq tumor-version-history ',history))
	(form form))))))

(defun tumor-set-file-variable (symbol value)
  ;; TODO
  )

(defun tumor-update-file (monad-func)
  ;; TODO
  )

;; TODO: Parse file into tree containing the following node types:
;; - sexp, line-comment, block-comment, hard-newline
;; Keep start+end marker for each node.
;; Derive grouping structure from newlines?
;;
;; Zipper ops:
;; - forward-node
;; - down-node
;; - up-node
;; - backward-node
;; - delete-node
;; - insert-node-before

(defun tumor-map-buffer-forms (func)
  "Calls func for each form in the current buffer, passing the
form as the argument.  The function is called with the buffer
narrowed to the form itself, and (point) is set to (point-max)."
  (goto-char (point-min))
  (condition-case nil
      (while t
	(let* ((form (read (current-buffer)))
	       (form-end (point)))
	  (save-excursion
	    (save-restriction
	      (backward-sexp)
	      (narrow-to-region (point)
				form-end)
	      (goto-char (point-max))
	      (funcall func form)))))
    (end-of-file)))

(defun tumor-narrow-to-inner-form ()
  "Assuming the buffer is narrowed to a single list sexp, this
function narrows to buffer to include just the contents of the
list."
  (save-excursion
    (narrow-to-region
     (progn
       (goto-char (point-min))
       (down-list)
       (point))
     (progn
       (goto-char (point-max))
       (backward-down-list)
       (point)))))

(defun tumor-insert-form (form)
  "Inserts a form in the current buffer, with pretty-printing and
indentation."
  (save-excursion
    (pp form (current-buffer))
    (backward-delete-char 1))
  (indent-pp-sexp))

(defun tumor-replace-next-form (func)
  "Assuming point is positioned just after a form, reads the
following form and replaces it with the result of calling FUNC on
it."
  (save-restriction
    (widen)
    (let ((start (point))
	  (form (read (current-buffer))))
      (delete-region start (point))
      (tumor-insert-form (funcall func form)))))

(defun tumor-update-version-history (history)
  `',(make-symbol
      (tumor-uuid-string)))

(defun tumor-uuid-string ()
  "Generates a new random UUID."
  (with-temp-buffer
    (call-process "uuidgen" nil t)
    (backward-delete-char 1)
    (buffer-string)))

(defun tumor-update-keys (key default-value update-func)
  "Looks inside a top-level `tumor-module' and updates the sexp
following the specified key using UPDATE-FUNC.  If the key is
absent, it is inserted followed by DEFAULT-VALUE."
  (save-excursion
    (tumor-map-buffer-forms
     (lambda (form)
       (pcase form
	 (`(tumor-module . ,_)
	  (let (has-key)
	    (tumor-narrow-to-inner-form)
	    (tumor-map-buffer-forms
	     (lambda (form)
	       (when (eq form key)
		 (setq has-key t)
		 (insert " ")
		 (tumor-replace-next-form update-func))))
	    ;; (unless has-key
	    ;;    (goto-char (point-max))
	    ;;    (tumor-insert-form key)
	    ;;    (insert " ")
	    ;;    (tumor-insert-form default-value))
	    )))))))

(add-hook
 'before-save-hook
 (defun tumor-uuid-before-save-hook ()
   (when (derived-mode-p 'emacs-lisp)
     ;; TODO: commit on every save
     (eval-buffer)
     (tumor-update-keys :tumor-version-history nil
			'tumor-update-version-history))))

(add-hook
 'after-save-hook
 (defun tumor-uuid-after-save-hook ()
   (call-process "git" nil nil nil "stash" "save")
   (call-process "git" nil nil nil "stash" "apply")
   ;; (call-process "git" nil nil nil "commit" "-a" "-mWIP")
   ;; (call-process "git" nil nil nil "branch" "-f" "tumor-wip")
   ;; (call-process "git" nil nil nil "reset" "HEAD^")

))


;; Test module.
(tumor-module 
 :tumor-version-history '4c870d62-57a2-4df2-8bf5-8d0ae1c2cc3)

;; (macroexpand
;;  '(tumor-module
;;    :tumor-version-history '(v1 v2)
;;    a b c))
