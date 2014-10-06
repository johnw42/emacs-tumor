(require 'cl-macs)
(require 'pcase)
(require 'macroexp)

(defmacro tumor-module (&rest body)
  (macroexp-progn
   (loop
    while body collect
    (pcase (pop body)
      (`(:tumor-version-history . ,history)
       (pop body)
       `(setq tumor-version-history ',history))
      (form form)))))

(defun tumor-map-buffer-forms (func)
  "Calls func for each form in the current buffer, with (point)
set to the end of the form.  The form itself and the start offset
of the form are passed as arguments."
  (condition-case nil
      (while t
	(let* ((form (read (current-buffer)))
	       (form-end (point)))
	  (save-excursion
	    (save-restriction
	      (backward-sexp)
	      (narrow-to-region (point)
				form-end)
	      (funcall func form)))))
    (end-of-file)))

(defun tumor-narrow-to-inner-form ()
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

(defun tumor-delete-sexp ()
  (let ((start (point)))
    (prog1 (read (current-buffer))
      (delete-region start (point)))))

(defun tumor-replace-next-form (func)
  (goto-char (point-max))
  (save-restriction
    (widen)
    (let ((start (point))
	  (form (read (current-buffer))))
      (delete-region start (point))
      (pp (funcall func form) (current-buffer))
      (goto-char start)
      (indent-pp-sexp)))) 

(add-hook
 'before-save-hook
 (defun tumor-uuid-before-save-hook ()
   (save-excursion
     (goto-char (point-min))
     (tumor-map-buffer-forms
      (lambda (form)
	(pcase form
	  (`(tumor-module . ,_)
	   (tumor-narrow-to-inner-form)
	   (tumor-map-buffer-forms
	    (lambda (form)
	      (pcase form
	  	(:tumor-version-history
		 (tumor-replace-next-form
		  (lambda (form)
		    (cons 'a form)
		    `'(a ,form)))
		 ;; (goto-char (point-max))
		 ;; (save-restriction
		 ;;   (widen)
		 ;;   (tumor-delete-sexp))
	  	 ;; (insert " 'new-history")
		 ))
	      )))))))))


(tumor-module
 :tumor-version-history'(a
			 '(a
			   '(a
			     '(a
			       (a a a a quote baz2)))))







 nil   )

;; (macroexpand
;;  '(tumor-module
;;    :tumor-version-history '(v1 v2)
;;    a b c))



