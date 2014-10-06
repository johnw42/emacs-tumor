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
  (save-excursion
    (pp form (current-buffer))
    (backward-delete-char 1))
  (indent-pp-sexp))

(defun tumor-replace-next-form (func)
  (save-restriction
    (widen)
    (let ((start (point))
	  (form (read (current-buffer))))
      (delete-region start (point))
      (tumor-insert-form (funcall func form)))))

(defun tumor-update-version-history (history)
  `'(a ,history))

(defun tumor-update-keys (key default-value update-func)
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
   (tumor-update-keys :tumor-version-history nil
		      'tumor-update-version-history)))


(tumor-module
 :tumor-version-history '(a
			  '(a
			    '(a
			      '(a
				'(a
				  '(a
				    '(a
				      '(a
					'('('(a
					      '(a
						'(a
						  '(a
						    '(a
						      '(a nil)))))))))))))))))

;; (macroexpand
;;  '(tumor-module
;;    :tumor-version-history '(v1 v2)
;;    a b c))
