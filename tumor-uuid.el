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
	(message "about to read at %s" (point))
	(let* (
	       ;; (str (buffer-substring (point)
	       ;; 			      (point-max)))
	       ;; (pair (read-from-string str))
	       ;; (form (car pair))
	       ;; (form-end (cdr pair))
	       (form (read (current-buffer)))
	       (form-end (point))

)

	  ;; (goto-char form-end)
	  (save-excursion
	    (save-restriction
	      (backward-sexp)
	      (narrow-to-region (point)
				form-end)
	      (funcall func form)))
	  ;; (save-restriction
	  ;;   (narrow-to-region (point) form-end)
	  ;;   (funcall func form)
	  ;;   (goto-char (point-max)))
))
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
    (forward-sexp)
    (delete-region start (point))))

(add-hook
 'before-save-hook
 (defun tumor-uuid-before-save-hook ()
   (save-excursion
     (goto-char (point-min))

     
     (message "starting to read")

     (tumor-map-buffer-forms
      (lambda (form)
	(message "found form before %s" (point))
	(pcase form
	  (`(tumor-module . ,_)
	   (message "!!found tumor-module at %s" (point-min))
	   (tumor-narrow-to-inner-form)
	   (tumor-map-buffer-forms
	    (lambda (form)
	      (pcase form
	  	(:tumor-version-history
		 (goto-char (point-max))
		 (save-restriction
		   (widen)
		   (tumor-delete-sexp))
	  	 (insert " 'new-history")
		 ))
	      )))

	  ((or (and head (pred symbolp))
	       `(,head . ,_))
	   (message "found %s at %s" head (point-min))))))

     (message "done reading"))))


(tumor-module
 :tumor-version-history 'new-history)

;; (macroexpand
;;  '(tumor-module
;;    :tumor-version-history '(v1 v2)
;;    a b c))



