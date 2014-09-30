(defmacro tumor-defclass (name args &rest body)
  (declare (indent defun))
  (let* ((name (symbol-name name))
	 (ctor-name (intern (concat "make-" name)))
	 docstring
	 let-forms
	 top-forms
	 init-forms)
    (when (stringp (car body))
      (setq docstring (car body)
	    body (cdr body)))
    (dolist (form body)
      (cl-assert (listp form))
      (pcase form
	(`(progn . ,body)
	 (setq init-forms
	       (append (reverse body) init-forms)))
	(`(defvar ,var-name . ,tail)
	 (push var-name let-forms)
	 (pcase tail
	   (`(,init-form . ,tail)
	    (push `(setq ,var-name ,init-form)
		  init-forms)))
	 )
	((and `(,head ,method-name ,args . ,body)
	      (guard (memq head '(defun cl-defun))))
	 (cl-assert (symbolp method-name))
	 (cl-assert (listp args))
	 (push (list* head
		      (intern (concat name "-" (symbol-name method-name)))
		      args
		      body)
	       top-forms))))
    
   `(progn
      (defun ,ctor-name ,args ,docstring
	(let ,(nreverse let-forms)
	  ,@(nreverse init-forms)))
      ,@(nreverse top-forms))))

(cl-assert (not

(pcase (macroexpand
	'(tumor-defclass myclass (ctor-arg1)
	   "The docstring."
	   (defvar var1 var1-init)	   
	   (progn
	     init1
	     init2)
	   (defvar var2 var2-init)
	   (defvar var3)
	   (defun mymethod ())))
  (`(progn
      (defun make-myclass (ctor-arg1) "The docstring." ,ctor-body)
      (defun myclass-mymethod ()))
   (pcase ctor-body
     (`(let (var1 var2 var3)
	 (setq var1 var1-init)
	 init1 init2
	 (setq var2 var2-init)))
     (bad (list :ctor-body bad))))
  (bad (list :exp bad)))

))


(provide 'tumor-object)
