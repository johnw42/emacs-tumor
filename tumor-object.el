(defmacro tumor-defclass (name-symbol args &rest body)
  (declare (indent defun))
  (let* ((name (symbol-name name-symbol))
	 (ctor-name (intern (concat "make-" name)))
	 docstring
	 let-forms
	 top-forms
	 init-forms
	 dispatch-forms)
    (when (stringp (car body))
      (setq docstring (car body)
	    body (cdr body)))
    (dolist (form body)
      (cl-assert (listp form))
      (cl-assert (symbolp (car form)))
      (eval `(pcase form ,@tumor-defclass-cases) t))
   `(eval
     '(progn
	(defun ,ctor-name ,args ,docstring
	  (let ,(nreverse let-forms)
	    ,@(nreverse init-forms)
	    (lambda (method &rest args)
	      (case method
		,@(nreverse dispatch-forms)))))
	,@(nreverse top-forms))
     t)))

(defconst tumor-defclass-cases nil)

(defmacro tumor-add-defclass-case (pcase-pattern &rest pcase-body)
  (declare (indent 1))
  `(push '(,pcase-pattern ,@pcase-body) tumor-defclass-cases))

(tumor-add-defclass-case `(progn . ,body)
  (setq init-forms
	(append (reverse body) init-forms)))

(tumor-add-defclass-case `(defvar ,var-name . ,tail)
  (push var-name let-forms)
  (pcase tail
    (`(,init-form . ,tail)
     (push `(setq ,var-name ,init-form)
	   init-forms))))

(tumor-add-defclass-case 
    `(,(or (and `defun
		(let function-form 'function))
	   (and `cl-defun
		(let function-form 'cl-function)))
      ,method-name ,args . ,body)
  (cl-assert (symbolp method-name))
  (cl-assert (listp args))
  (push `((,method-name)
	  (funcall (,function-form (lambda (,@args) ,@body)) args))
	dispatch-forms)
  (push `(defun
	   ,(intern (concat name "-" (symbol-name method-name)))
	   (this &rest args)
	   (apply this ',method-name args))
	top-forms))

(defun tumor-object-bind* (method-name object)
  (lambda (&rest args)
    (apply object method-name args)))

(defmacro tumor-object-bind (method object)
  `(tumor-object-bind* ',method ,object))

(defmacro pcase-strict (pattern &rest cases)
  (declare (indent 1))
  `(pcase ,pattern
     ,@cases
     (bad (list ',pattern bad))))

(cl-assert (not

(pcase-strict
 (macroexpand
  '(tumor-defclass myclass (ctor-arg1)
     "The docstring."
     (defvar var1 var1-init)	   
     (progn
       init1
       init2)
     (defvar var2 var2-init)
     (defvar var3)
     (defun mymethod ()
       mymethod-body)))
 (`(eval ',progn-form t)
  (pcase-strict progn-form
    (`(progn
	(defun make-myclass (ctor-arg1) "The docstring." ,ctor-body)
	(defun myclass-mymethod (this &rest args)
	  (apply this 'mymethod args)))
     (or
      (pcase-strict ctor-body
	(`(let (var1 var2 var3)
	    (setq var1 var1-init)
	    init1 init2
	    (setq var2 var2-init)
	    (lambda (method &rest args) ,method-bodies))
	 (pcase-strict method-bodies
	   (`(case method
	       ((mymethod)
		(funcall
		 (function
		  (lambda ()
		    mymethod-body))
		 args))))))))))))))



(provide 'tumor-object)
