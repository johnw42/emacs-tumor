(require 'cl-macs)

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
	  (let ((this (lambda (method &rest args)
			(case method
			  ,@(nreverse dispatch-forms)))))
	    (let ,(nreverse let-forms)
	      ,@(nreverse init-forms))
	    this))
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

(pcase-let* ((n 1)
	     (2 2)
	     (`2 2)
	     (_ 'a)
	     (`x 'y)
	     (`(,a) '(a))
	     (`(,a . ,bc) '(a b c)))
  (list n a bc))

(pcase-let* ((n 1)
	     (2 2)
	     (1 2)			; !
	     (`1 2)			; !
	     (`2 2)
	     (_ 'a)
	     (`x 'y)			; !
	     (`(,a) '(a))
	     (`(a) '(a))
	     (`(a . ,bc) '(a b c)))
  (list n a bc))

(pcase-let (((and x y) 0))
  (list x y))

(let ((z 'b))
  (pcase-let ((`(a ,x ,y) (list z 0 1))
	       (`(b ,x ,y) (list z 2 3))
	       (`(c ,x ,y) (list z 4 5)))
    (list x y)))

(let (x y z)
  (pcase-let* (((or (and x (guard nil) y))
	       42))
    (list x y)))

(let (x y z)
  (pcase-let (((or (and x (pred null)) 
		   (and y (guard nil)) 
		   z) 
		0))
    (list x y z)))

(let ((magic t))
  (eval '(defun i-hate-you () 42))
  (eval '(cl-macrolet ((defun (name &rest rest)
			 `(eval
			   '(defun ,(intern (concat (symbol-name name)
						    "-more"))
			      ,@rest))))
	   (let ((magic t))
	     (defun i-hate-you ()
	       43))
	   (i-hate-you-more))))

(tumor-add-defclass-case 
    `(,(or (and (let function 'function)
    		`defun)
    	   (and (let function 'cl-function)
    		`cl-defun))
      ,method-name ,args . ,body)
;;   (or `(defun ,method-name ,args . (,(let function 'function) . ,body))
;;       ;; `(cl-defun ,(let function 'cl-function) ,method-name ,args . ,body)
;; )
  (cl-assert (symbolp method-name))
  (cl-assert (listp args))
  (push `((,method-name)
	  (funcall (,function (lambda (,@args) ,@body)) args))
	dispatch-forms)
  (push `(defun
	   ,(intern (concat name "-" (symbol-name method-name)))
	   (this &rest args)
	   (apply this ',method-name args))
	top-forms))

;; (tumor-add-defclass-case
;;     (and `(defun ,method-name ,function-symbol)
;; 	 (guard (not (listp function-symbol)))
;; 	 (guard (symbolp function-symbol))
;; 	 (guard (symbolp method-name)))
;;   (push `((,method-name)
;; 	  (apply ,function-symbol args))
;; 	dispatch-forms))

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
	(`(let ((this (lambda (method &rest args) ,method-bodies)))
	    (let (var1 var2 var3)
	      (setq var1 var1-init)
	      init1 init2
	      (setq var2 var2-init))
	    this)
	 (pcase-strict method-bodies
	   (`(case method
	       ((mymethod)
		(funcall
		 (function
		  (lambda ()
		    mymethod-body))
		 args))))))))))))))

(provide 'tumor-object)
