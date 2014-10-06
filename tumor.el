;; TODO
;; - Enable tumor minor mode in tumor buffers.
;; - Automatically make a git commit on each save.
;; - Automatically call mode hooks in relevant buffers.
;; - Automatically run unit tests for files under development.
;; - Add command to copy the previous subexpression.
;; - Automatically synchronize edits of tumor names.
;; - Auto-import "recommended" symbols for each tumor (with overrides based on version numbers.)
;; - Automatically consider library symbols for auto-completion.
;; - Automatically insert imports for referenecd symbols.
;; - Turn of lexical scoping for functions being debugged.

(require 'cl-macs)
(require 'cl)
(require 'paredit)
(require 'tumor-hooks)

(cl-defstruct tumor-expansion
  buffer beginning end candidates)

(defvar tumor-last-expansion (make-tumor-expansion))

(global-set-key (kbd "<C-tab>") 'tumor-try-expand)

;; TODO:
;; - Sort expansion candidates.
;; - Cache symbols.
;; - Generalize to support other languages.
;; - Consider symbols in other buffers.
;; - Consider interned symbols.
;; - Detect and ignore garbage prefixes (e.g. "tumor-", but not "make-")
;; - Pay attention to word boundaries inside symbols.
;; - Insert a temporary message into the buffer showing other expansions.
;; - Support case-insensitve expansion.
;; - Re-consdier use of anchored matching.
(defun tumor-try-expand ()
  "Try to expand the partial symbol before point using the
symbols in the current buffer.  Repeated calls at the same
location insert different symbols."
  (interactive)
  (unless (and (equal (current-buffer)
		      (tumor-expansion-buffer tumor-last-expansion))
	       (equal (point)
		      (tumor-expansion-end tumor-last-expansion)))
    (tumor-try-first-expansion))
  (tumor-expand-again))

(defun tumor-try-first-expansion ()
  (let* ((part (tumor-symbol-here))
	 (candidates (tumor-matching-strings (tumor-buffer-symbols)
					       (tumor-symbol-pattern part))))
    (setq tumor-last-expansion
	  (and candidates
	       (make-tumor-expansion
		:buffer (current-buffer)
		:candidates candidates
		:beginning (- (point)
			      (length part))
		:end (point))))))

(defun tumor-expand-again ()
  (when tumor-last-expansion
    (let ((beginning (tumor-expansion-beginning tumor-last-expansion))
	  (end (tumor-expansion-end tumor-last-expansion))
	  (candidates (tumor-expansion-candidates tumor-last-expansion)))
      (goto-char beginning)
      (delete-char (- end beginning))
      (insert (car candidates))
      (setf (tumor-expansion-end tumor-last-expansion)
	    (point))
      (setf (tumor-expansion-candidates tumor-last-expansion)
	    (append (cdr candidates)
		    (list (car candidates)))))))

(defconst tumor-symbol-char (rx (any "-A-Za-z0-9")))

(defun tumor-symbol-here ()
  (let (case-fold-search)
    (when (looking-back (tumor-symbol-pattern)
			(save-excursion
			  (beginning-of-line)
			  (point))
			'greedy)
      (match-string-no-properties 0))))

(defun tumor-symbol-pattern (&optional symbol-part)
  "Convert a partial symbol name to a regexp that matches that
part."
  (if (not symbol-part)
      (rx (+ (eval `(regexp ,tumor-symbol-char))))
    (apply 'concat
	   (let ((parts nil))
	     (dotimes (i (length symbol-part)
			 (cons "^" (nreverse (cdr parts))))
	       (push (substring symbol-part i (1+ i))
		     parts)
	       (push (rx (* (eval `(regexp ,tumor-symbol-char))))
		     parts))))))

(defun tumor-matching-strings (string-list regexp)
  (cl-remove-if (lambda (item)
		  (not (string-match-p regexp item)))
		string-list))

(defun tumor-candidates-here ()
  (let* ((symbol-names (tumor-buffer-symbols))
	 (part-here (tumor-symbol-here))
	 (part-pattern (tumor-symbol-pattern part-here)))
    (tumor-matching-strings symbol-names part-pattern)))

(let* (foo (foo (dotimes (i 10 foo)
		  (push i foo)))) foo)

(defun tumor-buffer-symbols ()
  "Find and return all the symbols in the current buffer as a
list of strings."
  (save-excursion
    (goto-char (point-min))
    (let ((table (make-hash-table :test 'equal))
	  result
	  case-fold-search)
      (while (re-search-forward (tumor-symbol-pattern) nil 'noerror)
	(puthash (match-string-no-properties 0) t table))
      (maphash (lambda (key value)
		 (push key result))
	       table)
      result)))


(defvar -defining-tumor nil)
(defvar -tumor-buffer nil)

(defun tumor-fuck-this ()
  (interactive)
  (paredit-mode)
  (set (make-local-variable '-tumor-buffer) t))

(tumor-fuck-this)

(defun begin-tumor (name)
  (cl-assert (and name (symbolp name)))
  (setq -defining-tumor name))

(defun end-tumor (name)
  (cl-assert -defining-tumor)
  (cl-assert (eq name -defining-tumor))
  (setq -defining-tumor nil)
  (provide name))

(defun -tumor-private-symbol (sym)
  (cl-assert -defining-tumor)
  (intern (concat (symbol-name -defining-tumor)
		  "-"
		  (symbol-name sym))))

;; (defmacro tumor-global-defun (&rest defun-args)
;;   (let (-defining-tumor)
;;     `(cl-defun ,@defun-args)))

;; (defun -tumor-transform-symbol (sym)
;;   (if (not -tumor-transform-symbol)
;;       sym
;;     (intern (concat (symbol-name -defining-tumor)
;; 		    "/"
;; 		    (symbol-name sym)))))

;; (defadvice defun (before tumor-defun activate)
;;   (ad-set-arg 0 (-tumor-transform-symbol (ad-get-arg 0))))

;; (defadvice defvar (before tumor-defvar activate)
;;   (ad-set-arg 0 (-tumor-transform-symbol (ad-get-arg 0))))

(defmacro tumor-define-hook (hook-name hook-args &rest hook-body)
  (declare (indent defun))
  `(add-hook ',hook-name
     (defun ,(-tumor-private-symbol hook-name)
       ,hook-args
       ,@hook-body)))

(begin-tumor 'tumor)

(tumor-define-hook emacs-lisp-mode-hook ()
  (tumor-fuck-this))

(tumor-define-hook after-save-hook ()
  (when (and (derived-mode-p 'emacs-lisp-mode)
	     -tumor-buffer)
    (eval-buffer)))

(end-tumor 'tumor)
