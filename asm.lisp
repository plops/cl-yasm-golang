;(ql:quickload "optima")
(ql:quickload "alexandria")
(defpackage :cl-yasm-generator
  (:use :cl
	;:optima
	:alexandria)
  (:export
   #:write-source))

(in-package :cl-yasm-generator)

(setf (readtable-case *readtable*) :invert)

(defparameter *file-hashes* (make-hash-table))

(defun write-source (name code &optional (dir (user-homedir-pathname))
				 ignore-hash)
  (let* ((fn (merge-pathnames (format nil "~a.go" name)
			      dir))
	(code-str (emit-go
		   :code code))
	(fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
     (when (or (not exists) ignore-hash (/= code-hash old-code-hash))
       ;; store the sxhash of the c source in the hash table
       ;; *file-hashes* with the key formed by the sxhash of the full
       ;; pathname
       (setf (gethash fn-hash *file-hashes*) code-hash)
       (with-open-file (s fn
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	 (write-sequence code-str s))

       (sb-ext:run-program
					;"/usr/local/go/bin/go"
	"/usr/bin/go"
	(list "fmt" (namestring fn))
	)))))


;; http://clhs.lisp.se/Body/s_declar.htm
;; http://clhs.lisp.se/Body/d_type.htm

;; go through the body until no declare anymore

(defun consume-declare (body)
  "take a list of instructions (body), parse type declarations,
return the body without them and a hash table with an environment. the
entry return-values contains a list of return values"

  ;; (declare (type int a b) (type float c)
  ;; (declare (values int &optional))
  ;; (declare (values int float &optional))

  ;; FIXME doesnt handle documentation strings
  (let ((env (make-hash-table))
	(looking-p t)
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (loop for declaration in (cdr e) do
		      (when (eq (first declaration) 'type)
			(destructuring-bind (symb type &rest vars) declaration
			  (declare (ignorable symb))
			  (loop for var in vars do
			       (setf (gethash var env) type))))
		      (when (eq (first declaration) 'values)
			(destructuring-bind (symb &rest types-opt) declaration
			  (declare (ignorable symb))
			  (let ((types nil))
			    ;; only collect types until occurrance of &optional
			    (loop for type in types-opt do
				 (unless (eq #\& (aref (format nil "~a" type) 0))
				   (push type types)))
			    (setf (gethash 'return-values env) (reverse types))))))
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values (reverse new-body) env)))

(defun lookup-type (name &key env)
  "get the type of a variable from an environment"
  (gethash name env))

(defun parse-let (code emit)
  "let ({var | (var [init-form])}*) declaration* form*"
  (destructuring-bind (decls &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (with-output-to-string (s)
	(format s "~a"
		(funcall emit
			`(do0
			  ,@(loop for decl in decls collect
				 (if (listp decl) ;; split into name and initform
				     (destructuring-bind (name &optional value) decl
				       (format nil "var ~a~@[ ~a~]~@[ = ~a~]"
					       name
					       (lookup-type name :env env)
					       (funcall emit value)))
				     (format nil "var ~a ~a"
					     decl
					     (let ((type (lookup-type decl :env env)))
					       (if type
						   type
						   (break "type ~a not defined." decl))))))
			  ,@body)))))))

(defun parse-defun (code emit)
  ;;  defun function-name lambda-list [declaration*] form*
  ;; https://golang.org/ref/spec#Function_declarations
  ;; func(x float, y int) int {
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body) ;; py
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "func ~a~a ~@[~a ~]"
		  name
		  (funcall emit `(paren
				  ,@(loop for p in req-param collect
					 (format nil "~a ~a"
						 p
						 (let ((type (gethash p env)))
						   (if type
						       type
						       (break "can't find type for ~a in defun"
							      p)))))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r))))
	  (format s "~a" (funcall emit `(progn ,@body))))))))

(defun parse-defun-declaration (code emit)
  ;; only emit the declaration of the function
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "~a~a ~@[~a ~]"
		  name
		  (funcall emit `(paren
				  ,@(loop for p in req-param collect
					 (format nil "~a ~a"
						 p
						 (let ((type (gethash p env)))
						   (if type
						       type
						       (break "can't find type for ~a in defun"
							      p)))))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r)))))))))

(defun parse-defmethod (code emit)
  ;;  defmethod function-name specialized-lambda-list [declaration*] form*
  ;; specialized-lambda-list::= ({var | (var parameter-specializer-name)}*
  ;; first element of lambda-list declares the object (the methods receiver)
  ;; (defmethod Distance ((p Point) q) ...
  ;; => func (p Point) Distance(q Point) float64 { ...
  
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (destructuring-bind (receiver-name receiver-type) (car lambda-list)
	(multiple-value-bind (req-param opt-param res-param
					key-param other-key-p
					aux-param key-exist-p)
	    (parse-ordinary-lambda-list (cdr lambda-list))
	  (declare (ignorable req-param opt-param res-param
			      key-param other-key-p aux-param key-exist-p))
	  (with-output-to-string (s)
	    (format s "func (~a ~a) ~a~a ~@[~a ~]"
		    receiver-name
		    receiver-type
		    name
		    (funcall emit `(paren
				    ,@(loop for p in req-param collect
					   (format nil "~a ~a"
						   p
						   (let ((type (gethash p env)))
						     (if type
							 type
							 (break "can't find type for ~a in defun"
								p)))))))
		    (let ((r (gethash 'return-values env)))
		      (if (< 1 (length r))
			  (funcall emit `(paren ,@r))
			  (car r))))
	    (format s "~a" (funcall emit `(progn ,@body)))))))))

(defun parse-defmethod-declaration (code emit)
  ;; only emit declaration 
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (destructuring-bind (receiver-name receiver-type) (car lambda-list)
	(multiple-value-bind (req-param opt-param res-param
					key-param other-key-p
					aux-param key-exist-p)
	    (parse-ordinary-lambda-list (cdr lambda-list))
	  (declare (ignorable req-param opt-param res-param
			      key-param other-key-p aux-param key-exist-p))
	  (with-output-to-string (s)
	    (format s "func (~a ~a) ~a~a ~@[~a ~]"
		    receiver-name
		    receiver-type
		    name
		    (funcall emit `(paren
				    ,@(loop for p in req-param collect
					   (format nil "~a ~a"
						   p
						   (let ((type (gethash p env)))
						     (if type
							 type
							 (break "can't find type for ~a in defun"
								p)))))))
		    (let ((r (gethash 'return-values env)))
		      (if (< 1 (length r))
			  (funcall emit `(paren ,@r))
			  (car r))))))))))

(defun parse-defmethod-interface (code emit)
  ;; only emit declaration that can be used in an interface
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (destructuring-bind (receiver-name receiver-type) (car lambda-list)
	(multiple-value-bind (req-param opt-param res-param
					key-param other-key-p
					aux-param key-exist-p)
	    (parse-ordinary-lambda-list (cdr lambda-list))
	  (declare (ignorable req-param opt-param res-param
			      key-param other-key-p aux-param key-exist-p))
	  (with-output-to-string (s)
	    (format s "~a~a ~@[~a ~]"
		    name
		    (funcall emit `(paren
				    ,@(loop for p in req-param collect
					   (format nil "~a ~a"
						   p
						   (let ((type (gethash p env)))
						     (if type
							 type
							 (break "can't find type for ~a in defun"
								p)))))))
		    (let ((r (gethash 'return-values env)))
		      (if (< 1 (length r))
			  (funcall emit `(paren ,@r))
			  (car r))))))))))


(defun parse-lambda (code emit)
  ;;  lambda lambda-list [declaration*] form*
  (destructuring-bind (lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "func ~a ~@[~a ~]"
		  (funcall emit `(paren
				  ,@(loop for p in req-param collect
					 (format nil "~a ~a"
						 p (gethash p env)))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r))))
	  (format s "~a" (funcall emit `(progn ,@body))))))))




(defun parse-setf (code emit)
  "setf {pair}*"
  (let ((args (cdr code)))
   (format nil "~a"
	   (funcall emit
		    `(do0 
		      ,@(loop for i below (length args) by 2 collect
			     (let ((a (elt args i))
				   (b (elt args (+ 1 i))))
			       `(= ,a ,b))))))))

(defun parse-const (code emit)
  "const {pair}*"
  (let ((args (cdr code)))
    (with-output-to-string (s)
      (format s "const (")
      (format s "~a~%"
	     (funcall emit
		      `(do0 
			,@(loop for i below (length args) by 2 collect
			       (let ((a (elt args i))
				     (b (elt args (+ 1 i))))
				 `(= ,a ,b))))))
      (format s ")"))))





(progn
  (defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'double-float))
	 (s (format nil "~E" ff)))
    #+nil (assert (= 0d0 (- ff
			    (read-from-string s))))
    (assert (< (abs (- ff
		       (read-from-string s)))
	       1d-12))
   (substitute #\e #\d s)))
  (defun emit-asm (&key code (str nil)  (level 0))
    (flet ((emit (code &optional (dl 0))
	     "change the indentation level. this is used in do"
	     (emit-asm :code code :level (+ dl level))))
      (if code
	  (if (listp code)
	      (case (car code)
		(tuple (let ((args (cdr code)))
		       (format nil "~{~a~^, ~}" (mapcar #'emit args))))
		(indent
		 ;; indent form
		 (format nil "~{~a~}~a"
				;; print indentation characters
			      (loop for i below level collect "    ")
			      (emit (cadr code))))
		(do0 (with-output-to-string (s)
		       ;; do0 {form}*
		       ;; write each form into a newline, keep current indentation level
		     (format s "~&~a~{~&~a~}"
			     (emit (cadr code))
			     (mapcar #'(lambda (x) (emit `(indent ,x) 0)) (cddr code)))))
		(global (destructuring-bind (&rest vars) (cdr code)
			  (format nil "~{global ~a~%~}" vars)))
		(tag (let ((name (car (cdr code))))
		       (format nil "~a:~%" name)))
		(string (format nil "\"~a\"" (cadr code)))
		(section (destructuring-bind (name &rest body) (cdr code)
			     (format nil "section ~a~%~a" name (emit `(do0 ,@body)))))
		(data
		 ;; var{8,16,32,64,128}, float128
		 (with-output-to-string (s)
		  (loop for (name type val) in (cdr code) do
		       (format s "~a ~a ~a~%" name
			       (ecase type
				 (var8 "db")
				 (var16 "dw")
				 (var32 "dd")
				 (var64 "dq")
				 (var128 "ddq")
				 (float128 "dt"))
			       (emit val)))))
		(t (destructuring-bind (name &rest args) code
		     (if (listp name)
		       ;; lambda call and similar complex constructs
			 (format nil "(~a)(~a)"
				 (emit name)
				 (if args
				     (emit `(paren ,@args))
				     ""))
			 ;; function call
			 (progn
			   (format nil "~a~a" name
				   (emit `(paren ,@args))))))))
	      (cond
		((or (symbolp code)
		     (stringp code)) ;; print variable
		 (format nil "~a" code))
		((numberp code) ;; print constants
		 (cond ((integerp code) (format str "~a" code))
		       ((floatp code) ;; FIXME arbitrary precision?
			(format str "(~a)" (print-sufficient-digits-f64 code)))))))
	  "")))
  (defparameter *bla*
    (emit-asm :code `(do0
		      (section .data
		       (data (bVar var8 10)
			     (cVar var8 (string H))
			     (strng var8 (string "Hello world"))
			     (arr var32 (tuple 100 200 300))))
		      (section .text
			       (global _start)
			       (tag _start)))))
  (with-output-to-file (s "/dev/shm/o.s" ; :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format s *bla*)))
