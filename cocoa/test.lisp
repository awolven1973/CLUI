(in-package :clui)

(defun show-cifs (lisp-cif c-cif)
  (flet ((slot-val (cif slot) (cffi:foreign-slot-value cif '(:struct cffi::ffi-cif) slot)))
    (let ((c-argument-types)
	  (l-argument-types))
      (print (slot-val c-cif 'cffi::abi))
      (print (slot-val lisp-cif 'cffi::abi))
      (print (slot-val c-cif 'cffi::argument-count))
      (print (slot-val lisp-cif 'cffi::argument-count))
      (setq c-argument-types (print (slot-val c-cif 'cffi::argument-types)))
      (setq l-argument-types (print (slot-val lisp-cif 'cffi::argument-types)))
      (print (slot-val c-cif 'cffi::return-type))
      (print (slot-val lisp-cif 'cffi::return-type))
      (print (slot-val c-cif 'cffi::bytes))
      (print (slot-val lisp-cif 'cffi::bytes))
      (print (slot-val c-cif 'cffi::flags))
      (print (slot-val lisp-cif 'cffi::flags))
      (loop for i from 0 below 1
	    do
	       (print (cffi::foreign-slot-value (cffi::mem-aref c-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::size))
	       (print (cffi::foreign-slot-value (cffi::mem-aref l-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::size))
	       (print (cffi::foreign-slot-value (cffi::mem-aref c-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::alignment))
	       (print (cffi::foreign-slot-value (cffi::mem-aref l-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::alignment))
	       (print (cffi::foreign-slot-value (cffi::mem-aref c-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::type))
	       (print (cffi::foreign-slot-value (cffi::mem-aref l-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::type))
	       (let ((c (cffi::foreign-slot-value (cffi::mem-aref c-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::elements))
		     (l (cffi::foreign-slot-value (cffi::mem-aref c-argument-types :pointer i) '(:struct cffi::ffi-type) 'cffi::elements)))
		 (loop for i from 0 below 4
		       do  (print (cffi::foreign-slot-value (cffi::mem-aref c :pointer i) '(:struct cffi::ffi-type) 'cffi::size))
			   (print (cffi::foreign-slot-value (cffi::mem-aref l :pointer i) '(:struct cffi::ffi-type) 'cffi::size))
			   (print (cffi::foreign-slot-value (cffi::mem-aref c :pointer i) '(:struct cffi::ffi-type) 'cffi::alignment))
			   (print (cffi::foreign-slot-value (cffi::mem-aref l :pointer i) '(:struct cffi::ffi-type) 'cffi::alignment))
			   (print (cffi::foreign-slot-value (cffi::mem-aref c :pointer i) '(:struct cffi::ffi-type) 'cffi::type))
		       finally (print (cffi::mem-aref c :pointer 4))
			       (print (cffi::mem-aref l :pointer 4))))))))

	
      
    
    
    

(cffi:defcfun (test_function "test_function") (:struct ns::|CGRect|) (input (:struct ns::|CGRect|)))

;;clang -dynamiclib -o test_function.dylib test.c

(cffi:load-foreign-library (asdf/system:system-relative-pathname :clui "cocoa/test_function.dylib"))

(cffi:load-foreign-library (asdf/system:system-relative-pathname :clui "cocoa/test2.dylib"))

(named-readtables:in-readtable :objc-readtable)

(cffi:defcstruct test-struct
  (a :double)
  (b :double)
  (c :double))

(defun test (myclass struct)
  (let ((message-lambda 
	  (make-message-lambda @(test:) (((:struct test-struct)) (:struct test-struct)))))
   (funcall message-lambda (objc-object-id myclass) struct)))

(defun test2 ()
  (let ((myclass (ns:|alloc| #@MyClass)))
    (test myclass (list 'a 5.0d0 'b 0.0d0 'c 20.0d0))))

(defun test3 ()
  (cffi::with-foreign-object (foo '(:struct test-struct))
    (cffi:translate-into-foreign-memory (list 'a 5.0d0 'b 0.0d0 'c 20.0d0)
					(cffi::parse-type '(:struct test-struct))
					foo)
    (cffi:translate-from-foreign foo (cffi::parse-type '(:struct test-struct)))))

(defun test4 ()
  (cffi::make-libffi-cif "objc_msgSend"
			 '(:struct test-struct)
			 '(:pointer :pointer
			   (:struct test-struct))
			 ':default-abi))

(defun testy1 ()
  (test_function (make-nsrect 1 2 300 300)))

(defun testy2 ()
  (test_function1 (make-nsrect 1 2 300 300)))

(defun testy3 ()
  (test_function3 (make-nsrect 1 2 300 300)))

(defun testy ()
  (test5 (ns:|alloc| #@MyClass) (list 'a 5.0d0 'b 0.0d0 'c 20.0d0)))

(cffi:load-foreign-library (asdf/system:system-relative-pathname :clui "cocoa/test5.dylib"))

(cffi:defcfun (test_cif "test_cif") :int (cif* :pointer))

(cffi:defcfun (test_cif2 "test_cif2") :int (cif* :pointer))

(defun test-cif ()
  (let ((cif (cffi:foreign-alloc '(:struct cffi::ffi-cif))))
    (when (= 1 (test_cif cif))
      cif)))

(defun test-cif2 ()
  (let ((cif (cffi:foreign-alloc '(:struct cffi::ffi-cif))))
    (when (= 1 (test_cif2 cif))
      cif)))

(defun test_function1 (input)
  (let ((cgrect-tclass (cffi::parse-type '(:struct ns::|CGRect|))))
    (cffi:with-foreign-objects ((cffi::argument-values :pointer 1)
				(cffi::result '(:struct ns::|CGRect|)))
      (cffi:with-foreign-object (g5471 '(:struct ns::|CGRect|))
	(cffi:translate-into-foreign-memory input cgrect-tclass
                                           g5471)
	(print (list (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::x)
		     (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::y)
		     (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::width)
		     (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::height)))
	(loop :for cffi::arg :in (list g5471)
	      :for
	      count :from 0
	      :do (setf (cffi:mem-aref cffi::argument-values :pointer count)
			cffi::arg))
	(let* ((cffi::libffi-cif-cache
		 (load-time-value (cons 'cffi::libffi-cif-cache nil)))
	       (cffi::libffi-cif
		 (or ;;(cdr cffi::libffi-cif-cache)
		     (setf (cdr cffi::libffi-cif-cache)
			   (test-cif2)))))
	  (cffi::libffi/call cffi::libffi-cif
			     (cffi:foreign-symbol-pointer "test_function")
			     cffi::result cffi::argument-values)
	  (list (cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::x)
		(cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::y)
		(cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::width)
		(cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::height)))))))

(defun test_function3 (input)
  (let ((cgrect-tclass (cffi::parse-type '(:struct ns::|CGRect|))))
    (cffi:with-foreign-objects ((cffi::argument-values :pointer 1)
				(cffi::result '(:struct ns::|CGRect|)))
      (cffi:with-foreign-object (g5471 '(:struct ns::|CGRect|))
	(cffi:translate-into-foreign-memory input cgrect-tclass
                                           g5471)
	(print (list (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::x)
		     (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::y)
		     (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::width)
		     (cffi::foreign-slot-value g5471 '(:struct ns::|CGRect|) 'ns::height)))
	(loop :for cffi::arg :in (list g5471)
	      :for
	      count :from 0
	      :do (setf (cffi:mem-aref cffi::argument-values :pointer count)
			cffi::arg))
	(let* ((cffi::libffi-cif-cache
		 (load-time-value (cons 'cffi::libffi-cif-cache nil)))
	       (cffi::libffi-cif
		 (or ;;(cdr cffi::libffi-cif-cache)
		     (setf (cdr cffi::libffi-cif-cache)
			   (cffi::make-libffi-cif "foo"
						  '(:struct ns::|CGRect|)
						  '((:struct ns::|CGRect|))
						  ':default-abi)))))
	  (setf (cffi:foreign-slot-value cffi::libffi-cif '(:struct cffi::ffi-cif) 'cffi::abi) 1)
	  (setf (cffi::foreign-slot-value (cffi::mem-aref (cffi::foreign-slot-value cffi::libffi-cif '(:struct cffi::ffi-cif) 'cffi::argument-types) :pointer 0) '(:struct cffi::ffi-type) 'cffi::size) 32)
	  (cffi::libffi/call cffi::libffi-cif
			     (cffi:foreign-symbol-pointer "test_function")
			     cffi::result cffi::argument-values)
	  (list (cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::x)
		(cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::y)
		(cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::width)
		(cffi::foreign-slot-value cffi::result '(:struct ns::|CGRect|) 'ns::height)))))))
    

(defun test5 (target g5461)
  (let ((test-struct-tclass (cffi::parse-type '(:struct test-struct)))
	(pointer-type (cffi::parse-type :pointer)))
    (cffi:with-foreign-objects ((cffi::argument-values :pointer 3)
				(cffi::result '(:struct test-struct)))
    
      (cffi:with-foreign-object (g5462 :pointer)
	(cffi:translate-into-foreign-memory target
					    pointer-type
					    g5462)
	(cffi:with-foreign-object (g5463 :pointer)
	  (cffi:translate-into-foreign-memory
	   (objc-runtime:ensure-selector "test:")
	   pointer-type g5463)
	  (cffi:with-foreign-object (g5464 '(:struct test-struct))
	    (cffi:translate-into-foreign-memory g5461
						test-struct-tclass
						g5464)
	    ;;(cffi:translate-from-foreign
	     (progn
	       (loop :for cffi::arg :in (list g5462 g5463 g5464)
		     :for
		     count :from 0
		     :do (setf (cffi:mem-aref cffi::argument-values :pointer count)
			       cffi::arg))
	       (let* ((cffi::libffi-cif-cache
			(load-time-value (cons 'cffi::libffi-cif-cache nil)))
		      (cffi::libffi-cif
			(or (cdr cffi::libffi-cif-cache)
			    (setf (cdr cffi::libffi-cif-cache)
				  (test-cif)))))
		 (cffi::libffi/call cffi::libffi-cif
				    (cffi:foreign-symbol-pointer "objc_msgSend")
				    cffi::result cffi::argument-values)
		 cffi::result))
	  ;;  test-struct-tclass)
	  ))))))

#|
(defun test_function (input)
   (cffi:with-foreign-objects ((cffi::argument-values :pointer 1)
                               (cffi::result '(:struct ns::|CGRect|)))
     (cffi:with-foreign-object (g5466 '(:struct ns::|CGRect|))
       (cffi:translate-into-foreign-memory input #<|CGRect-TCLASS| ns::|CGRect|>
                                           g5466)
       (cffi:translate-from-foreign
        (progn
         (loop :for cffi::arg :in (list g5466)
               :for
               count :from 0
               :do (setf (cffi:mem-aref cffi::argument-values :pointer count)
                           cffi::arg))
         (let* ((cffi::libffi-cif-cache
                 (load-time-value (cons 'cffi::libffi-cif-cache nil)))
                (cffi::libffi-cif
                 (or (cdr cffi::libffi-cif-cache)
                     (setf (cdr cffi::libffi-cif-cache)
                             (cffi::make-libffi-cif "test_function"
                                                    '(:struct ns::|CGRect|)
                                                    '((:struct ns::|CGRect|))
                                                    ':default-abi)))))
           (cffi::libffi/call cffi::libffi-cif
                              (cffi:foreign-symbol-pointer "test_function")
                              cffi::result cffi::argument-values)
           cffi::result))
        #<|CGRect-TCLASS| ns::|CGRect|>))))
|#
