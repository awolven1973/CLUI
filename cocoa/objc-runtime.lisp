(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(eval-when (:compile-toplevel :load-toplevel)
  #+sbcl(declaim (sb-ext:muffle-conditions sb-ext:compiler-note)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library cocoa
    (:darwin (:framework "Cocoa")))
  (cffi:define-foreign-library foundation
    (:darwin (:framework "Foundation")))
  (cffi:define-foreign-library appkit
    (:darwin (:framework "AppKit"))))

(cffi:use-foreign-library foundation)
(cffi:use-foreign-library cocoa)
(cffi:use-foreign-library appkit)

(cffi:defctype o-class :pointer)
(cffi:defctype o-selector :pointer)

(defparameter *selector-cache* (make-hash-table :test 'equal))
(defparameter *class-cache* (make-hash-table :test 'equal))

(cffi:defcfun (objc-sel-register-name "sel_registerName" :library foundation)
    o-selector
  (name :string))

(cffi:defcfun (objc-look-up-class "objc_lookUpClass" :library foundation)
    o-class
  (name :string))

(cffi:defcfun (objc-sel-get-name "sel_getName")
    :string
  (sel o-selector))

(cffi:defcfun (objc-allocate-class-pair "objc_allocateClassPair" :library foundation)
    :pointer
  (superclass :pointer)
  (name :string)
  (extra-bytes :int))

(cffi:defcfun (objc-register-class-pair "objc_registerClassPair" :library foundation)
    :void
  (superclass :pointer))

(cffi:defcfun (objc-class-add-method "class_addMethod" :library foundation)
    :boolean
  (class :pointer)
  (selector :pointer)
  (cb :pointer)
  (type :string))

(cffi:defcvar (ns-app "NSApp" :library appkit) :pointer)

(defun extract-nsstring (ns-str)
  (ns::|UTF8String| ns-str))

(defun objc-ensure-class (name)
  (let ((objc-class (objc-look-up-class name)))
    (when (and objc-class (not (cffi:null-pointer-p objc-class)))
      (alexandria:ensure-gethash name *class-cache* objc-class))))

(defun objc-ensure-selector (name)
  (alexandria:ensure-gethash name
                             *selector-cache*
                             (objc-sel-register-name name)))



(cffi:define-foreign-library metalkit
  (:darwin (:framework "MetalKit")))

(cffi:use-foreign-library metalkit)

(defvar *trace-callbacks* nil)

(defmacro deftraceable-callback (name return-type (&rest args) &body body)
  `(cffi:defcallback ,name ,return-type (,@args)
     (declare (ignorable ,@(mapcar #'car args)))
     (when *trace-callbacks*
       (format t "~%~A" ',name)
       (finish-output))
     (locally 
	 ,@body)))

(cffi:defcfun (class_copyMethodList "class_copyMethodList") :pointer (cls :pointer) (out-count :pointer))
(cffi:defcfun (class_copyIvarList "class_copyIvarList") :pointer (cls :pointer) (out-count :pointer))
(cffi:defcfun (method_copyReturnType "method_copyReturnType") :pointer (m :pointer))
(cffi:defcfun (method_getNumberOfArguments "method_getNumberOfArguments") :unsigned-int (m :pointer))
(cffi:defcfun (method_getArgumentType "method_getArgumentType") :void (m :pointer) (index :unsigned-int) (dst :pointer) (dst_len :long-long))
(cffi:defcfun (method_copyArgumentType "method_copyArgumentType") :pointer (m :pointer) (index :unsigned-int))
(cffi:defcfun (class_getClassMethod "class_getClassMethod") :pointer (cls :pointer) (name :pointer))
(cffi:defcfun (class_getInstanceMethod "class_getInstanceMethod") :pointer (cls :pointer) (name :pointer))
(cffi:defcfun (method_getName "method_getName") :pointer (m :pointer))
(cffi:defcfun (ivar_getName "ivar_getName") :pointer (m :pointer))
(cffi:defcfun (class_getSuperclass "class_getSuperclass") :pointer (cls :pointer))
(cffi:defcfun (object_setInstanceVariable "object_setInstanceVariable") :pointer (object :pointer) (name :string) (value :pointer))
(cffi:defcfun (object_getClass "object_getClass") :pointer (object :pointer))
;;(cffi:defcfun (objc_msgSendSuper "objc_msgSendSuper") :pointer (obj :pointer) (selector :pointer) &rest)
;;(cffi:defcfun (objc_msgSendSuper_stret "objc_msgSendSuper_stret") :pointer (obj :pointer) (selector :pointer) &rest)
(cffi:defcfun (class_getProperty "class_getProperty") :pointer (class :pointer) (name :string))
(cffi:defcfun (objc_registerClassPair "objc_registerClassPair") :void (class :pointer))

(cffi:defcstruct objc_super
  (receiver :pointer)
  (super_class :pointer))

(cffi:defcfun (set-uncaught-exception-handler "objc_setUncaughtExceptionHandler")
    :void
  (cb :pointer))



(cffi:defcallback exception-handler :void ((exception :pointer))
  (error "~&objc exception: ~a~%" (extract-nsstring (ns::|reason| exception))))

(set-uncaught-exception-handler (cffi:callback exception-handler))

(defmacro with-autorelease-pool ((var) &body body)
  `(let ((,var (ns::|new| #@NSAutoReleasePool)))
     (unwind-protect (progn ,@body)
       (ns::|release| ,var))))

#+sbcl
(defmethod objc-object-id ((thing sb-sys:system-area-pointer))
  thing)

#+ccl
(defmethod objc-object-id ((thing ccl::macptr))
  thing)

(defmethod objc-object-id ((thing null))
  (cffi:null-pointer))

(defun ff-call (name return-type &rest args)
  (multiple-value-bind (types ctypes fargs rettype)
      (cffi::parse-args-and-types (append args (list return-type)))
    (let ((syms (cffi::make-gensym-list (length types))))
      (eval 
       (cffi::foreign-funcall-form/fsbv-with-libffi name
						    fargs
						    syms
						    types
						    rettype
						    ctypes
						    nil)))))


(defun objc-msg-send (object message return-type &rest args)
  #+ARM64
  (apply #'ff-call "objc_msgSend"
	     return-type
	     :pointer (objc-object-id object)
	     :pointer message
	     args)
  #-ARM64
  (if (consp return-type)
      (apply #'ff-call "objc_msgSend_stret"
	     return-type
	     :pointer (objc-object-id object)
	     :pointer message
	     args)
      (apply #'ff-call "objc_msgSend"
	     return-type
	     :pointer (objc-object-id object)
	     :pointer message
	     args)))


(defun alloc-init (objc-class)
  (ns:|init| (alloc objc-class)))

(defun alloc (objc-class)
  (ns:|alloc| objc-class))

(defun init (objc-object)
  (ns:|init| objc-object))
