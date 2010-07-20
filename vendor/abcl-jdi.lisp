;;;; $Id$
;;;; $URL$

;;;; Proposed contribution to the JAVA package, by Erik Huelsmann

(defpackage :jdi
  (:use :cl)
  (:export #:jcoerce
           #:jop-deref
           #:do-jmethod-call
           #:do-jmethod
           #:do-jstatic-call
           #:do-jstatic
           #:do-jnew-call
           #:do-jfield
           #:jequals))

;; but still requires the :java package.

(in-package :jdi)

(defstruct (java-object-proxy (:conc-name :jop-)
                              :copier)
  value
  class)

(defvar *jm-get-return-type*
  (java:jmethod "java.lang.reflect.Method" "getReturnType"))

(defvar *jf-get-type*
  (java:jmethod "java.lang.reflect.Field" "getType"))

(defvar *jc-get-declaring-class*
  (java:jmethod "java.lang.reflect.Constructor" "getDeclaringClass"))

(declaim (inline make-return-type-proxy))
(defun make-return-type-proxy (jmethod jreturned-value)
  (if (java:java-object-p jreturned-value)
      (let ((rt (java:jcall *jm-get-return-type* jmethod)))
        (make-java-object-proxy :value jreturned-value
                                :class rt))
    jreturned-value))

(defun make-field-type-proxy (jfield jreturned-value)
  (if (java:java-object-p jreturned-value)
      (let ((rt (java:jcall *jf-get-type* jfield)))
        (make-java-object-proxy :value jreturned-value
                                :class rt))
    jreturned-value))

(defun make-constructor-type-proxy (jconstructor jreturned-value)
  (if (java:java-object-p jreturned-value)
      (let ((rt (java:jcall *jc-get-declaring-class* jconstructor)))
        (make-java-object-proxy :value jreturned-value
                                :class rt))
    jreturned-value))

(defun jcoerce (instance &optional output-type-spec)
  (cond
   ((java-object-proxy-p instance)
    (let ((new-instance (copy-structure (the java-object-proxy instance))))
      (setf (jop-class new-instance)
            (java:jclass output-type-spec))
      new-instance))
   ((java:java-object-p instance)
    (make-java-object-proxy :class (java:jclass output-type-spec)
                            :value instance))
   ((stringp instance)
    (make-java-object-proxy :class "java.lang.String"
                            :value instance))
   ((keywordp output-type-spec)
    ;; all that remains is creating an immediate type...
    (let ((jval (java:make-immediate-object instance output-type-spec)))
      (make-java-object-proxy :class output-type-spec
                              :value jval)))
   ))

(defun jtype-of (instance) ;;instance must be a jop
  (cond
   ((stringp instance)
    "java.lang.String")
   ((keywordp (jop-class instance))
    (string-downcase (symbol-name (jop-class instance))))
   (t
    (java:jclass-name (jop-class instance)))))

(declaim (inline jop-deref))
(defun jop-deref (instance)
  (if (java-object-proxy-p instance)
      (jop-value instance)
    instance))

(defun java-value-and-class (object)
  (values (jop-deref object)
          (jtype-of object)))

(defun do-jmethod-call (object method-name &rest arguments)
  (multiple-value-bind
      (instance class-name)
      (java-value-and-class object)
    (let* ((argument-types (mapcar #'jtype-of arguments))
           (jm (apply #'java:jmethod class-name method-name argument-types))
           (rv (apply #'java:jcall jm instance
                      (mapcar #'jop-deref arguments))))
      (make-return-type-proxy jm rv))))

(defun do-jstatic-call (class-name method-name &rest arguments)
  (let* ((argument-types (mapcar #'jtype-of arguments))
         (jm (apply #'java:jmethod class-name method-name argument-types))
         (rv (apply #'java:jstatic jm (java:jclass class-name)
                    (mapcar #'jop-deref arguments))))
    (make-return-type-proxy jm rv)))

(defun do-jnew-call (class-name &rest arguments)
  (let* ((argument-types (mapcar #'jtype-of arguments))
         (jm (apply #'java:jconstructor class-name argument-types))
         (rv (apply #'java:jnew jm (mapcar #'jop-deref arguments))))
    (make-constructor-type-proxy jm rv)))

(defun do-jfield (class-or-instance-or-name field-name)
  (let* ((class (cond
                 ((stringp class-or-instance-or-name)
                  (java:jclass class-or-instance-or-name))
                 ((java:java-object-p class-or-instance-or-name)
                  (java:jclass-of class-or-instance-or-name))
                 ((java-object-proxy-p class-or-instance-or-name)
                  (java:jclass (jtype-of class-or-instance-or-name)))))
         (jf (java:jcall (java:jmethod "java.lang.Class" "getField"
                                       "java.lang.String")
                         class field-name)))
    (make-field-type-proxy jf
                           (java:jfield class field-name)))) ;;class))))

(defmacro do-jstatic (&rest arguments)
  `(do-jstatic-call ,@arguments))

(defmacro do-jmethod (&rest arguments)
  `(do-jmethod-call ,@arguments))

;;

(defmacro jstatic-call (class-name (method-name &rest arg-spec)
                                   &rest args)
  (let ((class-sym (gensym)))
    `(let ((,class-sym ,class-name))
       (java:jstatic
        (java:jmethod ,class-sym ,method-name ,@arg-spec)
        (java:jclass ,class-sym) ,@args))))

(defmacro jmethod-call (instance-and-class (method &rest arg-spec) &rest args)
  (let ((isym (gensym)))
    (multiple-value-bind
        (instance class-name)
        (if (listp instance-and-class)
            (values (first instance-and-class)
                    (second instance-and-class))
          (values instance-and-class))
      (when (null class-name)
        (setf class-name `(java:jclass-name (java:jclass-of ,isym))))
      `(let* ((,isym ,instance))
         (java:jcall (java:jmethod ,class-name ,method ,@arg-spec)
                     ,isym ,@args)))))

(defun jequals (x y)
  (do-jmethod-call (jcoerce x "java.lang.Object") "equals"
                   (jcoerce y "java.lang.Object")))

(defmacro jnew-call ((class &rest arg-spec) &rest args)
  `(java:jnew (java:jconstructor ,class ,@arg-spec)
         ,@args))
