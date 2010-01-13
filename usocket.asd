;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; $URL$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:usocket-system
    (:use #:cl #:asdf))

(in-package #:usocket-system)

(defsystem usocket
    :name "usocket"
    :author "Erik Enge & Erik Huelsmann"
    :version "0.5.0"
    :licence "MIT"
    :description "Universal socket library for Common Lisp"
    :depends-on (#+sbcl :sb-bsd-sockets)
    :components ((:file "package")
		 (:module "vendor"
		  :components ((:file "split-sequence")
			       #+mcl (:file "kqueue")))
                 (:file "usocket" :depends-on ("package" "vendor"))
                 (:file "condition" :depends-on ("usocket"))
		 (:module "backend" :depends-on ("usocket" "condition")
		  :components (#+clisp		(:file "clisp")
			       #+cmu		(:file "cmucl")
			       #+scl		(:file "scl")
			       #+(or sbcl ecl)	(:file "sbcl")
			       #+lispworks	(:file "lispworks")
			       #+mcl		(:file "mcl")
			       #+openmcl	(:file "openmcl")
			       #+allegro	(:file "allegro")
			       #+armedbear	(:file "armedbear")))
		 (:file "server" :depends-on ("backend"))))
