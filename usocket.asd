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
    :version "0.5.0-dev"
    :licence "MIT"
    :description "Universal socket library for Common Lisp"
    :depends-on (:split-sequence
                 #+sbcl :sb-bsd-sockets)
    :components ((:file "package")
                 (:file "rtt"
                  :depends-on ("package"))
                 (:file "usocket"
                  :depends-on ("package" "rtt"))
                 (:file "condition"
                  :depends-on ("usocket" "rtt"))
                 (:module "backend"
                  :components (#+clisp         (:file "clisp")
                               #+cmu           (:file "cmucl")
                               #+scl           (:file "scl")
                               #+(or sbcl ecl) (:file "sbcl")
                               #+lispworks     (:file "lispworks")
                               #+openmcl       (:file "openmcl")
                               #+allegro       (:file "allegro")
                               #+armedbear     (:file "armedbear"))
                  :depends-on ("condition"))
                 (:file "rtt-client"
                  :depends-on ("rtt" "backend" "condition"))
                 (:file "server"
                  :depends-on ("backend"))))
