;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket-test
      (:use :cl :rt)
    (:nicknames :usoct)
    (:export :do-tests)))

