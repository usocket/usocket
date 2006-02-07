;;;; $Id$
;;;; $URL$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket-test
      (:use :cl :regression-test)
    (:nicknames :usoct)
    (:export :do-tests :run-usocket-tests)))

