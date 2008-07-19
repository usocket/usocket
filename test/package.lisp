;;;; $Id: package.lisp 57 2006-02-07 19:39:46Z ehuelsmann $
;;;; $URL: svn://common-lisp.net/project/usocket/svn/usocket/branches/hans/test/package.lisp $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket-test
      (:use :cl :regression-test)
    (:nicknames :usoct)
    (:export :do-tests :run-usocket-tests)))

