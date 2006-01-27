;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

#+lispworks (require "comm")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket
      (:use :cl)
    (:nicknames :usoc)
    (:shadowing-import-from "COMMON-LISP" :close
                  :open
                  :read-line
                  :write-sequence)
    (:export :open ; socket related operations
             :make-socket 
             :close
             :read-line
             :write-sequence
             :socket ; socket object and accessors
             :host
             :port
             :get-host-by-address ; name services
             :get-host-by-name
             :host-byte-order ; utility operators
             :usocket-error ; conditions
             :no-route-to-host)))

