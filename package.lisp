;;;; $Id$
;;;; $URL$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

#+lispworks (require "comm")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket
      (:use :cl)
    (:export :socket-connect ; socket constructors and methods
             :socket-close

             :with-connected-socket ; macros

             :usocket ; socket object and accessors
             :socket
             :socket-stream

             :host-byte-order ; IPv4 utility functions
             :hbo-to-dotted-quad
             :hbo-to-vector-quad
             :vector-quad-to-dotted-quad
             :dotted-quad-to-vector-quad

             :usocket-condition ; conditions
             :usocket-error ; errors
             :unknown-condition
             :unknown-error)))

