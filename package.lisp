;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

#+lispworks (require "comm")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket
      (:use :cl)
    (:export :socket-connect ; socket constructors and methods
             :socket-close

             :usocket ; socket object and accessors
             :socket-stream

             :get-host-by-address ; name services
             :get-hosts-by-name
             :get-host-by-name
             :get-random-host-by-name

             :host-byte-order ; IPv4 utility functions
             :hbo-to-dotted-quad
             :hbo-to-vector-quad
             :vector-quad-to-dotted-quad
             :dotted-quad-to-vector-quad

             :usocket-error ; conditions
             :no-route-to-host)))

