;;;; $Id$
;;;; $URL$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

#+lispworks (require "comm")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :usocket
      (:use :cl)
    (:export #:socket-connect ; socket constructors and methods
             #:socket-listen
             #:socket-accept
             #:socket-close
             #:get-local-address
             #:get-peer-address
             #:get-local-port
             #:get-peer-port
             #:get-local-name
             #:get-peer-name

             #:with-connected-socket ; convenience macros
             #:with-server-socket
             #:with-client-socket
             #:with-socket-listener

             #:usocket ; socket object and accessors
             #:stream-usocket
             #:stream-server-usocket
             #:socket
             #:socket-stream

             #:host-byte-order ; IP(v4) utility functions
             #:hbo-to-dotted-quad
             #:hbo-to-vector-quad
             #:vector-quad-to-dotted-quad
             #:dotted-quad-to-vector-quad
             #:ip=
             #:ip/=

             #:socket-condition ; conditions
             #:socket-error ; errors
             #:unknown-condition
             #:unknown-error)))

