;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(define-condition usocket-error (error)
  ((real-condition
    :reader real-condition
    :initarg :real-condition)
   (socket
    :reader socket
    :initarg :socket))
  (:report (lambda (c stream)
             (format stream "Error (~A) occured in socket: ~A."
                     (real-condition c) (socket c)))))
                  
