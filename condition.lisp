;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

;; (define-condition usocket-error (error)
;;   ((real-condition
;;     :reader real-condition
;;     :initarg :real-condition)
;;    (socket
;;     :reader socket
;;     :initarg :socket))
;;   (:report (lambda (c stream)
;;              (format stream "Error (~A) occured in socket: ~A."
;;                      (real-condition c) (socket c)))))

(define-condition usocket-condition (condition)
  () ;;###FIXME: no slots (yet); should at least be the affected usocket...
  (:documentation ""))

(define-condition usocket-error (usocket-condition error)
  () ;; no slots (yet)
  (:documentation ""))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-usocket-condition-class (class &rest parents)
    `(progn
       (define-condition ,class ,parents ())
       (export ',class))))

(defmacro define-usocket-condition-classes (class-list parents)
  `(progn ,@(mapcar #'(lambda (x)
                        (apply #'define-usocket-condition-class
                               x parents))
                    class-list)))

;; Mass define and export our conditions
(define-usocket-condition-classes
  (usocket-interrupted-condition)
  (usocket-condition))

(define-condition usocket-unknown-condition (usocket-condition)
  ((real-condition))
  (:documentation ""))


;; Mass define and export our errors
(define-usocket-condition-classes
  (usocket-address-in-use-error
   usocket-address-not-available-error
   usocket-bad-file-descriptor-error
   usocket-connection-refused-error
   usocket-invalid-argument-error
   usocket-no-buffers-error
   usocket-operation-not-supported-error
   usocket-operation-not-permitted-error
   usocket-protocol-not-supported-error
   usocket-socket-type-not-supported-error
   usocket-network-unreachable-error
   usocket-network-down-error
   usocket-network-reset-error
   usocket-host-down-error
   usocket-host-unreachable-error
   usocket-shutdown-error
   usocket-timeout-error)
  (usocket-error))

(define-condition usocket-unknown-error (usocket-error)
  ((real-error))
  (:documentation ""))
