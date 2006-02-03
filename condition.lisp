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
  ((real-condition :initarg :real-condition
                   :accessor usocket-real-condition))
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
  ((real-error :initarg :real-error
               :accessor usocket-real-error))
  (:documentation ""))


(defmacro with-mapped-conditions ((&optional socket) &body body)
  `(handler-case
       (progn ,@body)
     (condition (condition) (handle-condition condition ,socket))))

(defparameter +unix-errno-condition-map+
  `((11 . usocket-retry-condition) ;; EAGAIN
    (35 . usocket-retry-condition) ;; EDEADLCK
    (4 . usocket-interrupted-condition))) ;; EINTR

(defparameter +unix-errno-error-map+
  ;;### the first column is for non-(linux or srv4) systems
  ;; the second for linux
  ;; the third for srv4
  ;;###FIXME: How do I determine on which Unix we're running
  ;;          (at least in clisp and sbcl; I know about cmucl...)
  ;; The table below works under the assumption we'll *only* see
  ;; socket associated errors...
  `(((48 98) . usocket-address-in-use-error)
    ((49 99) . usocket-address-not-available-error)
    ((9) . usocket-bad-file-descriptor-error)
    ((61 111) . usocket-connection-refused-error)
    ((22) . usocket-invalid-argument-error)
    ((55 105) . usocket-no-buffers-error)
    ((12) . usocket-out-of-memory-error)
    ((45 95) . usocket-operation-not-supported-error)
    ((1) . usocket-operation-not-permitted-error)
    ((43 92) . usocket-protocol-not-supported-error)
    ((44 93) . usocket-socket-type-not-supported-error)
    ((51 102) . usocket-network-unreachable-error)
    ((50 100) . usocket-network-down-error)
    ((52 102) . usocket-network-reset-error)
    ((58 108) . usocket-already-shutdown-error)
    ((60 110) . usocket-connection-timeout-error)
    ((64 112) . usocket-host-down-error)
    ((65 113) . usocket-host-unreachable-error)))




(defun map-errno-condition (errno)
  (cdr (assoc errno +unix-errno-error-map+)))


(defun map-errno-error (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))
