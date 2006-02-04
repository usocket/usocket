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
  ((socket :initarg :socket
           :accessor :usocket-socket))
  ;;###FIXME: no slots (yet); should at least be the affected usocket...
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
  (interrupted-condition)
  (usocket-condition))

(define-condition unknown-condition (usocket-condition)
  ((real-condition :initarg :real-condition
                   :accessor usocket-real-condition))
  (:documentation ""))


;; Mass define and export our errors
(define-usocket-condition-classes
  (address-in-use-error
   address-not-available-error
   bad-file-descriptor-error
   connection-refused-error
   connection-aborted-error
   connection-reset-error
   invalid-argument-error
   no-buffers-error
   operation-not-supported-error
   operation-not-permitted-error
   protocol-not-supported-error
   socket-type-not-supported-error
   network-unreachable-error
   network-down-error
   network-reset-error
   host-down-error
   host-unreachable-error
   shutdown-error
   timeout-error)
  (usocket-error))

(define-condition unknown-error (usocket-error)
  ((real-error :initarg :real-error
               :accessor usocket-real-error))
  (:documentation ""))


(defmacro with-mapped-conditions ((&optional socket) &body body)
  `(handler-case
       (progn ,@body)
     (condition (condition) (handle-condition condition ,socket))))

(defparameter +unix-errno-condition-map+
  `(((11) . retry-condition) ;; EAGAIN
    ((35) . retry-condition) ;; EDEADLCK
    ((4) . interrupted-condition))) ;; EINTR

(defparameter +unix-errno-error-map+
  ;;### the first column is for non-(linux or srv4) systems
  ;; the second for linux
  ;; the third for srv4
  ;;###FIXME: How do I determine on which Unix we're running
  ;;          (at least in clisp and sbcl; I know about cmucl...)
  ;; The table below works under the assumption we'll *only* see
  ;; socket associated errors...
  `(((48 98) . address-in-use-error)
    ((49 99) . address-not-available-error)
    ((9) . bad-file-descriptor-error)
    ((61 111) . connection-refused-error)
    ((64 131) . connection-reset-error)
    ((130) . connection-aborted-error)
    ((22) . invalid-argument-error)
    ((55 105) . no-buffers-error)
    ((12) . out-of-memory-error)
    ((45 95) . operation-not-supported-error)
    ((1) . operation-not-permitted-error)
    ((43 92) . protocol-not-supported-error)
    ((44 93) . socket-type-not-supported-error)
    ((51 102) . network-unreachable-error)
    ((50 100) . network-down-error)
    ((52 102) . network-reset-error)
    ((58 108) . already-shutdown-error)
    ((60 110) . connection-timeout-error)
    ((64 112) . host-down-error)
    ((65 113) . host-unreachable-error)))




(defun map-errno-condition (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))


(defun map-errno-error (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))
