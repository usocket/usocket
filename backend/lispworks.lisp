;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

#+win32
(defun remap-maybe-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +lispworks-error-map+
  #+win32
  (append (remap-maybe-for-win32 +unix-errno-condition-map+)
          (remap-maybe-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))



(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (simple-error (destructuring-bind (&optional host port err-msg errno)
                      (simple-condition-format-arguments condition)
                    (declare (ignore host port err-msg))
                    (let* ((usock-err
                            (cdr (assoc errno +lispworks-error-map+
                                        :test #'member))))
                      (if usock-err
                          (if (subtypep usock-err 'error)
                              (error usock-err :socket socket)
                            (signal usock-err :socket socket))
                        (error 'unknown-error
                               :socket socket
                               :real-error condition)))))))
;;     (condition (error 'usocket-error
;;                       :real-condition condition
;;                       :socket socket))))

(defun socket-connect (host port &key (element-type 'base-char))
  (let ((hostname (host-to-hostname host))
        (stream))
    (setf stream
          (with-mapped-conditions ()
             (comm:open-tcp-stream hostname port
                                   :element-type element-type)))
    (if stream
        (make-stream-socket :socket (comm:socket-stream-socket stream)
                            :stream stream)
      (error 'unknown-error))))
;;                 :host host
;;                 :port port))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (close (socket-stream usocket)))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-peer-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket usocket))
  (nth-value 1 (get-peer-name usocket)))
