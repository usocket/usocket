;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)



(defparameter +openmcl-error-map+
  '((:address-in-use . address-in-use-error)
    (:connection-aborted . connection-aborted-error)
    (:no-buffer-space . no-buffers-error)
    (:connection-timed-out . timeout-error)
    (:connection-refused . connection-refused-error)
    (:host-unreachable . host-unreachable-error)
    (:host-down . host-down-error)
    (:network-down . network-down-error)
    (:address-not-available . address-not-available-error)
    (:network-reset . network-reset-error)
    (:connection-reset . connection-reset-error)
    (:shutdown . shutdown-error)
    (:access-denied . operation-not-permitted-error)))


(defun handle-condition (condition &optional socket)
  (typecase condition
    (socket-error
     (let ((usock-err (cdr (assoc (socket-error-identifier condition)
                                  +openmcl-error-map+))))
       (if usock-err
           (error usock-err :socket socket)
         (error 'unknown-error :socket socket :real-error condition))))
    (error (error 'unknown-error :socket socket :real-error condition))
    (condition (signal 'unknown-condition :real-condition condition))))

(defun socket-connect (host port)
  (let ((sock))
    (with-mapped-conditions (sock)
      (setf sock
            (make-socket :remote-host (host-to-hostname host)
                         :remote-port port))
      (socket-connect sock))))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))
