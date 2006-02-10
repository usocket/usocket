;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)



(defun handle-condition (condition &optional socket)
  (describe condition)
  (typecase condition
    (error (error 'unknown-error :socket socket :real-error condition))))

(defun socket-connect (host port)
  (let ((usock))
    (with-mapped-conditions (usock)
       (let ((sock (ext:make-socket (host-to-hostname host) port)))
         (setf usock
               (make-socket :socket sock
                            :stream (ext:get-socket-stream sock)))))))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (ext:socket-close (socket usocket))))
