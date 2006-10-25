;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)



(defun handle-condition (condition &optional socket)
  (typecase condition
    (error (error 'unknown-error :socket socket :real-error condition))))

(defun socket-connect (host port)
  (let ((usock))
    (with-mapped-conditions (usock)
       (let ((sock (ext:make-socket (host-to-hostname host) port)))
         (setf usock
               (make-stream-socket :socket sock
                                   :stream (ext:get-socket-stream sock)))))))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (ext:socket-close (socket usocket))))



(defmethod get-local-address ((usocket usocket))
  (dotted-quad-to-vector-quad (ext:socket-local-address (socket usocket))))

(defmethod get-peer-address ((usocket usocket))
  (dotted-quad-to-vector-quad (ext:socket-peer-address (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (ext:socket-local-port (socket usocket)))

(defmethod get-peer-port ((usocket usocket))
  (ext:socket-peer-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))
