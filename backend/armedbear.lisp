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
               (make-socket :socket sock
                            :stream (ext:get-socket-stream sock)))))))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (ext:socket-close (socket usocket))))


#.(if (null (find-symbol "SOCKET-LOCAL-ADDRESS" :ext))
      ;; abcl 0.0.9 compat code
      '(progn
         (declaim (inline %socket-address %socket-port))
         (defun %socket-address (socket addressName)
           (java:jcall "getHostAddress" (java:jcall-raw addressName socket)))

         (defun %socket-port (socket portName)
           (java:jcall portName socket))



         (defun socket-local-address (socket)
           "Returns the local address of the given socket as a dotted quad string."
           (%socket-address socket "getLocalAddress"))

         (defun socket-peer-address (socket)
           "Returns the peer address of the given socket as a dotted quad string."
           (%socket-address socket "getInetAddress"))

         (defun socket-local-port (socket)
           "Returns the local port number of the given socket."
           (%socket-port socket "getLocalPort"))

         (defun socket-peer-port (socket)
           "Returns the peer port number of the given socket."
           (%socket-port socket "getPort")))
    '(progn
       (import (:socket-peer-port :socket-peer-address
                :socket-local-port :socket-local-address) :ext)))

(defmethod get-local-address ((usocket usocket))
  (dotted-quad-to-vector-quad (socket-local-address (socket usocket))))

(defmethod get-peer-address ((usocket usocket))
  (dotted-quad-to-vector-quad (socket-peer-address (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (socket-local-port (socket usocket)))

(defmethod get-peer-port ((usocket usocket))
  (socket-peer-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))
