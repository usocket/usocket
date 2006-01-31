;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (condition (error 'usocket-error
                      :real-condition condition
                      :socket socket))))

(defun socket-connect (host port &optional (type :stream))
  (declare (ignore type))
  (let ((socket (socket:socket-connect port host
                                       :element-type 'character
                                       :buffered t)))
    (make-socket :socket socket
                 :stream socket ;; the socket is a stream too
                 :host host
                 :port port))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (close (socket usocket)))



(defun get-host-by-address (address)
  (handler-case
   (posix:hostent-name
    (posix:resolve-host-ipaddr (vector-quad-to-dotted-quad address)))
   (condition (condition) (handle-condition condition))))

(defun get-hosts-by-name (name)
  (handler-case
   (mapcar #'dotted-quad-to-vector-quad
           (posix:hostent-addr-list (posix:resolve-host-ipaddr name)))
   (condition (condition) (handle-condition condition))))

