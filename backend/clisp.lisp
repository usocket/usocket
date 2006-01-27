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

(defun open (host port &optional (type :stream))
  (declare (ignore type))
  (make-socket :socket (socket:socket-connect port host)
               :host host
               :port port))

(defmethod close ((socket socket))
  "Close socket."
  (socket:socket-server-close (real-socket socket)))

(defmethod read-line ((socket socket))
  (cl:read-line (real-socket socket)))

(defmethod write-sequence ((socket socket) sequence)
  (cl:write-sequence sequence (real-socket socket)))

(defun get-host-by-address (address)
  (handler-case (posix:hostent-name
                 (posix:resolve-host-ipaddr (vector-quad-to-dotted-quad address)))
    (condition (condition) (handle-condition condition))))

(defun get-host-by-name (name)
  (handler-case (mapcar #'dotted-quad-to-vector-quad
                        (posix:hostent-addr-list (posix:resolve-host-ipaddr name)))
    (condition (condition) (handle-condition condition))))
  
