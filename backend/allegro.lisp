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
  (make-socket :socket (sock:make-socket :remote-host host
                                         :remote-port port)))

(defmethod close ((socket socket))
  "Close socket."
  (sock:close (real-socket socket)))

(defmethod read-line ((socket socket))
  (cl:read-line (real-socket socket)))

(defmethod write-sequence ((socket socket) sequence)
  (cl:write-sequence sequence (real-socket socket)))

(defun get-host-by-address (address)
  (sock:lookup-host address))

(defun get-host-by-name (name)
  (sock:lookup-host name))
