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
  (let ((socket (sock:make-socket :remote-host host
                                  :remote-port port)))
    (make-socket :socket socket :stream socket)))

(defmethod close ((usocket usocket))
  "Close socket."
  (sock:close (socket usocket)))



(defun get-host-by-address (address)
  (sock:lookup-host address))

(defun get-hosts-by-name (name)
  (sock:lookup-host name))
