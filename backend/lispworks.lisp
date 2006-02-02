;;;; $Id$
;;;; $URL$

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
  (make-socket :socket (comm:open-tcp-stream host port)
               :host host
               :port port))
  
(defmethod close ((socket socket))
  "Close socket."
  (cl:close (real-socket socket)))

(defmethod read-line ((socket socket))
  (cl:read-line (real-socket socket)))

(defmethod write-sequence ((socket socket) sequence)
  (cl:write-sequence sequence (real-socket socket)))

(defun get-host-by-address (address)
  (comm:get-host-entry (vector-quad-to-dotted-quad address)
                       :fields '(:name)))

(defun get-host-by-name (name)
  (mapcar #'hbo-to-vector-quad 
          (comm:get-host-entry name :fields '(:addresses))))
