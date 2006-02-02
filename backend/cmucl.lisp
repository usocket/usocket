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

(defun socket-connect (host port &optional (type :stream))
  (let* ((socket (ext:connect-to-inet-socket (host-byte-order host) port type))
         (stream (sys:make-fd-stream socket :input t :output t
                                     :element-type 'character
                                     :buffering :full))
         ;;###FIXME the above line probably needs an :external-format
         (usocket (make-socket :socket socket
                               :host host :port port :stream stream)))
    usocket))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (ext:close-socket (socket usocket)))



(defun get-host-by-address (address)
  (handler-case (ext:host-entry-name
                 (ext::lookup-host-entry (host-byte-order address)))
    (condition (condition) (handle-condition condition))))

(defun get-host-by-name (name)
  (handler-case (mapcar #'hbo-to-vector-quad
                        (ext:host-entry-addr-list
                         (ext:lookup-host-entry name)))
    (condition (condition) (handle-condition condition))))

