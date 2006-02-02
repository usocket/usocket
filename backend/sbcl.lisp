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
  "Connect to `host' on `port'.  `host' is assumed to be a string of
an IP address represented in vector notation, such as #(192 168 1 1).
`port' is assumed to be an integer.

Returns a usocket object."
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type type :protocol :tcp))
         (stream (sb-bsd-sockets:socket-make-stream socket
                                                    :input t
                                                    :output t
                                                    :buffering :full
                                                    :element-type 'character))
         ;;###FIXME: The above line probably needs an :external-format
         (usocket (make-instance 'usocket :stream stream :socket socket)))
    (handler-case (sb-bsd-sockets:socket-connect socket host port)
      (condition (condition) (handle-condition condition usocket)))
    usocket))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (handler-case (sb-bsd-sockets:socket-close (socket usocket))
    (condition (condition) (handle-condition condition usocket))))



(defun get-host-by-address (address)
  (handler-case (sb-bsd-sockets::host-ent-name
                 (sb-bsd-sockets:get-host-by-address address))
    (condition (condition) (handle-condition condition))))

(defun get-hosts-by-name (name)
  (handler-case (sb-bsd-sockets::host-ent-addresses
                 (sb-bsd-sockets:get-host-by-name name))
    (condition (condition) (handle-condition condition))))

