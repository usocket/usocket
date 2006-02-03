;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun map-socket-error (sock-err)
  (map-errno-error (sb-bsd-sockets::socket-error-errno sock-err)))

(defparameter +sbcl-condition-map+
  '((interrupted-error . usocket-interrupted-condition)))

(defparameter +sbcl-error-map+
  `((sb-bsd-sockets:address-in-use-error . usocket-address-in-use-error)
    (sb-bsd-sockets::no-address-error . usocket-address-not-available-error)
    (sb-bsd-sockets:bad-file-descriptor-error . usocket-bad-file-descriptor-error)
    (sb-bsd-sockets:connection-refused-error . usocket-connection-refused-error)
    (sb-bsd-sockets:invalid-argument-error . usocket-invalid-argument-error)
    (no-buffers-error . usocket-no-buffers-error)
    (operation-not-supported-error . usocket-operation-not-supported-error)
    (operation-not-permitted-error . usocket-operation-not-permitted-error)
    (protocol-not-supported-error . usocket-protocol-not-supported-error)
    (socket-type-not-supported-error . usocket-socket-type-not-supported-error)
    (network-unreachable-error . usocket-network-unreachable-error)
    ;;    (... . usocket-network-down-error)
    (no-recovery-error . usocket-network-reset-error)
    ;;    (... . usocket-host-down-error)
    ;;    (... . usocket-host-unreachable-error)
    ;;    (... . usocket-shutdown-error)
    (operation-timeout-error . usocket-timeout-error)
    (sb-bsd-sockets:socket-error . ,#'map-socket-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (error (let* ((usock-error (cdr (assoc (type-of condition)
                                           +sbcl-error-map+)))
                  (usock-error (if (functionp usock-error)
                                   (funcall usock-error condition)
                                 usock-error)))
             (if usock-error
                 (error usock-error :socket socket)
               (error 'usocket-unknown-error
                      :socket socket
                      :real-error condition))))
    (condition (let* ((usock-cond (cdr (assoc (type-of condition)
                                              +sbcl-condition-map+)))
                      (usock-cond (if (functionp usock-cond)
                                      (funcall usock-cond condition)
                                    usock-cond)))
                 (if usock-cond
                     (signal usock-cond :socket socket)
                   (signal 'usocket-unkown-condition
                           :real-condition condition))))))


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
         (usocket (make-instance 'usocket :stream stream :socket socket))
         (ip (host-to-vector-quad host)))
    (with-mapped-conditions (usocket)
      (sb-bsd-sockets:socket-connect socket ip port))
    usocket))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (sb-bsd-sockets:socket-close (socket usocket))))



(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (sb-bsd-sockets::host-ent-name
        (sb-bsd-sockets:get-host-by-address address))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (sb-bsd-sockets::host-ent-addresses
         (sb-bsd-sockets:get-host-by-name name))))

