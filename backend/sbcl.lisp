;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

;; There's no way to preload the sockets library other than by requiring it
;;
;; ECL sockets has been forked off sb-bsd-sockets and implements the
;; same interface. We use the same file for now.
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sockets))

(defun map-socket-error (sock-err)
  (map-errno-error (sb-bsd-sockets::socket-error-errno sock-err)))

(defparameter +sbcl-condition-map+
  '((interrupted-error . interrupted-condition)))

(defparameter +sbcl-error-map+
  `((sb-bsd-sockets:address-in-use-error . address-in-use-error)
    (sb-bsd-sockets::no-address-error . address-not-available-error)
    (sb-bsd-sockets:bad-file-descriptor-error . bad-file-descriptor-error)
    (sb-bsd-sockets:connection-refused-error . connection-refused-error)
    (sb-bsd-sockets:invalid-argument-error . invalid-argument-error)
    (sb-bsd-sockets:no-buffers-error . no-buffers-error)
    (sb-bsd-sockets:operation-not-supported-error
     . operation-not-supported-error)
    (sb-bsd-sockets:operation-not-permitted-error
     . operation-not-permitted-error)
    (sb-bsd-sockets:protocol-not-supported-error
     . protocol-not-supported-error)
    (sb-bsd-sockets:socket-type-not-supported-error
     . socket-type-not-supported-error)
    (sb-bsd-sockets:network-unreachable-error . network-unreachable-error)
    (sb-bsd-sockets:operation-timeout-error . timeout-error)
    (sb-bsd-sockets:socket-error . ,#'map-socket-error)
    ;; Nameservice errors: mapped to unknown-error
;;    (sb-bsd-sockets:no-recovery-error . network-reset-error)
;;    (sb-bsd-sockets:try-again-condition ...)
;;    (sb-bsd-sockets:host-not-found ...)
    ))

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
               (error 'unknown-error
                      :socket socket
                      :real-error condition))))
    (condition (let* ((usock-cond (cdr (assoc (type-of condition)
                                              +sbcl-condition-map+)))
                      (usock-cond (if (functionp usock-cond)
                                      (funcall usock-cond condition)
                                    usock-cond)))
                 (if usock-cond
                     (signal usock-cond :socket socket)
                   (signal 'unknown-condition
                           :real-condition condition))))))


(defun socket-connect (host port &key (element-type 'character))
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp))
         (stream (sb-bsd-sockets:socket-make-stream socket
                                                    :input t
                                                    :output t
                                                    :buffering :full
                                                    :element-type element-type))
         ;;###FIXME: The above line probably needs an :external-format
         (usocket (make-stream-socket :stream stream :socket socket))
         (ip (host-to-vector-quad host)))
    (with-mapped-conditions (usocket)
      (sb-bsd-sockets:socket-connect socket ip port))
    usocket))

(defun socket-listen (host port
                           &key reuseaddress
                           (backlog 5)
                           (element-type 'character))
  (let* ((ip (host-to-vector-quad host))
         (sock (make-instance 'sb-bsd-sockets:inet-socket
                              :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) reuseaddress)
    (sb-bsd-sockets:socket-bind sock ip port)
    (sb-bsd-sockets:socket-listen sock backlog)
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (let ((sock (sb-bsd-sockets:socket-accept (socket socket))))
    (make-stream-socket :socket sock
                        :stream (sb-bsd-sockets:socket-make-stream sock
                                 :input t :output t :buffering :full
                                 :element-type (or element-type
                                                   (element-type socket))))))

;; Sockets and their associated streams are modelled as
;; different objects. Be sure to close the stream (which
;; closes the socket too) when closing a stream-socket.
(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (sb-bsd-sockets:socket-close (socket usocket))))

(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod get-local-name ((usocket usocket))
  (sb-bsd-sockets:socket-name (socket usocket)))

(defmethod get-peer-name ((usocket usocket))
  (sb-bsd-sockets:socket-peername (socket usocket)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket usocket))
  (nth-value 1 (get-peer-name usocket)))


(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (sb-bsd-sockets::host-ent-name
        (sb-bsd-sockets:get-host-by-address address))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (sb-bsd-sockets::host-ent-addresses
         (sb-bsd-sockets:get-host-by-name name))))

