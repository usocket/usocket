;;;; dotcl (Common Lisp on .NET) networking support, on dotcl's DOTCL-BSD-SOCKETS
;;;; module, which plays the part sb-bsd-sockets plays for the SBCL backend.

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defparameter +dotcl-error-map+
  '((dotcl-bsd-sockets:interrupted-error . interrupted-condition)
    (dotcl-bsd-sockets:bad-file-descriptor-error . bad-file-descriptor-error)
    (dotcl-bsd-sockets:operation-not-permitted-error . operation-not-permitted-error)
    (dotcl-bsd-sockets:invalid-argument-error . invalid-argument-error)
    (dotcl-bsd-sockets:protocol-not-supported-error . protocol-not-supported-error)
    (dotcl-bsd-sockets:socket-type-not-supported-error . socket-type-not-supported-error)
    (dotcl-bsd-sockets:operation-not-supported-error . operation-not-supported-error)
    (dotcl-bsd-sockets:address-in-use-error . address-in-use-error)
    (dotcl-bsd-sockets:address-not-available-error . address-not-available-error)
    (dotcl-bsd-sockets:network-down-error . network-down-error)
    (dotcl-bsd-sockets:network-unreachable-error . network-unreachable-error)
    (dotcl-bsd-sockets:network-reset-error . network-reset-error)
    (dotcl-bsd-sockets:connection-aborted-error . connection-aborted-error)
    (dotcl-bsd-sockets:connection-reset-error . connection-reset-error)
    (dotcl-bsd-sockets:no-buffers-error . no-buffers-error)
    (dotcl-bsd-sockets:already-shutdown-error . already-shutdown-error)
    (dotcl-bsd-sockets:operation-timeout-error . timeout-error)
    (dotcl-bsd-sockets:connection-refused-error . connection-refused-error)
    (dotcl-bsd-sockets:host-down-error . host-down-error)
    (dotcl-bsd-sockets:host-unreachable-error . host-unreachable-error)
    (dotcl-bsd-sockets:host-not-found-error . ns-host-not-found-error)
    (dotcl-bsd-sockets:try-again-error . ns-try-again-error)
    (dotcl-bsd-sockets:no-recovery-error . ns-no-recovery-error)))

;; dotcl-bsd-sockets signals its own conditions; a .NET exception raised outside
;; it (a read or write on a socket stream) is classified the same way by
;; SOCKET-ERROR-FROM-EXCEPTION
(defun handle-condition (condition &optional (socket nil) (host-or-ip nil))
  (typecase condition
    ((or socket-condition ns-condition) condition) ; already ours
    (error
     (let ((c (dotcl-bsd-sockets:socket-error-from-exception condition)))
       (when c
         (let ((class (cdr (assoc (type-of c) +dotcl-error-map+))))
           (cond ((null class)
                  (if (typep c 'dotcl-bsd-sockets:name-service-error)
                      (error 'ns-unknown-error :host-or-ip host-or-ip :real-error c)
                      (error 'unknown-error :socket socket :real-error c
                                            :errno (or (dotcl-bsd-sockets:socket-error-code c) 0))))
                 ((subtypep class 'ns-error)
                  (error class :host-or-ip host-or-ip))
                 ((subtypep class 'error)
                  (error class :socket socket))
                 (t (signal class :socket socket)))))))))

(defun socket-connect-internal (host &key port (protocol :stream)
                                          (element-type 'character)
                                          timeout deadline
                                          (nodelay nil nodelay-specified)
                                          local-host local-port)
  (declare (ignore nodelay-specified))
  (when deadline (unsupported 'deadline 'socket-connect))
  (with-mapped-conditions (nil host)
    (ecase protocol
      (:stream
       (let ((sock (dotcl-bsd-sockets:tcp-connect host port
                                               :timeout timeout :nodelay nodelay
                                               :local-host local-host
                                               :local-port local-port)))
         (make-stream-socket :socket sock
                             :stream (dotcl-bsd-sockets:socket-make-stream
                                      sock :element-type element-type))))
      (:datagram
       (make-datagram-socket (dotcl-bsd-sockets:udp-open :host host :port port
                                                      :local-host local-host
                                                      :local-port local-port)
                             :connected-p (and host port t))))))

(defun socket-listen-internal (host &key port reuseaddress
                                         (reuse-address nil reuse-address-supplied-p)
                                         (backlog 5)
                                         (element-type 'character))
  (with-mapped-conditions (nil host)
    (make-stream-server-socket
     (dotcl-bsd-sockets:tcp-listen host port :backlog backlog
                                          :reuse-address (if reuse-address-supplied-p
                                                             reuse-address
                                                             reuseaddress))
     :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (with-mapped-conditions (usocket)
    (let ((sock (dotcl-bsd-sockets:socket-accept (socket usocket))))
      (make-stream-socket :socket sock
                          :stream (dotcl-bsd-sockets:socket-make-stream
                                   sock :element-type (or element-type
                                                          (element-type usocket)))))))

;; closes the socket's stream too
(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (dotcl-bsd-sockets:socket-close (socket usocket))))

(defmethod socket-shutdown ((usocket usocket) direction)
  (with-mapped-conditions (usocket)
    (dotcl-bsd-sockets:socket-shutdown (socket usocket) :direction direction)))

(defmethod get-local-name ((usocket usocket))
  (with-mapped-conditions (usocket)
    (dotcl-bsd-sockets:socket-name (socket usocket))))

(defmethod get-peer-name ((usocket usocket))
  (with-mapped-conditions (usocket)
    (dotcl-bsd-sockets:socket-peername (socket usocket))))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun get-hosts-by-name (name)
  (with-mapped-conditions (nil name)
    (dotcl-bsd-sockets:host-ent-addresses (dotcl-bsd-sockets:get-host-by-name name))))

(defun get-host-by-address (address)
  (with-mapped-conditions (nil address)
    (dotcl-bsd-sockets:host-ent-name (dotcl-bsd-sockets:get-host-by-address address))))

(defun get-host-name ()
  (dotcl-bsd-sockets:get-host-name))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (with-mapped-conditions (usocket host)
    (dotcl-bsd-sockets:socket-send (socket usocket) buffer size
                                :address (and host port (list host port))
                                :start offset)))

;; a datagram longer than LENGTH is truncated, as recvfrom(2) does
(defmethod socket-receive ((usocket datagram-usocket) buffer length
                           &key (element-type '(unsigned-byte 8)))
  (with-mapped-conditions (usocket)
    (dotcl-bsd-sockets:socket-receive (socket usocket) buffer
                                   (or length +max-datagram-packet-size+)
                                   :element-type element-type)))

;;; Wait-list

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (let* ((waiters (wait-list-waiters wait-list))
           (ready (dotcl-bsd-sockets:wait-for-input (mapcar #'socket waiters)
                                                 :timeout timeout)))
      (dolist (w waiters)
        (when (member (socket w) ready)
          (setf (state w) :read)))))
  wait-list)
