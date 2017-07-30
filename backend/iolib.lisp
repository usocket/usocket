;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:load-toplevel :execute)
  (setq *backend* :iolib))

(defparameter +iolib-error-map+
  '((:address-in-use . address-in-use-error)
    (:address-not-available . address-not-available-error)
    (:network-down . network-down-error)
    (:network-reset . network-reset-error)
    (:network-unreachable . network-unreachable-error)
    (:connection-aborted . connection-aborted-error)
    (:connection-reset . connection-reset-error)
    (:no-buffer-space . no-buffers-error)
    (:shutdown . shutdown-error)
    (:connection-timed-out . timeout-error)
    (:connection-refused . connection-refused-error)
    (:host-down . host-down-error)
    (:host-unreachable . host-unreachable-error)))

(defun handle-condition (condition &optional (socket nil))
  )

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline
                       (nodelay t) ;; nodelay == t is the ACL default
                       local-host local-port)
  (with-mapped-conditions ()
    (let* ((remote (when (and host port)
		     (car (get-hosts-by-name (host-to-hostname host)))))
	   (local  (when (and local-host local-port)
		     (car (get-hosts-by-name (host-to-hostname local-host)))))
	   (ipv6 (or (and remote (= 16 (length remote)))
		     (and local (= 16 (length local)))))
	   (socket (apply #'iolib:make-socket
			  `(:type ,protocol
			    :address-family :internet
			    :ipv6 ipv6
			    :connect ,(if (eq protocol :stream) :active
					(if (and host port) :active
					  :passive))
			    ,@(when remote
				`(:remote-host remove :remote-port port))
			    ,@(when local
				`(:local-host local-host :local-port local-port))
			    :nodelay nodelay))))
      (ecase protocol
	(:stream
	 (make-stream-socket :stream socket :socket socket))
	(:datagram
	 (make-datagram-socket socket :connected-p (and remote t)))))))

(defmethod socket-close ((usocket usocket))
  (close (socket usocket)))

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  (apply #'iolib:shutdown
	 `(,(socket usocket) ,@(case direction
				 (:input '(:read t))
				 (:output '(:write t))
				 (:io '(:read t :write t))))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (with-mapped-conditions ()
    (make-stream-server-socket
      (iolib:make-socket :connect :passive
			 :address-family :internet
			 :local-host host
			 :local-port port
			 :backlog backlog
			 :reuse-address (or reuse-address reuseaddress)))))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (with-mapped-condtitions (usocket)
    (let ((socket (iolib:accept-connection (socket usocket))))
      (make-stream-socket :socket socket :stream socket))))

(defmethod get-local-address ((usocket usocket))
  (iolib:local-host (socket usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (iolib:remote-host (socket usocket)))

(defmethod get-local-port ((usocket usocket))
  (iolib:local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (iolib:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (iolib:send-to (socket usocket) buffer :start offset :end (+ offset size)
		 :remote-host host :remote-port port))

(defmethod socket-receive ((usocket datagram-usocket) buffer length &key start end)
  (iolib:receive-from (socket usocket) :buffer buffer :size length :start start :end end))

(defun get-hosts-by-name (name)
  (iolib:lookup-hostname name))

(defun %setup-wait-list (wait-list)
  )

(defun %add-waiter (wait-list waiter)
  )

(defun %remove-waiter (wait-list waiter)
  )

(defun wait-for-input-internal (wait-list &key timeout)
  )
