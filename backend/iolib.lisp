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
	   (socket (apply #'iolib/sockets:make-socket
			  `(:type ,protocol
			    :address-family :internet
			    :ipv6 ,ipv6
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
  (with-mapped-conditions ()
    (case direction
      (:input
       (iolib/sockets:shutdown (socket usocket) :read t))
      (:output
       (iolib/sockets:shutdown (socket usocket) :write t))
      (t ; :io by default
       (iolib/sockets:shutdown (socket usocket) :read t :write t)))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (with-mapped-conditions ()
    (make-stream-server-socket
      (iolib/sockets:make-socket :connect :passive
				 :address-family :internet
				 :local-host host
				 :local-port port
				 :backlog backlog
				 :reuse-address (or reuse-address reuseaddress)))))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (with-mapped-conditions (usocket)
    (let ((socket (iolib/sockets:accept-connection (socket usocket))))
      (make-stream-socket :socket socket :stream socket))))

(defmethod get-local-address ((usocket usocket))
  (iolib/sockets:local-host (socket usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (iolib/sockets:remote-host (socket usocket)))

(defmethod get-local-port ((usocket usocket))
  (iolib/sockets:local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (iolib/sockets:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (iolib/sockets:send-to (socket usocket) buffer
			 :start offset :end (+ offset size)
			 :remote-host host :remote-port port))

;; TODO: check the return values structure
(defmethod socket-receive ((usocket datagram-usocket) buffer length &key start end)
  (iolib/sockets:receive-from (socket usocket)
			      :buffer buffer :size length :start start :end end))

(defun get-hosts-by-name (name)
  (iolib/sockets:lookup-hostname name))

(defvar *default-event-base* nil)

(defun %setup-wait-list (wait-list)
  (setf (wait-list-%wait wait-list)
	(or *default-event-base*
	    ;; iolib/multiplex:*default-multiplexer* is used here
	    (make-instance 'iolib/multiplex:event-base))))

(defun make-usocket-read-handler (usocket disconnector)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (handler-case
	(if (eq (state usocket) :write)
	    (setf (state usocket) :read-write)
	  (setf (state usocket) :read))
      (end-of-file ()
	(funcall disconnector :close)))))

(defun make-usocket-write-handler (usocket disconnector)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (handler-case
	(if (eq (state usocket) :read)
	    (setf (state usocket) :read-write)
	  (setf (state usocket) :write))
      (end-of-file ()
	(funcall disconnector :close))
      (iolib/streams:hangup ()
	(funcall disconnector :close)))))

(defun make-usocket-error-handler (usocket disconnector)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (handler-case
	(setf (state usocket) nil)
      (end-of-file ()
	(funcall disconnector :close))
      (iolib/streams:hangup ()
	(funcall disconnector :close)))))

(defun make-usocket-disconnector (event-base usocket)
  (lambda (&rest events)
    (let* ((socket (socket usocket))
	   (fd (iolib/sockets:socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
	  (iolib/multiplex:remove-fd-handlers event-base fd :read t :write t :error t)
	(progn
	  (when (member :read events)
	    (iolib/multiplex:remove-fd-handlers event-base fd :read t))
	  (when (member :write events)
	    (iolib/multiplex:remove-fd-handlers event-base fd :write t))
	  (when (member :error events)
	    (iolib/multiplex:remove-fd-handlers event-base fd :error t))))
      ;; and finally if were asked to close the socket, we do so here
      (when (member :close events)
	(close socket :abort t)))))

(defun %add-waiter (wait-list waiter)
  (let ((event-base (wait-list-%wait wait-list)))
    ;; reset socket state
    (setf (state waiter) nil)
    ;; set I/O handlers
    (iolib/multiplex:set-io-handler
      event-base
      (iolib/sockets:socket-os-fd (socket waiter))
      :read
      (make-usocket-read-handler waiter
				 (make-usocket-disconnector event-base waiter)))
    (iolib/multiplex:set-io-handler
      event-base
      (iolib/sockets:socket-os-fd (socket waiter))
      :write
      (make-usocket-write-handler waiter
				  (make-usocket-disconnector event-base waiter)))
    ;; set error handler
    (iolib/multiplex:set-error-handler
      event-base
      (iolib/sockets:socket-os-fd (socket waiter))
      (make-usocket-error-handler waiter
				  (make-usocket-disconnector event-base waiter)))))

(defun %remove-waiter (wait-list waiter)
  (let ((event-base (wait-list-%wait wait-list)))
    (iolib/multiplex:remove-fd-handlers event-base
					(iolib/sockets:socket-os-fd (socket waiter))
					:read t
					:write t
					:error t)))

;; NOTE: `wait-list-waiters` returns all usockets
(defun wait-for-input-internal (wait-list &key timeout)
  (let ((event-base (wait-list-%wait wait-list)))
    (handler-case
	(iolib/multiplex:event-dispatch event-base
					:timeout timeout)
      (iolib/streams:hangup ())
      (end-of-file ()))
    ;; close the event-base after use
    (unless (eq event-base *default-event-base*)
      (close event-base))))
