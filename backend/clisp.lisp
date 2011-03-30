;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-ffi
  (warn "This image doesn't contain FFI package, GET-HOST-NAME won't work.")
  #-(or ffi rawsock)
  (warn "This image doesn't contain either FFI or RAWSOCK package, no UDP support."))

;; utility routine for looking up the current host name
#+ffi
(ffi:def-call-out get-host-name-internal
         (:name "gethostname")
         (:arguments (name (FFI:C-PTR (FFI:C-ARRAY-MAX ffi:character 256))
                           :OUT :ALLOCA)
                     (len ffi:int))
         #+win32 (:library "WS2_32")
	 #-win32 (:library :default)
         (:language #-win32 :stdc
                    #+win32 :stdc-stdcall)
         (:return-type ffi:int))

(defun get-host-name ()
  #+ffi
  (multiple-value-bind (retcode name)
      (get-host-name-internal 256)
    (when (= retcode 0)
      name))
  #-ffi
  "localhost")

(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (let ((hostent (posix:resolve-host-ipaddr (host-to-hostname address))))
      (posix:hostent-name hostent))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
    (let ((hostent (posix:resolve-host-ipaddr name)))
      (mapcar #'host-to-vector-quad
              (posix:hostent-addr-list hostent)))))

#+win32
(defun remap-maybe-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +clisp-error-map+
  #+win32
  (append (remap-maybe-for-win32 +unix-errno-condition-map+)
          (remap-maybe-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (system::simple-os-error
       (let ((usock-err
              (cdr (assoc (car (simple-condition-format-arguments condition))
                          +clisp-error-map+ :test #'member))))
         (when usock-err ;; don't claim the error if we don't know
	   ;; it's actually a socket error ...
             (if (subtypep usock-err 'error)
                 (error usock-err :socket socket)
               (signal usock-err :socket socket)))))))

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
                       local-host local-port)
  (declare (ignore nodelay))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when nodelay-specified (unsupported 'nodelay 'socket-connect))
  (case protocol
    (:stream
     (let ((socket)
	   (hostname (host-to-hostname host)))
       (with-mapped-conditions (socket)
	 (setf socket
	       (if timeout
		   (socket:socket-connect port hostname
					  :element-type element-type
					  :buffered t
					  :timeout timeout)
		   (socket:socket-connect port hostname
					  :element-type element-type
					  :buffered t))))
       (make-stream-socket :socket socket
			   :stream socket))) ;; the socket is a stream too
    (:datagram
     #+rawsock
     (socket-create-datagram (or local-port *auto-port*)
			     :local-host (or local-host *wildcard-host*)
			     :remote-host host
			     :remote-port port)
     #+(and ffi (not rawsock))
     ()
     #-(or rawsock ffi)
     (unsupported '(protocol :datagram) 'socket-connect))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  ;; clisp 2.39 sets SO_REUSEADDRESS to 1 by default; no need to
  ;; to explicitly turn it on; unfortunately, there's no way to turn it off...
  (declare (ignore reuseaddress reuse-address reuse-address-supplied-p))
  (let ((sock (apply #'socket:socket-server
                     (append (list port
                                   :backlog backlog)
                             (when (ip/= host *wildcard-host*)
                               (list :interface host))))))
    (with-mapped-conditions ()
        (make-stream-server-socket sock :element-type element-type))))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (let ((stream
         (with-mapped-conditions (socket)
           (socket:socket-accept (socket socket)
                                 :element-type (or element-type
                                                   (element-type socket))))))
    (make-stream-socket :socket stream
                        :stream stream)))

;; Only one close method required:
;; sockets and their associated streams
;; are the same object
(defmethod socket-close ((usocket usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod socket-close ((usocket stream-server-usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (socket:socket-server-close (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (socket:socket-stream-local (socket usocket) t)
    (values (dotted-quad-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (socket:socket-stream-peer (socket usocket) t)
    (values (dotted-quad-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (push (cons (socket waiter) NIL) (wait-list-%wait wait-list)))

(defun %remove-waiter (wait-list waiter)
  (setf (wait-list-%wait wait-list)
        (remove (socket waiter) (wait-list-%wait wait-list) :key #'car)))

(defmethod wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (multiple-value-bind
        (secs musecs)
        (split-timeout (or timeout 1))
      (dolist (x (wait-list-%wait wait-list))
        (setf (cdr x) :INPUT))
      (let* ((request-list (wait-list-%wait wait-list))
             (status-list (if timeout
                              (socket:socket-status request-list secs musecs)
                            (socket:socket-status request-list)))
             (sockets (wait-list-waiters wait-list)))
        (do* ((x (pop sockets) (pop sockets))
              (y (pop status-list) (pop status-list)))
             ((null x))
          (when (eq y :INPUT)
            (setf (state x) :READ)))
        wait-list))))

;;;
;;; UDP/Datagram sockets (RAWSOCK version)
;;;

#+rawsock
(progn
  (defun make-sockaddr_in ()
    (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))

  (declaim (inline fill-sockaddr_in))
  (defun fill-sockaddr_in (sockaddr_in ip port)
    (port-to-octet-buffer port sockaddr_in)
    (ip-to-octet-buffer ip sockaddr_in :start 2)
    sockaddr_in)

  (defun socket-create-datagram (local-port
                                 &key (local-host *wildcard-host*)
                                      remote-host
                                      remote-port)
    (let ((sock (rawsock:socket :inet :dgram 0))
          (lsock_addr (fill-sockaddr_in (make-sockaddr_in)
                                        local-host local-port))
          (rsock_addr (when remote-host
                        (fill-sockaddr_in (make-sockaddr_in)
                                          remote-host (or remote-port
                                                          local-port)))))
      (rawsock:bind sock (rawsock:make-sockaddr :inet lsock_addr))
      (when rsock_addr
        (rawsock:connect sock (rawsock:make-sockaddr :inet rsock_addr)))
      (make-datagram-socket sock :connected-p (if rsock_addr t nil))))

  (defmethod socket-receive ((socket datagram-usocket) buffer length &key)
    "Returns the buffer, the number of octets copied into the buffer (received)
and the address of the sender as values."
    (let* ((sock (socket socket))
           (sockaddr (unless (connected-p socket)
                       (rawsock:make-sockaddr :inet)))
           (rv (if sockaddr
                   (rawsock:recvfrom sock buffer sockaddr :start 0 :end length)
                   (rawsock:recv sock buffer :start 0 :end length)))
           (host 0) (port 0))
      (unless (connected-p socket)
        (let ((data (rawsock:sockaddr-data sockaddr)))
          (setq host (ip-from-octet-buffer data :start 4)
                port (port-from-octet-buffer data :start 2))))
      (values buffer rv host port)))

  (defmethod socket-send ((socket datagram-usocket) buffer length &key host port)
    "Returns the number of octets sent."
    (let* ((sock (socket socket))
           (sockaddr (when (and host port)
                       (rawsock:make-sockaddr :inet
                                              (fill-sockaddr_in
                                               (make-sockaddr_in)
                                               (host-byte-order host)
                                               port))))
           (rv (if (and host port)
                   (rawsock:sendto sock buffer sockaddr
                                   :start 0
                                   :end length)
                   (rawsock:send sock buffer
                                 :start 0
                                 :end length))))
      rv))

  (defmethod socket-close ((usocket datagram-usocket))
    (when (wait-list usocket)
       (remove-waiter (wait-list usocket) usocket))
    (rawsock:sock-close (socket usocket)))
) ; progn

;;;
;;; UDP/Datagram sockets (FFI version)
;;;

#+(and ffi (not rawsock))
(progn
  ;; C primitive types
  (ffi:def-c-type size_t)
  (ffi:def-c-type in_addr_t   ffi:uint32)
  (ffi:def-c-type in_port_t   ffi:uint16)
  (ffi:def-c-type sa_family_t ffi:uint8)
  (ffi:def-c-type socklen_t   ffi:uint32)

  ;; C structures
  (ffi:def-c-struct sockaddr
    (sa_len     ffi:uint8)
    (sa_family  sa_family_t)
    (sa_data    (ffi:c-array ffi:char 14)))

  #+ignore
  (ffi:def-c-struct in_addr
    (s_addr     in_addr_t))

  (ffi:def-c-struct sockaddr_in
    (sin_len    ffi:uint8)
    (sin_family sa_family_t)
    (sin_port   in_port_t)
    (sin_addr   in_addr_t) ; should be struct in_addr
    (sin_zero   (ffi:c-array ffi:char 8)))

  (ffi:def-c-struct timeval
    (tv_sec     ffi:long)
    (tv_usec    ffi:long))

  ;; foreign functions
  (ffi:def-call-out %sendto (:name "sendto")
    (:arguments (socket ffi:int)
		(buffer (ffi:c-ptr ffi:uint8))
		(length ffi:int)
		(flags ffi:int)
		(address (ffi:c-ptr sockaddr))
		(address-len ffi:int))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %recvfrom (:name "recvfrom")
    (:arguments (socket ffi:int)
		(buffer (ffi:c-ptr ffi:uint8) :out)
		(length ffi:int)
		(flags ffi:int)
		(address (ffi:c-ptr sockaddr) :out)
		(address-len (ffi:c-ptr ffi:int) :out))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %socket (:name "socket")
    (:arguments (family ffi:int)
		(type ffi:int)
		(protocol ffi:int))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %getsockopt (:name "getsockopt")
    (:arguments (sockfd ffi:int)
		(level ffi:int)
		(optname ffi:int)
		(optval ffi:c-pointer)
		(optlen (ffi:c-ptr socklen_t) :out))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %setsockopt (:name "setsockopt")
    (:arguments (sockfd ffi:int)
		(level ffi:int)
		(optname ffi:int)
		(optval ffi:c-pointer)
		(optlen socklen_t))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  ;; socket constants
  (defconstant +socket-af-inet+ 2)
  (defconstant +socket-pf-unspec+ 0)
  (defconstant +socket-sock-dgram+ 2)
  (defconstant +sockopt-so-rcvtimeo+ #-linux #x1006 #+linux 20 "Socket receive timeout")

  (defun open-udp-socket (&key local-address local-port read-timeout)
    "Open a unconnected UDP socket. For binding on address ANY(*), just not set LOCAL-ADDRESS (NIL),
for binding on random free unused port, set LOCAL-PORT to 0."
    (let ((socket-fd (%socket +socket-af-inet+ +socket-sock-dgram+ +socket-pf-unspec+)))
      (if socket-fd
	  (progn
	    )
	  (error "cannot create socket"))))
) ; progn
