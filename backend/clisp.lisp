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
     #+(or rawsock ffi)
     (socket-create-datagram (or local-port *auto-port*)
			     :local-host (or local-host *wildcard-host*)
			     :remote-host host
			     :remote-port port)
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
    (declare (values (simple-array (unsigned-byte 8) (*)) ; buffer
		     (integer 0)                          ; size
		     (unsigned-byte 32)                   ; host
		     (unsigned-byte 16)))                 ; port
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
		(buffer (ffi:c-ptr ffi:uint8) :in-out)
		(length ffi:int)
		(flags ffi:int)
		(address (ffi:c-ptr sockaddr) :in-out)
		(address-len (ffi:c-ptr ffi:int) :in-out))
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

  (ffi:def-call-out %connect (:name "connect")
    (:arguments (socket ffi:int)
		(address (ffi:c-ptr sockaddr) :in)
		(address_len socklen_t))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %bind (:name "bind")
    (:arguments (socket ffi:int)
		(address (ffi:c-ptr sockaddr) :in)
		(address_len socklen_t))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:int))

  (ffi:def-call-out %close (:name #-win32 "close" #+win32 "closesocket")
    (:arguments (socket ffi:int))
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

  (ffi:def-call-out %htonl (:name "htonl")
    (:arguments (hostlong ffi:uint32))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint32))

  (ffi:def-call-out %htons (:name "htons")
    (:arguments (hostshort ffi:uint16))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint16))

  (ffi:def-call-out %ntohl (:name "ntohl")
    (:arguments (netlong ffi:uint32))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint32))

  (ffi:def-call-out %ntohs (:name "ntohs")
    (:arguments (netshort ffi:uint16))
    #+win32 (:library "WS2_32")
    #-win32 (:library :default)
    (:language #-win32 :stdc
	       #+win32 :stdc-stdcall)
    (:return-type ffi:uint16))

  ;; socket constants
  (defconstant +socket-af-inet+ 2)
  (defconstant +socket-sock-dgram+ 2)
  (defconstant +sockopt-so-rcvtimeo+ #-linux #x1006 #+linux 20 "Socket receive timeout")

  (defvar *length-of-sockaddr_in* (ffi:sizeof 'sockaddr_in))

  (declaim (inline fill-sockaddr_in))
  (defun fill-sockaddr_in (sockaddr host port)
    (let ((hbo (host-to-hbo #(127 0 0 1))))
      #+ignore
      (setf (ffi:slot (ffi:foreign-value sockaddr) 'sin_len) *length-of-sockaddr_in*
	    (ffi:slot (ffi:foreign-value sockaddr) 'sin_family) +socket-af-inet+
	    (ffi:slot (ffi:foreign-value sockaddr) 'sin_port) (%htons port)
	    (ffi:slot (ffi:foreign-value sockaddr) 'sin_addr) (%htonl hbo))
      (ffi:with-c-place (place sockaddr)
	(setf (ffi:slot place 'sin_len) *length-of-sockaddr_in*
	      (ffi:slot place 'sin_family) +socket-af-inet+
	      (ffi:slot place 'sin_port) (%htons port)
	      (ffi:slot place 'sin_addr) (%htonl hbo)))
      sockaddr))

  (defun socket-create-datagram (local-port
				 &key (local-host *wildcard-host*)
				      remote-host
				      remote-port)
    (let ((sock (%socket +socket-af-inet+ +socket-sock-dgram+ 0))
	  (lsock_addr (fill-sockaddr_in (ffi:allocate-shallow 'sockaddr_in)
					local-host local-port))
	  (rsock_addr (when remote-host
			(fill-sockaddr_in (ffi:allocate-shallow 'sockaddr_in)
					  remote-host (or remote-port local-port)))))
      (unwind-protect
	   (progn
	     (%bind sock (ffi:cast (ffi:foreign-value lsock_addr) 'sockaddr)
		    *length-of-sockaddr_in*)
	     (when rsock_addr
	       (%connect sock
			 (ffi:cast (ffi:foreign-value rsock_addr) 'sockaddr)
			 *length-of-sockaddr_in*)))
	(ffi:foreign-free lsock_addr)
	(when remote-host
	  (ffi:foreign-free rsock_addr)))
      (make-datagram-socket sock :connected-p (if rsock_addr t nil))))

  (defmethod initialize-instance :after ((usocket datagram-usocket) &key)
    (with-slots (send-buffer recv-buffer) usocket
      (setf send-buffer (ffi:allocate-shallow 'ffi:uint8 :count +max-datagram-packet-size+)
	    recv-buffer (ffi:allocate-shallow 'ffi:uint8 :count +max-datagram-packet-size+))))

  (defmethod socket-close ((usocket datagram-usocket))
    (when (wait-list usocket)
      (remove-waiter (wait-list usocket) usocket))
    (with-slots (send-buffer recv-buffer socket) usocket
      (ffi:foreign-free send-buffer)
      (ffi:foreign-free recv-buffer)
      (zerop (%close socket))))

  (defmethod socket-receive ((usocket datagram-usocket) buffer length &key)
    (declare (values (simple-array (unsigned-byte 8) (*)) ; buffer
		     (integer 0)                          ; size
		     (unsigned-byte 32)                   ; host
		     (unsigned-byte 16)))                 ; port
    (let ((remote-address (ffi:allocate-shallow 'sockaddr_in))
	  (remote-address-length (ffi:allocate-shallow 'ffi:int))
	  nbytes)
      (unwind-protect
	   (with-slots (recv-buffer) usocket
	     (multiple-value-bind (n buffer address address-len)
		 (%recvfrom (socket usocket)
			    recv-buffer
			    +max-datagram-packet-size+
			    0 ; flags
			    remote-address
			    remote-address-length)
	       (setq nbytes n)
	       (cond ((plusp n)
		      (if buffer ; replace exist buffer of create new return buffer
			  (replace buffer (ffi:foreign-value recv-buffer)
				   :end1 (min length +max-datagram-packet-size+)
				   :end2 (min n +max-datagram-packet-size+))
			  (setq buffer (subseq (ffi:foreign-value recv-buffer)
					       0 (min n +max-datagram-packet-size+)))))
		     ((zerop n)) ; do nothing
		     (t)))) ; TODO: handle error here.
	(ffi:foreign-free remote-address)
	(ffi:foreign-free remote-address-length))
  (values buffer nbytes 0 0))) ; TODO: remote-host and remote-port needed

  (defmethod socket-send ((socket datagram-usocket) buffer length &key host port)
    )
) ; progn
