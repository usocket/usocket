;;;; $Id$
;;;; $URL$

;;;; New ABCL networking support (replacement to old armedbear.lisp)
;;;; Author: Chun Tian (binghe)

;;;; See LICENSE for licensing information.

(in-package :usocket)

;;; Symbols in JAVA package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *java-package-symbols*
    '(java:jarray-length
      java:jarray-ref
      java:java-exception
      java:java-exception-cause
      java:jconstructor
      java:jcall
      java:jclass
      java:jclass-of
      java:jfield
      java:jmethod
      java:jnew
      java:jstatic
      java:make-immediate-object))
  (import *java-package-symbols*))

;;; Java Classes ($*...)
(defvar $*boolean (jclass "boolean"))
(defvar $*int (jclass "int"))
(defvar $*long (jclass "long"))
(defvar $*DatagramChannel (jclass "java.nio.channels.DatagramChannel"))
(defvar $*DatagramSocket (jclass "java.net.DatagramSocket"))
(defvar $*Inet4Address (jclass "java.net.Inet4Address"))
(defvar $*InetAddress (jclass "java.net.InetAddress"))
(defvar $*InetSocketAddress (jclass "java.net.InetSocketAddress"))
(defvar $*Iterator (jclass "java.util.Iterator"))
(defvar $*SelectableChannel (jclass "java.nio.channels.SelectableChannel"))
(defvar $*SelectionKey (jclass "java.nio.channels.SelectionKey"))
(defvar $*Selector (jclass "java.nio.channels.Selector"))
(defvar $*ServerSocket (jclass "java.net.ServerSocket"))
(defvar $*ServerSocketChannel (jclass "java.nio.channels.ServerSocketChannel"))
(defvar $*Set (jclass "java.util.Set"))
(defvar $*Socket (jclass "java.net.Socket"))
(defvar $*SocketAddress (jclass "java.net.SocketAddress"))
(defvar $*SocketChannel (jclass "java.nio.channels.SocketChannel"))
(defvar $*String (jclass "java.lang.String"))

;;; Java Constructor ($%.../n)
(defvar $%DatagramSocket/0 (jconstructor $*DatagramSocket))
(defvar $%DatagramSocket/1 (jconstructor $*DatagramSocket $*int))
(defvar $%DatagramSocket/2 (jconstructor $*DatagramSocket $*int $*InetAddress))
(defvar $%InetSocketAddress/1 (jconstructor $*InetSocketAddress $*int))
(defvar $%InetSocketAddress/2 (jconstructor $*InetSocketAddress $*InetAddress $*int))
(defvar $%ServerSocket/0 (jconstructor $*ServerSocket))
(defvar $%ServerSocket/1 (jconstructor $*ServerSocket $*int))
(defvar $%ServerSocket/2 (jconstructor $*ServerSocket $*int $*int))
(defvar $%ServerSocket/3 (jconstructor $*ServerSocket $*int $*int $*InetAddress))
(defvar $%Socket/0 (jconstructor $*Socket))
(defvar $%Socket/2 (jconstructor $*Socket $*InetAddress $*int))
(defvar $%Socket/4 (jconstructor $*Socket $*InetAddress $*int $*InetAddress $*int))

;;; Java Methods ($@...[/Class]/n)
(defvar $@accept/0 (jmethod $*ServerSocket "accept"))
(defvar $@bind/DatagramSocket/1 (jmethod $*DatagramSocket "bind" $*SocketAddress))
(defvar $@bind/ServerSocket/1 (jmethod $*ServerSocket "bind" $*SocketAddress))
(defvar $@bind/ServerSocket/2 (jmethod $*ServerSocket "bind" $*SocketAddress $*int))
(defvar $@bind/Socket/1 (jmethod $*Socket "bind" $*SocketAddress))
(defvar $@channel/0 (jmethod $*SelectionKey "channel"))
(defvar $@close/DatagramSocket/0 (jmethod $*DatagramSocket "close"))
(defvar $@close/Selector/0 (jmethod $*Selector "close"))
(defvar $@close/ServerSocket/0 (jmethod $*ServerSocket "close"))
(defvar $@close/Socket/0 (jmethod $*Socket "close"))
(defvar $@configureBlocking/1 (jmethod $*SelectableChannel "configureBlocking" $*boolean))
(defvar $@connect/DatagramChannel/1 (jmethod $*DatagramChannel "connect" $*SocketAddress))
(defvar $@connect/Socket/1 (jmethod $*Socket "connect" $*SocketAddress))
(defvar $@connect/Socket/2 (jmethod $*Socket "connect" $*SocketAddress $*int))
(defvar $@connect/SocketChannel/1 (jmethod $*SocketChannel "connect" $*SocketAddress))
(defvar $@getAddress/0 (jmethod $*Inet4Address "getAddress"))
(defvar $@getAllByName/1 (jmethod $*InetAddress "getAllByName" $*String))
(defvar $@getByName/1 (jmethod $*InetAddress "getByName" $*String))
(defvar $@getChannel/DatagramSocket/0 (jmethod $*DatagramSocket "getChannel"))
(defvar $@getChannel/ServerSocket/0 (jmethod $*ServerSocket "getChannel"))
(defvar $@getChannel/Socket/0 (jmethod $*Socket "getChannel"))
(defvar $@getHostName/0 (jmethod $*InetAddress "getHostName"))
(defvar $@getInetAddress/DatagramSocket/0 (jmethod $*DatagramSocket "getInetAddress"))
(defvar $@getInetAddress/ServerSocket/0 (jmethod $*ServerSocket "getInetAddress"))
(defvar $@getInetAddress/Socket/0 (jmethod $*Socket "getInetAddress"))
(defvar $@getLocalAddress/DatagramSocket/0 (jmethod $*DatagramSocket "getLocalAddress"))
(defvar $@getLocalAddress/Socket/0 (jmethod $*Socket "getLocalAddress"))
(defvar $@getLocalPort/DatagramSocket/0 (jmethod $*DatagramSocket "getLocalPort"))
(defvar $@getLocalPort/ServerSocket/0 (jmethod $*ServerSocket "getLocalPort"))
(defvar $@getLocalPort/Socket/0 (jmethod $*Socket "getLocalPort"))
(defvar $@getPort/DatagramSocket/0 (jmethod $*DatagramSocket "getPort"))
(defvar $@getPort/Socket/0 (jmethod $*Socket "getPort"))
(defvar $@hasNext/0 (jmethod $*Iterator "hasNext"))
(defvar $@iterator/0 (jmethod $*Set "iterator"))
(defvar $@next/0 (jmethod $*Iterator "next"))
(defvar $@open/DatagramChannel/0 (jmethod $*DatagramChannel "open"))
(defvar $@open/Selector/0 (jmethod $*Selector "open"))
(defvar $@open/ServerSocketChannel/0 (jmethod $*ServerSocketChannel "open"))
(defvar $@open/SocketChannel/0 (jmethod $*SocketChannel "open"))
(defvar $@register/2 (jmethod $*SelectableChannel "register" $*Selector $*int))
(defvar $@select/0 (jmethod $*Selector "select"))
(defvar $@select/1 (jmethod $*Selector "select" $*long))
(defvar $@selectedKeys/0 (jmethod $*Selector "selectedKeys"))
(defvar $@setReuseAddress/1 (jmethod $*ServerSocket "setReuseAddress" $*boolean))
(defvar $@setSoTimeout/DatagramSocket/1 (jmethod $*DatagramSocket "setSoTimeout" $*int))
(defvar $@setSoTimeout/Socket/1 (jmethod $*Socket "setSoTimeout" $*int))
(defvar $@setTcpNoDelay/1 (jmethod $*Socket "setTcpNoDelay" $*boolean))
(defvar $@socket/DatagramChannel/0 (jmethod $*DatagramChannel "socket"))
(defvar $@socket/ServerSocketChannel/0 (jmethod $*ServerSocketChannel "socket"))
(defvar $@socket/SocketChannel/0 (jmethod $*SocketChannel "socket"))
(defvar $@validOps/0 (jmethod $*SelectableChannel "validOps"))

;;; Java Field Variables ($+...)
(defvar $+op-accept (jfield $*SelectionKey "OP_ACCEPT"))
(defvar $+op-connect (jfield $*SelectionKey "OP_CONNECT"))
(defvar $+op-read (jfield $*SelectionKey "OP_READ"))
(defvar $+op-write (jfield $*SelectionKey "OP_WRITE"))

(defconstant +java-true+ (make-immediate-object t :boolean))
(defconstant +java-false+ (make-immediate-object nil :boolean))

;;; Wrapper functions (return-type: java-object)
(defun %get-address (address)
  (jcall $@getAddress/0 address))
(defun %get-all-by-name (string) ; return a simple vector
  (jstatic $@getAllByName/1 $*InetAddress string))
(defun %get-by-name (string)
  (jstatic $@getByName/1 $*InetAddress string))

(defun host-to-inet4 (host)
  "USOCKET host formats to Java Inet4Address, used internally."
  (%get-by-name (host-to-hostname host)))

;;; HANDLE-CONTITION

(defun handle-condition (condition &optional (socket nil))
  (typecase condition
    (java-exception
     (let ((java-cause (java-exception-cause condition)))
       (let* ((usock-error (cdr (assoc (jclass-of java-cause) +abcl-error-map+
				       :test #'string=)))
	      (usock-error (if (functionp usock-error)
			       (funcall usock-error condition)
			       usock-error))
	      (nameserver-error (cdr (assoc (jclass-of java-cause) +abcl-nameserver-error-map+
					    :test #'string=))))
	 (if nameserver-error
	     (error nameserver-error :host-or-ip nil)
	     (when usock-error
	       (error usock-error :socket socket))))))))

(defparameter +abcl-error-map+
  `(("java.net.ConnectException" . connection-refused-error)
    ("java.net.SocketTimeoutException" . timeout-error)
    ("java.net.BindException" . operation-not-permitted-error)))

(defparameter +abcl-nameserver-error-map+
  `(("java.net.UnknownHostException" . ns-host-not-found-error)))

;;; GET-HOSTS-BY-NAME

(defun get-address (address)
  (let* ((array (%get-address address))
	 (length (jarray-length array)))
    (labels ((jbyte (n)
	       (let ((byte (jarray-ref array n)))
		 (if (plusp byte)
		     byte
		     (+ 256 byte)))))
      (if (= 4 length)
	  (vector (jbyte 0) (jbyte 1) (jbyte 2) (jbyte 3))
	  nil)))) ; not a IPv4 address?!

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
    (map 'list #'get-address (%get-all-by-name name))))

;;; GET-HOST-BY-ADDRESS

(defun get-host-by-address (host)
  (let ((inet4 (host-to-inet4 host)))
    (with-mapped-conditions ()
      (jcall $@getHostName/0 inet4))))

;;; SOCKET-CONNECT

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-supplied-p)
                       local-host local-port)
  (when deadline (unsupported 'deadline 'socket-connect))
  (let (socket stream usocket)
    (ecase protocol
      (:stream ; TCP
       (let ((channel (jstatic $@open/SocketChannel/0 $*SocketChannel))
	     (address (jnew $%InetSocketAddress/2 (host-to-inet4 host) port)))
	 (setq socket (jcall $@socket/SocketChannel/0 channel))
	 ;; bind to local address if needed
	 (when (or local-host local-port)
	   (let ((local-address (jnew $%InetSocketAddress/2 (host-to-inet4 local-host) (or local-port 0))))
	     (with-mapped-conditions ()
	       (jcall $@bind/Socket/1 socket local-address))))
	 ;; connect to dest address
	 (with-mapped-conditions ()
	   (jcall $@connect/SocketChannel/1 channel address))
	 (setq stream (ext:get-socket-stream socket :element-type element-type)
	       usocket (make-stream-socket :stream stream :socket socket))
	 (when nodelay-supplied-p
	   (jcall $@setTcpNoDelay/1 socket (if nodelay +java-true+ +java-false+)))
	 (when timeout
	   (jcall $@setSoTimeout/Socket/1 socket (truncate (* 1000 timeout))))))
      (:datagram ; UDP
       (let ((channel (jstatic $@open/DatagramChannel/0 $*DatagramChannel)))
	 (setq socket (jcall $@socket/DatagramChannel/0 channel))
	 ;; bind to local address if needed
	 (when (or local-host local-port)
	   (let ((local-address (jnew $%InetSocketAddress/2 (host-to-inet4 local-host) (or local-port 0))))
	     (with-mapped-conditions ()
	       (jcall $@bind/DatagramSocket/1 socket local-address))))
	 ;; connect to dest address if needed
	 (when (and host port)
	   (let ((address (jnew $%InetSocketAddress/2 (host-to-inet4 host) port)))
	     (with-mapped-conditions ()
	       (jcall $@connect/DatagramChannel/1 channel address))))
	 (setq usocket (make-datagram-socket socket))
	 (when timeout
	   (jcall $@setSoTimeout/DatagramSocket/1 socket (truncate (* 1000 timeout)))))))
    usocket))

;;; SOCKET-LISTEN

(defun socket-listen (host port &key (element-type 'character)
                      (reuse-address nil reuse-address-supplied-p)
		      (backlog 5 backlog-supplied-p))
  (declare (type boolean reuse-address))
  (let* ((channel (jstatic $@open/ServerSocketChannel/0 $*ServerSocketChannel))
	 (socket (jcall $@socket/ServerSocketChannel/0 channel))
	 (endpoint (jnew $%InetSocketAddress/2 (host-to-inet4 host) (or port 0))))
    (when reuse-address-supplied-p
      (jcall $@setReuseAddress/1 socket (if reuse-address +java-true+ +java-false+)))
    (with-mapped-conditions (socket)
      (if backlog-supplied-p
	  (jcall $@bind/ServerSocket/2 socket endpoint backlog)
	  (jcall $@bind/ServerSocket/1 socket endpoint)))
    (make-stream-server-socket socket :element-type element-type)))

;;; SOCKET-ACCEPT

(defmethod socket-accept ((socket stream-server-usocket) &key (element-type 'character))
  (with-mapped-conditions (socket)
    (let* ((client-socket (jcall $@accept/0 socket))
	   (stream (ext:get-socket-stream client-socket :element-type element-type)))
      (make-stream-socket :stream stream :socket client-socket))))

;;; SOCKET-CLOSE

(defmethod socket-close :before ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket)))

(defmethod socket-close ((usocket stream-server-usocket))
  (with-mapped-conditions (usocket)
    (jcall $@close/ServerSocket/0 (socket usocket))))

(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))
    (jcall $@close/Socket/0 (socket usocket))))

(defmethod socket-close ((usocket datagram-usocket))
  (with-mapped-conditions (usocket)
    (jcall $@close/DatagramSocket/0 (socket usocket))))

;;; GET-LOCAL/PEER-NAME/ADDRESS/PORT

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
	  (get-local-port usocket)))

(defmethod get-peer-name ((usocket usocket))
  (values (get-peer-address usocket)
	  (get-peer-port usocket)))

(defmethod get-local-address ((usocket stream-usocket))
  (get-address (jcall $@getLocalAddress/Socket/0 (socket usocket))))

(defmethod get-local-address ((usocket stream-server-usocket))
  (get-address (jcall $@getInetAddress/ServerSocket/0 (socket usocket))))

(defmethod get-local-address ((usocket datagram-usocket))
  (get-address (jcall $@getLocalAddress/DatagramSocket/0 (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (get-address (jcall $@getInetAddress/Socket/0 (socket usocket))))

(defmethod get-peer-address ((usocket datagram-usocket))
  (get-address (jcall $@getInetAddress/DatagramSocket/0 (socket usocket))))

(defmethod get-local-port ((usocket stream-usocket))
  (jcall $@getLocalPort/Socket/0 (socket usocket)))

(defmethod get-local-port ((usocket stream-server-usocket))
  (jcall $@getLocalPort/ServerSocket/0 (socket usocket)))

(defmethod get-local-port ((usocket datagram-usocket))
  (jcall $@getLocalPort/DatagramSocket/0 (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (jcall $@getPort/Socket/0 (socket usocket)))

(defmethod get-peer-port ((usocket datagram-usocket))
  (jcall $@getPort/DatagramSocket/0 (socket usocket)))

;;; SOCKET-SEND & SOCKET-RECEIVE

(defmethod socket-send ((socket datagram-usocket) buffer length &key host port)
  (with-mapped-conditions (socket)
    ))

(defmethod socket-receive ((socket datagram-usocket) buffer length
			   &key (element-type '(unsigned-byte 8)))
  (with-mapped-conditions (socket)
    ))

;;; WAIT-FOR-INPUT

(defun socket-channel-class (usocket)
  (cond ((stream-usocket-p usocket) $*SocketChannel)
	((stream-server-usocket-p usocket) $*ServerSocketChannel)
	((datagram-usocket-p usocket) $*DatagramChannel)))

(defun get-socket-channel (usocket)
  (let ((method (cond ((stream-usocket-p usocket) $@getChannel/Socket/0)
		      ((stream-server-usocket-p usocket) $@getChannel/ServerSocket/0)
		      ((datagram-usocket-p usocket) $@getChannel/DatagramSocket/0))))
    (jcall method (socket usocket))))

(defun wait-for-input-internal (wait-list &key timeout)
  (let* ((sockets (wait-list-waiters wait-list))
	 (ops (logior $+op-read $+op-accept))
	 (selector (jstatic $@open/Selector/0 $*Selector))
	 (channels (mapcar #'get-socket-channel sockets)))
    (unwind-protect
	 (with-mapped-conditions ()
	   (dolist (channel channels)
	     (jcall $@configureBlocking/1 channel +java-false+)
	     (jcall $@register/2 channel selector (logand ops (jcall $@validOps/0 channel))))
	   (let ((ready-count (if timeout
				  (jcall $@select/1 selector (truncate (* timeout 1000)))
				  (jcall $@select/0 selector))))
	     (when (plusp ready-count)
	       (let* ((keys (jcall $@selectedKeys/0 selector))
		      (iterator (jcall $@iterator/0 keys))
		      (%wait (wait-list-%wait wait-list)))
		 (loop while (jcall $@hasNext/0 iterator)
		       do (let* ((key (jcall $@next/0 iterator))
				 (channel (jcall $@channel/0 key)))
			    (setf (state (gethash channel %wait)) :read)))))))
      (jcall $@close/Selector/0 selector)
      (dolist (channel channels)
	(jcall $@configureBlocking/1 channel +java-true+)))))

;;; WAIT-LIST

;;; NOTE from original worker (Erik):
;;; Note that even though Java has the concept of the Selector class, which
;;; remotely looks like a wait-list, it requires the sockets to be non-blocking.
;;; usocket however doesn't make any such guarantees and is therefore unable to
;;; use the concept outside of the waiting routine itself (blergh!).

(defun %setup-wait-list (wl)
  (setf (wait-list-%wait wl)
        (make-hash-table :test #'equal :rehash-size 1.3d0)))

(defun %add-waiter (wl w)
  (setf (gethash (get-socket-channel w) (wait-list-%wait wl)) w))

(defun %remove-waiter (wl w)
  (remhash (get-socket-channel w) (wait-list-%wait wl)))
