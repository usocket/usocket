;;;; $Id$
;;;; $URL$

;;;; New ABCL networking support (replacement to old armedbear.lisp)
;;;; Author: Chun Tian (binghe)

;;;; See LICENSE for licensing information.

(in-package :usocket)

;;; Java Classes ($*...)
(defvar $*boolean (jclass "boolean"))
(defvar $*int (jclass "int"))
(defvar $*DatagramSocket (jclass "java.net.DatagramSocket"))
(defvar $*Inet4Address (jclass "java.net.Inet4Address"))
(defvar $*InetAddress (jclass "java.net.InetAddress"))
(defvar $*InetSocketAddress (jclass "java.net.InetSocketAddress"))
(defvar $*ServerSocket (jclass "java.net.ServerSocket"))
(defvar $*Socket (jclass "java.net.Socket"))
(defvar $*SocketAddress (jclass "java.net.SocketAddress"))
(defvar $*String (jclass "java.lang.String"))

;;; Java Constructor ($%.../n)
(defvar $%DatagramSocket/0 (jconstructor $*DatagramSocket))
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
(defvar $@bind/1 (jmethod $*ServerSocket "bind" $*SocketAddress))
(defvar $@bind/2 (jmethod $*ServerSocket "bind" $*SocketAddress $*int))
(defvar $@close/ServerSocket/0 (jmethod $*ServerSocket "close"))
(defvar $@close/Socket/0 (jmethod $*Socket "close"))
(defvar $@connect/1 (jmethod $*Socket "connect" $*SocketAddress))
(defvar $@connect/2 (jmethod $*Socket "connect" $*SocketAddress $*int))
(defvar $@getAddress/0 (jmethod $*Inet4Address "getAddress"))
(defvar $@getAllByName/1 (jmethod $*InetAddress "getAllByName" $*String))
(defvar $@getByName/1 (jmethod $*InetAddress "getByName" $*String))
(defvar $@getHostName/0 (jmethod $*InetAddress "getHostName"))
(defvar $@getInetAddress/ServerSocket/0 (jmethod $*ServerSocket "getInetAddress"))
(defvar $@getInetAddress/Socket/0 (jmethod $*Socket "getInetAddress"))
(defvar $@getLocalAddress/Socket/0 (jmethod $*Socket "getLocalAddress"))
(defvar $@getLocalPort/ServerSocket/0 (jmethod $*ServerSocket "getLocalPort"))
(defvar $@getLocalPort/Socket/0 (jmethod $*Socket "getLocalPort"))
(defvar $@getPort/Socket/0 (jmethod $*Socket "getPort"))
(defvar $@setReuseAddress/1 (jmethod $*ServerSocket "setReuseAddress" $*boolean))

;;; Wrapper functions (return-type: java-object)
(defun %get-address (address)
  (jcall $@getAddress/0 address))
(defun %get-all-by-name (string) ; return a simple vector
  (jstatic $@getAllByName/1 $*InetAddress string))
(defun %get-by-name (string)
  (jstatic $@getByName/1 $*InetAddress string))

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
  `(;("java.io.IOException" . )
    ("java.net.ConnectException" . connection-refused-error)
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

(defun host-to-inet4 (host)
  "USOCKET host formats to Java Inet4Address, used internally."
    (%get-by-name (host-to-hostname host)))

;;; GET-HOST-BY-ADDRESS
(defun get-host-by-address (host)
  (let ((inet4 (host-to-inet4 host)))
    (with-mapped-conditions ()
      (jcall $@getHostName/0 inet4))))

;;; SOCKET-CONNECT

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-supplied-p)
                       local-host local-port)
  (declare (type integer timeout))
  (if (eq protocol :stream)
    (let* ((socket (with-mapped-conditions ()
		     (if (or local-host local-port)
			 (jnew $%Socket/4 (host-to-inet4 host) port (host-to-inet4 local-host) local-port)
		       (if timeout
			   (let ((socket (jnew $%Socket/0))
				 (address (jnew $%InetSocketAddress/2 (host-to-inet4 host) port)))
			     (jcall $@connect/2 socket address timeout)
			     socket)
			 (jnew $%Socket/2 (host-to-inet4 host) port)))))
	   (stream (ext:get-socket-stream socket :element-type element-type))
	   (usocket (make-stream-socket :stream stream :socket socket)))
      usocket)
    (socket-connect-for-udp host port :timeout timeout :local-host local-host :local-port local-port)))

(defun socket-connect-for-udp (host port &key timeout local-host local-port)
  )

(defun socket-listen (host port &key reuseaddress (element-type 'character)
                      (reuse-address nil reuse-address-supplied-p)
		      (backlog 5 backlog-supplied-p))
  (let ((socket (jnew $%ServerSocket/0))
	(endpoint (jnew $%InetSocketAddress/2 (host-to-inet4 host) port)))
    #+ignore ;; TODO: java.lang.IllegalArgumentException?
    (when reuse-address-supplied-p
      (jcall $@setReuseAddress/1 socket reuse-address))
    (with-mapped-conditions (socket)
      (if backlog-supplied-p
	  (jcall $@bind/2 socket endpoint backlog)
	  (jcall $@bind/1 socket endpoint)))
    (make-stream-server-socket socket :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket) &key (element-type 'character))
  (with-mapped-conditions (socket)
    (let* ((client-socket (jcall $@accept/0 socket))
	   (stream (ext:get-socket-stream client-socket :element-type element-type)))
      (make-stream-socket :stream stream :socket client-socket))))

(defmethod socket-close :before ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket)))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (jcall $@close/Socket/0 (socket usocket))))

(defmethod socket-close ((usocket stream-server-usocket))
  (with-mapped-conditions (usocket)
    (jcall $@close/ServerSocket/0 (socket usocket))))

(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
	  (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
	  (get-peer-port usocket)))

(defmethod get-local-address ((usocket usocket))
  (get-address (jcall $@getLocalAddress/Socket/0 (socket usocket))))

(defmethod get-local-address ((usocket stream-server-usocket))
  (get-address (jcall $@getInetAddress/ServerSocket/0 (socket usocket))))

(defmethod get-peer-address ((usocket usocket))
  (get-address (jcall $@getInetAddress/Socket/0 (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (jcall $@getLocalPort/Socket/0 (socket usocket)))

(defmethod get-local-port ((usocket stream-server-usocket))
  (jcall $@getLocalPort/ServerSocket/0 (socket usocket)))

(defmethod get-peer-port ((usocket usocket))
  (jcall $@getPort/Socket/0 (socket usocket)))
