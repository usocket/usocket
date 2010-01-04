;; MCL backend for USOCKET 0.4.1
;; Terje Norderhaug <terje@in-progress.com>, January 1, 2009

(in-package :ccl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :opentransport))

;; MCL Issue 29: Passive TCP connections on OS assigned ports
;; see http://code.google.com/p/mcl/issues/detail?id=29 for details
(ccl:advise ot-conn-tcp-passive-connect
            (destructuring-bind (conn port &optional (allow-reuse t)) arglist
              (declare (ignore allow-reuse))
              (if (eql port #$kOTAnyInetAddress)
		;; Avoids registering a proxy for port 0 but instead registers one for the true port:
		(multiple-value-bind (proxy result)
		    (let* ((*opentransport-class-proxies* NIL) ; makes ot-find-proxy return NIL
			   (result (:do-it)) ;; pushes onto *opentransport-class-proxies*
			   (proxy (prog1
				      (pop *opentransport-class-proxies*)
				    (assert (not *opentransport-class-proxies*))))
			   (context (cdr proxy))
			   (tmpconn (make-ot-conn :context context 
						  :endpoint (pref context :ot-context.ref)))
			   (localaddress (ot-conn-tcp-get-addresses tmpconn)))
		      (declare (dynamic-extent tmpconn))
		      ;; replace original set in body of function
		      (setf (ot-conn-local-address conn) localaddress)
		      (values
		       (cons localaddress context)
		       result))
                  ;; need to be outside local binding of *opentransport-class-proxies* 
                  (without-interrupts
		      (push proxy *opentransport-class-proxies*))
                  result)
                (:do-it)))
         :when :around :name 'ot-conn-tcp-passive-connect-any-address)

(in-package :usocket)

(defun handle-condition (condition &optional socket)
  ; incomplete, needs to handle additional conditions
  (flet ((raise-error (&optional socket-condition)
           (if socket-condition
           (error socket-condition :socket socket)
           (error  'unknown-error :socket socket :real-error condition))))
    (typecase condition
      (ccl:host-stopped-responding
       (raise-error 'host-down-error))
      (ccl:host-not-responding
       (raise-error 'host-unreachable-error))
      (ccl:connection-reset 
       (raise-error 'connection-reset-error))
      (ccl:connection-timed-out
       (raise-error 'timeout-error))
      (ccl:opentransport-protocol-error
       (raise-error 'protocol-not-supported-error))       
      (otherwise
       (raise-error)))))

(defun socket-connect (host port &key (element-type 'character) timeout deadline nodelay 
                            local-host local-port)
  (with-mapped-conditions ()
    (let* ((socket
            (make-instance 'active-socket
              :remote-host (when host (host-to-hostname host)) 
              :remote-port port
              :local-host (when local-host (host-to-hostname local-host)) 
              :local-port local-port
              :deadline deadline
              :nodelay nodelay
              :connect-timeout (and timeout (round (* timeout 60)))
              :element-type element-type))
           (stream (socket-open-stream socket)))
      (make-stream-socket :socket socket :stream stream))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (declare (ignore reuseaddress reuse-address-supplied-p))
  (let ((socket (with-mapped-conditions ()
                  (make-instance 'passive-socket 
                    :local-port port
                    :local-host host
                    :reuse-address reuse-address
                    :backlog backlog))))
    (make-stream-server-socket socket :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (let* ((socket (socket usocket))
         (stream (with-mapped-conditions (usocket)
                   (socket-accept socket :element-type element-type))))
    (make-stream-socket :socket socket :stream stream)))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (socket-close (socket usocket))))

(defmethod ccl::stream-close ((usocket usocket))
  (socket-close usocket))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
    (list (hbo-to-vector-quad (ccl::get-host-address
                               (host-to-hostname name))))))

(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (ccl::inet-host-name (host-to-hbo address))))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defmethod get-local-address ((usocket usocket))
  (hbo-to-vector-quad (ccl::get-host-address (or (local-host (socket usocket)) ""))))

(defmethod get-local-port ((usocket usocket))
  (local-port (socket usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (hbo-to-vector-quad (ccl::get-host-address (remote-host (socket usocket)))))

(defmethod get-peer-port ((usocket stream-usocket))
  (remote-port (socket usocket)))


(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC MCL SOCKET IMPLEMENTATION

(defclass socket ()
  ((local-port :reader local-port :initarg :local-port)
   (local-host :reader local-host :initarg :local-host)
   (element-type :reader element-type :initform 'ccl::base-character :initarg :element-type)))

(defclass active-socket (socket)
  ((remote-host :reader remote-host :initarg :remote-host)
   (remote-port :reader remote-port :initarg :remote-port)
   (deadline :initarg :deadline)
   (nodelay :initarg :nodelay)
   (connect-timeout :reader connect-timeout :initform NIL :initarg :connect-timeout
                    :type (or null fixnum) :documentation "ticks (60th of a second)")))

(defmethod socket-open-stream ((socket active-socket))
  (ccl::open-tcp-stream (or (remote-host socket)(ccl::local-interface-ip-address)) (remote-port socket)
   :element-type (if (subtypep (element-type socket) 'character) 'ccl::base-character 'unsigned-byte)
   :connect-timeout (connect-timeout socket)))

(defmethod socket-close ((socket active-socket))
  NIL)

(defclass passive-socket (socket)
  ((streams :accessor socket-streams :type list :initform NIL
            :documentation "Circular list of streams with first element the next to open")
   (reuse-address :reader reuse-address :initarg :reuse-address)))

(defmethod initialize-instance :after ((socket passive-socket) &key backlog)
  (loop repeat backlog
        collect (socket-open-listener socket) into streams
        finally (setf (socket-streams socket)
                      (cdr (rplacd (last streams) streams))))
  (when (zerop (local-port socket))
    (setf (slot-value socket 'local-port)
          (or (ccl::process-wait-with-timeout "binding port" (* 10 60) 
               #'ccl::stream-local-port (car (socket-streams socket)))
              (error "timeout")))))

(defmethod socket-accept ((socket passive-socket) &key element-type)
  (flet ((connection-established-p (stream) 
           (ccl::with-io-buffer-locked ((ccl::stream-io-buffer stream nil)) 
             (let ((state (ccl::opentransport-stream-connection-state stream)))
               (not (eq :unbnd state))))))
    (with-mapped-conditions ()
      (let* ((new (socket-open-listener socket element-type))
             (connection (car (socket-streams socket))))
        (assert connection)
        (rplaca (socket-streams socket) new)
        (setf (socket-streams socket) 
              (cdr (socket-streams socket)))
        (ccl::process-wait "Socket Accept" #'connection-established-p connection) ; expensive polling...
        connection))))

(defmethod socket-close ((socket passive-socket))
  (loop
    with streams = (socket-streams socket)
    for (stream tail) on streams
    do (close stream :abort T)
    until (eq tail streams)
    finally (setf (socket-streams socket) NIL)))

(defmethod socket-open-listener (socket &optional element-type)
  ; see http://code.google.com/p/mcl/issues/detail?id=28
  (let* ((ccl::*passive-interface-address* (local-host socket))
         (new (ccl::open-tcp-stream NIL (or (local-port socket) #$kOTAnyInetAddress) 
                                    :reuse-local-port-p (reuse-address socket) 
                                    :element-type (if (subtypep (or element-type (element-type socket))
                                                                'character) 
                                                    'ccl::base-character 
                                                    'unsigned-byte))))
    (declare (special ccl::*passive-interface-address*))
    new))


(defun wait-for-input-internal (wait-list &key timeout &aux result)
  (macrolet ((when-io-buffer-lock-grabbed ((lock &optional multiple-value-p) &body body)
	       "Evaluates the body if and only if the lock is successfully grabbed"
	       ;; like with-io-buffer-lock-grabbed but returns immediately instead of polling the lock
	       (let ((needs-unlocking-p (gensym))
		     (lock-var (gensym)))
		 `(let* ((,lock-var ,lock)
			 (ccl::*grabbed-io-buffer-locks* (cons ,lock-var ccl::*grabbed-io-buffer-locks*))
			 (,needs-unlocking-p (needs-unlocking-p ,lock-var)))
		    (declare (dynamic-extent ccl::*grabbed-io-buffer-locks*))
		    (when ,needs-unlocking-p
		      (,(if multiple-value-p 'multiple-value-prog1 'prog1)
                        (progn ,@body)
                        (ccl::%release-io-buffer-lock ,lock-var)))))))
    (labels ((needs-unlocking-p (lock)
	       (declare (type ccl::lock lock))
	       ;; crucial - clears bogus lock.value as in grab-io-buffer-lock-out-of-line:
	       (ccl::%io-buffer-lock-really-grabbed-p lock)
	       (ccl:store-conditional lock nil ccl:*current-process*))
	     (input-available (stream)
	       "similar to stream-listen on buffered-input-stream-mixin but without waiting for lock"
	       (let ((io-buffer (ccl::stream-io-buffer stream)))
		 (or (not (eql 0 (ccl::io-buffer-incount io-buffer)))
		     (ccl::io-buffer-untyi-char io-buffer)
		     (locally (declare (optimize (speed 3) (safety 0)))
		       (when-io-buffer-lock-grabbed ((ccl::io-buffer-lock io-buffer))
		         (funcall (ccl::io-buffer-listen-function io-buffer) stream io-buffer))))))
	     (ready-sockets (sockets)
	       (dolist (sock sockets result)
		 (when (input-available (socket-stream sock))
		   (push sock result)))))
      (with-mapped-conditions ()
	(ccl:process-wait-with-timeout
	 "socket input"
	 (when timeout (truncate (* timeout 60)))
	 #'ready-sockets
	 (wait-list-waiters wait-list)))
      (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| Test for wait-for-input
(let* ((sock1 (usocket:socket-connect "in-progress.com" 80))
      (sock2 (usocket:socket-connect "common-lisp.net" 80))
      (sockets (list sock1 sock2)))
 (dolist (sock sockets)
   (format (usocket:socket-stream sock)
           "GET / HTTP/1.0~A~A~A~A"
           #\Return #\Linefeed #\Return #\Linefeed)
   (force-output (usocket:socket-stream sock)))
 (wait-for-input sockets :timeout 5000))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| TEST (from test-usocket.lisp)


(defparameter +non-existing-host+ "192.168.1.1")
(defparameter +unused-local-port+ 15213)
(defparameter *soc1* (usocket::make-stream-socket :socket :my-socket
                                                  :stream :my-stream))
(defparameter +common-lisp-net+ #(208 72 159 207)) ;; common-lisp.net IP


(usocket:socket *soc1*)

(usocket:socket-connect "127.0.0.0" +unused-local-port+)

(usocket:socket-connect #(127 0 0 0) +unused-local-port+)

(usocket:socket-connect 2130706432 +unused-local-port+)

    (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
      (unwind-protect
          (typep sock 'usocket:usocket)
        (usocket:socket-close sock)))

    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (typep sock 'usocket:usocket)
        (usocket:socket-close sock)))

    (let ((sock (usocket:socket-connect (usocket::host-byte-order +common-lisp-net+) 80)))
      (unwind-protect
          (typep sock 'usocket:usocket)
        (usocket:socket-close sock)))

(let ((sock (usocket:socket-connect "common-lisp.net" 80)))
      (unwind-protect
          (progn
            (format (usocket:socket-stream sock)
                    "GET / HTTP/1.0~A~A~A~A"
                    #\Return #\Linefeed #\Return #\Linefeed)
            (force-output (usocket:socket-stream sock))
            (read-line (usocket:socket-stream sock)))
        (usocket:socket-close sock)))

    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-peer-address sock)
        (usocket:socket-close sock)))

    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-peer-port sock)
        (usocket:socket-close sock)))

    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-peer-name sock)
        (usocket:socket-close sock)))

    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-local-address sock)
        (usocket:socket-close sock)))

|#


#|

(defun socket-server (host port)
  (let ((socket (socket-listen host port)))
    (unwind-protect
      (loop
        (with-open-stream (stream (socket-stream (socket-accept socket))) 
          (ccl::telnet-write-line stream "~A" 
           (reverse (ccl::telnet-read-line stream)))
          (ccl::force-output stream)))
      (close socket))))

(ccl::process-run-function "Socket Server" #'socket-server NIL 4088)

(let* ((sock (socket-connect nil 4088))
       (stream (usocket:socket-stream sock)))
  (assert (streamp stream))
  (ccl::telnet-write-line stream "hello ~A" (random 10))
  (ccl::force-output stream)
  (ccl::telnet-read-line stream))

|#
