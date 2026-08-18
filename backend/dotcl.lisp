;;;; dotcl (Common Lisp on .NET) networking support, using System.Net.Sockets
;;;; through dotcl's DOTNET interop package.

;;;; See LICENSE for licensing information.

(in-package :usocket)


;; keyed on SocketException's SocketErrorCode, whose numeric values are the
;; Winsock numbers on every platform, so this is the counterpart of the errno
;; maps the unix backends carry. don't use ErrorCode instead: that one is the
;; native error, 111 rather than 10061 for a refused connection on linux.
;; message text is never consulted either, .NET localises it.
(defparameter +dotcl-socket-error-map+
  '((10004 . interrupted-condition)          ; WSAEINTR
    (10009 . bad-file-descriptor-error)      ; WSAEBADF
    (10013 . operation-not-permitted-error)  ; WSAEACCES
    (10014 . invalid-argument-error)         ; WSAEFAULT
    (10022 . invalid-argument-error)         ; WSAEINVAL
    (10043 . protocol-not-supported-error)   ; WSAEPROTONOSUPPORT
    (10044 . socket-type-not-supported-error) ; WSAESOCKTNOSUPPORT
    (10045 . operation-not-supported-error)  ; WSAEOPNOTSUPP
    (10047 . protocol-not-supported-error)   ; WSAEAFNOSUPPORT
    (10048 . address-in-use-error)           ; WSAEADDRINUSE
    (10049 . address-not-available-error)    ; WSAEADDRNOTAVAIL
    (10050 . network-down-error)             ; WSAENETDOWN
    (10051 . network-unreachable-error)      ; WSAENETUNREACH
    (10052 . network-reset-error)            ; WSAENETRESET
    (10053 . connection-aborted-error)       ; WSAECONNABORTED
    (10054 . connection-reset-error)         ; WSAECONNRESET
    (10055 . no-buffers-error)               ; WSAENOBUFS
    (10058 . already-shutdown-error)         ; WSAESHUTDOWN
    (10060 . timeout-error)                  ; WSAETIMEDOUT
    (10061 . connection-refused-error)       ; WSAECONNREFUSED
    (10064 . host-down-error)                ; WSAEHOSTDOWN
    (10065 . host-unreachable-error)         ; WSAEHOSTUNREACH
    (11001 . ns-host-not-found-error)        ; WSAHOST_NOT_FOUND
    (11002 . ns-try-again-error)             ; WSATRY_AGAIN
    (11003 . ns-no-recovery-error)))         ; WSANO_RECOVERY

;; a socket failure doesn't always arrive as a SocketException: NetworkStream
;; reads and writes wrap it in an IOException, and a connect that failed while
;; we were waiting on its task arrives as an AggregateException. walk the
;; InnerException chain rather than special-casing each wrapper.
(defun %socket-exception (condition)
  (loop with ex = (dotnet:exception-object condition)
        repeat 4
        while ex
        do (when (dotnet:is-instance-of ex "System.Net.Sockets.SocketException")
             (return ex))
           (setf ex (ignore-errors (dotnet:invoke ex "get_InnerException")))))

(defun %signal-usocket-error (class socket host-or-ip)
  (if (subtypep class 'ns-error)
      (error class :host-or-ip host-or-ip)
      (error class :socket socket)))

(defun handle-condition (condition &optional (socket nil) (host-or-ip nil))
  (typecase condition
    (usocket-condition condition)       ; already ours, let it through
    (error
     (let ((sockex (%socket-exception condition)))
       (cond
         (sockex
          (let* ((code (ignore-errors
                        (dotnet:static "System.Convert" "ToInt32"
                                       (dotnet:invoke sockex "get_SocketErrorCode"))))
                 (class (cdr (assoc code +dotcl-socket-error-map+))))
            (if class
                (%signal-usocket-error class socket host-or-ip)
                (error 'unknown-error :socket socket :real-error condition
                                      :errno (or code 0)))))
         ((dotnet:exception-typep condition "System.ObjectDisposedException")
          (error 'already-shutdown-error :socket socket))
         (t nil))))))                   ; decline, caller sees the original

(defun %parse-ip (host)
  (let ((name (host-to-hostname host)))
    (if (or (string= name "") (string= name "0.0.0.0"))
        (dotnet:static "System.Net.IPAddress" "Any")
        (handler-case (dotnet:static "System.Net.IPAddress" "Parse" name)
          (error ()
            (let ((addrs (dotnet:static "System.Net.Dns" "GetHostAddresses" name)))
              (if (plusp (dotnet:invoke addrs "Length"))
                  (aref addrs 0)
                  (error 'ns-host-not-found-error :host-or-ip host))))))))

;; dual-stack sockets report IPv4 peers as ::ffff:a.b.c.d, but callers expect
;; a vector-quad
(defun %unmap-ip (address)
  (if (handler-case (dotnet:invoke address "IsIPv4MappedToIPv6") (error () nil))
      (dotnet:invoke address "MapToIPv4")
      address))

(defun %ip-to-vector (address)
  (let* ((bytes (dotnet:invoke (%unmap-ip address) "GetAddressBytes"))
         (n (dotnet:invoke bytes "Length"))
         (v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (aref bytes i)))))

(defun %endpoint-values (endpoint)
  (values (%ip-to-vector (dotnet:invoke endpoint "Address"))
          (dotnet:invoke endpoint "Port")))

(defun %raw-socket (usocket)
  (let ((s (socket usocket)))
    (cond ((stream-server-usocket-p usocket) (dotnet:invoke s "Server"))
          ((stream-usocket-p usocket) (dotnet:invoke s "Client"))
          (t s))))

(defun %make-byte-array (length)
  (dotnet:static "System.Array" "CreateInstance"
                 (dotnet:resolve-type "System.Byte") length))

(defun %lisp-to-byte-array (buffer size &key (offset 0))
  (let ((arr (%make-byte-array size)))
    (etypecase buffer
      (vector (dotimes (i size) (setf (aref arr i) (aref buffer (+ offset i)))))
      (list (loop for i from 0 below size
                  for x in (nthcdr offset buffer)
                  do (setf (aref arr i) x))))
    arr))

(defun %binary-element-type-p (element-type)
  (not (or (null element-type)
           (subtypep element-type 'character))))

(defun %net-stream (tcp-client element-type)
  (let ((ns (dotnet:invoke tcp-client "GetStream")))
    (if (%binary-element-type-p element-type)
        (dotnet:to-stream ns :binary t)
        (dotnet:to-stream ns :bivalent t))))

;; 4 octets or 16; sturdier than comparing the printed AddressFamily, where
;; InterNetwork is a prefix of InterNetworkV6
(defun %ipv6-p (ip)
  (= 16 (dotnet:invoke (dotnet:invoke ip "GetAddressBytes") "Length")))

(defun %same-family-p (a b)
  (eq (%ipv6-p a) (%ipv6-p b)))

(defun %wildcard-for (ip)
  (dotnet:static "System.Net.IPAddress" (if (%ipv6-p ip) "IPv6Any" "Any")))

;; the socket's address family has to match the endpoints it will be used
;; with: localhost resolves to ::1 first on a dual-stack host, and binding an
;; IPv6 endpoint on an AF_INET socket fails outright
(defun %datagram-connect (host port local-host local-port)
  (with-mapped-conditions (nil host)
    (let* ((target-ip (when host (%parse-ip host)))
           (local-ip (when local-host (%parse-ip local-host)))
           (family-ip (or target-ip local-ip
                          (dotnet:static "System.Net.IPAddress" "Any")))
           (bind-ip (cond ((null local-ip) (%wildcard-for family-ip))
                          ((%same-family-p local-ip family-ip) local-ip)
                          (t (%wildcard-for family-ip))))
           (sock (dotnet:new "System.Net.Sockets.Socket"
                             (dotnet:invoke family-ip "AddressFamily")
                             (dotnet:enum-or "System.Net.Sockets.SocketType" "Dgram")
                             (dotnet:enum-or "System.Net.Sockets.ProtocolType" "Udp"))))
      ;; always bind: you may receive on a socket that was never connected,
      ;; and .NET reports no LocalEndPoint until the socket is bound
      (dotnet:invoke sock "Bind"
                     (dotnet:new "System.Net.IPEndPoint" bind-ip
                                 (or local-port *auto-port*)))
      (let ((usocket (make-datagram-socket sock)))
        (when (and host port)
          (dotnet:invoke sock "Connect"
                         (dotnet:new "System.Net.IPEndPoint" target-ip port))
          (setf (connected-p usocket) t))
        usocket))))

(defun socket-connect-internal (host &key port (protocol :stream)
                                          (element-type 'character)
                                          timeout deadline
                                          (nodelay nil nodelay-specified)
                                          (local-host nil local-host-p)
                                          (local-port nil local-port-p))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when (eq protocol :datagram)
    (return-from socket-connect-internal
      (%datagram-connect host port local-host local-port)))
  (with-mapped-conditions (nil host)
    (let ((client (dotnet:new "System.Net.Sockets.TcpClient")))
      (when (or local-host-p local-port-p)
        (dotnet:invoke (dotnet:invoke client "Client") "Bind"
                       (dotnet:new "System.Net.IPEndPoint"
                                   (%parse-ip (or local-host *wildcard-host*))
                                   (or local-port 0))))
      (when (and nodelay-specified (not (eq nodelay :if-supported)))
        (setf (dotnet:invoke client "NoDelay") (and nodelay t)))
      (when (eq nodelay :if-supported)
        (ignore-errors (setf (dotnet:invoke client "NoDelay") t)))
      (if timeout
          ;; .NET has no blocking connect with a timeout, so wait on the task
          (let ((task (dotnet:invoke client "ConnectAsync"
                                     (host-to-hostname host) port)))
            (unless (dotnet:invoke task "Wait" (round (* 1000 timeout)))
              (ignore-errors (dotnet:invoke client "Close"))
              (error 'timeout-error)))
          (dotnet:invoke client "Connect" (host-to-hostname host) port))
      (make-stream-socket :socket client
                          :stream (%net-stream client element-type)))))

(defun socket-listen-internal (host &key port reuseaddress
                                         (reuse-address nil reuse-address-supplied-p)
                                         (backlog 5)
                                         (element-type 'character))
  (let ((reuse (if reuse-address-supplied-p reuse-address reuseaddress)))
    (with-mapped-conditions (nil host)
      (let ((listener (dotnet:new "System.Net.Sockets.TcpListener"
                                  (%parse-ip host) (or port 0))))
        (when reuse
          (ignore-errors
           (dotnet:invoke (dotnet:invoke listener "Server") "SetSocketOption"
                          (dotnet:enum-or "System.Net.Sockets.SocketOptionLevel" "Socket")
                          (dotnet:enum-or "System.Net.Sockets.SocketOptionName" "ReuseAddress")
                          1)))
        (dotnet:invoke listener "Start" backlog)
        (make-stream-server-socket listener :element-type element-type)))))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (let ((et (or element-type (element-type usocket))))
    (with-mapped-conditions (usocket)
      (let ((client (dotnet:invoke (socket usocket) "AcceptTcpClient")))
        (make-stream-socket :socket client :stream (%net-stream client et))))))

(defmethod socket-close ((usocket stream-server-usocket))
  (with-mapped-conditions (usocket)
    (dotnet:invoke (socket usocket) "Stop")
    t))

(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (ignore-errors (close (socket-stream usocket)))
    (dotnet:invoke (socket usocket) "Close")
    t))

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  (with-mapped-conditions (usocket)
    (dotnet:invoke (%raw-socket usocket) "Shutdown"
                   (dotnet:enum-or "System.Net.Sockets.SocketShutdown"
                                   (ecase direction
                                     (:input "Receive")
                                     (:output "Send")
                                     (:io "Both"))))
    t))

(defmethod get-local-name ((usocket usocket))
  (with-mapped-conditions (usocket)
    (%endpoint-values (dotnet:invoke (%raw-socket usocket) "LocalEndPoint"))))

(defmethod get-peer-name ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (%endpoint-values (dotnet:invoke (%raw-socket usocket) "RemoteEndPoint"))))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun get-hosts-by-name (name)
  (with-mapped-conditions (nil name)
    (let* ((addrs (dotnet:static "System.Net.Dns" "GetHostAddresses" name))
           (n (dotnet:invoke addrs "Length")))
      (loop for i from 0 below n
            collect (%ip-to-vector (aref addrs i))))))

(defun get-host-by-address (address)
  (with-mapped-conditions (nil address)
    (dotnet:invoke (dotnet:static "System.Net.Dns" "GetHostEntry"
                                  (host-to-hostname address))
                   "HostName")))

(defun get-host-name ()
  (dotnet:static "System.Net.Dns" "GetHostName"))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (let ((sock (socket usocket))
        (n (or size (length buffer))))
    (with-mapped-conditions (usocket host)
      (let ((arr (%lisp-to-byte-array buffer n :offset offset))
            (flags (dotnet:enum-or "System.Net.Sockets.SocketFlags" "None")))
        (if (and host port)
            (dotnet:invoke sock "SendTo" arr flags
                           (dotnet:new "System.Net.IPEndPoint" (%parse-ip host) port))
            (dotnet:invoke sock "Send" arr flags))))))

;; ReceiveFrom reports the sender through a `ref EndPoint', hence call-out:
;; the endpoint passed in is the initial value and comes back as the second
;; value. it has to match the socket's family, so take it from LocalEndPoint.
(defmethod socket-receive ((usocket datagram-usocket) buffer length
                           &key (element-type '(unsigned-byte 8)))
  (with-mapped-conditions (usocket)
    (let* ((n (or length +max-datagram-packet-size+))
           (arr (%make-byte-array n))
           (local-ip (dotnet:invoke (dotnet:invoke (socket usocket) "LocalEndPoint")
                                    "Address")))
      (multiple-value-bind (received remote)
          (dotnet:call-out (socket usocket) "ReceiveFrom" arr
                           (dotnet:new "System.Net.IPEndPoint"
                                       (%wildcard-for local-ip) 0))
        (let ((out (or buffer (make-array received :element-type element-type))))
          (dotimes (i (min received (length out)))
            (setf (aref out i) (aref arr i)))
          (multiple-value-bind (rhost rport) (%endpoint-values remote)
            (values out received rhost rport)))))))

(defmethod socket-close ((usocket datagram-usocket))
  (with-mapped-conditions (usocket)
    (dotnet:invoke (socket usocket) "Close")
    t))

(defmethod socket-shutdown ((usocket datagram-usocket) direction)
  (with-mapped-conditions (usocket)
    (dotnet:invoke (socket usocket) "Shutdown"
                   (dotnet:enum-or "System.Net.Sockets.SocketShutdown"
                                   (ecase direction
                                     (:input "Receive")
                                     (:output "Send")
                                     (:io "Both"))))
    t))

(defmethod get-peer-name ((usocket datagram-usocket))
  (with-mapped-conditions (usocket)
    (%endpoint-values (dotnet:invoke (socket usocket) "RemoteEndPoint"))))

(defmethod get-peer-address ((usocket datagram-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-peer-port ((usocket datagram-usocket))
  (nth-value 1 (get-peer-name usocket)))

;;; Wait-list

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defconstant +select-forever+ -1)       ; Select takes microseconds; <0 blocks

(defun %socket-list-type ()
  (dotnet:make-generic-type "System.Collections.Generic.List"
                            (list (dotnet:resolve-type "System.Net.Sockets.Socket"))))

;; readiness comes from Socket.Select, select(2) over the whole set, blocking
;; in the kernel rather than spinning. it mutates the IList it is given so that
;; only the readable sockets remain, and membership afterwards is what marks
;; each usocket ready.
(defun wait-for-input-internal (wait-list &key timeout)
  (let* ((waiters (wait-list-waiters wait-list))
         (pairs (loop for w in waiters
                      for raw = (ignore-errors (%raw-socket w))
                      when raw collect (cons w raw))))
    (when pairs
      (with-mapped-conditions ()
        (let ((read-list (dotnet:new (%socket-list-type))))
          (loop for (nil . raw) in pairs do (dotnet:invoke read-list "Add" raw))
          (dotnet:static "System.Net.Sockets.Socket" "Select"
                         read-list nil nil
                         (if timeout
                             (max 0 (round (* timeout 1000000)))
                             +select-forever+))
          (loop for (usock . raw) in pairs
                do (when (dotnet:invoke read-list "Contains" raw)
                     (setf (state usock) :read))))))
    wait-list))
