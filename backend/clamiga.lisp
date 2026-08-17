;;;; -*- Mode: Common-Lisp -*-

;;;; CL-Amiga (clamiga) backend for usocket.
;;;;
;;;; clamiga exposes TCP sockets through its EXT package as ordinary
;;;; bidirectional (unsigned-byte 8) streams:
;;;;
;;;;   (ext:open-tcp-stream host port)   => active/client connection stream
;;;;   (ext:socket-listen port loopback) => passive/listening socket stream
;;;;   (ext:socket-accept listener)      => accepted connection stream
;;;;   (ext:socket-local-port listener)  => bound local port (for port 0)
;;;;
;;;; These map almost one-to-one onto usocket's stream socket protocol, so a
;;;; usocket stream-/stream-server-socket simply wraps the clamiga stream.
;;;; There is no separate "socket handle" object on clamiga — the stream IS the
;;;; socket — so the usocket :socket and :stream slots hold the same stream.
;;;;
;;;; Connected datagram (UDP) sockets map onto clamiga's
;;;;
;;;;   (ext:open-udp-stream host port)              => connected UDP stream
;;;;   (ext:udp-stream-send stream buf &optional n) => t
;;;;   (ext:udp-stream-receive stream buf &optional max) => length
;;;;   (ext:socket-stream-local-endpoint stream)    => ip-string, port
;;;;
;;;; Unconnected UDP (socket-connect with NIL host, or :host/:port overrides
;;;; on socket-send) is not exposed and reported as unsupported.  Peer
;;;; introspection beyond the local endpoint returns best-effort placeholders.
;;;;
;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun handle-condition (condition &optional (socket nil) (host-or-ip nil))
  "Map a clamiga stream/IO error to the closest usocket condition.  clamiga
signals plain CL conditions for socket failures, so without more specific
information we surface them as an UNKNOWN-ERROR carrying the socket."
  (declare (ignore host-or-ip))
  (typecase condition
    (error (error 'unknown-error
                  :socket socket
                  :real-error condition))))

(defun socket-connect-internal (host &key port (protocol :stream) element-type
                                      timeout deadline (nodelay nil nodelay-p)
                                      local-host local-port)
  ;; clamiga's open-tcp-stream gives a fixed (unsigned-byte 8) bidirectional
  ;; stream; element-type, deadline, nodelay and local binding are not
  ;; configurable through the EXT layer, so those values are ignored.  TIMEOUT
  ;; (seconds, fractional allowed) IS honored: it bounds the connect handshake
  ;; so an unreachable host fails fast instead of stalling on the OS timeout.
  (declare (ignore element-type deadline local-host local-port))
  (when deadline
    (unsupported 'deadline 'socket-connect))
  (when (and nodelay-p (not (eq nodelay :if-supported)))
    (unsupported 'nodelay 'socket-connect))
  (with-mapped-conditions (nil host)
    (ecase protocol
      (:stream
       (let ((s (if timeout
                    (ext:open-tcp-stream (host-to-hostname host) port timeout)
                    (ext:open-tcp-stream (host-to-hostname host) port))))
         (make-stream-socket :socket s :stream s)))
      (:datagram
       (unless (and host port)
         ;; an unconnected UDP socket (bind-only) is not exposed by EXT
         (unsupported 'datagram 'socket-connect))
       (let ((s (ext:open-udp-stream (host-to-hostname host) port)))
         (make-datagram-socket s :connected-p t))))))

(defun socket-listen-internal (host &key port reuseaddress
                                    (reuse-address nil reuse-address-supplied-p)
                                    (backlog 5)
                                    (element-type 'character))
  ;; clamiga's socket-listen takes a port and a loopback flag; it has no
  ;; reuse-address / backlog controls.  We bind loopback-only when HOST names
  ;; the loopback interface, all interfaces otherwise.
  (declare (ignore reuseaddress reuse-address reuse-address-supplied-p backlog))
  (let* ((hostname (host-to-hostname host))
         (loopback (member hostname '("127.0.0.1" "localhost" "::1")
                           :test #'string=)))
    (with-mapped-conditions (nil host)
      (let ((s (ext:socket-listen (or port 0) (and loopback t))))
        (make-stream-server-socket s :element-type element-type)))))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (declare (ignore element-type))
  (with-mapped-conditions (usocket)
    (let ((s (ext:socket-accept (socket usocket))))
      (make-stream-socket :socket s :stream s))))

(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod socket-close ((usocket stream-server-usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod socket-close ((usocket datagram-usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

;;; Datagram I/O.  The EXT layer only supports CONNECTED UDP sockets, so the
;;; :host/:port per-send overrides are rejected rather than silently ignored.

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port
                        (offset 0))
  (when (or host port)
    (unsupported 'host 'socket-send))
  (with-mapped-conditions (usocket)
    (let ((buf (if (zerop offset)
                   buffer
                   (subseq buffer offset (+ offset size)))))
      (ext:udp-stream-send (socket usocket) buf size)
      size)))

(defmethod socket-receive ((usocket datagram-usocket) buffer length &key)
  (with-mapped-conditions (usocket)
    (let* ((buf (or buffer
                    (make-array (or length +max-datagram-packet-size+)
                                :element-type '(unsigned-byte 8))))
           (n (ext:udp-stream-receive (socket usocket) buf
                                      (or length (length buf)))))
      ;; remote host/port: the socket is connected, so the sender is the
      ;; peer it was created with; EXT doesn't expose it — return NILs.
      (values buf n nil nil))))

(defmethod get-local-name ((usocket datagram-usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-local-address ((usocket datagram-usocket))
  (multiple-value-bind (ip port)
      (ext:socket-stream-local-endpoint (socket usocket))
    (declare (ignore port))
    ip))

(defmethod get-local-port ((usocket datagram-usocket))
  (multiple-value-bind (ip port)
      (ext:socket-stream-local-endpoint (socket usocket))
    (declare (ignore ip))
    port))

;;; Name / address introspection.
;;;
;;; clamiga's EXT layer only exposes the local port of a listening socket, so
;;; the peer/local *address* accessors return the unspecified-address
;;; placeholder rather than erroring — enough for callers that print or compare
;;; loosely, while honestly reflecting what the platform can tell us.

(defmethod get-local-name ((usocket stream-usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

;; hunchentoot's taskmaster calls GET-LOCAL-NAME on the LISTENER socket to
;; find where to open its shutdown-wake connection; without a method for the
;; server socket the listener setup errors and the server never serves.
;; Report the loopback address + the bound port — a self-connect to
;; 127.0.0.1:<port> reaches the listener regardless of the bind interface.
(defmethod get-local-name ((usocket stream-server-usocket))
  (values #(127 0 0 1)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defmethod get-local-address ((usocket usocket))
  #(0 0 0 0))

(defmethod get-peer-address ((usocket stream-usocket))
  #(0 0 0 0))

(defmethod get-local-port ((usocket stream-server-usocket))
  (ext:socket-local-port (socket usocket)))

(defmethod get-local-port ((usocket stream-usocket))
  0)

(defmethod get-peer-port ((usocket stream-usocket))
  0)

(defun get-hosts-by-name (name)
  ;; clamiga resolves names inside open-tcp-stream; it offers no standalone
  ;; resolver, so report none here.
  (declare (ignore name))
  nil)

(defun get-host-by-address (address)
  (declare (ignore address))
  nil)

;;; Input multiplexing.
;;;
;;; clamiga has no select()/poll() exposed to Lisp, so readiness is probed with
;;; LISTEN on each waiter's stream.  This is a best-effort, level-triggered
;;; check that is correct for blocking client use (drakma) and adequate for the
;;; single-acceptor server loop in the test fixtures.

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %clamiga-mark-ready (wait-list)
  "Probe each waiter with CL:LISTEN on its underlying clamiga stream, marking
ready ones :READ.  Returns T if any waiter became ready.

We use the base SOCKET accessor (not SOCKET-STREAM): on clamiga the stream IS
the socket, so the SOCKET slot holds the clamiga stream for BOTH connection
sockets (stream-usocket) and listening sockets (stream-server-usocket) —
whereas SOCKET-STREAM only exists on stream-usocket and would error on a
server socket, killing hunchentoot's accept loop.  For a listener,
(listen stream) is true exactly when a client connection is pending."
  (let ((any nil))
    (dolist (waiter (wait-list-waiters wait-list))
      (when (listen (socket waiter))
        (setf (state waiter) :read)
        (setf any t)))
    any))

(defun wait-for-input-internal (wait-list &key timeout)
  "Block until a waiter is ready, TIMEOUT seconds elapse, or — when TIMEOUT is
NIL — indefinitely.  clamiga exposes no select()/poll() over a fd set to Lisp,
so readiness is polled with CL:LISTEN; between polls we SLEEP briefly to YIELD
rather than busy-spin.  Busy-spinning here is not merely wasteful: each
usocket:wait-for-input conses a fresh wait-list, so a no-timeout spin allocates
hundreds of thousands of objects per second, and the resulting compacting-GC
storm starves the very I/O we are waiting on (hunchentoot's accept loop calls
us with no timeout).  A short sleep keeps the thread at a GC safepoint and lets
connections land."
  (when (%clamiga-mark-ready wait-list)
    (return-from wait-for-input-internal wait-list))
  ;; A zero timeout means "poll once and return" — caller already saw nothing.
  (when (and timeout (zerop timeout))
    (return-from wait-for-input-internal wait-list))
  (let ((deadline (when timeout
                    (+ (get-internal-real-time)
                       (round (* timeout internal-time-units-per-second))))))
    (loop
      (sleep 0.005)                     ; ~5ms yield between readiness polls
      (when (%clamiga-mark-ready wait-list)
        (return))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return))))
  wait-list)
