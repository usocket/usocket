;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)


;; utility routine for looking up the current host name
(FFI:DEF-CALL-OUT get-host-name-internal
         (:name "gethostname")
         (:arguments (name (FFI:C-PTR (FFI:C-ARRAY-MAX ffi:character 256))
                           :OUT :ALLOCA)
                     (len ffi:int))
         #+win32 (:library "WS2_32")
         (:language #-win32 :stdc
                    #+win32 :stdc-stdcall)
         (:return-type ffi:int))


(defun get-host-name ()
  (multiple-value-bind (retcode name)
      (get-host-name-internal 256)
    (when (= retcode 0)
      name)))


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

(defun socket-connect (host port &key (element-type 'character))
  (let ((socket)
        (hostname (host-to-hostname host)))
    (with-mapped-conditions (socket)
       (setf socket
             (socket:socket-connect port hostname
                                    :element-type element-type
                                    :buffered t)))
    (make-stream-socket :socket socket
                        :stream socket))) ;; the socket is a stream too

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
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod socket-close ((usocket stream-server-usocket))
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


(defmethod wait-for-input-internal (sockets &key timeout)
  (with-mapped-conditions ()
    (multiple-value-bind
        (secs musecs)
        (split-timeout (or timeout 1))
      (let* ((request-list (mapcar #'(lambda (x)
                                       (if (stream-server-usocket-p x)
                                           (socket x)
                                         (list (socket x) :input)))
                                   sockets))
             (status-list (if timeout
                              (socket:socket-status request-list secs musecs)
                            (socket:socket-status request-list))))
        (remove nil
                (mapcar #'(lambda (x y)
                            (when y x))
                        sockets status-list))))))


;;
;; UDP/Datagram sockets!
;;

#+rawsock
(progn

  (defun make-sockaddr_in ()
    (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))

  (declaim (inline fill-sockaddr_in))
  (defun fill-sockaddr_in (sockaddr_in ip port)
    (port-to-octet-buffer sockaddr_in port)
    (ip-to-octet-buffer sockaddr_in ip :start 2)
    sockaddr_in)

  (defun socket-receive (socket buffer &key (size (length buffer)))
    "Returns the buffer, the number of octets copied into the buffer (received)
and the address of the sender as values."
    (let* ((sock (socket socket))
           (sockaddr (when (not (connected-p socket))
                       (rawsock:make-sockaddr)))
           (rv (if sockaddr
                   (rawsock:recvfrom sock buffer sockaddr
                                     :start 0
                                     :end size)
                   (rawsock:recv sock buffer
                                 :start 0
                                 :end size))))
      (values buffer
              rv
              (list (ip-from-octet-buffer (sockaddr-data sockaddr) 4)
                    (port-from-octet-buffer (sockaddr-data sockaddr) 2)))))

  (defun socket-send (socket buffer &key address (size (length buffer)))
    "Returns the number of octets sent."
    (let* ((sock (socket socket))
           (sockaddr (when address
                       (rawsock:make-sockaddr :INET
                                              (fill-sockaddr_in
                                               (make-sockaddr_in)
                                               (host-byte-order
                                                (second address))
                                               (first address)))))
           (rv (if address
                   (rawsock:sendto sock buffer sockaddr
                                   :start 0
                                   :end size)
                   (rawsock:send sock buffer
                                 :start 0
                                 :end size))))
      rv))

  (defmethod socket-close ((usocket datagram-usocket))
    (rawsock:sock-close (socket usocket)))
  
  )

#-rawsock
(progn
  (warn "This image doesn't contain the RAWSOCK package.
To enable UDP socket support, please be sure to use the -Kfull parameter
at startup, or to enable RAWSOCK support during compilation.")

  )