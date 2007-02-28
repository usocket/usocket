;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defparameter *wildcard-host* #(0 0 0 0)
  "Hostname to pass when all interfaces in the current system are to be bound.")

(defparameter *auto-port* 0
  "Port number to pass when an auto-assigned port number is wanted.")

(defclass usocket ()
  ((socket
    :initarg :socket
    :accessor socket
    :documentation "Implementation specific socket object instance."))
  (:documentation
"The main socket class."))

(defclass stream-usocket (usocket)
   ((stream
     :initarg :stream
     :accessor socket-stream
     :documentation "Stream instance associated with the socket.

Iff an external-format was passed to `socket-connect' or `socket-listen'
the stream is a flexi-stream. Otherwise the stream is implementation
specific."))
   (:documentation ""))

(defclass stream-server-usocket (usocket)
  ((element-type
    :initarg :element-type
    :initform #-lispworks 'character
              #+lispworks 'base-char
    :reader element-type
    :documentation "Default element type for streams created by
`socket-accept'."))
  (:documentation "Socket which listens for stream connections to
be initiated from remote sockets."))

;;Not in use yet:
;;(defclass datagram-usocket (usocket)
;;  ()
;;  (:documentation ""))

(defun make-socket (&key socket)
  "Create a usocket socket type from implementation specific socket."
  (unless socket
    (error 'invalid-socket))
  (make-stream-socket :socket socket))

(defun make-stream-socket (&key socket stream)
  "Create a usocket socket type from implementation specific socket
and stream objects."
  (unless socket
    (error 'invalid-socket-error))
  (unless stream
    (error 'invalid-socket-stream-error))
  (make-instance 'stream-usocket
                 :socket socket
                 :stream stream))

(defun make-stream-server-socket (socket &key (element-type
                                               #-lispworks 'character
                                               #+lispworks 'base-char))
  "Create a usocket-server socket type from an
implementation-specific socket object.

The returned value is a subtype of `stream-server-usocket'."
  (make-instance 'stream-server-usocket
                 :socket socket
                 :element-type element-type))

(defgeneric socket-close (usocket)
  (:documentation "Close a previously opened `usocket'."))

(defgeneric get-local-address (socket)
  (:documentation "Returns the IP address of the socket."))

(defgeneric get-peer-address (socket)
  (:documentation
   "Returns the IP address of the peer the socket is connected to."))

(defgeneric get-local-port (socket)
  (:documentation "Returns the IP port of the socket.

This function applies to both `stream-usocket' and `server-stream-usocket'
type objects."))

(defgeneric get-peer-port (socket)
  (:documentation "Returns the IP port of the peer the socket to."))

(defgeneric get-local-name (socket)
  (:documentation "Returns the IP address and port of the socket as values.

This function applies to both `stream-usocket' and `server-stream-usocket'
type objects."))

(defgeneric get-peer-name (socket)
  (:documentation
   "Returns the IP address and port of the peer
the socket is connected to as values."))

(defmacro with-connected-socket ((var socket) &body body)
  "Bind `socket' to `var', ensuring socket destruction on exit.

`body' is only evaluated when `var' is bound to a non-null value.

The `body' is an implied progn form."
  `(let ((,var ,socket))
     (unwind-protect
         (when ,var
           ,@body)
       (when ,var
         (socket-close ,var)))))

(defmacro with-client-socket ((socket-var stream-var &rest socket-connect-args)
                              &body body)
  "Bind the socket resulting from a call to `socket-connect' with
the arguments `socket-connect-args' to `socket-var' and if `stream-var' is
non-nil, bind the associated socket stream to it."
  `(with-connected-socket (,socket-var (socket-connect ,@socket-connect-args))
       ,(if (null stream-var)
           `(progn ,@body)
          `(let ((,stream-var (socket-stream ,socket-var)))
             ,@body))))

(defmacro with-server-socket ((var server-socket) &body body)
  "Bind `server-socket' to `var', ensuring socket destruction on exit.

`body' is only evaluated when `var' is bound to a non-null value.

The `body' is an implied progn form."
  `(with-connected-socket (var server-socket)
      ,@body))

(defmacro with-socket-listener ((socket-var &rest socket-listen-args)
                                &body body)
  "Bind the socket resulting from a call to `socket-listen' with arguments
`socket-listen-args' to `socket-var'."
  `(with-server-socket (,socket-var (socket-listen ,@socket-listen-args))
      ,@body))


;;
;; IP(v4) utility functions
;;

(defun list-of-strings-to-integers (list)
  "Take a list of strings and return a new list of integers (from
parse-integer) on each of the string elements."
  (let ((new-list nil))
    (dolist (element (reverse list))
      (push (parse-integer element) new-list))
    new-list))

(defun hbo-to-dotted-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (format nil "~A.~A.~A.~A" first second third fourth)))

(defun hbo-to-vector-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (vector first second third fourth)))

(defun vector-quad-to-dotted-quad (vector)
  (format nil "~A.~A.~A.~A"
          (aref vector 0)
          (aref vector 1)
          (aref vector 2)
          (aref vector 3)))

(defun dotted-quad-to-vector-quad (string)
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (vector (first list) (second list) (third list) (fourth list))))

(defgeneric host-byte-order (address))
(defmethod host-byte-order ((string string))
  "Convert a string, such as 192.168.1.1, to host-byte-order,
such as 3232235777."
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (+ (* (first list) 256 256 256) (* (second list) 256 256)
       (* (third list) 256) (fourth list))))

(defmethod host-byte-order ((vector vector))
  "Convert a vector, such as #(192 168 1 1), to host-byte-order, such as
3232235777."
  (+ (* (aref vector 0) 256 256 256) (* (aref vector 1) 256 256)
     (* (aref vector 2) 256) (aref vector 3)))

(defmethod host-byte-order ((int integer))
  int)

(defun host-to-hostname (host)
  "Translate a string or vector quad to a stringified hostname."
  (etypecase host
    (string host)
    ((vector t 4) (vector-quad-to-dotted-quad host))
    (integer (hbo-to-dotted-quad host))))

(defun ip= (ip1 ip2)
  (etypecase ip1
    (string (string= ip1 (host-to-hostname ip2)))
    ((vector t 4) (or (eq ip1 ip2)
                      (and (= (aref ip1 0) (aref ip2 0))
                           (= (aref ip1 1) (aref ip2 1))
                           (= (aref ip1 2) (aref ip2 2))
                           (= (aref ip1 3) (aref ip2 3)))))
    (integer (= ip1 (host-byte-order ip2)))))

(defun ip/= (ip1 ip2)
  (not (ip= ip1 ip2)))

;;
;; DNS helper functions
;;

#-(or clisp armedbear)
(progn
  (defun get-host-by-name (name)
    (let ((hosts (get-hosts-by-name name)))
      (car hosts)))

  (defun get-random-host-by-name (name)
    (let ((hosts (get-hosts-by-name name)))
      (elt hosts (random (length hosts)))))

  (defun host-to-vector-quad (host)
    "Translate a host specification (vector quad, dotted quad or domain name)
to a vector quad."
    (etypecase host
      (string (let* ((ip (ignore-errors
                           (dotted-quad-to-vector-quad host))))
                (if (and ip (= 4 (length ip)))
                    ;; valid IP dotted quad?
                    ip
                  (get-random-host-by-name host))))
      ((vector t 4) host)
      (integer (hbo-to-vector-quad host))))

  (defun host-to-hbo (host)
    (etypecase host
      (string (let ((ip (ignore-errors
                          (dotted-quad-to-vector-quad host))))
                (if (and ip (= 4 (length ip)))
                    (host-byte-order ip)
            (host-to-hbo (get-host-by-name host)))))
      ((vector t 4) (host-byte-order host))
      (integer host))))

;;
;; Setting of documentation for backend defined functions
;;

;; Documentation for the function
;;
;; (defun SOCKET-CONNECT (host port) ..)
;;

(setf (documentation 'socket-connect 'function)
      "Connect to `host' on `port'.  `host' is assumed to be a string or
an IP address represented in vector notation, such as #(192 168 1 1).
`port' is assumed to be an integer.

Returns a usocket object.")

;; Documentation for the function
;;
;; (defun SOCKET-LISTEN (host port &key reuseaddress backlog) ..)
;;###FIXME: extend with default-element-type
(setf (documentation 'socket-listen 'function)
      "Bind to interface `host' on `port'. `host' should be the
representation of an interface address.  The implementation is not
required to do an address lookup, making no guarantees that hostnames
will be correctly resolved.  If `*wildcard-host*' is passed for `host',
the socket will be bound to all available interfaces for the IPv4
protocol in the system.  `port' can be selected by the IP stack by
passing `*auto-port*'.

Returns an object of type `stream-server-usocket'.

`reuseaddress' and `backlog' are advisory parameters for setting socket
options at creation time. `element-type' is the element type of the
streams to be created by `socket-accept'.
")

;; Documentation for the function
;;
;; (defun SOCKET-ACCEPT (socket &key element-type)
(setf (documentation 'socket-accept 'function)
      "Accepts a connection from `socket', returning a `stream-socket'.

The stream associated with the socket returned has `element-type' when
explicitly specified, or the element-type passed to `socket-listen' otherwise.")
