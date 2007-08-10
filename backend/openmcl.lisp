;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :execute)
  ;; also present in OpenMCL l1-sockets.lisp
  #+linuxppc-target
  (require "LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWIN-SYSCALLS")
  #+darwinx86-target
  (require "DARWINX8664-SYSCALLS"))

(defun get-host-name ()
  (ccl::%stack-block ((resultbuf 256))
    (when (zerop (#_gethostname resultbuf 256))
      (ccl::%get-cstring resultbuf))))

(defparameter +openmcl-error-map+
  '((:address-in-use . address-in-use-error)
    (:connection-aborted . connection-aborted-error)
    (:no-buffer-space . no-buffers-error)
    (:connection-timed-out . timeout-error)
    (:connection-refused . connection-refused-error)
    (:host-unreachable . host-unreachable-error)
    (:host-down . host-down-error)
    (:network-down . network-down-error)
    (:address-not-available . address-not-available-error)
    (:network-reset . network-reset-error)
    (:connection-reset . connection-reset-error)
    (:shutdown . shutdown-error)
    (:access-denied . operation-not-permitted-error)))


;; we need something which the openmcl implementors 'forgot' to do:
;; wait for more than one socket-or-fd

(defun input-available-p (sockets &optional ticks-to-wait)
  (ccl::rletZ ((tv :timeval))
    (ccl::ticks-to-timeval ticks-to-wait tv)
    (ccl::%stack-block ((infds ccl::*fd-set-size*))
      (ccl::fd-zero infds)
      (let ((max-fd -1))
        (dolist (sock sockets)
          (let ((fd (openmcl-socket:socket-os-fd sock)))
            (setf max-fd (max max-fd fd))
            (ccl::fd-set fd infds)))
        (let* ((res (ccl::syscall syscalls::select (1+ max-fd)
                                  infds (ccl::%null-ptr) (ccl::%null-ptr)
                                  (if ticks-to-wait tv (ccl::%null-ptr)))))
          (when (> res 0)
            (remove-if #'(lambda (x)
                           (not (ccl::fd-is-set (openmcl-socket:socket-os-fd x)
                                                infds)))
                       sockets)))))))

(defun raise-error-from-id (condition-id socket real-condition)
  (let ((usock-err (cdr (assoc condition-id +openmcl-error-map+))))
    (if usock-err
        (error usock-err :socket socket)
      (error 'unknown-error :socket socket :real-error real-condition))))

(defun handle-condition (condition &optional socket)
  (typecase condition
    (openmcl-socket:socket-error
     (raise-error-from-id (openmcl-socket:socket-error-identifier condition)
                          socket condition))
    (ccl::socket-creation-error #| ugh! |#
     (raise-error-from-id (ccl::socket-creationg-error-identifier condition)
                          socket condition))
    (error (error 'unknown-error :socket socket :real-error condition))
    (condition (signal 'unknown-condition :real-condition condition))))

(defun to-format (element-type)
  (if (subtypep element-type 'character)
      :text
    :binary))

(defun socket-connect (host port &key (element-type 'character))
  (with-mapped-conditions ()
     (let ((mcl-sock
	     (openmcl-socket:make-socket :remote-host (host-to-hostname host)
                                         :remote-port port
					 :format (to-format element-type))))
        (openmcl-socket:socket-connect mcl-sock)
        (make-stream-socket :stream mcl-sock :socket mcl-sock))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (sock (apply #'openmcl-socket:make-socket
                      (append (list :connect :passive
                                    :reuse-address reuseaddress
                                    :local-port port
                                    :backlog backlog
                                    :format (to-format element-type))
                              (when (ip/= host *wildcard-host*)
                                (list :local-host host))))))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (declare (ignore element-type)) ;; openmcl streams are bi/multivalent
  (let ((sock (openmcl-socket:accept-connection (socket usocket))))
    (make-stream-socket :socket sock :stream sock)))

;; One close method is sufficient because sockets
;; and their associated objects are represented
;; by the same object.
(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod get-local-address ((usocket usocket))
  (hbo-to-vector-quad (openmcl-socket:local-host (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (hbo-to-vector-quad (openmcl-socket:remote-host (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (openmcl-socket:local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (openmcl-socket:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defun get-host-by-address (address)
  (with-mapped-conditions ()
     (openmcl-socket:ipaddr-to-hostname (host-to-hbo address))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (list (hbo-to-vector-quad (openmcl-socket:lookup-hostname
                                (host-to-hostname name))))))

(defun wait-for-input-internal (sockets &key timeout)
  (with-mapped-conditions ()
    (let* ((ticks-timeout (truncate (* (or timeout 1) ccl::*ticks-per-second*)))
           (active-internal-sockets
            (input-available-p (mapcar #'socket sockets)
                               (when timeout ticks-timeout))))
      ;; this is quadratic, but hey, the active-internal-sockets
      ;; list is very short and it's only quadratic in the length of that one.
      ;; When I have more time I could recode it to something of linear
      ;; complexity.
      ;; [Same code is also used in lispworks.lisp, allegro.lisp]
      (remove-if #'(lambda (x)
                     (not (member (socket x) active-internal-sockets)))
                 sockets))))


