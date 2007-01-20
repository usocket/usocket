;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)



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
                           (backlog 5)
                           (element-type 'character))
  (let* ((sock (apply #'openmcl-socket:make-socket
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

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defmethod get-local-address ((usocket usocket))
  (hbo-to-vector-quad (openmcl-socket:local-host (socket usocket))))

(defmethod get-peer-address ((usocket usocket))
  (hbo-to-vector-quad (openmcl-socket:remote-host (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (openmcl-socket:local-port (socket usocket)))

(defmethod get-peer-port ((usocket usocket))
  (openmcl-socket:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defun get-host-by-address (address)
  (with-mapped-conditions ()
     (openmcl-socket:ipaddr-to-hostname (host-to-hbo address))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (list (hbo-to-vector-quad (openmcl-socket:lookup-hostname
                                (host-to-hostname name))))))
