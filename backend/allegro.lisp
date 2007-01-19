;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock))

(defparameter +allegro-identifier-error-map+
  '((:address-in-use . address-in-use-error)
    (:address-not-available . address-not-available-error)
    (:network-down . network-down-error)
    (:network-reset . network-reset-error)
    (:network-unreachable . network-unreachable-error)
    (:connection-aborted . connection-aborted-error)
    (:connection-reset . connection-reset-error)
    (:no-buffer-space . no-buffers-error)
    (:shutdown . shutdown-error)
    (:connection-timed-out . timeout-error)
    (:connection-refused . connection-refused-error)
    (:host-down . host-down-error)
    (:host-unreachable . host-unreachable-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (excl:socket-error
     (let ((usock-err
            (cdr (assoc (excl:stream-error-identifier condition)
                        +allegro-identifier-error-map+))))
       (if usock-err
           (error usock-err :socket socket)
         (error 'unknown-error
                :real-error condition
                :socket socket))))))

(defun to-format (element-type)
  (if (subtypep element-type 'character)
      :text
    :binary))

(defun socket-connect (host port &key (element-type 'character))
  (let ((socket))
    (setf socket
          (with-mapped-conditions (socket)
             (socket:make-socket :remote-host (host-to-hostname host)
                                 :remote-port port
                                 :format (to-format element-type))))
    (make-stream-socket :socket socket :stream socket)))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defun socket-listen (host port
                           &key reuseaddress
                           (backlog 5)
                           (element-type 'character))
  ;; Allegro and OpenMCL socket interfaces bear very strong resemblence
  ;; whatever you change here, change it also for OpenMCL
  (let ((sock (with-mapped-conditions ()
                 (apply #'socket:make-socket
                        (append (list :connect :passive
                                      :reuse-address reuseaddress
                                      :local-port port
                                      :backlog backlog
                                      :format (to-format element-type)
                                      ;; allegro now ignores :format
                                      )
                                (when (not (eql host *wildcard-host*))
                                           (list :local-host host)))))))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket))
  (let ((stream-sock (socket:accept-connection (socket socket))))
    (make-stream-socket :socket stream-sock :stream stream-sock)))

(defmethod get-local-address ((usocket usocket))
  (hbo-to-vector-quad (socket:local-host (socket usocket))))

(defmethod get-peer-address ((usocket stream-server-usocket))
  (hbo-to-vector-quad (socket:remote-host (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (socket:local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-server-usocket))
  (socket:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-server-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))


(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (socket:ipaddr-to-hostname (host-to-hbo address))))

(defun get-hosts-by-name (name)
  ;;###FIXME: ACL has the acldns module which returns all A records
  ;; only problem: it doesn't fall back to tcp (from udp) if the returned
  ;; structure is too long.
  (with-mapped-conditions ()
    (list (hbo-to-vector-quad (socket:lookup-hostname
                               (host-to-hostname name))))))
