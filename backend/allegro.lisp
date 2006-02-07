;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

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

(defun socket-connect (host port &optional (type :stream))
  (declare (ignore type))
  (let ((socket))
    (setf socket
          (with-mapped-conditions (socket)
             (socket:make-socket :remote-host (host-to-hostname host)
                                 :remote-port port)))
    (make-socket :socket socket :stream socket)))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (close (socket usocket))))



(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (socket:ipaddr-to-hostname address)))

(defun get-hosts-by-name (name)
  ;;###FIXME: ACL has the acldns module which returns all A records
  ;; only problem: it doesn't fall back to tcp (from udp) if the returned
  ;; structure is too long.
  (with-mapped-conditions ()
    (list (hbo-to-vector-quad (socket:lookup-hostname name)))))
