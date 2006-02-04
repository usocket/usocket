;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defparameter +allegro-identifier-error-map+
  '((:address-in-use . usocket-address-in-use-error)
    (:address-not-available . usocket-address-not-available-error)
    (:network-down . usocket-network-down-error)
    (:network-reset . usocket-network-reset-error)
;;    (:connection-aborted . ) FIXME: take these 2 errors in the supported list
;;    (:connection-reset . )
    (:no-buffer-space . usocket-no-buffers-error)
    (:shutdown . usocket-shutdown-error)
    (:connection-timed-out . usocket-timeout-error)
    (:connection-refused . usocket-connection-refused-error)
    (:host-down . usocket-host-down-error)
    (:host-unreachable . usocket-host-unreachable-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (socket-error (let ((usock-err
                         (cdr (assoc (stream-error-identifier condition)
                                     +allegro-identifier-error-map+))))
                    (if usock-err
                        (error usock-err :socket socket)
                      (error 'usocket-unknown-error
                             :real-condition condition
                             :socket socket))))))

(defun socket-connect (host port &optional (type :stream))
  (declare (ignore type))
  (let ((socket))
    (setf socket
          (with-mapped-conditions (socket)
             (sock:make-socket :remote-host (host-to-hostname host)
                               :remote-port port)))
    (make-socket :socket socket :stream socket)))

(defmethod close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (sock:close (socket usocket))))



(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (sock:ipaddr-to-hostname address)))

(defun get-hosts-by-name (name)
  ;;###FIXME: ACL has the acldns module which returns all A records
  ;; only problem: it doesn't fall back to tcp (from udp) if the returned
  ;; structure is too long.
  (with-mapped-conditions ()
    (list (hbo-to-vector-quad (sock:lookup-hostname name)))))
