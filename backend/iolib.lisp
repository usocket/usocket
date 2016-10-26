;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:load-toplevel :execute)
  (setq *backend* :iolib))

(defun get-host-name ()
  )

(defparameter +iolib-error-map+
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
  )

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline
                       (nodelay t) ;; nodelay == t is the ACL default
                       local-host local-port)
  )

(defmethod socket-close ((usocket usocket))
  )

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  )

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  )

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  )

(defmethod get-local-address ((usocket usocket))
  )

(defmethod get-peer-address ((usocket stream-usocket))
  )

(defmethod get-local-port ((usocket usocket))
  )

(defmethod get-peer-port ((usocket stream-usocket))
  )

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  )

(defmethod socket-receive ((socket datagram-usocket) buffer length &key)
  )

(defun get-host-by-address (address)
  )

(defun get-hosts-by-name (name)
  )

(defun %setup-wait-list (wait-list)
  )

(defun %add-waiter (wait-list waiter)
  )

(defun %remove-waiter (wait-list waiter)
  )

(defun wait-for-input-internal (wait-list &key timeout)
  )
