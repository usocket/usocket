;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)


;; CMUCL error handling is brain-dead: it doesn't preserve any
;; information other than the OS error string from which the
;; error can be determined. The OS error string isn't good enough
;; given that it may have been localized (l10n).
;;
;; Just catch the errors and encapsulate them in an unknown-error
(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (simple-error (error 'unknown-error
                         :real-condition condition
                         :socket socket))))

(defun socket-connect (host port &optional (type :stream))
  (let* ((socket))
    (setf socket
          (with-mapped-conditions (socket)
             (ext:connect-to-inet-socket (host-byte-order host) port type)))
    (let* ((stream (sys:make-fd-stream socket :input t :output t
                                       :element-type 'character
                                       :buffering :full))
           ;;###FIXME the above line probably needs an :external-format
           (usocket (make-socket :socket socket
                                 :host host :port port :stream stream)))
      usocket)))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (ext:close-socket (socket usocket))))



(defun get-host-by-address (address)
  (handler-case (ext:host-entry-name
                 (ext::lookup-host-entry (host-byte-order address)))
    (condition (condition) (handle-condition condition))))

(defun get-host-by-name (name)
  (handler-case (mapcar #'hbo-to-vector-quad
                        (ext:host-entry-addr-list
                         (ext:lookup-host-entry name)))
    (condition (condition) (handle-condition condition))))

