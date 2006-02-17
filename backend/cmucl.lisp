;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

#+win32
(defun remap-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +cmucl-error-map+
  #+win32
  (append (remap-for-win32 +unix-errno-condition-map+)
          (remap-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))

(defun cmucl-map-socket-error (err &key condition socket)
  (let ((usock-err
         (cdr (assoc err +cmucl-error-map+ :test #'member))))
    (if usock-err
        (if (subtypep usock-err 'error)
            (error usock-err :socket socket)
          (signal usock-err :socket socket))
      (error 'unknown-error
             :socket socket
             :real-error condition))))

;; CMUCL error handling is brain-dead: it doesn't preserve any
;; information other than the OS error string from which the
;; error can be determined. The OS error string isn't good enough
;; given that it may have been localized (l10n).
;;
;; The above applies to versions pre 19b; 19d and newer are expected to
;; contain even better error reporting.
;;
;;
;; Just catch the errors and encapsulate them in an unknown-error
(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (ext::socket-error (cmucl-map-socket-error (ext::socket-errno condition)
                                               :socket socket
                                               :condition condition))
    (simple-error (error 'unknown-error
                         :real-condition condition
                         :socket socket))))

(defun socket-connect (host port)
  (let* ((socket))
    (setf socket
          (with-mapped-conditions (socket)
             (ext:connect-to-inet-socket (host-to-hbo host) port :stream)))
    (if socket
        (let* ((stream (sys:make-fd-stream socket :input t :output t
                                           :element-type 'character
                                           :buffering :full))
               ;;###FIXME the above line probably needs an :external-format
               (usocket (make-socket :socket socket
                                     :stream stream)))
          usocket)
      (let ((err (unix:unix-errno)))
        (when err (cmucl-map-socket-error err))))))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (ext:close-socket (socket usocket))))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (ext:get-socket-host-and-port (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (ext:get-peer-host-and-port (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket usocket))
  (nth-value 1 (get-peer-name usocket)))


(defun get-host-by-address (address)
  (handler-case (ext:host-entry-name
                 (ext::lookup-host-entry (host-byte-order address)))
    (condition (condition) (handle-condition condition))))

(defun get-hosts-by-name (name)
  (handler-case (mapcar #'hbo-to-vector-quad
                        (ext:host-entry-addr-list
                         (ext:lookup-host-entry name)))
    (condition (condition) (handle-condition condition))))

