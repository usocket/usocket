;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun remap-maybe-for-win32 (z &optional errorp)
  (mapcar #'(lambda (x)
              (list #-win32 (car x)
                    #+win32 (mapcar #'(lambda (y)
                                        (+ 10000 y))
                                    (car x))
                    (cdr x)
                    errorp))
          z))

(defparameter +clisp-error-map+
  (append (remap-maybe-for-win32 +unix-errno-condition-map+)
          (remap-maybe-for-win32 +unix-errno-error-map+ t)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (system::simple-os-error
       (destructuring-bind
           (&optional usock-err errorp)
           (cdr (assoc (car (system::$format-arguments))
                       +clisp-error-map+))
         (if usock-err
             (if errorp
                 (error usock-err :socket socket)
               (signal usock-err :socket socket))
           (error 'usocket-unkown-error
                  :socket socket
                  :real-error condition))))))

(defun socket-connect (host port &optional (type :stream))
  (declare (ignore type))
  (let ((socket (socket:socket-connect port (host-to-hostname host)
                                       :element-type 'character
                                       :buffered t)))
    (make-socket :socket socket
                 :stream socket))) ;; the socket is a stream too
;;                 :host host
;;                 :port port))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (close (socket usocket)))



(defun get-host-by-address (address)
  (handler-case
   (posix:hostent-name
    (posix:resolve-host-ipaddr (vector-quad-to-dotted-quad address)))
   (condition (condition) (handle-condition condition))))

(defun get-hosts-by-name (name)
  (handler-case
   (mapcar #'dotted-quad-to-vector-quad
           (posix:hostent-addr-list (posix:resolve-host-ipaddr name)))
   (condition (condition) (handle-condition condition))))

