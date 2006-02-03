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
           (cdr (assoc (car (simple-condition-format-arguments condition))
                       +clisp-error-map+ :test #'member))
         (if usock-err
             (if errorp
                 (error usock-err :socket socket)
               (signal usock-err :socket socket))
           (error 'usocket-unknown-error
                  :socket socket
                  :real-error condition))))))

(defun socket-connect (host port &optional (type :stream))
  (declare (ignore type))
  (let ((socket)
        (hostname (host-to-hostname host)))
    (with-mapped-conditions (socket)
       (setf socket
             (socket:socket-connect port hostname
                                    :element-type 'character
                                    :buffered t)))
    (make-socket :socket socket
                 :stream socket))) ;; the socket is a stream too
;;                 :host host
;;                 :port port))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

