;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)


#+win32
(defun remap-maybe-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +lispworks-error-map+
  #+win32
  (append (remap-for-win32 +unix-errno-condition-map+)
          (remap-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))



(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (simple-error (destructuring-bind (&optional host port err-msg errno)
                      (simple-condition-format-arguments condition)
                    (declare (ignore host port err-msg))
                    (let* ((usock-err
                            (cdr (assoc errno +lispworks-error-map+
                                        :test #'member))))
                      (if usock-err
                          (if (subtypep usock-err 'error)
                              (error usock-err :socket socket)
                            (signal usock-err :socket socket))
                        (error 'unknown-error
                               :socket socket
                               :real-error condition)))))))
;;     (condition (error 'usocket-error
;;                       :real-condition condition
;;                       :socket socket))))

(defun socket-connect (host port &optional (type :stream))
  (declare (ignore type))
  (let ((hostname (host-to-hostname host))
        (stream))
    (setf stream
          (with-mapped-conditions ()
             (comm:open-tcp-stream host port)))
    (make-socket :socket (comm:socket-stream-socket stream)
                 :stream stream)))
;;                 :host host
;;                 :port port))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (close (stream usocket)))

