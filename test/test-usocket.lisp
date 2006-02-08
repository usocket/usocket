;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket-test)


(defparameter +non-existing-host+ "10.0.0.13")
(defparameter *soc1* (usocket::make-socket :socket :my-socket
                                           :stream :my-stream))

(deftest make-socket.1 (usocket:socket *soc1*) :my-socket)
(deftest make-socket.2 (usocket:socket-stream *soc1*) :my-stream)

(deftest socket-no-connect.1
  (catch 'caught-error
    (handler-bind ((usocket:usocket-error
                    #'(lambda (c) (throw 'caught-error nil))))
      (usocket:socket-connect "127.0.0.0" 80)
      t))
  nil)
(deftest socket-no-connect.2
  (catch 'caught-error
    (handler-bind ((usocket:usocket-error
                    #'(lambda (c) (throw 'caught-error nil))))
      (usocket:socket-connect #(127 0 0 0) 80)
      t))
  nil)
(deftest socket-no-connect.3
  (catch 'caught-error
    (handler-bind ((usocket:usocket-error
                    #'(lambda (c) (throw 'caught-error nil))))
      (usocket:socket-connect 2130706432 80) ;; == #(127 0 0 0)
      t))
  nil)

(deftest socket-failure.1
  (catch 'caught-error
    (handler-bind ((usocket:network-unreachable-error
                    #'(lambda (c) (throw 'caught-error nil)))
                   ;; cmu doesn't report as specific as above
                   #+(or cmu lispworks)
                   (usocket:unknown-error
                    #'(lambda (c) (throw 'caught-error nil)))
                   (condition
                    #'(lambda (c) (throw 'caught-error t))))
      (usocket:socket-connect 2130706432 80) ;; == #(127 0 0 0)
      :unreach))
  nil)
(deftest socket-failure.2
  (catch 'caught-error
    (handler-bind ((usocket:host-unreachable-error
                    #'(lambda (c) (throw 'caught-error nil)))
                   ;; cmu doesn't report as specific as above
                   #+(or cmu lispworks)
                   (usocket:unknown-error
                    #'(lambda (c) (throw 'caught-error nil)))
                   (condition
                    #'(lambda (c) (throw 'caught-error t))))
      (usocket:socket-connect +non-existing-host+ 80) ;; == #(127 0 0 0)
      :unreach))
  nil)


;; let's hope c-l.net doesn't move soon, or that people start to
;; test usocket like crazy..
(deftest socket-connect.1
  (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
    (unwind-protect
        (typep sock 'usocket:usocket)
      (usocket:socket-close sock)))
  t) 
(deftest socket-connect.2
  (let ((sock (usocket:socket-connect #(65 110 12 237) 80)))
    (unwind-protect
        (typep sock 'usocket:usocket)
      (usocket:socket-close sock)))
  t)
(deftest socket-connect.3
  (let ((sock (usocket:socket-connect 1097731309 80)))
    (unwind-protect
        (typep sock 'usocket:usocket)
      (usocket:socket-close sock)))
  t)

;; let's hope c-l.net doesn't change its software any time soon
(deftest socket-stream.1
  (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
    (unwind-protect
        (progn
          (format (usocket:socket-stream sock)
                  "GET / HTTP/1.0~A~A~A~A"
                  #\Return #\Newline #\Return #\Newline)
          (force-output (usocket:socket-stream sock))
          (read-line (usocket:socket-stream sock)))
      (usocket:socket-close sock)))
  #+clisp "HTTP/1.1 200 OK"
  #-clisp #.(format nil "HTTP/1.1 200 OK~A" #\Return) nil)


(defun run-usocket-tests ()
  (do-tests))
