;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.
(in-package :usocket-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *wait-for-input-timeout* 2))

(deftest wait-for-input.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80))
          (time (get-universal-time)))
      (unwind-protect
          (progn (usocket:wait-for-input sock :timeout *wait-for-input-timeout*)
            (- (get-universal-time) time))
        (usocket:socket-close sock))))
  #.*wait-for-input-timeout*)

(deftest wait-for-input.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80))
          (time (get-universal-time)))
      (unwind-protect
          (progn (usocket:wait-for-input sock :timeout *wait-for-input-timeout* :ready-only t)
            (- (get-universal-time) time))
        (usocket:socket-close sock))))
  #.*wait-for-input-timeout*)

(deftest wait-for-input.3
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (progn
            (format (usocket:socket-stream sock)
                    "GET / HTTP/1.0~2%")
            (force-output (usocket:socket-stream sock))
            (usocket:wait-for-input sock :timeout *wait-for-input-timeout*)
            (subseq (read-line (usocket:socket-stream sock)) 0 15))
        (usocket:socket-close sock))))
  "HTTP/1.1 200 OK")

;;; Advanced W-F-I tests by Elliott Slaughter <elliottslaughter@gmail.com>

(defvar *socket-server-port* 12345)
(defvar *socket-server-listen* nil)
(defvar *socket-server-connection*)
(defvar *socket-client-connection*)
(defvar *output-p* t)

(defun stage-1 ()
  (unless *socket-server-listen*
    (setf *socket-server-listen*
	  (socket-listen *wildcard-host* *socket-server-port* :element-type '(unsigned-byte 8))))

  (setf *socket-server-connection*
	(when (usocket:wait-for-input *socket-server-listen* :timeout 0 :ready-only t)
	  (socket-accept *socket-server-listen*)))
  
  (when *output-p* ; should be NIL
    (format t "First time (before client connects) is ~s.~%"
	    *socket-server-connection*)))

(defun stage-2 ()
  (setf *socket-client-connection*
	(socket-connect "localhost" *socket-server-port* :protocol :stream
			:element-type '(unsigned-byte 8) :timeout 5)) ; old timeout (0) is too small

  (setf *socket-server-connection*
	(when (usocket:wait-for-input *socket-server-listen* :timeout 0 :ready-only t)
	  (socket-accept *socket-server-listen*)))

  (when *output-p* ; should be a usocket object
    (format t "Second time (after client connects) is ~s.~%"
	    *socket-server-connection*)))

(defun stage-3 ()
  (setf *socket-server-connection*
	(when (wait-for-input *socket-server-listen* :timeout 0 :ready-only t)
	  (socket-accept *socket-server-listen*)))

  (when *output-p* ; should be NIL again
    (format t "Third time (before second client) is ~s.~%"
	    *socket-server-connection*)))

(deftest elliott-slaughter.1
    (let ((*output-p* nil))
      (let* ((s-1 (stage-1)) (s-2 (stage-2)) (s-3 (stage-3)))
	(prog1 (and (null s-1) (usocket::usocket-p s-2) (null s-3))
	  (socket-close *socket-server-listen*)
	  (setf *socket-server-listen* nil))))
  t)
