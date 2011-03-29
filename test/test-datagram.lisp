;;;; $Id$
;;;; $URL$

(in-package :usocket-test)

(defvar *echo-server*
  (usocket:socket-server "127.0.0.1" 10243 #'identity nil
			 :in-new-thread t
			 :protocol :datagram))

(defparameter *max-buffer-size* 32)

(defvar *send-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *receive-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defun clean-buffers ()
  (fill *send-buffer* 0)
  (fill *receive-buffer* 0))

;;; UDP Send Test #1: connected socket
(deftest udp-send.1
    (let ((s (usocket:socket-connect "127.0.0.1" 10243 :protocol :datagram)))
      (clean-buffers)
      (replace *send-buffer* #(1 2 3 4 5))
      (usocket:socket-send s *send-buffer* 5)
      (usocket:wait-for-input s :timeout 3)
      (multiple-value-bind (buffer size host port)
	  (usocket:socket-receive s *receive-buffer* *max-buffer-size*)
	(reduce #'+ *receive-buffer* :start 0 :end 5)))
  15)

;;; UDP Send Test #2: unconnected socket
(deftest udp-send.2
    (let ((s (usocket:socket-connect nil nil :protocol :datagram)))
      (clean-buffers)
      (replace *send-buffer* #(1 2 3 4 5))
      (usocket:socket-send s *send-buffer* 5 :host "127.0.0.1" :port 10243)
      (usocket:wait-for-input s :timeout 3)
      (multiple-value-bind (buffer size host port)
	  (usocket:socket-receive s *receive-buffer* *max-buffer-size*)
	(reduce #'+ *receive-buffer* :start 0 :end 5)))
  15)
