;;;; -*- Mode: Lisp -*-
;;;; $Id$

;;;; Clozure CL's socket function SEND-TO doesn't support operations on connected UDP sockets.

(in-package :ccl)

(defun c_send-for-usocket (sockfd msgptr len flags)
  (ignoring-eintr (check-socket-error (#_send sockfd msgptr len flags))))

(defun send-for-usocket (socket msg size &key offset)
  "Send a UDP packet over a connected socket."
  (let ((fd (socket-device socket)))
    (multiple-value-setq (msg offset) (verify-socket-buffer msg offset size))
    (%stack-block ((bufptr size))
      (%copy-ivector-to-ptr msg offset bufptr 0 size)
      (socket-call socket "send"
	(with-eagain fd :output
	  (c_send-for-usocket fd bufptr size 0))))))
