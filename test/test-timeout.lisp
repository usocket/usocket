;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: USOCKET-TEST -*-
;;;; See LICENSE for licensing information.

(in-package :usocket-test)

;; echo server with binary protocol:
;;
;;   msg = cmd-byte || length-byte || data*

;; tells server echo back the data bytes
(defconstant +cmd-echo+ 0)
;; tells server to simply consume the data bytes
(defconstant +cmd-read+ 1)
;; blocks server for n seconds before next echo / read
(defconstant +cmd-setdelay+ 8)

(defun tcp-echo-handler (client)
  (handler-bind ((error
                   (lambda (c)
                     (format *error-output* "Client handler error: ~A~%" c)
                     (return-from tcp-echo-handler))))
    (loop with delay = 0 do
      (let ((command (read-byte client nil nil)))
        (unless command
          (return)) ;; client disconnected
        (let* ((length (read-byte client))
               (buffer (make-array length :element-type '(unsigned-byte 8))))
          (read-sequence buffer client)
          (cond ((= command +cmd-echo+)
                 (when (plusp delay)
                   (sleep delay)
                   (setf delay 0))
                 (write-sequence buffer client))
                ((= command +cmd-read+)
                 (when (plusp delay)
                   (sleep delay)
                   (setf delay 0)))
                ((= command +cmd-setdelay+)
                 (unless (= length 1)
                   (error "Bad setdelay data length ~A, want 1 byte" length))
                 (setf delay (aref buffer 0)))
                (t
                 (error "Unknown command ~A" command))))))))

(defun send-tcp-echo-command (socket command &optional data)
  (assert (< (length data) 256))
  (let ((stream (socket-stream socket)))
    (write-byte command stream)
    (write-byte (length data) stream)
    (write-sequence data stream)
    (finish-output stream)))

(defvar *tcp-echo-thread* nil)
(defvar *tcp-echo-port* nil)

(defun start-tcp-echo-server ()
  (unless *tcp-echo-thread*
    (multiple-value-bind (thread socket)
        (socket-server "127.0.0.1" 0
                       'tcp-echo-handler nil
                       :protocol :stream
                       :in-new-thread t
                       :multi-threading t
                       :element-type '(unsigned-byte 8))
      (setq *tcp-echo-thread* thread
            *tcp-echo-port* (get-local-port socket)))))

(deftest tcp-timeout-in-read-sequence
  (progn
    (start-tcp-echo-server)
    (with-client-socket (s stream "127.0.0.1" *tcp-echo-port* :element-type '(unsigned-byte 8))
      ;; Server will respond after 5s.
      (send-tcp-echo-command s +cmd-setdelay+ #(5))
      (send-tcp-echo-command s +cmd-echo+ #(1 2 3 4))
      ;; Our timeout is 1s.
      (setf (socket-option s :receive-timeout) 1)
      ;; So this read should fail.
      (with-caught-conditions (usocket:timeout-error :got-timeout)
        (with-mapped-conditions (s)
          (let ((response (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence response stream))))))
  :got-timeout)

(deftest tcp-timeout-in-write-sequence
  (progn
    (start-tcp-echo-server)
    (with-client-socket (s stream "127.0.0.1" *tcp-echo-port* :element-type '(unsigned-byte 8))
      ;; Our write timeout is 1s.
      (setf (socket-option s :send-timeout) 1)
      (with-caught-conditions (usocket:timeout-error :got-timeout)
        (with-mapped-conditions (s)
          ;; Server will unblock after 5s.
          (send-tcp-echo-command s +cmd-setdelay+ #(5))
          ;; So this write should fail.
          (send-tcp-echo-command s +cmd-read+ #(1 2 3 4))))))
  :got-timeout)
