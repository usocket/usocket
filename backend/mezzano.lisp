;;;; -*- Mode: Common-Lisp -*-

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun handle-condition (condition &optional (socket nil))
  (typecase condition
    ;;---*** TODO: Add additional conditions as appropriate
))

(defun socket-connect (host port &key (protocol :stream) element-type
			    timeout deadline (nodelay nil nodelay-p)
			    local-host local-port)
  (declare (ignore local-host local-port))
  (when deadline
    (unsupported 'deadline 'socket-connect))
  (when (and nodelay-p (not (eq nodelay :if-supported)))
    (unsupported 'nodelay 'socket-connect))
  (when timeout
    (unsupported 'timeout 'socket-connect))
  (with-mapped-conditions ()
    (ecase protocol
      (:stream
       (let ((s (mezzano.network.tcp:tcp-stream-connect host port :element-type element-type)))
         (make-stream-socket :socket s
                             :stream s)))
      (:datagram
	;;---*** TODO
	(unsupported 'datagram 'socket-connect)))))
