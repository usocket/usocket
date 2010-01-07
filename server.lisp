;;;; $Id$
;;;; $URL$

(in-package :usocket)

(defvar *remote-host*)
(defvar *remote-port*)

(defun socket-server (host port function &optional arguments
                      &key (timeout 1)
		           (max-buffer-size +max-datagram-packet-size+))
  (let ((socket (socket-connect nil nil
				:protocol :datagram
				:local-host host
				:local-port port))
        (buffer (make-array max-buffer-size
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (unwind-protect
        (loop (progn
		(multiple-value-bind (sockets real-time)
                    (wait-for-input socket :timeout timeout)
                  (declare (ignore sockets))
                  (when real-time
                    (multiple-value-bind (recv n *remote-host* *remote-port*)
                        (socket-receive socket buffer max-buffer-size)
                      (declare (ignore recv))
                      (if (plusp n)
                          (progn
                            (let ((reply
                                   (apply function
                                          (cons (subseq buffer 0 n) arguments))))
                              (when reply
                                (replace buffer reply)
                                (let ((n (socket-send socket buffer (length reply)
                                                      :host *remote-host*
                                                      :port *remote-port*)))
                                  (when (minusp n)
                                    (error "send error: ~A~%" n))))))
			(error "receive error: ~A" n))))
                  #+scl (when thread:*quitting-lisp*
                          (return))
                  #+(and cmu mp) (mp:process-yield))))
      (socket-close socket)
      (values))))
