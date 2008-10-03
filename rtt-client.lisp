;;;; $Id$
;;;; $URL$

(in-package :usocket)

(defun default-rtt-function (message) (values message 0))

(defmethod socket-sync ((socket datagram-usocket) message &key address port
                        (max-receive-length +max-datagram-packet-size+)
                        (encode-function #'default-rtt-function)
                        (decode-function #'default-rtt-function))
  (rtt-newpack socket)
  (multiple-value-bind (data send-seq) (funcall encode-function message)
    (let ((data-length (length data)))
      (loop
	 with send-ts = (rtt-ts socket)
	 and recv-message = nil
	 and recv-seq = -1
	 and continue-p = t
	 do (progn
	      (socket-send socket data data-length :address address :port port)
	      (multiple-value-bind (sockets real-time)
		  (wait-for-input socket :timeout (rtt-start socket))
		(declare (ignore sockets))
		(if real-time
		    ;; message received
		    (loop
		       do (multiple-value-setq (recv-message recv-seq)
			    (funcall decode-function
				     (socket-receive socket nil max-receive-length)))
		       until (or (= recv-seq send-seq)
                                 (warn 'rtt-seq-mismatch-warning
                                       :socket socket
                                       :send-seq send-seq
                                       :recv-seq recv-seq))
		       finally (let ((recv-ts (rtt-ts socket)))
				 (rtt-stop socket (- recv-ts send-ts))
				 (return nil)))
		    ;; message not received
		    (let ((old-rto (slot-value socket 'rto)))
		      (setf continue-p (rtt-timeout socket))
                      (warn 'rtt-timeout-warning
                            :socket socket
                            :old-rto old-rto
                            :new-rto (slot-value socket 'rto))
		      (unless continue-p
                        (error 'rtt-timeout-error)
			(rtt-init socket))))))
	 until (or recv-message (not continue-p))
	 finally (return recv-message)))))
