;;;-*-Mode: LISP; Package: CCL -*-
;;
;;; OpenTransportUDP.lisp
;;; Copyright 2012 Chun Tian (binghe) <binghe.lisp@gmail.com>

;;; UDP extension to OpenTransport.lisp (with some TCP patches)

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :opentransport))

;; MCL Issue 28: Passive TCP streams should be able to listen to the loopback interface
;; see http://code.google.com/p/mcl/issues/detail?id=28 for details

(defparameter *passive-interface-address* NIL
  "Address to use for passive connections - optionally bind to loopback address while opening a tcp stream")

(advise local-interface-ip-address
  (or *passive-interface-address* (:do-it))
  :when :around :name 'override-local-interface-ip-address)

;; MCL Issue 29: Passive TCP connections on OS assigned ports
;; see http://code.google.com/p/mcl/issues/detail?id=29 for details
(advise ot-conn-tcp-passive-connect
  (destructuring-bind (conn port &optional (allow-reuse t)) arglist
    (declare (ignore allow-reuse))
    (if (eql port #$kOTAnyInetAddress)
	;; Avoids registering a proxy for port 0 but instead registers one for the true port:
	(multiple-value-bind (proxy result)
	    (let* ((*opentransport-class-proxies* NIL) ; makes ot-find-proxy return NIL
		   (result (:do-it)) ;; pushes onto *opentransport-class-proxies*
		   (proxy (prog1
			      (pop *opentransport-class-proxies*)
			    (assert (not *opentransport-class-proxies*))))
		   (context (cdr proxy))
		   (tmpconn (make-ot-conn :context context 
					  :endpoint (pref context :ot-context.ref)))
		   (localaddress (ot-conn-tcp-get-addresses tmpconn)))
	      (declare (dynamic-extent tmpconn))
	      ;; replace original set in body of function
	      (setf (ot-conn-local-address conn) localaddress)
	      (values
	       (cons localaddress context)
	       result))
	  ;; need to be outside local binding of *opentransport-class-proxies* 
	  (without-interrupts
	      (push proxy *opentransport-class-proxies*))
	  result)
	(:do-it)))
  :when :around :name 'ot-conn-tcp-passive-connect-any-address)
