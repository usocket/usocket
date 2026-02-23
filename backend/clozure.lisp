;;;; See LICENSE for licensing information.

;;;; Functions for CCL 1.11 (IPv6) only, see openmcl.lisp for rest of functions.

(in-package :usocket)

#+ipv6
(defun socket-connect-internal (host &key port
                       (protocol :stream) element-type
                       timeout deadline (nodelay t) local-host local-port)
  (with-mapped-conditions (nil host)
    (let (remote local mcl-sock (host-local-socket-p (pathnamep host)))
      (loop
        :for address-family :in (if host-local-socket-p '(:file) '(:internet6 :internet))
        :do (tagbody
               (handler-bind
                   ((ccl:socket-creation-error
                      (lambda (err)
                        (if (eq address-family :internet6) ; the first try, let's ignore the error
                            (go :continue))
                        (signal err))))
                 (setq remote
		       (unless host-local-socket-p
                       (when (and host port)
                         (openmcl-socket:resolve-address :host (host-to-hostname host)
                                                         :port port
                                                         :socket-type protocol
                                                         :address-family address-family)))
                       local
		       (unless host-local-socket-p
                       (when (and local-host local-port)
                         (openmcl-socket:resolve-address :host (host-to-hostname local-host)
                                                         :port local-port
                                                         :socket-type protocol
                                                         :address-family address-family)))
                       mcl-sock
                       (apply #'openmcl-socket:make-socket
                              `(:type ,protocol
                                      ,@(when (or remote local)
                                          `(:address-family
                                            ,(openmcl-socket:socket-address-family (or remote local))))
				,@(when host-local-socket-p
				    `(:address-family :file))

                                      ,@(when remote
                                          `(:remote-address ,remote))
                                      ,@(when local
                                          `(:local-address ,local))

				,@(when host-local-socket-p
				    `(:remote-filename ,(namestring host)))

                                      :format ,(to-format element-type protocol)
                                      :external-format ,ccl:*default-external-format*
                                      :deadline ,deadline
                                      :nodelay ,(when (eql protocol :stream) nodelay)
                                      :connect-timeout ,timeout
                                      :input-timeout ,timeout))))
               (loop-finish)
             :continue)) 
      (ecase protocol
        (:stream
         (make-stream-socket :stream mcl-sock :socket mcl-sock))
        (:datagram
         (make-datagram-socket mcl-sock :connected-p (and remote t)))))))

#+ipv6
(defun socket-listen-internal
                     (host &key port
                      (reuse-address nil reuse-address-supplied-p)
                      (reuseaddress (when reuse-address-supplied-p reuse-address))
                      (backlog 5)
                      (element-type 'character))
  (let ((local-address (openmcl-socket:resolve-address :host (host-to-hostname host)
                                                       :port port :connect :passive)))
    (with-mapped-conditions (nil host)
      (make-stream-server-socket
        (openmcl-socket:make-socket :connect :passive
                                    :address-family (openmcl-socket:socket-address-family local-address)
                                    :local-address local-address
                                    :reuse-address reuseaddress
                                    :backlog backlog
                                    :format (to-format element-type :stream))
        :element-type element-type))))

#+ipv6
(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (let* ((ccl-socket (socket usocket))
         (socket-keys (ccl::socket-keys ccl-socket)))
    (with-mapped-conditions (usocket host)
      (if (and host port)
          (openmcl-socket:send-to ccl-socket buffer size
                                  :remote-host (host-to-hostname host)
                                  :remote-port port
                                  :offset offset)
          (openmcl-socket:send-to ccl-socket buffer size
                                  :remote-address (getf socket-keys :remote-address)
                                  :offset offset)))))
