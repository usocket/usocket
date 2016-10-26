;;;; See LICENSE for licensing information.

(in-package :usocket)

;; all SOCKET-OPTIONs shuold be implemented here

(defgeneric socket-option (socket option &key)
  (:documentation
   "Get a socket's internal options"))

(defgeneric (setf socket-option) (new-value socket option &key)
  (:documentation
   "Set a socket's internal options"))

;;; Handling of wrong type of arguments

(defmethod socket-option ((socket usocket) (option t) &key)
  (error 'type-error :datum option :expected-type 'keyword))

(defmethod (setf socket-option) (new-value (socket usocket) (option t) &key)
  (declare (ignore new-value))
  (socket-option socket option))

(defmethod socket-option ((socket usocket) (option symbol) &key)
  (if (keywordp option)
      (error 'unimplemented :feature option :context 'socket-option)
    (error 'type-error :datum option :expected-type 'keyword)))

(defmethod (setf socket-option) (new-value (socket usocket) (option symbol) &key)
  (declare (ignore new-value))
  (if (keywordp option)
      (error 'unimplemented :feature option :context 'socket-option)
    (error 'type-error :datum option :expected-type 'keyword)))

;;; Socket option: RECEIVE-TIMEOUT (SO_RCVTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :receive-timeout)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    ))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :receive-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    timeout))

;;; Socket option: SEND-TIMEOUT (SO_SNDTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :send-timeout)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    ))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :send-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    timeout))

;;; Socket option: REUSE-ADDRESS (SO_REUSEADDR), for TCP server

(defmethod socket-option ((usocket stream-server-usocket)
                          (option (eql :reuse-address)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    ))

(defmethod (setf socket-option) (new-value (usocket stream-server-usocket)
                                           (option (eql :reuse-address)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    new-value))

;;; Socket option: BROADCAST (SO_BROADCAST), for UDP client

(defmethod socket-option ((usocket datagram-usocket)
                          (option (eql :broadcast)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    ))

(defmethod (setf socket-option) (new-value (usocket datagram-usocket)
                                           (option (eql :broadcast)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    new-value))

;;; Socket option: TCP-NODELAY (TCP_NODELAY), for TCP client

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-no-delay)) &key)
  (declare (ignore option))
  (socket-option usocket :tcp-nodelay))

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-nodelay)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    ))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-no-delay)) &key)
  (declare (ignore option))
  (setf (socket-option usocket :tcp-nodelay) new-value))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-nodelay)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    new-value))
