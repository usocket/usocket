;;;; $Id$
;;;; $URL$

;;;; SOCKET-OPTION, a high-level socket option get/set facility
;;;; Author: Chun Tian (binghe)

;;;; See LICENSE for licensing information.

(in-package :usocket)

;;; Interface definition

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
  (socket-option socket option))

;;; Socket option: RECEIVE-TIMEOUT (SO_RCVTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :receive-timeout)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (socket:socket-options socket :so-rcvtimeo)
    #+clozure
    (ccl:stream-input-timeout socket)
    #+cmu
    (lisp::fd-stream-timeout (socket-stream usocket))
    #+ecl
    (sb-bsd-sockets:sockopt-receive-timeout socket)
    #+lispworks
    (get-socket-receive-timeout socket)
    #+mcl
    () ; TODO
    #+sbcl
    (sb-impl::fd-stream-timeout (socket-stream usocket))
    #+scl
    ()))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :receive-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    (declare (ignorable socket timeout))
    #+abcl
    () ; TODO
    #+allegro
    () ; TODO
    #+clisp
    (socket:socket-options socket :so-rcvtimeo timeout)
    #+clozure
    (setf (ccl:stream-input-timeout socket) timeout)
    #+cmu
    (setf (lisp::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'integer))
    #+ecl
    (setf (sb-bsd-sockets:sockopt-receive-timeout socket) timeout)
    #+lispworks
    (set-socket-receive-timeout socket timeout)
    #+mcl
    () ; TODO
    #+sbcl
    (setf (sb-impl::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'single-float))
    #+scl
    ()
    new-value))

(declaim (inline lisp->c) (inline lisp<-c))
(defun lisp->c (bool) (if bool 1 0))
(defun lisp<-c (int) (= 1 int))

;;; Socket option: REUSE-ADDRESS (SO_REUSEADDR), for TCP server

(defmethod socket-option ((usocket stream-server-usocket)
                          (option (eql :reuse-address)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    ()
    #+allegro
    ()
    #+clisp
    (lisp<-c (socket:socket-options socket :so-reuseaddr))
    #+clozure
    (lisp<-c (get-socket-option-reuseaddr socket))
    #+cmu
    ()
    #+ecl
    ()
    #+lispworks
    ()
    #+mcl
    ()
    #+sbcl
    (sb-bsd-sockets:sockopt-reuse-address socket)
    #+scl
    ()))

(defmethod (setf socket-option) (new-value (usocket stream-server-usocket)
                                           (option (eql :reuse-address)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    ()
    #+alloero
    ()
    #+clisp
    (socket:socket-options socket :so-reuseaddr (lisp->c new-value))
    #+clozure
    (set-socket-option-reuseaddr socket (lisp->c new-value))
    #+cmu
    ()
    #+ecl
    ()
    #+lispworks
    ()
    #+mcl
    ()
    #+sbcl
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) new-value)
    #+scl
    ()
    new-value))

;;; Socket option: BROADCAST (SO_BROADCAST), for UDP client

(defmethod socket-option ((usocket datagram-usocket)
                          (option (eql :broadcast)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    ()
    #+alloero
    ()
    #+clisp
    (lisp<-c (socket:socket-options socket :so-broadcast))
    #+clozure
    (lisp<-c (get-socket-option-broadcast socket))
    #+cmu
    ()
    #+ecl
    ()
    #+lispworks
    ()
    #+mcl
    ()
    #+sbcl
    (sb-bsd-sockets:sockopt-broadcast socket)
    #+scl
    ()))

(defmethod (setf socket-option) (new-value (usocket datagram-usocket)
                                           (option (eql :broadcast)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    #+abcl
    ()
    #+alloero
    ()
    #+clisp
    (socket:socket-options socket :so-broadcast (lisp->c new-value))
    #+clozure
    (set-socket-option-broadcast socket (lisp->c new-value))
    #+cmu
    ()
    #+ecl
    ()
    #+lispworks
    ()
    #+mcl
    ()
    #+sbcl
    (setf (sb-bsd-sockets:sockopt-broadcast socket) new-value)
    #+scl
    ()
    new-value))
