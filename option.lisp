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

;;; Option: RECEIVE-TIMEOUT (RCVTIMEO)
;;;  Scope: TCP & UDP

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :receive-timeout)) &key)
  (let ((socket (socket usocket)))
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
  (declare (type number new-value))
  (let ((socket (socket usocket))
        (timeout new-value))
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
