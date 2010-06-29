;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

;;;; Usage: (usoct:run-usocket-tests) or (usoct:do-tests)

(in-package :usocket-test)

(defparameter +non-existing-host+ "1.2.3.4")
(defparameter +unused-local-port+ 15213)

(defparameter *fake-usocket*
  (usocket::make-stream-socket :socket :my-socket
                               :stream :my-stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *common-lisp-net*
    #.(first (usocket::get-hosts-by-name "common-lisp.net"))))

(defvar *local-ip*)

(defmacro with-caught-conditions ((expect throw) &body body)
  `(catch 'caught-error
     (handler-case
      (progn ,@body)
      (usocket:unknown-error (c) (if (typep c ,expect)
                                     (throw 'caught-error ,throw)
                                   (progn
                                     (describe c)
                                     (describe
                                      (usocket::usocket-real-error c))
                                     c)))
      (error (c) (if (typep c ,expect)
                     (throw 'caught-error ,throw)
                   (progn
                     (describe c)
                     c)))
      (usocket:unknown-condition (c) (if (typep c ,expect)
                                         (throw 'caught-error ,throw)
                                       (progn
                                         (describe c)
                                         (describe
                                          (usocket::usocket-real-condition c))
                                         c)))
      (condition (c) (if (typep c ,expect)
                         (throw 'caught-error ,throw)
                       (progn
                         (describe c)
                         c))))))

(deftest make-socket.1 (usocket:socket *fake-usocket*) :my-socket)
(deftest make-socket.2 (usocket:socket-stream *fake-usocket*) :my-stream)

(deftest socket-no-connect.1
  (with-caught-conditions ('usocket:socket-error nil)
      (usocket:socket-connect "127.0.0.0" +unused-local-port+ :timeout 0)
      t)
  nil)

(deftest socket-no-connect.2
  (with-caught-conditions ('usocket:socket-error nil)
    (usocket:socket-connect #(127 0 0 0) +unused-local-port+ :timeout 0)
    t)
  nil)

(deftest socket-no-connect.3
  (with-caught-conditions ('usocket:socket-error nil)
    (usocket:socket-connect 2130706432 +unused-local-port+ :timeout 0) ;; == #(127 0 0 0)
    t)
  nil)

(deftest socket-failure.1
  (with-caught-conditions (#-(or cmu lispworks armedbear openmcl mcl)
                             'usocket:network-unreachable-error
                           #+(or cmu lispworks armedbear)
                             'usocket:unknown-error
                           #+(or openmcl mcl)
                             'usocket:timeout-error
                           nil)
    (usocket:socket-connect 2130706432 +unused-local-port+ :timeout 0) ;; == #(127 0 0 0)
    :unreach)
  nil)

(deftest socket-failure.2
  (with-caught-conditions (#+(or lispworks armedbear)
                             'usocket:unknown-error
                           #+cmu
                             'usocket:network-unreachable-error
                           #+(or openmcl mcl)
                             'usocket:timeout-error
                           #-(or lispworks armedbear cmu openmcl mcl)
                             'usocket:host-unreachable-error
                           nil)
    (usocket:socket-connect +non-existing-host+ 80 :timeout 0) ;; 80 = just a port
    :unreach)
  nil)

;; let's hope c-l.net doesn't move soon, or that people start to
;; test usocket like crazy..
(deftest socket-connect.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
      (unwind-protect
          (when (typep sock 'usocket:usocket) t)
        (usocket:socket-close sock))))
  t)

(deftest socket-connect.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (when (typep sock 'usocket:usocket) t)
        (usocket:socket-close sock))))
  t)

(deftest socket-connect.3
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect (usocket::host-byte-order *common-lisp-net*) 80)))
      (unwind-protect
          (when (typep sock 'usocket:usocket) t)
        (usocket:socket-close sock))))
  t)

;; let's hope c-l.net doesn't change its software any time soon
(deftest socket-stream.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
      (unwind-protect
          (progn
            (format (usocket:socket-stream sock)
                    "GET / HTTP/1.0~c~c~c~c"
                    #\Return #\linefeed #\Return #\linefeed)
            (force-output (usocket:socket-stream sock))
            (read-line (usocket:socket-stream sock)))
        (usocket:socket-close sock))))
  #+(or mcl clisp) "HTTP/1.1 200 OK"
  #-(or mcl clisp) #.(format nil "HTTP/1.1 200 OK~A" #\Return) nil)

(deftest socket-name.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (usocket::get-peer-address sock)
        (usocket:socket-close sock))))
  #.*common-lisp-net*)

(deftest socket-name.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (usocket::get-peer-port sock)
        (usocket:socket-close sock))))
  80)

(deftest socket-name.3
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (usocket::get-peer-name sock)
        (usocket:socket-close sock))))
  #.*common-lisp-net* 80)

#+ignore
(deftest socket-name.4
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80)))
      (unwind-protect
          (equal (usocket::get-local-address sock) *local-ip*)
        (usocket:socket-close sock))))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *wait-for-input-timeout* 2))

(deftest wait-for-input.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80))
          (time (get-universal-time)))
      (unwind-protect
          (progn (usocket:wait-for-input sock :timeout *wait-for-input-timeout*)
            (- (get-universal-time) time))
        (usocket:socket-close sock))))
  #.*wait-for-input-timeout*)

(deftest wait-for-input.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect *common-lisp-net* 80))
          (time (get-universal-time)))
      (unwind-protect
          (progn (usocket:wait-for-input sock :timeout *wait-for-input-timeout* :ready-only t)
            (- (get-universal-time) time))
        (usocket:socket-close sock))))
  #.*wait-for-input-timeout*)

(defun run-usocket-tests ()
  (do-tests))
