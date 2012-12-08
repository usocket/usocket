;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; $URL$

;;;; Foreign functions defined by ECL's DFFI, used for #+ecl-bytecmp only.
;;;; See LICENSE for licensing information.

(in-package :usocket)

#+(and ecl-bytecmp windows)
(eval-when (:load-toplevel :execute)
  (ffi:load-foreign-library "ws2_32.dll" :module "ws2_32"))

#+(and ecl-bytecmp windows)
(progn

(ffi:def-function ("gethostname" c-gethostname)
  ((name (* :unsigned-char))
   (len :int))
  :returning :int
  :module "ws2_32")

(defun get-host-name ()
  "Returns the hostname"
  (ffi:with-foreign-object (name '(:array :unsigned-char 256))
    (when (zerop (c-gethostname (ffi:char-array-to-pointer name) 256))
      (ffi:convert-from-foreign-string name))))

(ffi:def-foreign-type ws-socket :signed)
(ffi:def-foreign-type ws-dword :unsigned-long)
(ffi:def-foreign-type ws-event :pointer-void)

(ffi:def-struct wsa-network-events
  (network-events :long)
  (error-code (:array :int 10)))

(ffi:def-function ("WSACreateEvent" wsa-event-create)
  ()
  :returning ws-event
  :module "ws2_32")

(ffi:def-function ("WSACloseEvent" c-wsa-event-close)
  ((event-object ws-event))
  :returning :int
  :module "ws2_32")

(defun wsa-event-close (ws-event)
  (not (zerop (c-wsa-event-close ws-event))))

(ffi:def-function ("WSAEnumNetworkEvents" wsa-enum-network-events)
  ((socket ws-socket)
   (event-object ws-event)
   (network-events (* wsa-network-events)))
  :returning :int
  :module "ws2_32")

(ffi:def-function ("WSAEventSelect" wsa-event-select)
  ((socket ws-socket)
   (event-object ws-event)
   (network-events :long))
  :returning :int
  :module "ws2_32")

(ffi:def-function ("WSAWaitForMultipleEvents" c-wsa-wait-for-multiple-events)
  ((number-of-events ws-dword)
   (events (* ws-event))
   (wait-all-p :int)
   (timeout ws-dword)
   (alertable-p :int))
  :returning ws-dword
  :module "ws2_32")

(defun wsa-wait-for-multiple-events (number-of-events events wait-all-p timeout alertable-p)
  (c-wsa-wait-for-multiple-events number-of-events
                                  events
                                  (if wait-all-p -1 0)
                                  timeout
                                  (if alertable-p -1 0)))

(ffi:def-function ("ioctlsocket" wsa-ioctlsocket)
  ((socket ws-socket)
   (cmd :long)
   (argp (* :unsigned-long)))
  :returning :int
  :module "ws2_32")

) ; #+(and ecl-bytecmp windows)
