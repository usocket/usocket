;;;; $Id$
;;;; $Source$

;;;; See LICENSE for licensing information.

(in-package :usocket-test)

(defvar *soc1* (usoc:make-socket :socket :stream
                                 :host #(1 2 3 4)
                                 :port 80
                                 :stream :my-stream))

(deftest make-socket.1 (usoc::real-socket usoct::*soc1*) :my-socket)
(deftest make-socket.2 (usoc::real-stream usoct::*soc1*) :my-stream)
(deftest make-socket.3 (usoc:host usoct::*soc1*) #(1 2 3 4))
(deftest make-socket.4 (usoc:host usoct::*soc1*) 80)

