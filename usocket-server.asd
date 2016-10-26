;;;; -*- Mode: Lisp -*-
;;;;
;;;; See the LICENSE file for licensing information.

(in-package :asdf)

(defsystem #:usocket-server
    :name "usocket (server)"
    :author "Chun Tian (binghe)"
    :version "0.8.0-dev"
    :licence "MIT"
    :description "Universal socket library for Common Lisp (server side)"
    :depends-on (:usocket :portable-threads)
    :components ((:file "server")))
