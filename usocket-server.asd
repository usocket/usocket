(in-package :asdf)

(defsystem usocket-server
    :name "usocket (server)"
    :author "Chun Tian (binghe)"
    :version "1.0"
    :licence "MIT"
    :description "Universal socket library for Common Lisp (server side)"
    :depends-on (:usocket :portable-threads)
    :components ((:file "server")))
