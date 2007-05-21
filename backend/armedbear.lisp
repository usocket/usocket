;;;; $Id$
;;;; $URL$

;;;; See LICENSE for licensing information.

(in-package :usocket)


(defmacro jstatic-call (class-name (method-name &rest arg-spec)
                                   &rest args)
  (let ((class-sym (gensym)))
    `(let ((,class-sym ,class-name))
       (java:jstatic
        (java:jmethod ,class-sym ,method-name ,@arg-spec)
        (java:jclass ,class-sym) ,@args))))

(defmacro jmethod-call (instance-and-class (method &rest arg-spec) &rest args)
  (let ((isym (gensym)))
    (multiple-value-bind
        (instance class-name)
        (if (listp instance-and-class)
            (values (first instance-and-class)
                    (second instance-and-class))
          (values instance-and-class))
      (when (null class-name)
        (setf class-name `(java:jclass-name (java:jclass-of ,isym))))
      `(let* ((,isym ,instance))
         (java:jcall (java:jmethod ,class-name ,method ,@arg-spec)
                     ,isym ,@args)))))

(defun jequals (x y)
  (jmethod-call (x "java.lang.Object")
                ("equals" "java.lang.Object")
                y))

(defmacro jnew-call ((class &rest arg-spec) &rest args)
  `(java:jnew (java:jconstructor ,class ,@arg-spec)
         ,@args))

(defun get-host-name ()
  (let ((localAddress (java:jstatic
                       (java:jmethod "java.net.InetAddress"
                                     "getLocalHost")
                       (java:jclass "java.net.InetAddress"))))
    (java:jcall (java:jmethod "java.net.InetAddress" "getHostName")
                localAddress)))

(defun handle-condition (condition &optional socket)
  (typecase condition
    (error (error 'unknown-error :socket socket :real-error condition))))

(defun socket-connect (host port &key (element-type 'character))
  (let ((usock))
    (with-mapped-conditions (usock)
       (let ((sock (ext:make-socket (host-to-hostname host) port)))
         (setf usock
               (make-stream-socket
                :socket sock
                :stream (ext:get-socket-stream sock
                                               :element-type element-type)))))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (sock-addr (jnew-call ("java.net.InetSocketAddress"
                                "java.lang.String" "int")
                               (host-to-hostname host) port))
         (chan (jstatic-call "java.nio.channels.ServerSocketChannel" ("open")))
         (sock (java:jcall
                (java:jmethod "java.nio.channels.ServerSocketChannel"
                              "socket") chan)))
    (when reuseaddress
      (jmethod-call sock
                    ("setReuseAddress" "boolean")
                    (java:make-immediate-object reuseaddress :boolean)))
    (jmethod-call sock
                  ("bind" "java.net.SocketAddress" "int")
                  sock-addr backlog)
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (let* ((jsock (socket socket))
         (jacc-sock (jmethod-call jsock ("accept")))
         (jacc-stream
          (ext:get-socket-stream jacc-sock
                                 :element-type (or element-type
                                                   (element-type socket)))))
    (make-stream-socket :socket jacc-sock
                        :stream jacc-stream)))

;;(defun print-java-exception (e)
;;  (let* ((native-exception (java-exception-cause e)))
;;    (print (jcall (jmethod "java.net.BindException" "getMessage") native-exception))))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (ext:socket-close (socket usocket))))

;; Socket streams are different objects than
;; socket streams. Closing the stream flushes
;; its buffers *and* closes the socket.
(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod get-local-address ((usocket usocket))
  (dotted-quad-to-vector-quad (ext:socket-local-address (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (dotted-quad-to-vector-quad (ext:socket-peer-address (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (ext:socket-local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (ext:socket-peer-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))


#|
Pseudo code version of what we're trying to do:

We're being called with 2 args:

  - sockets (list)
  - timeout (non-negative real)

Selector := java.nio.channels.Selector.open()

For all usockets
  get the java socket
  get its channel
  register the channel with the selector
    with ops (operations) OP_READ and OP_ACCEPT

make the selector wait trunc(timeout*1000) miliseconds,
  unless (null timeout), because then:
  selectNow()

retrieve the selectedKeys() set from the selector
  unless select() returned 0 selected keys.

for set-iterator.hasNextKey()
  with that key
    retrieve the channel
    retrieve the channel's socket
    add the retrieved socket to the list of ready sockets

for all usockets
  check if the associated java object
    is in the list of ready sockets
  it is? add it to the function result list

close() the selector

return the function result list.

|#

(defun jsocket-channel (jsocket)
  (jmethod-call jsocket ("getChannel")))

(defun jselkey-channel (jselectionkey)
  (jmethod-call (jselectionkey "java.nio.channels.SelectionKey")
                ("channel")))

(defun op-read ()
  (java:jfield (java:jclass "java.nio.channels.SelectionKey")
               "OP_READ"))

(defun op-accept ()
  (java:jfield (java:jclass "java.nio.channels.SelectionKey")
               "OP_ACCEPT"))

(defun op-connect ()
  (java:jfield (java:jclass "java.nio.channels.SelectionKey")
               "OP_CONNECT"))

(defun valid-ops (jchannel)
  (jmethod-call (jchannel "java.nio.channels.SelectableChannel")
                ("validOps")))

(defun register (jchannel jselector ops)
  (jmethod-call (jchannel "java.nio.channels.SelectableChannel")
                ("register" "java.nio.channels.Selector" "int")
                jselector ops))

(defun toggle-blocking (jchannel mode)
  (jmethod-call (jchannel "java.nio.channels.SelectableChannel")
                ("configureBlocking" "boolean")
                mode))

(defun jselector-select (jselector timeout)
  (let ((to (truncate (* (or timeout 0) 1000))))
    (if (/= timeout 0)
        (jmethod-call (jselector "java.nio.channels.Selector")
                      ("select" "long") to)
      (jmethod-call (jselector "java.nio.channels.Selector")
                    ("selectNow")))))

(defun jselector-selected-keys (jselector)
  (jmethod-call (jselector "java.nio.channels.Selector")
                ("selectedKeys")))

(defun jset-iterator (jset)
  (jmethod-call (jset "java.util.Set") ("iterator")))

(defun jiterator-has-next (jiterator)
  (jmethod-call (jiterator "java.util.Iterator") ("hasNext")))

(defun jiterator-next (jiterator)
  (jmethod-call (jiterator "java.util.Iterator") ("next")))

(defun channel-class (jchannel)
  (let ((valid-ops (valid-ops jchannel)))
    (cond ((/= 0 (logand valid-ops (op-connect)))
           "java.nio.channels.SocketChannel")
          ((/= 0 (logand valid-ops (op-accept)))
           "java.nio.channels.ServerSocketChannel")
          (t
           "java.nio.channels.DatagramChannel"))))

(defun wait-for-input-internal (sockets &key timeout)
  (let* ((ops (logior (op-read) (op-accept)))
         (selector (jstatic-call "java.nio.channels.Selector" ("open")))
         (channels
          (mapcar #'(lambda (s)
                      (jsocket-channel (socket s)))
                  sockets)))
    (unwind-protect
        (progn
          (let ((jfalse (java:make-immediate-object nil :boolean)))
            (dolist (channel channels)
              (toggle-blocking channel jfalse)
              (register channel selector (logand ops (valid-ops channel)))))
          (let ((ready-count
                 (jselector-select selector timeout)))
            (when (< 0 ready-count)
              ;; we actually have work to do
              (let* ((selkeys (jselector-selected-keys selector))
                     (selkey-iterator (jset-iterator selkeys))
                     ready-sockets)
                (loop while (jiterator-has-next selkey-iterator)
                      do (let* ((key (jiterator-next selkey-iterator))
                                (chan (jselkey-channel key)))
                           (push (jmethod-call (chan (channel-class chan))
                                               ("socket"))
                                 ready-sockets)))
                (print ready-sockets)
                (print (remove-if #'(lambda (s)
                                      (not (member (socket s) ready-sockets
                                                   :test #'jequals)))
                                  sockets))))))
      ;; cancel all Selector registrations
      (let* ((keys (jmethod-call (selector "java.nio.channels.Selector")
                                 ("keys")))
             (iter (jset-iterator keys)))
        (loop while (jiterator-has-next iter)
              do (jmethod-call ((jiterator-next iter)
                                "java.nio.channels.SelectionKey")
                               ("cancel"))))
      ;; close the selectorx
      (jmethod-call (selector "java.nio.channels.Selector") ("close"))
      ;; make all sockets blocking again.
      (let ((jtrue (java:make-immediate-object t :boolean)))
        (dolist (chan channels)
          (toggle-blocking chan jtrue))))))
