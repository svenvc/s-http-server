;;;; -*- mode: lisp -*-
;;;;
;;;; Tools to setup, configure and manage S-HTTP[S]-SERVER
;;;;
;;;; Copyright (C) 2005-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package s-http-server)

(defun make-s-http-server (&key 
                           (port *http-server-port*)
                           (name "s-http-server")
                           (log-stream *standard-output*)
                           (access-log-stream *standard-output*))
  "Create a new object representing an S-HTTP-SERVER"
  (make-instance 's-http-server
                 :port port
                 :name name
                 :log-stream log-stream
                 :access-log-stream access-log-stream))

(defun configure-default-handlers (s-http-server)
  "Setup a set of default handlers to a server"
  (let ((specs `((s-http-server-handler "/s-http-server")
                 (ps-handler "/ps")
                 (hello-world "/helloworld")
                 (dw-bench "/dw-bench")
                 (echo-debug-handler "/echo-debug")
                 (,(wrap-with-basic-authentication 'echo-debug-handler 
                                                   :authenticator '(("foo" . "bar")) 
                                                   :realm "test") 
                  "/echo-debug-auth")
                 (favicon-handler "/favicon.ico")
                 (random-handler "/random" 256)
                 (random-handler "/random-1k")
                 (random-handler "/random-64" 64))))
    (loop :for spec :in specs :do
          (destructuring-bind (handler-function context-prefix &rest arguments)
              spec
            (register-context-handler s-http-server context-prefix handler-function 
                                      :arguments arguments)))))

(defun ps (s-http-server)
  "Show the state of all http connections known to the server"
  (format t "~s [debug-mode:~a]~%" s-http-server (get-debug-mode s-http-server))
  (loop :for c :in (get-http-connections s-http-server) :do
        (format t "  ~s~%" c)
        (format t "      stream: ~a~%" (or (get-stream c) "--"))
        (format t "     process: ~a~%" (or (get-process c) "--")))
  (values))

(defmethod cleanup-all-pooled-connections ((s-http-server s-http-server))
  "Kill all pooled connections"
  (cleanup-connections s-http-server
                       :filter (lambda (c) (eql (get-state c) :pooled))
                       :do-not-pool t))

(defun toggle-connection-pooling (s-http-server on-off)
  "Turn (global) connection pooling on or off"
  (if on-off
      (unless +pool-connections+
        (setf +pool-connections+ t)
        (logm s-http-server :info "Turned connection pooling on"))
    (when +pool-connections+
      (setf +pool-connections+ nil)
      (cleanup-all-pooled-connections s-http-server)
      (logm s-http-server :info "Turned connection pooling off"))))

#+lispworks
(defun cleanup-lost-connection-handlers (&key do-not-ask)
  (let* ((all-processes (mp:list-all-processes))
         (connection-handlers (remove-if-not (lambda (p)
                                               (search "connection-handler" (mp:process-name p)))
                                             all-processes)))
    (loop :for p :in connection-handlers
          :when (or do-not-ask (y-or-n-p "Kill ~s ?" p))
          :do (mp:process-kill p))))

;;;; eof
