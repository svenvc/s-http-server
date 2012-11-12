;;;; -*- mode: lisp -*-
;;;;
;;;; Test and example code for S-HTTP[S]-SERVER
;;;;
;;;; Copyright (C) 2006-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-server)

;; some convenience globals

(defvar test-server)

(defun start-test-server ()
  (start-server test-server))

(defun stop-test-server ()
  (stop-server test-server))

;; a quick example on setting up a server on a directory

(defun make-test-server (&key (port 1701)
                              (www "/Library/WebServer/Documents/"))
  (setf test-server (make-s-http-server :port port))
  (configure-default-handlers test-server)
  (register-context-handler test-server "/www" 'static-resource-handler :arguments (list www))
  test-server)

;; after starting the above server, you can access http://localhost:1701/s-http-server

;; by disabling debug mode and logging to *standard-output*, you get a faster server

(defun make-fast-server ()
  (setf test-server (make-s-http-server :log-stream nil :access-log-stream nil))
  (configure-default-handlers test-server)
  (setf (get-debug-mode test-server) nil)
  test-server)

;; the files in the rsrc directory are provided as examples to configure an https server

(defun make-secure-server (&key (rsrc-dir "/Users/sven/darcs/s-http-server/rsrc/"))
  (setf test-server
        (make-s-https-server :certificate (merge-pathnames "test-server.crt" rsrc-dir)
                             :private-key (merge-pathnames "test-server.key" rsrc-dir)
                             :dhparam (merge-pathnames "dhparam.pem" rsrc-dir)
                             :password "123456"))
  (configure-default-handlers test-server)
  test-server)

;; please consult the readme.txt file in the rsrc directory for more information.

;; after starting the above server, you can access https://localhost:1701/s-http-server

;;;; eof
