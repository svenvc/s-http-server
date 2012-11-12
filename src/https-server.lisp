;;;; -*- mode: lisp -*-
;;;;
;;;; Experimental HTTPS server (LispWorks only for now)
;;;;
;;;; Copyright (C) 2006-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-server)

#-lispworks (error "This code only works for LispWorks")

(defclass s-https-server (s-http-server)
  ((ssl-ctx :accessor get-ssl-context :initarg :ssl-ctx :initform nil))
  (:documentation "The object representing a minimal standalone HTTPS Server"))

;; the difference is how the server handles incoming connections

(defmethod start-server ((s-https-server s-https-server))
  (let ((connection-id 0))
    (flet ((connection-handler (client-socket-stream)
             (do-periodic-check s-https-server)
             (handle-new-http-server-connection s-https-server 
                                                client-socket-stream 
                                                (lambda () (incf connection-id)))))
      (unless (get-ssl-context s-https-server) (error "SSL context not yet configured"))
      (let ((process (comm:start-up-server
                      :function #'(lambda (socket-handle)
                                    (let ((client-stream (make-instance 'comm:socket-stream
                                                                        :socket socket-handle
                                                                        :direction :io
                                                                        :element-type 'base-char
                                                                        :ssl-ctx (get-ssl-context s-https-server))))
                                      (funcall #'connection-handler client-stream)))
                      :service (get-port s-https-server)
                      :announce t
                      :wait t
                      :process-name (get-name s-https-server))))
        (setf (get-server-process s-https-server) process
              (get-boot-time s-https-server) (get-universal-time))
        (logm s-https-server :info "Started a new secure server on port ~d" (get-port s-https-server)))))
  s-https-server)

(defmethod stop-server ((s-https-server s-https-server))
  (with-slots (ssl-ctx) 
      s-https-server
    (when ssl-ctx (comm:destroy-ssl-ctx ssl-ctx))
    (setf ssl-ctx nil))
  (call-next-method))

;; setting up an HTTPS server is of course more complex
;; you have to specify the following extra parameters:
;; - a filename of a certificate
;; - a filename of a private key
;; - the password to unlock the above
;; - the DH exchange parameters
;; the main object to configure is called an SSL context

(defun make-ssl-context (&key certificate private-key password dhparam)
  "Create, configure and return a new SSL conttext with the parameters given to use for HTTPS"
  (let ((ssl-ctx (comm:make-ssl-ctx)))
    ;; we force filenames to exist and use absoloute machine specific pathname strings
    (comm:set-ssl-ctx-password-callback ssl-ctx :password password)
    (comm:ssl-ctx-use-certificate-chain-file ssl-ctx 
                                             (namestring (truename (pathname certificate))))
    (comm:ssl-ctx-use-rsaprivatekey-file ssl-ctx 
                                         (namestring (truename (pathname private-key)))
                                         comm:ssl_filetype_pem)
    (comm:set-ssl-ctx-dh ssl-ctx :filename (namestring (truename (pathname dhparam)))) 
    (comm:set-cipher-list ssl-ctx "ALL")
    ssl-ctx))

(defun make-s-https-server (&key 
                            (port *http-server-port*)
                            (name "s-https-server")
                            (log-stream *standard-output*)
                            (access-log-stream *standard-output*)
                            certificate
                            private-key
                            password
                            dhparam)
  "Create a new object representing an S-HTTPS-SERVER optionally confiuring the SSL context"
  (let ((s-https-server (make-instance 's-https-server
                                       :port port
                                       :name name
                                       :log-stream log-stream
                                       :access-log-stream access-log-stream)))
    (when (and certificate private-key password dhparam)
      (setf (get-ssl-context s-https-server) (make-ssl-context :certificate certificate
                                                               :private-key private-key
                                                               :password password
                                                               :dhparam dhparam)))
    s-https-server))

(export '(make-s-https-server
          make-ssl-context
          get-ssl-context))

;;;; eof
