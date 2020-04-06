;;;; -*- mode: lisp -*-
;;;;
;;;; This is the S-HTTP-SERVER package definition
;;;;
;;;; Copyright (C) 2002-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :s-http-server
  (:use common-lisp)
  (:export
   #:s-http-server
   #:make-s-http-server
   #:configure-default-handlers
   #:start-server
   #:stop-server
   #:ps
   #:register-context-handler
   #:unregister-context-handler
   #:*http-server-identification*
   #:*http-server-port*
   #:http-request
   #:get-path
   #:get-full-path
   #:get-method
   #:get-uri
   #:get-http-version
   #:get-keep-alive
   #:get-user
   #:get-port
   #:get-name
   #:get-headers
   #:get-server-process
   #:get-http-connections
   #:get-boot-time
   #:get-log-stream
   #:get-access-log-stream
   #:get-debug-mode
   #:get-contexts
   #:get-http-connection
   #:get-http-server
   #:get-line-buffer
   #:get-byte-buffer
   #:get-gzip-compressor
   #:logm
   #:find-handler
   #:handle-http-server-connection
   #:write-http-response-status-line
   #:write-http-response-headers
   #:write-http-response-line
   #:standard-http-response-headers
   #:standard-http-html-message-response
   #:standard-http-html-error-response
   #:s-http-server-handler
   #:static-resource-handler
   #:*favicon*
   #:favicon-handler
   #:redirect-handler
   #:request-header-value
   #:wrap-with-basic-authentication
   #:*last-http-request*
   #:+enable-gzip-compression+
   #:compressible-mime-type-p
   #:accepts-gzip-encoding-p
   #:gzip-compress)
  (:documentation "S-HTTP-SERVER is a minial standalone Common Lisp HTTP Server"))

;;;; eof
