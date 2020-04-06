;;;; -*- mode: lisp -*-
;;;;
;;;; Globally defined variable and parameters, both public and internal, for S-HTTP[S]-SERVER
;;;;
;;;; Copyright (C) 2005-2009,2020 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-server)

;; public globals and parameters

(defvar *http-server-identification* 
  (format nil "S-HTTP-SERVER ~a ~a" (lisp-implementation-type) (lisp-implementation-version))
  "Identification string sent as value of the 'Server' HTTP Response Header")

(defvar *http-server-port* 1701
  "Default port used when creating a new S-HTTP-SERVER")

(defvar *last-http-request* nil 
  "The last HTTP-REQUEST object handled by S-HTTP-SERVER")

(defparameter +access-log-format+ :common-log-format
  "Either :common-log-format or :extended-common-log-format")

(defparameter +period-check-interval+ 5
  "Do some periodic checks every 5 seconds")

(defparameter +allowed-connection-keepalive-age+ 15
  "Number of seconds a kept alive connection is allowed to be inactive (Apache default)")

(defparameter +allowed-keepalive-connections+ 32
  "Maximum number of simulataneous kept alive connections (hard resoure limit)")

(defparameter +allowed-http-methods+ '(:get :put :post :delete :head :options :trace)
  "The HTTP methods that we allow in the request line")

(defparameter +common-log-timestamp-format+
  '(#\[ :date #\/ :month-name #\/ :year #\: :hour #\: :minute #\: :second " +0000]"))

(defparameter +basic-mime-type-suffix-map+ 
  '(("html" . "text/html") ("htm" . "text/html") ("txt" . "text/plain") ("text" . "text/plain")
    ("gif" . "image/gif") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg") ("png" . "image/png")
    ("css" . "text/css") ("js" . "application/javascript"))
  "Fallback for when no mime-type info can be loaded from a known location")

(defparameter +known-mime.type-locations+ 
  '("/etc/httpd/mime.types" "/etc/mime.types" "/etc/apache2/mime.types")
  "Places to search for a system level mime-type to suffix map")

(defparameter *mime-type-suffix-map* nil
  "Hashtable mapping suffixes to mime-types, computed at run-time")

(defparameter +enable-gzip-compression+ t 
  "Use Salza2 to GZIP compress certain mime types")

(defparameter +compressible-mime-types+ 
  '("text/html" "text/plain" "text/css" 
    "application/javascript" "application/x-javascript" "text/javascript")
  "The list of mime-types that can/should be gzip compressed")

(defvar *favicon* 
  (let ((bytes #(0 0 1 0 1 0 16 16 16 0 0 0 0 0 40 1 0 0 22 0 0 0 40 0 0 0 16 0 0 0 32 0 0 0 1 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255 255 0 16 239 247 0 0 0 247 0 247 16 239 0 8 231 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 68 68 68 68 51 51 51 51 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 34 34 34 34 17 17 17 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))
  "If not nil, the pathname to a favicon.ico or a (unsigned-byte 8) array representing a favicon")

;; internal globals and parameters

(defparameter +common-request-headers+
  (mapcar #'(lambda (header) (cons header (intern (string-upcase header) :keyword)))
          '("Host" "User-Agent" "Accept" "Accept-Language" "Accept-Encoding" "Accept-Charset"
            "Content-Length" "Content-Type" "Authorization" "Cookie" 
            "Connection" "Keep-Alive" "Cache-Control" "Pragma" "If-Modified-Since")))

(defvar *doctype-html-401-transitional*
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")

(defvar *doctype-html-401-strict*
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")

;;;; eof
