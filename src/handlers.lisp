;;;; -*- mode: lisp -*-
;;;;
;;;; These are some predefined handlers for S-HTTP-SERVER
;;;;
;;;; Copyright (C) 2005-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-server)

;; some extra infrastructure

(defmethod get-byte-buffer ((http-connection http-connection))
  (with-slots (byte-buffer)
      http-connection
    (unless byte-buffer
      (setf byte-buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    byte-buffer))

(defmethod get-gzip-compressor ((http-connection http-connection))
  (with-slots (gzip-compressor)
      http-connection
    (if gzip-compressor
        (salza2:reset gzip-compressor)
      (make-instance 'salza2:gzip-compressor))))

;; an S-HTTP-HANDLER is a function taking 4 arguments:
;; - the s-http-server object representing the server itself
;; - the handler binding (a list whose first element is always the context that matched)
;; - the current http-request object (containing request method, uri, version and the headers alist)
;; - the stream to the client (where the request line and headers are already consumed)
;; the handler should handle the request, outputing 3 things (in order):
;; - a response status line
;; - the response headers
;; - the contents
;; finally, the handler should return the following values:
;; - t if it was succesful in handling the request or nil otherwise
;; - the response code if a response was given, nil otherwise
;; - the number of bytes in the response if there was one, 0 or nil otherwise

;; the hello world handler

(defun hello-world (s-http-server handler http-request stream)
  (declare (ignore handler))
  (logm s-http-server :info "Running the hello-world handler")
  (values t 
          200
          (standard-http-html-message-response http-request stream 
                                               "Hello World!" 
                                               "<p>Welcome to S-HTTP-SERVER.</p>")))

;; the builtin introspection handler

(defun s-http-server-handler (s-http-server handler http-request stream)
  "The builtin S-HTTP-SERVER testing/debugging handler returning a simple status/echo/snoop page"
  (logm s-http-server :info "Running builtin s-http-server handler")
  (let ((body (with-output-to-string (out)
                (format out "<p>S-HTTP-SERVER-HANDLER handling a ~a request for path ~s and version ~a.</p>" 
                        (get-method http-request) (get-path http-request) (get-http-version http-request))
                (format out "<p>Active handler binding is ~s. " handler)
                (format out "Remote host is ~a. " (s-sysdeps:get-socket-stream-property stream :remote-host))
                (format out "Request headers:</p>")
                (format out "<table border='1' width='100%'><tr><th>Key</th><th>Value</th></tr>")
                (loop :for (k . v) :in (get-headers http-request) 
                      :do (format out "<tr><td>~a</td><td>~a</td></tr>" k v))
                (format out "</table>")
                (format out "<p>Server identification is ~s, " *http-server-identification*)
                (format out "server name is ~s running on ~a:~d.</p>" 
                        (get-name s-http-server) (s-sysdeps:get-socket-stream-property stream :local-host) 
                        (get-port s-http-server))
                (format out "<p>Server clock is ~a. " (s-utils:format-universal-time (get-universal-time)))
                (format out "Server uptime is ~a. " (s-utils:format-duration (- (get-universal-time) 
                                                                                (get-boot-time s-http-server))))
                (format out "Server HTTP Connections:</p><ul>~{<li>~a</li>~}</ul>" 
                        (mapcar #'(lambda (x) 
                                    (concatenate 'string (escape (prin1-to-string x)) 
                                                 (if (eql x (get-http-connection http-request)) " CURRENT" "")))
                                (get-http-connections s-http-server)))
                (format out "<p>Context configuration:</p><table border='1' width='100%'>")
                (format out "<tr><th>Handler</th><th>Context</th><th>Parameters</th></tr>")
                (loop :for b :in (get-contexts s-http-server) 
                      :do (format out "<tr><td>~a</td><td>~a</td><td>~a</td></tr>" 
                                  (escape (prin1-to-string (first b))) 
                                  (second b) 
                                  (escape (prin1-to-string (rest (rest b))))))
                (format out "</table>"))))
    (values t 200 (standard-http-html-message-response http-request stream "S-HTTP-SERVER" body))))

;; the static resource (file server) handler

(defun mime-type-suffix-map ()
  (or *mime-type-suffix-map*
      (setf *mime-type-suffix-map*
            (let ((map (make-hash-table :test 'equal)))
              (loop :for (suffix . mime-type) :in +basic-mime-type-suffix-map+ 
                    :do (setf (gethash suffix map) mime-type))
              (loop :for location :in +known-mime.type-locations+
                    :do (when (probe-file location)
                          (with-open-file (in location)
                            (loop :for line = (read-line in nil)
                                  :until (null line)
                                  :unless (or (zerop (length line))
                                              (member (elt line 0) '(#\return #\linefeed #\#)))
                                  :do (let* ((tokens (s-utils:tokens line :separators '(#\tab)))
                                             (mime-type (string-trim '(#\space) (first tokens)))
                                             (suffixes (s-utils:tokens (second tokens) :separators '(#\space))))
                                        (loop :for suffix :in suffixes
                                              :do (setf (gethash suffix map) mime-type)))))))
              map))))

(defun mime-type-for-pathname (pathname)
  (or (gethash (string-downcase (or (pathname-type pathname) "")) (mime-type-suffix-map))
      "application/octet-stream"))

(defun compressible-mime-type-p (mime-type)
  "Return T when mime-type is compressible using gzip"
  (and +enable-gzip-compression+
       (member mime-type +compressible-mime-types+ :test 'string-equal)))

(defun accepts-gzip-encoding-p (http-request)
  "Return T when http-request accepts gzip content encoding"
  (and +enable-gzip-compression+
       (search "gzip" (request-header-value http-request "Accept-Encoding"))))

(defun make-real-resource-pathname (root-dir-components uri-dir-components name type)
  (make-pathname :name name
                 :type type
                 :directory `(:absolute ,@root-dir-components ,@uri-dir-components)))

(defun compute-real-resource-pathname (root path context pathname-builder)
  (labels ((ensure-trailing-slash (str)
             (let ((len (length str)))
               (if (and (> len 0)
                        (char/= #\/ (elt str (1- len))))
                   (concatenate 'string str "/")
                 str))))
    (let* ((real-root (pathname root))
           (real-root-dir-components (rest (pathname-directory real-root)))
           (context-pathname (pathname (ensure-trailing-slash context)))
           (context-components (rest (pathname-directory context-pathname))) 
           (uri-pathname (pathname path))
           (uri-dir-components (rest (pathname-directory uri-pathname)))
           (difference (mismatch context-components uri-dir-components :test #'string=)))
      (setf uri-dir-components (when difference (subseq uri-dir-components difference)))
      (funcall pathname-builder 
               real-root-dir-components
               uri-dir-components
               (or (pathname-name uri-pathname) "index")
               (or (pathname-type uri-pathname) "html")))))

(defun gzip-compress (in compressor byte-buffer)
  "Compress input stream or function in using gzip, returning (values data-chunks data-size)"
  (let ((data-chunks ())
        (data-size 0))
    (setf (salza2:callback compressor) (lambda (bytes end)
                                         (push (subseq bytes 0 end) data-chunks)
                                         (incf data-size end)))
    (s-utils:copy-stream in 
                         (lambda (buffer end)
                           (salza2:compress-octet-vector buffer compressor :end end))
                         byte-buffer)
    (salza2:finish-compression compressor)
    (values (nreverse data-chunks) data-size)))

(defun host-static-bytes (http-request stream bytes 
                          &key expires-max-age (mime-type "application/octet-stream") last-modified (start 0) end)
  "Return (values t <response-code> <size>) handling condition GET as well"
  (let ((if-modified-since (parse-http-date (request-header-value http-request "If-Modified-Since"))))
    (cond ((and if-modified-since last-modified (<= last-modified if-modified-since))
           (write-http-response-status-line stream 304 "Not Modified" (get-http-version http-request))
           (write-http-response-headers (standard-http-response-headers http-request :content-type nil) stream)
           (write-http-response-line "" stream)
           (values t 304 0))
          (t
           (let ((length (- (or end (length bytes)) start)))
           (write-http-response-status-line stream 200 "OK" (get-http-version http-request))
           (write-http-response-headers (standard-http-response-headers http-request
                                                                        :content-type mime-type 
                                                                        :content-length length)
                                        stream)
           (when last-modified
             (write-http-response-headers `(("Last-Modified" . ,(response-date last-modified))) stream))
           (when expires-max-age
             (write-http-response-headers `(("Expires" . ,(response-date (+ (get-universal-time) expires-max-age)))
                                            ("Cache-Control" . ,(format nil "max-age=~d" expires-max-age)))
                                          stream))
           (write-http-response-line "" stream)
           (write-sequence bytes stream :start start :end end)
           (values t 200 length))))))

(defun host-static-resource (http-request stream resource-pathname &key expires-max-age)
  "Return (values t <response-code> <size>) handling conditional GET as well"
  (let ((mime-type (mime-type-for-pathname resource-pathname))
        (if-modified-since (parse-http-date (request-header-value http-request "If-Modified-Since"))))
    (with-open-file (in resource-pathname :element-type '(unsigned-byte 8))
      (let ((last-modified (file-write-date in))
            (file-length (file-length in)))
        (cond ((and if-modified-since (<= last-modified if-modified-since))
               (write-http-response-status-line stream 304 "Not Modified" (get-http-version http-request))
               (write-http-response-headers (standard-http-response-headers http-request :content-type nil) stream)
               (write-http-response-line "" stream)
               (values t 304 0))
              ((and (accepts-gzip-encoding-p http-request) (compressible-mime-type-p mime-type) (< 255 file-length))
               (multiple-value-bind (data-chunks data-size)
                   (let ((http-connection (get-http-connection http-request)))
                     (gzip-compress in (get-gzip-compressor http-connection) (get-byte-buffer http-connection)))
                 (write-http-response-status-line stream 200 "OK" (get-http-version http-request))
                 (write-http-response-headers (standard-http-response-headers http-request
                                                                              :content-type mime-type 
                                                                              :content-length data-size)
                                              stream)
                 (write-http-response-headers `(("Content-Encoding" . "gzip")
                                                ("Last-Modified" . ,(response-date last-modified))) 
                                              stream)
                 (when expires-max-age
                   (write-http-response-headers `(("Expires" . ,(response-date (+ (get-universal-time) 
                                                                                  expires-max-age)))
                                                  ("Cache-Control" . ,(format nil "max-age=~d" expires-max-age)))
                                                stream))
                 (write-http-response-line "" stream)
                 (loop :for data :in data-chunks :do (write-sequence data stream))
                 (values t 200 data-size)))
              (t
               (write-http-response-status-line stream 200 "OK" (get-http-version http-request))
               (write-http-response-headers (standard-http-response-headers http-request
                                                                            :content-type mime-type 
                                                                            :content-length file-length)
                                            stream)
               (write-http-response-headers `(("Last-Modified" . ,(response-date last-modified))) stream)
               (when expires-max-age
                 (write-http-response-headers `(("Expires" . ,(response-date (+ (get-universal-time) expires-max-age)))
                                                ("Cache-Control" . ,(format nil "max-age=~d" expires-max-age)))
                                              stream))
               (write-http-response-line "" stream)
               (s-utils:copy-stream in stream (get-byte-buffer (get-http-connection http-request)))
               (values t 200 (file-length in))))))))

(defun static-resource-handler (s-http-server handler http-request stream)
  "Host static resources from a document root"
  (destructuring-bind (context document-root &rest options)
      handler
    (let* ((path (get-path http-request))
           (resource-pathname (compute-real-resource-pathname document-root path context
                                                              (or (getf options :pathname-builder)
                                                                  #'make-real-resource-pathname)))
           (expires-max-age (or (getf options :expires-max-age) (* 60 60))))
      (if (probe-file resource-pathname)
          (progn
            (logm s-http-server :debug "Serving ~s" resource-pathname)
            (host-static-resource http-request stream resource-pathname :expires-max-age expires-max-age))
        (progn
          (logm s-http-server :error "Failed to find ~s" resource-pathname)
          (values t 404 (standard-http-html-error-response http-request stream 404 "Resource Not Found" path)))))))

;; the favicon handler

(defun favicon-handler (s-http-server handler http-request stream)
  "Handle that annoying favicon.ico request in a more elegant way"
  (declare (ignore handler))
  (cond ((and *favicon* (arrayp *favicon*) (equal (array-element-type *favicon*) '(unsigned-byte 8)))
         (logm s-http-server :debug "Serving favicon ~d bytes" (length *favicon*))
         (host-static-bytes http-request stream *favicon* :mime-type "image/x-icon" :expires-max-age (* 60 60)))
        ((and *favicon* (pathnamep *favicon*) (probe-file *favicon*))
         (logm s-http-server :debug "Serving favicon ~s" *favicon*)
         (host-static-resource http-request stream *favicon* :expires-max-age (* 60 60)))
        (t
         (let ((path (get-path http-request)))
           (values t 404 (standard-http-html-error-response http-request stream 404 "Resource Not Found" path))))))

;; the random handler

(defun random-handler (s-http-server handler http-request stream)
  "Return a random hex string of a specified size"
  (declare (ignore s-http-server))
  (let ((size (or (second handler) 1024))
        (byte-buffer (get-byte-buffer (get-http-connection http-request)))
        (hex #.(map 'vector 'char-code "0123456789ABCDEF")))
    (loop :for i :below size :do (setf (aref byte-buffer i) 
                                       (aref hex (random 16))))
    (host-static-bytes http-request stream byte-buffer :mime-type "text/plain" :end size)))

;; the redirect handler

(defun redirect-handler (s-http-server handler http-request stream)
  "This handler immediately redirects to another URL"
  (destructuring-bind (context url)
      handler
    (logm s-http-server :info "Redirecting ~s to ~s" context url)
    (write-http-response-status-line stream 302 "Moved Temporarily" (get-http-version http-request))
    (write-http-response-headers `(,@(standard-http-response-headers http-request 
                                                                     :content-type nil :content-length 0)
                                   ("Location" . ,url))
                                 stream)
    (write-http-response-line "" stream)
    (values t 302 0)))

;; basic authentication support with a wrapping handler

(defun request-header-value (http-request header-name)
  "Get the value of a named header of http-request"
  (cdr (assoc header-name (s-http-server:get-headers http-request) 
              :test #'string-equal)))

(defun decode-basic-authorization (authorization)
  "Decode the Base64 encoding of username:password returning (username . password)"
  (let* ((decoded-string (map 'simple-string #'code-char 
                              (with-input-from-string (in authorization)
                                (s-base64:decode-base64-bytes in))))
         (tokens (s-utils:tokens decoded-string :separators '(#\:))))
    (cons (first tokens) (second tokens))))

(defun authorized-p (basic-authorization authenticator)
  "Check whether a basic-authorization is authorized by authenticator"
  (let ((username-password (decode-basic-authorization basic-authorization)))
    (cond ((and (consp authenticator)
                (member username-password authenticator :test #'equal))
           t)
          ((and (or (and (symbolp authenticator)
                         (fboundp authenticator))
                    (functionp authenticator)))
           (funcall authenticator username-password))
          (t
           nil))))

(defun basic-authentication-required-http-response (http-request stream realm)
  (let* ((path (s-http-server:get-path http-request))
         (content (with-output-to-string (out)
                    (format out
                            "~a<html lang=\"en\"><head><title>~a</title></head><body><h1>~a: ~s</h1></body></html>" 
                            *doctype-html-401-strict* "Unauthorized" "401 Unauthorized" path)))
         (headers `(("WWW-Authenticate" . ,(format nil "Basic realm=~s" realm))
                    ,@(s-http-server:standard-http-response-headers http-request
                                                                    :content-type "text/html"
                                                                    :content-length (length content)))))
    (s-http-server:write-http-response-status-line stream "401" "Unauthorized" (get-http-version http-request))
    (s-http-server:write-http-response-headers headers stream)
    (s-http-server:write-http-response-line "" stream)
    (write-string content stream)))

(defun wrap-with-basic-authentication (handler-function &key arguments authenticator realm)
  "Creates and returns a new handler that wraps handler-function and argument with basic authentication.
Authenticator is either a dotted alist of usernames and passwords or a function accepting (username . password).
Realm is for use in the WWW-Authenticate header response." 
  (lambda (s-http-server handler http-request stream)
    (let* ((authorization-header (request-header-value http-request "Authorization"))
           (authorization-tokens (s-utils:tokens authorization-header :separators '(#\Space)))
           (authentication-type (first authorization-tokens))
           (authorization (second authorization-tokens)))
      (cond ((and authorization-header 
                  (string-equal authentication-type "Basic") 
                  (authorized-p authorization authenticator))
             (let ((user (first (decode-basic-authorization authorization))))
               (logm s-http-server :info "Basic Authentication succeeded for ~a" user)
               (push `(:user . ,user) (s-http-server:get-headers http-request))
               (setf (get-user http-request) user)
               (funcall handler-function s-http-server `(,(first handler) ,@arguments) http-request stream))) 
            (t
             (basic-authentication-required-http-response http-request stream realm))))))

;; dw-bench implementation (see http://homepage.mac.com/svc/DW-Bench)

(defun dw-bench (s-http-server handler http-request stream)
  (declare (ignore handler s-http-server))
  (let ((contents (with-output-to-string (out)
                    (format out "<table border='1'>")
                    (dotimes (i 25)
                      (write-string "<tr>" out)
                      (dotimes (j 25)
                        (format out "<td>~d</td>" (* (1+ i) (1+ j))))
                      (write-string "</tr>" out))
                    (format out "</table><p>~a ~d</p>" 
                            (s-utils:format-universal-time (get-universal-time)) (get-internal-run-time)))))
    (values t 200 (standard-http-html-message-response http-request stream "DW-Bench Dynamic" contents))))

;; ps-handler listing all connections

(defun ps-handler (s-http-server handler http-request stream)
  "Show the state of all http connections known to the server"
  (declare (ignore handler))
  (let ((text (with-output-to-string (out)
                (format out "~s [debug-mode:~a]~%" s-http-server (get-debug-mode s-http-server))
                (loop :for c :in (get-http-connections s-http-server) :do
                      (format out "  ~s~%" c)
                      (format out "      stream: ~a~%" (or (get-stream c) "--"))
                      (format out "     process: ~a~%" (or (get-process c) "--"))))))
    (write-http-response-status-line stream 200 "OK" (get-http-version http-request))
    (write-http-response-headers (standard-http-response-headers http-request
                                                                 :content-type "text/plain"
                                                                 :content-length (length text)) 
                                 stream)
    (write-http-response-line "" stream)
    (write-string text stream)
    (values t 200 (length text))))

;; a text/plain echo debug handler

(defun echo-debug-handler (s-http-server handler http-request stream)
  "An echoing testing/debugging handler returning a simple text/plain status/echo/snoop page"
  (declare (ignore handler))
  (logm s-http-server :info "Running builtin echo-debug-handler")
  (let ((text (with-output-to-string (out)
                (format out "echo-debug-handler ~a ~a~%" (get-method http-request) (get-uri http-request))
                (format out "headers")
                (loop for (k . v) in (get-headers http-request) do (format out " ~a:~a" (string-capitalize k) v))
                (terpri out)
                (when (get-user http-request)
                  (format out "authenticated user is ~a~%" (get-user http-request)))
                (when (and (member (get-method http-request) '(:put :post))
                           (< 0 (s-utils:parse-integer-safely (request-header-value http-request :content-length) 
                                                              :default 0)))
                  (let* ((length (s-utils:parse-integer-safely (request-header-value http-request :content-length)))
                         (content (make-string length)))
                    (when (= (read-sequence content stream) length)
                      (format out "read content size ~d~%" length)
                      (when (equal (request-header-value http-request :content-type) "text/plain")
                        (format out "text/plain content ~a~%" content))))))))
    (write-http-response-status-line stream 200 "OK" (get-http-version http-request))
    (write-http-response-headers (standard-http-response-headers http-request
                                                                 :content-type "text/plain"
                                                                 :content-length (length text)) 
                                 stream)
    (write-http-response-line "" stream)
    (write-string text stream)
    (values t 200 (length text))))

;;;; eof
