;;;; -*- mode: lisp -*-
;;;;
;;;; This is a small standalone Common Lisp HTTP Server
;;;;
;;;; Copyright (C) 2005-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-server)

;; the basic objects: s-http-server, http-connection and http-request

(defclass s-http-server ()
  ((port :accessor get-port :initarg :port :initform *http-server-port*)
   (name :accessor get-name :initarg :name :initform "s-http-server")
   (debug-mode :accessor get-debug-mode :initarg :debug-mode :initform t)
   (server-process :accessor get-server-process :initform nil)
   (http-connections :accessor get-http-connections :initform nil)
   (log-stream :accessor get-log-stream :initarg :log-stream :initform nil)
   (access-log-stream :accessor get-access-log-stream :initarg :access-log-stream :initform nil)
   (log-lock :accessor get-log-lock :initform (s-sysdeps:make-process-lock "s-http-server-log-lock"))
   (boot-time :accessor get-boot-time :initform nil)
   (last-periodic-check :accessor get-last-periodic-check :initform (get-universal-time))
   (contexts :accessor get-contexts :initarg :contexts :initform nil))
  (:documentation "The object representing a minimal standalone HTTP Server"))

(setf (documentation 'get-port 'function)
      "Get the TCP port used by this S-HTTP-SERVER"
      (documentation 'get-name 'function)
      "Get the current name of this S-HTTP-SERVER"
      (documentation 'get-debug-mode 'function)
      "Get the current mode of debugging of this S-HTTP-SERVER, t is on, nil is off"
      (documentation 'get-server-process 'function)
      "Get the current server process used by this S-HTTP-SERVER, nil if not running"
      (documentation 'get-client-processes 'function)
      "Get the list of active (including kept-alive) client processes used by this S-HTTP-SERVER"
      (documentation 'get-boot-time 'function)
      "Get the universal time when this S-HTTP-SERVER was last started, nil if not running"
      (documentation 'get-log-stream 'function)
      "Get the current stream used by this S-HTTP-SERVER for general logging, nil means no logging"
      (documentation 'get-access-log-stream 'function)
      "Get the current stream used by this S-HTTP-SERVER for access logging, nil means no logging"
      (documentation 'get-contexts 'function)
      "Get the current list of context bindings used by this S-HTTP-SERVER")

#-allegro
(setf (documentation '(setf get-port) 'function)
      "Set the port of this S-HTTP-SERVER (before starting the server)"
      (documentation '(setf get-name) 'function)
      "Set the name of this S-HTTP-SERVER"
      (documentation '(setf get-debug-mode) 'function)
      "Set the current debugging mode of this S-HTTP-SERVER, t is on, nil is off"
      (documentation '(setf get-log-stream) 'function)
      "Set the stream this S-HTTP-SERVER uses for general logging, nil means no logging"
      (documentation '(setf get-access-log-stream) 'function)
      "Set the stream this S-HTTP-SERVER uses for access logging, nil means no logging")

(defmethod print-object ((s-http-server s-http-server) stream)
  (print-unreadable-object (s-http-server stream :type t :identity t)
    (with-slots (name port server-process)
        s-http-server
      (format stream "~s port ~d ~a" name port (if server-process "running" "not running")))))

(defclass http-connection ()
  ((id :accessor get-id :initarg :id :initform -1)
   (state :accessor get-state :initform :new :type (member :new :active :pooled :dead))
   (stream :accessor get-stream :initarg :stream :initform nil)
   (process :accessor get-process :initarg :process :initform nil)
   (line-buffer :accessor get-line-buffer :initform (make-array 256 :element-type 'character :adjustable t))
   (byte-buffer :initform nil)
   (gzip-compressor :initform nil)
   (request-count :accessor get-request-count :initform 0)
   (timestamp :accessor get-timestamp :initform (get-universal-time))
   (http-server :accessor get-http-server :initarg :server :initform nil))
  (:documentation "The object representing a kept-alive HTTP connection and handling process"))

(setf (documentation 'get-http-server 'function)
      "Get the HTTP server this object belongs to, nil if not set")

(defmethod get-age ((http-connection http-connection))
  (- (get-universal-time) (get-timestamp http-connection)))

(defmethod print-object ((http-connection http-connection) output-stream)
  (print-unreadable-object (http-connection output-stream :type t :identity t)
    (with-slots (id state request-count)
        http-connection
      (format output-stream "~d ~(~a~) #~d" id state request-count))))

(defclass http-request ()
  ((method :accessor get-method :initarg :method :initform :GET)
   (uri :accessor get-uri :initarg :uri :initform (puri:parse-uri "/"))
   (http-version :accessor get-http-version :initarg :http-version :initform "HTTP/1.1")
   (headers :accessor get-headers :initarg :headers :initform '())
   (keep-alive :accessor get-keep-alive :initarg :keep-alive :initform (s-sysdeps:multiprocessing-capable-p))
   (user :accessor get-user :initform nil)
   (http-connection :accessor get-http-connection :initform nil))
  (:documentation "The object representing an HTTP request as being handled by the S-HTTP-SERVER"))

(setf (documentation 'get-method 'function)
      "Get the method (keyword :get :put :post :delete ..) of this HTTP request"
      (documentation 'get-uri 'function)
      "Get the URI object of this HTTP request"
      (documentation 'get-http-version 'function)
      "Get the HTTP version string of this HTTP request"
      (documentation 'get-headers 'function)
      "Get the dotted alist (:keyword . 'value') of request headers of this HTTP request"
      (documentation 'get-keep-alive 'function)
      "Is this a keep-alive request (either 1.0 or 1.1)"
      (documentation 'get-user 'functon)
      "The authenticated user for this request, nil if unknown"
      (documentation 'get-http-connection 'function)
      "The HTTP connection this request belongs to, nil if not set")

(defgeneric get-path (http-request)
  (:method ((http-request http-request))
   (puri:uri-path (get-uri http-request)))
  (:documentation "Get the path of this HTTP request"))

(defgeneric get-full-path (http-request)
  (:method ((http-request http-request))
   (puri:render-uri (get-uri http-request) nil))
  (:documentation "Get the full path of this HTTP request (including the query)"))

(defmethod print-object ((http-request http-request) stream)
  (print-unreadable-object (http-request stream :type t :identity t)
    (format stream "~a ~s" (get-method http-request) (get-path http-request))))

;; generics

(defgeneric start-server (server)
  (:documentation "Start the server"))

(defgeneric stop-server (server)
  (:documentation "Stop the server"))

(defgeneric logm (server kind format-string &rest args)
  (:documentation "Log a formatted message"))

(defgeneric handle-http-server-connection (server http-connection)
  (:documentation "Handle connection requests"))

(defgeneric find-handler (server http-request)
  (:documentation "Given http-request select a handler from server"))

(defgeneric register-context-handler (server context-prefix handler-function &key arguments at-end-p do-not-replace-p)
  ;; optional handler arguments can be specified
  ;; normally, an existing context binding with the same prefix is overwritten
  ;; normally, new handlers are pushed at the front of the context bindings list
  ;; if at-end-p is t, a new binding will be added at the end of the context bindings list
  ;; if do-not-replace-p is t, an existing binding will not be overwritten and a new one will be created
  (:documentation "Configure server so that every request starting with context-prefix is sent to handler-function"))

(defgeneric unregister-context-handler (server context-prefix &key only-first-p only-last-p)
  ;; normally, all context bindings matching exactly the specified prefix are deleted
  ;; if only-first-p is t, only the first context binding with prefix is deleted
  ;; if only-last-p is t , only the last context binding with prefix is deleted
  ;; if both only-fast-p and only-last-p are t, an error is signalled
  (:documentation "Remove any configuration of server for context-prefix"))

;; setup

(defmethod start-server ((s-http-server s-http-server))
  (stop-server s-http-server)
  (let ((connection-id 0))
    (flet ((connection-handler (client-socket-stream)
             (do-periodic-check s-http-server)
             (handle-new-http-server-connection s-http-server 
                                                client-socket-stream 
                                                (lambda () (incf connection-id)))))
      (setf (get-boot-time s-http-server) (get-universal-time))
      (when (not (s-sysdeps:multiprocessing-capable-p))
        (logm s-http-server :info "Starting a new server on port ~d and blocking" (get-port s-http-server)))
      (let ((process (s-sysdeps:start-standard-server :port (get-port s-http-server)
                                                      :name (get-name s-http-server)
                                                      :connection-handler #'connection-handler)))
        (setf (get-server-process s-http-server) process)
        (logm s-http-server :info "Started a new server on port ~d" (get-port s-http-server)))))
  s-http-server)

(defmethod stop-server ((s-http-server s-http-server))
  (let ((process (get-server-process s-http-server)))
    (when process
      (cleanup-connections s-http-server :do-not-pool t)
      (setf (get-http-connections s-http-server) nil)
      (s-sysdeps:kill-process process)
      (setf (get-server-process s-http-server) nil
            (get-boot-time s-http-server) nil)
      (logm s-http-server :info "Stopped server")))
  s-http-server)

(defmethod register-context-handler ((s-http-server s-http-server) context-prefix handler-function 
                                     &key arguments at-end-p do-not-replace-p)
  (let* ((new-handler-binding `(,handler-function ,context-prefix ,@arguments))
         (context-bindings (get-contexts s-http-server))
         (existing-binding (find context-prefix context-bindings :key #'second :test #'string=)))
    (if (or do-not-replace-p (null existing-binding))
        (if at-end-p
            (setf (get-contexts s-http-server) (append context-bindings (list new-handler-binding)))
          (push new-handler-binding (get-contexts s-http-server)))
      (loop :for binding :in context-bindings :do
            (destructuring-bind (function prefix &rest args)
                binding
              (declare (ignore function args))
              (when (string= prefix context-prefix)
                (setf (car binding) handler-function
                      (cddr binding) arguments)
                ;; we assume there is only one binding, more doesn't really make sense
                (return binding)))))))

(defmethod unregister-context-handler ((s-http-server s-http-server) context-prefix 
                                       &key only-first-p only-last-p)
  (let ((context-bindings (get-contexts s-http-server)))
    (cond ((and only-first-p only-last-p) (error "You cannot specify both only-first-p and only-last-p"))
          (only-first-p 
           (setf (get-contexts s-http-server) (delete context-prefix context-bindings
                                                      :key #'second :test #'string= :count 1)))
          (only-last-p 
           (setf (get-contexts s-http-server) (delete context-prefix context-prefix 
                                                      :key #'second :test #'string= :count 1 :from-end t)))
          (t 
           (setf (get-contexts s-http-server) (delete context-prefix context-bindings
                                                      :key #'second :test #'string= :count 1 :from-end t))))))

;; logging

(defmethod logm ((s-http-server s-http-server) kind format-string &rest args)
  (with-slots (debug-mode)
      s-http-server
  (unless (and (eql kind :debug) (not debug-mode))
    (let ((out (get-log-stream s-http-server)))
      (when out
        (s-sysdeps:with-process-lock ((get-log-lock s-http-server))
          (let ((server (string-upcase (get-name s-http-server)))
                (timestamp (s-utils:format-iso-gmt-time (get-universal-time)))
                (message (apply #'format nil format-string args)))
            (format out ";; ~(~a~) ~a ~a: ~a~%" server kind timestamp message)
            (when debug-mode (finish-output out)))))))))

(defmethod log-access ((s-http-server s-http-server) http-connection http-request response bytes)
  (let ((out (get-access-log-stream s-http-server)))
    (when out
      (let ((client-ip (or (when (get-stream http-connection)
                             (s-sysdeps:get-socket-stream-property (get-stream http-connection) :remote-host))
                           "-"))
            (timestamp (s-utils:format-universal-time (get-universal-time)
                                                      :format +common-log-timestamp-format+
                                                      :decode-in-timezone 0))
            (method (get-method http-request))
            (resource (get-full-path http-request))
            (protocol (get-http-version http-request))
            (user (or (get-user http-request) "-")))
        (ecase +access-log-format+
          (:common-log-format 
           (s-sysdeps:with-process-lock ((get-log-lock s-http-server))
             (format out "~a - ~a ~a \"~a ~a ~a\" ~d ~d~%"
                     client-ip user timestamp method resource protocol response bytes)))
          (:extended-common-log-format
           (let ((referer (or (request-header-value http-request "Referer") "-"))
                 (agent (or (request-header-value http-request "User-Agent") "-")))
             (s-sysdeps:with-process-lock ((get-log-lock s-http-server))
               (format out "~a - ~a ~a \"~a ~a ~a\" ~d ~d ~s ~s~%"
                       client-ip user timestamp method resource protocol response bytes referer agent)))))))))

;; connection & process/thread management

(defmethod handle-new-http-server-connection ((s-http-server s-http-server) socket-stream connection-id)
  (let ((connection (when +pool-connections+
                      (find-if (lambda (c) 
                                 (and (eql (get-state c) :pooled)
                                      (get-process c)
                                      #+lispworks(mp:process-stopped-p (get-process c))))
                               (get-http-connections s-http-server)))))
    (if connection
        (progn
          (logm s-http-server :debug "Reusing ~s" connection)
          (setf (get-stream connection) socket-stream)
          #+lispworks (mp:process-unstop (get-process connection))
          #-lispworks (eror "Connection reuse not yet implemeted"))
      (let ((new-connection-id (funcall connection-id)))
        (logm s-http-server :debug "Creating new connection handler")
        (let* ((new-connection (make-instance 'http-connection 
                                              :id new-connection-id
                                              :stream socket-stream
                                              :server s-http-server))
               (new-process (s-sysdeps:run-process (format nil "connection-handler-~d" new-connection-id)
                                                   'handle-http-server-connection 
                                                   s-http-server
                                                   new-connection)))
          (setf (get-process new-connection) new-process)
          (push new-connection (get-http-connections s-http-server)))))))
  
(defmethod kill-connection ((http-connection http-connection) &optional do-not-pool)
  (with-slots (process stream http-server state)
      http-connection
    (when stream
      (logm http-server :debug "Closing ~a" stream)
      (ignore-errors 
        (close stream :abort t))
      (setf stream nil))
    (when process 
      (if (or (not +pool-connections+) do-not-pool)
          (progn
            (logm http-server :debug "Killing ~a" process)
            (s-sysdeps:kill-process process)
            (setf process nil state :dead))
        (setf state :pooled)))))

;; internal (periodic) tasks

(defmethod do-periodic-check ((s-http-server s-http-server))
  (let ((now (get-universal-time)))
    (when (< +period-check-interval+ (- now (get-last-periodic-check s-http-server)))
      (logm s-http-server :debug "Running periodic tasks")
      (cleanup-dead-connections s-http-server)
      (cleanup-old-connections s-http-server)
      (cleanup-old-pooled-connections s-http-server)
      (cleanup-excess-pooled-connections s-http-server)
      (cleanup-excess-alive-connections s-http-server)
      (flush-log-streams s-http-server)
      (setf (get-last-periodic-check s-http-server) now))))

(defmethod cleanup-connections ((s-http-server s-http-server) 
                                &key (filter 'identity) (selector 'identity) do-not-pool (threshold 0))
  (let* ((all-connections (get-http-connections s-http-server))
         (filtered-connections (remove-if-not filter all-connections))
         (connections-to-remove (when (< threshold (length filtered-connections))
                                  (funcall selector filtered-connections))))
    (when (and connections-to-remove (< 0 threshold))
      (setf connections-to-remove (subseq connections-to-remove 0 (- (length connections-to-remove) threshold))))
    (when connections-to-remove
      (loop :for connection :in connections-to-remove :do
            (logm s-http-server :debug "Cleaning up ~s [~a]" connection (if do-not-pool :do-not-pool :pool))
            (kill-connection connection do-not-pool))
      (when (or (not +pool-connections+) do-not-pool)
        (setf (get-http-connections s-http-server) 
              (set-difference (get-http-connections s-http-server) connections-to-remove))))))

(defmethod cleanup-dead-connections ((s-http-server s-http-server))
  (cleanup-connections s-http-server
                       :filter (lambda (c)
                                 (or (eql (get-state c) :dead)
                                     (null (get-process c))
                                     #+lispworks (not (mp:process-alive-p (get-process c)))))
                       :do-not-pool t))

(defmethod cleanup-old-connections ((s-http-server s-http-server))
  (let ((now (get-universal-time)))
    (cleanup-connections s-http-server
                         :filter (lambda (c)
                                   (and (eql (get-state c) :active)
                                        (< +allowed-connection-keepalive-age+ (- now (get-timestamp c))))))))

(defmethod cleanup-old-pooled-connections ((s-http-server s-http-server))
  (let ((now (get-universal-time)))
    (cleanup-connections s-http-server
                         :filter (lambda (c)
                                   (and (eql (get-state c) :pooled)
                                        (< +allowed-connection-pooled-age+ (- now (get-timestamp c)))))
                         :do-not-pool t)))

(defmethod cleanup-excess-pooled-connections ((s-http-server s-http-server))
  (cleanup-connections s-http-server
                       :filter (lambda (c) (eql (get-state c) :pooled))
                       :threshold +allowed-pooled-connections+
                       :do-not-pool t))

(defmethod cleanup-excess-alive-connections ((s-http-server s-http-server))
  (cleanup-connections s-http-server
                       :filter (lambda (c) (eql (get-state c) :active))
                       :threshold +allowed-keepalive-connections+
                       :selector (lambda (connections) (sort connections #'< :key #'get-timestamp))
                       :do-not-pool t))

(defmethod flush-log-streams ((s-http-server s-http-server))
  (with-slots (log-stream access-log-stream) 
      s-http-server
    (when log-stream (force-output log-stream))
    (when access-log-stream (force-output access-log-stream))))

;; low level input/output - we are using a reusable line-buffer to read lines

(defun read-crlf-line (line-buffer stream &optional (eof-error-p t) eof-value)
  "Read a CRLF termintated line from a character input stream into line-buffer. Return length excluding CRLF."
  (let ((offset 0)
        (previous-char #\null))
    (loop :for char = (read-char stream eof-error-p eof-value)
          :do (cond ((equal char eof-value) 
                     (return eof-value))
                    ((and (char= char #\linefeed)
                          (char= previous-char #\return))
                     (return (1- offset)))
                    ;; for the sake of robustness we allow for a lone LF to terminate a line as well
                    ((char= char #\linefeed)
                     (return offset))
                    ((>= offset (length line-buffer))
                     (adjust-array line-buffer (* 2 (length line-buffer))))
                    (t 
                     (setf (char line-buffer offset) char)
                     (setf previous-char char)
                     (incf offset))))))

(defun write-http-response-line (string &optional (stream *standard-output*))
  "Write string to stream, ending with the HTTP end of line convention (CR+LF)"
  (write-string string stream)
  (write-char #\return stream)
  (write-char #\linefeed stream))

(defun format-http-response-line (stream format-string &rest args)
  (write-http-response-line (apply #'format nil format-string args) stream))

;; parsing requests

(define-condition http-request-error (error) ())
(define-condition missing-http-request-line (http-request-error) ())
(define-condition bogus-http-request-method (http-request-error) ())
(define-condition bogus-http-request-uri (http-request-error) ())

(defun parse-http-request-line (stream line-buffer)
  (let* ((line-length (read-crlf-line line-buffer stream nil))
         (tokens (when line-length (s-utils:tokens line-buffer :separators '(#\space) :end line-length))))
    (if tokens
        (let ((method (if (member (first tokens) +allowed-http-methods+ :test 'string-equal)
                          (intern (first tokens) :keyword)
                        (error 'bogus-http-request-method)))
              (uri (handler-case (puri:parse-uri (second tokens))
                     (puri:uri-parse-error ()
                       (error 'bogus-http-request-uri))))
              (http-version (third tokens)))
          (if (and method uri http-version)
              (values method uri http-version)
            (error 'http-request-error)))
      (error 'missing-http-request-line))))

(defun header-field-name->keyword (string &optional (start 0) end)
  ;; optimize the case of common request headers and avoid interning/upcasing
  (let ((common-header (find-if #'(lambda (x) 
                                    (string-equal (car x) string :start2 start :end2 end))
                                +common-request-headers+)))
    (if common-header
        (cdr common-header)
      (intern (nstring-upcase (subseq string start end)) :keyword))))

(defun header-field-value->string (string &optional (start 0) end)
  ;; skip leading whitespace
  (loop :while (and (< start end) 
                    (member (char string start) '(#\space #\tab) :test #'char=))
        :do (incf start))
  (subseq string start end))

(defun parse-http-request-headers (stream line-buffer)
  (loop :for line-length = (read-crlf-line line-buffer stream nil)
        :until (or (null line-length)
                   (zerop line-length))
        :collect (let ((colon (position #\: line-buffer :end line-length :test #'char=)))
                   (cons (header-field-name->keyword line-buffer 0 colon)
                         (header-field-value->string line-buffer (1+ colon) line-length)))))

(defun parse-http-request (stream line-buffer)
  (multiple-value-bind (http-method uri http-version)
      (parse-http-request-line stream line-buffer)
    (let* ((request-headers (parse-http-request-headers stream line-buffer))
           (http-request (make-instance 'http-request 
                                        :method http-method
                                        :uri uri
                                        :http-version http-version
                                        :headers request-headers)))
      (when (and (string-equal http-version "HTTP/1.0")
                 (string-not-equal (cdr (assoc "Connection" request-headers :test #'string-equal)) 
                                   "Keep-Alive"))
        (setf (get-keep-alive http-request) nil))
      (setf *last-http-request* http-request)
      http-request)))

;; writing/generating responses

(defun write-http-response-status-line (stream &optional (status-code 200) (string "OK") (http-version "HTTP/1.1"))
  "Write an HTTP Response Status line to stream, using status-code string and http-version"
  (format-http-response-line stream "~a ~d ~a" http-version status-code string))

(defun write-http-response-headers (headers stream)
  "Write the headers alist as HTTP Response Headers to stream"
  (loop :for (header-key . header-value) :in headers
        :do (format-http-response-line stream "~a: ~a" header-key header-value)))

(defun response-date (&optional (universal-time (get-universal-time)))
  "Generate a GMT HTTP Response Date"
  (s-utils:format-universal-time universal-time
                                 :format '(:day-name ", " :date2 #\Space :month-name #\Space :year #\Space 
                                           :hour #\: :minute #\: :second " GMT")
                                 :decode-in-timezone 0))

(defun parse-http-date (string)
  "Return the CL universal-time represented in string using GMT HTTP Date format, or nil on error"
  (let ((tokens (rest (s-utils:tokens string :separators '(#\Space #\, #\:)))))
    (when (= (length tokens) 7)
      (let ((date (s-utils:parse-integer-safely (first tokens)))
            (month (position (second tokens) s-utils:+us-month-names+ :test #'string-equal))
            (year (s-utils:parse-integer-safely (third tokens)))
            (hour (s-utils:parse-integer-safely (fourth tokens)))
            (minute (s-utils:parse-integer-safely (fifth tokens)))
            (second (s-utils:parse-integer-safely (sixth tokens))))
        (when (and date month year hour minute second)
          (encode-universal-time second minute hour date (1+ month) year 0))))))

(defun standard-http-response-headers (http-request &key (content-type "text/plain") content-length)
  "Generate the standard headers alist given context-type and context-length, managing old-style Keep-Alive"
  `(("Server" . ,*http-server-identification*)
    ("Date" . ,(response-date))
    ,@(when content-type
        `(("Content-Type" . ,content-type)))
    ,@(when content-length
        `(("Content-Length" . ,content-length)))
    ,@(when (and http-request
                 (get-keep-alive http-request) 
                 (string-equal (get-http-version http-request) "HTTP/1.0"))
        `(("Connection" . "Keep-Alive")))
    ,@(when (and http-request
                 (not (get-keep-alive http-request)) 
                 (string-equal (get-http-version http-request) "HTTP/1.1"))
        `(("Connection" . "Close")))))

(defun escape (string)
  (with-output-to-string (stream)
    (loop :for char :across string
          :do (case char
                (#\& (write-string "&amp;" stream))
                (#\< (write-string "&lt;" stream))
                (#\> (write-string "&gt;" stream))
                (#\" (write-string "&quot;" stream))
                ((#\newline #\return #\tab) (write-char char stream))
                (t (if (and (<= 32 (char-code char))
                            (<= (char-code char) 126))
                       (write-char char stream)
                     (progn
                       (write-string "&#x" stream)
                       (write (char-code char) :stream stream :base 16)
                       (write-char #\; stream))))))))

(defun standard-http-html-message-response (http-request stream title message &optional (status 200) (string "OK"))
  "Generate and write a standard HTML message as HTTP Response using title, message, status and string"
  (let ((content (with-output-to-string (out)
                   (format out 
                           "~a<html lang=\"en\"><head><title>~a</title></head><body><h1>~a</h1>~a</body></html>~%"
                           *doctype-html-401-strict* title title message))))
    (when stream
      (write-http-response-status-line stream status string (get-http-version http-request))
      (write-http-response-headers (standard-http-response-headers http-request
                                                                   :content-type "text/html"
                                                                   :content-length (length content)) 
                                   stream)
      (write-http-response-line "" stream)
      (write-string content stream)
      (length content))))

(defun standard-http-html-error-response (http-request stream code reason extra)
  "Generate and write a standard HTML error as HTTP Response using code, reason and extra"
  (standard-http-html-message-response http-request 
                                       stream
                                       reason
                                       (format nil "<p>~d - ~a: ~a</p>" code reason (escape (prin1-to-string extra)))
                                       code
                                       reason))

;; core server implementation (http request/repsonse loop and dispatching to handlers)

(defmethod find-handler ((s-http-server s-http-server) http-request)
  (let ((path (get-path http-request)))
    (loop :for context-binding :in (get-contexts s-http-server)
          :do (destructuring-bind (handler context &rest rest)
                  context-binding
                (declare (ignore handler rest))
                (if (string= path context :end1 (min (length context) (length path)))
                    (return-from find-handler context-binding))))))

(defmethod handle-one-http-request-response ((s-http-server s-http-server) http-connection)
  (with-slots (stream line-buffer id)
      http-connection
    (let* ((http-request
            (handler-case (parse-http-request stream line-buffer)
              (http-request-error (condition)
                (logm s-http-server :error "[~d] Ignoring Bad Request ~s" id condition)
                (error condition))))
           (handler (progn
                      (setf (get-http-connection http-request) http-connection)
                      (find-handler s-http-server http-request))))
      (logm s-http-server :debug "[~d] Handling ~s" id http-request)
      (multiple-value-bind (success response bytes)
          (if handler
              (if (get-debug-mode s-http-server)
                  (funcall (first handler) 
                           s-http-server (rest handler) http-request stream)
                (multiple-value-bind (result second third)
                    (ignore-errors (funcall (first handler) 
                                            s-http-server (rest handler) http-request stream))
                  (if result
                      (values result second third)
                    (progn 
                      (logm s-http-server :error "[~d] Handler ~s failed for ~s" id handler http-request)
                      (values t 500 (standard-http-html-error-response http-request stream 500 "Internal Server Error" 
                                                                       second))))))
            (progn
              (logm s-http-server :error "[~d] No handler found for ~s" id http-request)
              (values t 404 (standard-http-html-error-response http-request stream 404 "Resource Not Found" 
                                                               (get-path http-request)))))
        (declare (ignore success))
        (log-access s-http-server http-connection http-request response bytes))
      (finish-output stream)
      (setf (get-http-connection http-request) nil)
      http-request)))

(defmethod handle-http-server-connection ((s-http-server s-http-server) http-connection)
  (loop
   (unwind-protect
       (loop
        (setf (get-timestamp http-connection) (get-universal-time)
              (get-state http-connection) :active)
        (incf (get-request-count http-connection))
        (if (get-debug-mode s-http-server)
            (unless (handler-case
                        (get-keep-alive (handle-one-http-request-response s-http-server http-connection))
                      #+lispworks(comm:socket-error () nil)
                      #+lispworks(simple-type-error (c)
                                   (if (eql (type-error-expected-type c) 'comm::ssl-obj)
                                       nil
                                     (error c)))
                      (stream-error () nil)
                      (http-request-error () nil))
              (return))
          (unless (ignore-errors 
                    (get-keep-alive (handle-one-http-request-response s-http-server http-connection)))
            (return))))
     (kill-connection http-connection)
     (if +pool-connections+
         (progn
           (logm s-http-server :debug "Stopping ~s and waiting to be reused" http-connection)
           #+lispworks(mp:process-stop (s-sysdeps:current-process) "Waiting to be reused")
           #-lispworks(eror "Connection reuse not yet implemeted"))
       (return)))))

;;;; eof
