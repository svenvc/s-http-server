;;;; -*- Mode: LISP -*-
;;;;
;;;; The S-HTTP-SERVER ASDF system definition
;;;;
;;;; Copyright (C) 2005-2009 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-http-server
  :name "S-HTTP-SERVER"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "1"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "S-HTTP-SERVER is a small standalone Common Lisp HTTP Server"
  :long-description "S-HTTP-SERVER is a small standalone Common Lisp HTTP Server"
  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "globals" :depends-on ("package"))
                 (:file "http-server" :depends-on ("package" "globals"))
                 #+lispworks (:file "https-server" :depends-on ("package" "http-server" "globals"))
                 (:file "handlers" :depends-on ("package" "http-server" "globals"))
                 (:file "tools" :depends-on ("package" "http-server" "handlers" "globals")))))
  :depends-on (:s-utils :s-sysdeps :s-base64 :puri :salza2))

;;;; eof
