;;;; interface.lisp

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:wsal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; request
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *request*)

(defun script-name* (&optional (request *request*))
  "Returns the file name of the REQUEST object REQUEST. That's the
requested URI without the query string \(i.e the GET parameters)."
  (script-name request))

(defun query-string* (&optional (request *request*))
  "Returns the query string of the REQUEST object REQUEST. That's
the part behind the question mark \(i.e. the GET parameters)."
  (query-string request))

(defun get-parameters* (&optional (request *request*))
  "Returns an alist of the GET parameters associated with the REQUEST
object REQUEST."
  (get-parameters request))

(defun post-parameters* (&optional (request *request*))
  "Returns an alist of the POST parameters associated with the REQUEST
object REQUEST."
  (post-parameters request))

(defun headers-in* (&optional (request *request*))
  "Returns an alist of the incoming headers associated with the
REQUEST object REQUEST."
  (headers-in request))

(defun cookies-in* (&optional (request *request*))
  "Returns an alist of all cookies associated with the REQUEST object
REQUEST."
  (cookies-in request))

(defgeneric header-in (name request)
  (:documentation "Returns the incoming header with name NAME.  NAME
can be a keyword \(recommended) or a string.")
  (:method (name request)
   (cdr (assoc* name (headers-in request)))))

(defun header-in* (name &optional (request *request*))
  "Returns the incoming header with name NAME.  NAME can be a keyword
\(recommended) or a string."
  (header-in name request))

(defun authorization (&optional (request *request*))
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (header-in :authorization request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (ppcre:scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (ppcre:split ":" (base64:base64-string-to-string (subseq authorization start)))
        (values user password)))))

(defun remote-address* (&optional (request *request*))
  "Returns the address the current request originated from."
  (remote-address request))

(defun remote-port* (&optional (request *request*))
  "Returns the port the current request originated from."
  (remote-port request))

(defun real-remote-address (&optional (request *request*))
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (header-in :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (ppcre:split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-address request)))))

(defun host (&optional (request *request*))
  "Returns the 'Host' incoming http header value."
  (header-in :host request))

(defun request-uri* (&optional (request *request*))
  "Returns the request URI."
  (request-uri request))

(defun request-method* (&optional (request *request*))
  "Returns the request method as a Lisp keyword."
  (request-method request))

(defun server-protocol* (&optional (request *request*))
  "Returns the request protocol as a Lisp keyword."
  (server-protocol request))

(defun user-agent (&optional (request *request*))
  "Returns the 'User-Agent' http header."
  (header-in :user-agent request))

(defun cookie-in (name &optional (request *request*))
  "Returns the cookie with the name NAME \(a string) as sent by the
browser - or NIL if there is none."
  (cdr (assoc name (cookies-in request) :test #'string=)))

(defun referer (&optional (request *request*))
  "Returns the 'Referer' \(sic!) http header."
  (header-in :referer request))

(defun get-parameter (name &optional (request *request*))
  "Returns the GET parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (get-parameters request) :test #'string=)))

(defun post-parameter (name &optional (request *request*))
  "Returns the POST parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (post-parameters request) :test #'string=)))

(defun parameter (name &optional (request *request*))
  "Returns the GET or the POST parameter with name NAME \(a string) -
or NIL if there is none.  If both a GET and a POST parameter with the
same name exist the GET parameter is returned.  Search is
case-sensitive."
  (or (get-parameter name request)
      (post-parameter name request)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *reply*)

(defun headers-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing headers associated with the
REPLY object REPLY."
  (headers-out reply))

(defun cookies-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing cookies associated with the
REPLY object REPLY."
  (cookies-out reply))

(defun (setf cookies-out*) (new-value &optional (reply *reply*))
  "Sets the alist of the outgoing cookies associated with the REPLY
object REPLY."
  (setf (cookies-out reply) new-value))

(defun content-type* (&optional (reply *reply*))
  "The outgoing 'Content-Type' http header of REPLY."
  (content-type reply))

(defun (setf content-type*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Type' http header of REPLY."
  (setf (header-out :content-type reply) new-value))

(defun content-length* (&optional (reply *reply*))
  "The outgoing 'Content-Length' http header of REPLY."
  (content-length reply))

(defun (setf content-length*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Length' http header of REPLY."
  (setf (header-out :content-length reply) new-value))

(defun return-code* (&optional (reply *reply*))
  "The http return code of REPLY.  The return codes Hunchentoot can
handle are defined in specials.lisp."
  (return-code reply))

(defun (setf return-code*) (new-value &optional (reply *reply*))
  "Sets the http return code of REPLY."
  (setf (return-code reply) new-value))

(defun reply-external-format* (&optional (reply *reply*))
  "The external format of REPLY which is used for character output."
  (reply-external-format reply))

(defun (setf reply-external-format*) (new-value &optional (reply *reply*))
  "Sets the external format of REPLY."
  (setf (reply-external-format reply) new-value))

(defun header-out-set-p (name &optional (reply *reply*))
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  (assoc* name (headers-out reply)))

(defun header-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out reply))))

(defun cookie-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out reply) :test #'string=)))

(defgeneric (setf header-out) (new-value name &optional reply)
  (:documentation "Changes the current value of the outgoing http
header named NAME \(a keyword or a string).  If a header with this
name doesn't exist, it is created.")
  (:method (new-value (name symbol) &optional (reply *reply*))
   ;; the default method
   (let ((entry (assoc name (headers-out reply))))
     (if entry
       (setf (cdr entry) new-value)
       (setf (headers-out reply)
             (acons name new-value (headers-out reply))))
     new-value))
  (:method (new-value (name string) &optional (reply *reply*))
   "If NAME is a string, it is converted to a keyword first."
   (setf (header-out (as-keyword name :destructivep nil) reply) new-value)))

  ;; (:method :after (new-value (name (eql :content-length)) &optional (reply *reply*))
  ;;  "Special case for the `Content-Length' header."
  ;;  (check-type new-value integer)
  ;;  (setf (slot-value reply 'content-length) new-value))
  ;; (:method :after (new-value (name (eql :content-type)) &optional (reply *reply*))
  ;;  "Special case for the `Content-Type' header."
  ;;  (check-type new-value (or null string))
  ;;  (setf (slot-value reply 'content-type) new-value)))