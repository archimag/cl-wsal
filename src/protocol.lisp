;;;; request.lisp

(in-package #:wsal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-parameters (request)
  (:documentation "Returns an alist of all GET parameters (as provided via the request URI). The car of each element of this list is the parameter's name while the cdr is its value (as a string). The elements of this list are in the same order as they were within the request URI."))

(defgeneric post-parameters (request)
  (:documentation "Returns an alist of all POST parameters (as provided via the request's body). The car of each element of this list is the parameter's name while the cdr is its value. The elements of this list are in the same order as they were within the request's body."))

(defgeneric cookies-in (request)
  (:documentation "Returns an alist of all cookies associated with the REQUEST object request."))

(defgeneric query-string (request)
  (:documentation "Returns the query string of the REQUEST object request. That's the part behind the question mark (i.e. the GET parameters)."))

(defgeneric request-method (request)
  (:documentation "Returns the request method as a Lisp keyword."))

(defgeneric request-uri (request)
  (:documentation "Returns the request URI."))

(defgeneric server-protocol (request)
  (:documentation "Returns the request protocol as a Lisp keyword."))

(defgeneric headers-in (request)
  (:documentation "Returns the incoming header with name name. name can be a keyword (recommended) or a string."))

(defgeneric remote-address (request)
  (:documentation "Returns the address the current request originated from."))

(defgeneric remote-port (request)
  (:documentation "Returns the port the current request originated from."))

(defgeneric script-name (request)
  (:documentation "Returns the file name of the REQUEST object request. That's the requested URI without the query string (i.e the GET parameters)."))

(defgeneric raw-post-data (request &key encoding force-text force-binary &allow-other-keys)
  (:documentation "Returns the content sent by the client in the request body if there was any (unless the content type was multipart/form-data in which case NIL is returned)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reply object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric headers-out (reply)
  (:documentation "Returns an alist of the outgoing headers associated with the REPLY object reply."))

(defgeneric content-length (reply)
  (:documentation "The outgoing 'Content-Length' http header of reply."))

(defgeneric (setf content-length) (newvalue reply)
  (:documentation "Set the outgoing 'Content-Length' http header of reply."))

(defgeneric content-type (reply)
  (:documentation "The outgoing 'Content-Type' http header of reply."))

(defgeneric (setf content-type) (newvalue reply)
  (:documentation "Set the outgoing 'Content-Type' http header of reply."))

(defgeneric cookies-out (reply)
  (:documentation "Return an alist of the outgoing cookies associated with the REPLY object reply."))

(defgeneric (setf cookies-out) (newvalue reply)
  (:documentation "Set an alist of the outgoing cookies associated with the REPLY object reply."))

(defgeneric return-code (reply)
  (:documentation "Get the http return code of reply. The return code of each REPLY object is initially set to +HTTP-OK+."))

(defgeneric (setf return-code) (newvalue reply)
  (:documentation "Set the http return code of reply."))

(defgeneric reply-external-format (reply)
  (:documentation "Get the external format of reply which is used for character output."))

(defgeneric (setf reply-external-format) (newvalue reply)
  (:documentation "Set the external format of reply which is used for character output."))