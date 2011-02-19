
(in-package #:wsal)

(define-condition wsal-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to web server."))

(define-condition wsal-error (wsal-condition error)
  ()
  (:documentation "Superclass for all errors related to web server."))

(define-condition wsal-simple-error (wsal-error simple-condition)
  ()
  (:documentation "Like WSAL-ERROR but with formatting capabilities."))

(defun wsal-error (format-control &rest format-arguments)
  "Signals an error of type WSAL-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'wsal-simple-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition wsal-warning (wsal-condition warning)
  ()
  (:documentation "Superclass for all warnings related to web server."))

(define-condition wsal-simple-warning (wsal-warning simple-condition)
  ()
  (:documentation "Like WSAL-WARNING but with formatting capabilities."))

(defun wsal-warn (format-control &rest format-arguments)
  "Signals a warning of type WSAL-SIMPLE-WARNING with the
provided format control and arguments."
  (warn 'wsal-simple-warning
        :format-control format-control
        :format-arguments format-arguments))


(define-condition parameter-error (wsal-simple-error)
  ()
  (:documentation "Signalled if a function was called with incosistent or illegal parameters."))

(defun parameter-error (format-control &rest format-arguments)
  "Signals an error of type PARAMETER-ERROR with the provided
format control and arguments."
  (error 'parameter-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition operation-not-implemented (wsal-error)
  ((operation :initarg :operation
              :reader wsal-operation-not-implemented-operation
              :documentation "The name of the unimplemented operation."))
  (:report (lambda (condition stream)
             (format stream "The operation ~A is not yet implemented for the implementation ~A.
Consider sending a patch..."
                     (wsal-operation-not-implemented-operation condition)
                     (lisp-implementation-type))))
  (:documentation "This warning is signalled when an operation \(like
SETUID for example) is not implemented for a specific Lisp."))

(defun not-implemented (name)
  "Used to signal an error if an operation named NAME is not implemented."
  (error 'operation-not-implemented :operation name))

