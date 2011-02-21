;;; conditions.lisp

;;; Copyright (c) 2008-2009, Dr. Edmund Weitz.  All rights reserved.

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

