;;; cookie.lisp

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

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun http-token-p (token)
  "Tests whether TOKEN is a string which is a valid 'token'
according to HTTP/1.1 \(RFC 2068)."
  (and (stringp token)
       (plusp (length token))
       (every (lambda (char)
                (and ;; CHAR is US-ASCII but not control character or ESC
                     (< 31 (char-code char) 127)
                     ;; CHAR is not 'tspecial'
                     (not (find char "()<>@,;:\\\"/[]?={} " :test #'char=))))
              token)))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun url-encode (string &optional encoding)
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (babel:string-to-octets string
                                                                     :start index
                                                                     :end (1+ index)
                                                                     :encoding encoding)
                            do (format s "%~2,'0x" octet)))))))

(defmacro upgrade-vector (vector new-type &key converter)
  "Returns a vector with the same length and the same elements as
VECTOR \(a variable holding a vector) but having element type
NEW-TYPE.  If CONVERTER is not NIL, it should designate a function
which will be applied to each element of VECTOR before the result is
stored in the new vector.  The resulting vector will have a fill
pointer set to its end.

The macro also uses SETQ to store the new vector in VECTOR."
  `(setq ,vector
         (loop with length = (length ,vector)
               with new-vector = (make-array length
                                             :element-type ,new-type
                                             :fill-pointer length)
               for i below length
               do (setf (aref new-vector i) ,(if converter
                                               `(funcall ,converter (aref ,vector i))
                                               `(aref ,vector i)))
               finally (return new-vector))))

(defun url-decode (string &optional (external-format :utf-8))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (prog1
                      (parse-integer string :start i :end (+ i length) :radix 16)
                    (incf i length)))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (aref string i))
                (advance ()
                  (setq char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setq unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+) #\Space)
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (babel:octets-to-string vector :encoding external-format)))))

(defgeneric assoc* (thing alist)
  (:documentation "Similar to CL:ASSOC, but 'does the right thing' if
THING is a string or a symbol.")
  (:method ((thing symbol) alist)
   (assoc thing alist :test #'eq))
  (:method ((thing string) alist)
   (assoc thing alist :test #'string-equal))
  (:method (thing alist)
   (assoc thing alist :test #'eql)))

(defun make-keyword (string destructivep)
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Destructively modifies STRING if DESTRUCTIVEP is true."
  (intern (funcall
           (if destructivep
             (if (eq (readtable-case *readtable*) :upcase)
               #'nstring-upcase
               #'nstring-downcase)
             (if (eq (readtable-case *readtable*) :upcase)
               #'string-upcase
               #'string-downcase))
           string)
          :keyword))

(defun reason-phrase (return-code)
  "Returns a reason phrase for the HTTP return code RETURN-CODE
\(which should be an integer) or NIL for return codes Hunchentoot
doesn't know."
  (gethash return-code *http-reason-phrase-map* 
           "No reason phrase known"))

(defun form-url-encoded-list-to-alist (form-url-encoded-list &optional (encoding :utf-8))
  "Converts a list FORM-URL-ENCODED-LIST of name/value pairs into an
alist.  Both names and values are url-decoded while doing this."
  (mapcar #'(lambda (entry)
              (destructuring-bind (name &optional value)
                  (ppcre:split "=" entry :limit 2)
                (cons (string-trim " " (url-decode name encoding))
                      (url-decode (or value "") encoding))))
          form-url-encoded-list))

(defun convert-hack (string encoding)
  "The rfc2388 package is buggy in that it operates on a character
stream and thus only accepts encodings which are 8 bit transparent.
In order to support different encodings for parameter values
submitted, we post process whatever string values the rfc2388 package
has returned."
  (babel:octets-to-string (map '(vector (unsigned-byte 8) *) 'char-code string)
                          :encoding encoding))

(defun parse-rfc2388-form-data (stream content-type-header encoding)
  "Creates an alist of POST parameters from the stream STREAM which is
supposed to be of content type 'multipart/form-data'."
  (let* ((parsed-content-type-header (wsal.rfc2388:parse-header content-type-header :value))
	 (boundary (or (cdr (wsal.rfc2388:find-parameter
                             "BOUNDARY"
                             (wsal.rfc2388:header-parameters parsed-content-type-header)))
		       (return-from parse-rfc2388-form-data))))
    (loop for part in (wsal.rfc2388:parse-mime stream boundary)
          for headers = (wsal.rfc2388:mime-part-headers part)
          for content-disposition-header = (wsal.rfc2388:find-content-disposition-header headers)
          for name = (cdr (wsal.rfc2388:find-parameter
                           "NAME"
                           (wsal.rfc2388:header-parameters content-disposition-header)))
          when name
          collect (cons name
                        (let ((contents (wsal.rfc2388:mime-part-contents part)))
                          (if (pathnamep contents)
                            (list contents
                                  (wsal.rfc2388:get-file-name headers)
                                  (wsal.rfc2388:content-type part :as-string t))
                            (convert-hack contents encoding)))))))

(defun parse-multipart-form-data (request encoding)
  "Parse the REQUEST body as multipart/form-data, assuming that its
content type has already been verified.  Returns the form data as
alist or NIL if there was no data or the data could not be parsed."
  (with-input-from-string (content-stream (raw-post-data request :encoding :latin1))
    (parse-rfc2388-form-data content-stream 
                             (header-in :content-type request)
                             encoding)))



(defun starts-with-scheme-p (string)
  "Checks whether the string STRING represents a URL which starts with
a scheme, i.e. something like 'https://' or 'mailto:'."
  (loop with scheme-char-seen-p = nil
        for c across string
        when (or (char-not-greaterp #\a c #\z)
                 (digit-char-p c)
                 (member c '(#\+ #\- #\.) :test #'char=))
        do (setq scheme-char-seen-p t)
        else return (and scheme-char-seen-p
                         (char= c #\:))))