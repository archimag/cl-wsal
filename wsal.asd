;;;; wsal.asd
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem wsal
  :depends-on (#:babel #:cl-ppcre #:cl-base64)
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "constants" :depends-on ("packages"))
             (:file "conditions" :depends-on ("packages"))
             (:file "protocol" :depends-on ("packages"))
             (:file "rfc2388" :depends-on ("constants"))
             (:file "util" :depends-on ("rfc2388" "protocol"))
             (:file "known-words" :depends-on ("constants" "util"))
             (:file "cookie" :depends-on ("util" "conditions" "interface"))
             (:file "mime-types" :depends-on ("packages"))
             (:file "interface" :depends-on ("known-words" "util" "protocol"))))))
                    