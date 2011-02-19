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
             (:file "util" :depends-on ("packages"))
             (:file "know-words" :depends-on ("constants" "util"))
             (:file "protocol" :depends-on ("packages"))
             (:file "cookie" :depends-on ("util" "conditions"))
             (:file "interface" :depends-on ("know-words" "util" "protocol" "cookie"))))))
                    