;;;; hobnob.asd

(asdf:defsystem #:hobnob
  :serial t
  :description "Hobnob is Internet of Things protocol simulator.
  - CoAP Protocol Client/Server as defined in RFC 7252.
  - Test case generator.
  - Integrated Webserver through which commands can be executed.
"
  :author "Sid Heroor <heroor@heptaxel.com>"
  :license "BSD 3-Clause"
  :depends-on (#:hunchentoot
               #:cl-who
	       #:usocket
	       #:css-lite)
  :components ((:file "package")
               (:file "src/hobnob")
	       (:file "src/hobnob-coap")
	       (:file "src/hobnob-coap-client")
	       (:file "src/hobnob-coap-server")))

