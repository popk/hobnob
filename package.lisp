;;;; package.lisp

(defpackage #:hobnob
  (:use #:cl #:hunchentoot #:cl-who)
  (:export :start-server :stop-server))

(defpackage #:coap
  (:use #:cl)
  (:export :make-packet-header
	   :packet->sequence
	   :make-protocol-parameters
	   :send-message-to-server
	   :ping-server
	   :+COAP-DEFAULT-PORT+))
