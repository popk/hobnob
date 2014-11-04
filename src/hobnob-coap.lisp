;;;; hobnob-coap.lisp

(in-package #:coap)

(defconstant +COAP-VERSION-1+ #b01)

(defconstant +COAP-PAYLOAD-MARKER+ #xFF)

(defconstant +COAP-TYPE-CONFIRMABLE+ #x0)
(defconstant +COAP-TYPE-NON-CONFIRMABLE+ #x1)
(defconstant +COAP-TYPE-ACKNOWLEDGEMENT+ #x2)
(defconstant +COAP-TYPE-RESET+ #x3)

(defconstant +COAP-DEFAULT-ACK-TIMEOUT+ 2)
(defconstant +COAP-DEFAULT-ACK-RANDOM-FACTOR+ 1.5)
(defconstant +COAP-DEFAULT-MAX-RETRANSMIT+ 4)
(defconstant +COAP-DEFAULT-NSTART+ 1)
(defconstant +COAP-DEFAULT-DEFAULT-LEISURE+ 5)
(defconstant +COAP-DEFAULT-PROBING-RATE+ 1)

(defconstant +COAP-SUCCESS-CREATED+ 2.01)
(defconstant +COAP-SUCCESS-DELETED+ 2.02)
(defconstant +COAP-SUCCESS-VALID+ 2.03)
(defconstant +COAP-SUCCESS-CHANGED+ 2.04)
(defconstant +COAP-SUCCESS-CONTENT+ 2.05)

(defconstant +COAP-ERROR-BAD-REQUEST+ 4.00)
(defconstant +COAP-ERROR-UNAUTHORIZED+ 4.01)
(defconstant +COAP-ERROR-BAD-OPTION+ 4.02)
(defconstant +COAP-ERROR-FORBIDDEN+ 4.03)
(defconstant +COAP-ERROR-NOT-FOUND+ 4.04)
(defconstant +COAP-ERROR-METHOD-NOT-ALLOWED+ 4.05)
(defconstant +COAP-ERROR-NOT-ACCEPTABLE+ 4.06)
(defconstant +COAP-ERROR-PRECONDITION-FAILED+ 4.12)
(defconstant +COAP-ERROR-REQUEST-ENTITY-TOO-LARGE+ 4.13)
(defconstant +COAP-ERROR-UNSUPPORTED-CONTENT-FORMAT+ 4.15)

(defconstant +COAP-SERVER-INTERNAL-SERVER-ERROR+ 5.00)
(defconstant +COAP-SERVER-NOT-IMPLEMENTED+ 5.01)
(defconstant +COAP-SERVER-BAD-GATEWAY+ 5.02)
(defconstant +COAP-SERVER-SERVICE-UNAVAILABLE+ 5.03)
(defconstant +COAP-SERVER-GATEWAY-TIMEOUT+ 5.04)
(defconstant +COAP-SERVER-PROXYING-NOT-SUPPORTED+ 5.05)

(defconstant +COAP-DEFAULT-PORT+ 5683)

(defstruct packet-header
  "The CoAP Packet Header."
  (version +COAP-VERSION-1+ :read-only t :type (unsigned-byte 2))
  (type 0 :type (unsigned-byte 2))
  (token-length 0 :type (unsigned-byte 4))
  (code 0 :type (unsigned-byte 8))
  (message-id 0 :type (unsigned-byte 16)))

(defstruct packet-token
  (token 0 :type (unsigned-byte 64)))

(defun packet->sequence (packet)
  (if (not (typep packet 'packet-header))
      nil
      (let ((v-t-t (get-v-t-t packet))
	    (seq (make-array 4 :element-type '(unsigned-byte 8))))
	(multiple-value-bind (message-id-1 message-id-2) (split-message-id packet)
	  (setf (aref seq 0) v-t-t)
	  (setf (aref seq 1) (packet-header-code packet))
	  (setf (aref seq 2) message-id-2)
	  (setf (aref seq 3) message-id-1))
	seq)))

(defun get-v-t-t (packet)
  (logior (logior (ash (packet-header-version packet) 6)
		  (ash (packet-header-type packet) 4))
	  (ldb (byte 4 0) (packet-header-token-length packet))))

(defun split-message-id (packet)
  (values (ldb (byte 8 8) (packet-header-message-id packet))
	  (logand #xff00 (packet-header-message-id packet))))

(defstruct protocol-parameters
  "The CoAP Protocol Transmission Parameters"
  (ack-timeout +COAP-DEFAULT-ACK-TIMEOUT+ :read-only t)
  (ack-random-factor +COAP-DEFAULT-ACK-RANDOM-FACTOR+ :read-only t)
  (max-retransmit +COAP-DEFAULT-MAX-RETRANSMIT+ :read-only t)
  (nstart +COAP-DEFAULT-NSTART+ :read-only t)
  (default-leisure +COAP-DEFAULT-DEFAULT-LEISURE+ :read-only t)
  (probing-rate +COAP-DEFAULT-PROBING-RATE+ :read-only t))

(defstruct protocol-retransmission-parameters
  "The CoAP Protocol Retransmission Parameters.
These are derived from a set of Protocol Parameters"
  (max-transmit-span 45)
  (max-transmit-wait 93)
  (max-latency 100)
  (processing-delay 2)
  (max-rtt 202)
  (exchange-lifetime 247)
  (non-lifetime 145))

(defun get-max-transmit-span (parameters)
  "Get the Maximum Transmit Span for a set of protocol-parameters

parameters - The protocol-parameters

returns the calculated maximum transmit span.
"
  (if (not (typep parameters 'protocol-parameters))
      (format t "Error: Invalid type") ; TODO - change to error.
      (let ((ack-timeout (protocol-parameters-ack-timeout parameters))
	    (ack-random-factor (protocol-parameters-ack-random-factor parameters))
	    (max-transmit (protocol-parameters-max-transmit parameters)))
	(* ack-timeout (- (expt 2 max-retransmit) 1) ack-random-factor))))

(defun get-max-transmit-wait (parameters)
  "Get the Maximum Transmit Wait for a set of protocol-parameters

parameters - The protocol-parameters

returns the calculated maximum transmit wait time
"
  (if (not (typep parameters 'protocol-parameters))
      (format t "Error: Invalid type") ; TODO - change to error
      (let ((ack-timeout (protocol-parameters-ack-timeout parameters))
	    (ack-random-factor (protocol-parameters-ack-random-factor parameters))
	    (max-retransmit (protocol-parameters-max-retransmit parameters)))
	(* ack-timeout (- (expt 2 (+ max-retransmit 1)) 1) ack-random-factor))))

(defun get-max-latency (parameters)
  "Get the Maximum Latency or Maximum Segment Latency

Unlike the other calculated values, this is an arbitrary value that's specified
in the RFC.

returns the maximum latency
"
100)

(defun get-processing-delay (parameters)
  "Get the Processing Delay for a set of protocol-parameters

parameters - The protocol-parameters

returns the estimated processing delay.
"
  (if (not (typep parameters 'protocol-parameters))
      (format t "Error: Invalid type") ; TODO - change to error
      (let ((ack-timeout (protocol-parameters-ack-timeout parameters)))
	ack-timeout)))

(defun get-max-rtt (parameters)
  "Get the Maximum Round Trip Time for a set of protocol-parameters

parameters - The protocol-parameters

returns the Maximum RTT expected.
"
  (if (not (typep parameters 'protocol-parameters))
      (format t "Error: Invalid type")
      (let ((max-latency (get-max-latency parameters))
	    (processing-delay (get-processing-delay parameters)))
	(+ (* 2 max-latency) processing-delay))))

(defun get-exchange-lifetime (parameters)
  "Get the Exchange Lifetime for a set of protocol-parameters

parameters - The protocol-parameters

returns the Exchange Lifetime expected.
"
  (if (not (typep parameters 'protocol-parameters))
      (format t "Error: Invalid type")
      (let ((max-transmit-span (get-max-transmit-span parameters))
	    (max-latency (get-max-latency parameters))
	    (processing-delay (get-processing-delay parameters)))
	(+ max-transmit-span (* 2 max-latency) processing-delay))))

(defun get-non-lifetime (parameters)
  "Get the Non Confirmable message Lifetime for a set of protocol-parameters

parameters - The protocol-parameters

returns the estimated Non Lifetime.
"
  (if (not (typep parameters 'protocol-parameters))
      (format t "Error: Invalid type")
      (let ((max-transmit-span (get-max-transmit-span parameters))
	    (max-latency (get-max-latency parameters)))
	(+ max-transmit-span max-latency))))
