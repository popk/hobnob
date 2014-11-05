;;;; hobnob-coap-client.lisp

(in-package #:coap)

(defconstant +GET+ #x1)
(defconstant +POST+ #x2)
(defconstant +PUT+ #x3)
(defconstant +DELETE+ #x4)

(defun send-message-to-server (&key (host "127.0.0.1") 
				 (port +COAP-DEFAULT-PORT+)
				 (packet nil))
  "Send a message to a server

host - IP Address of the server. Supports on IPv4 currently. By default
       this is the localhost - 127.0.0.1
port - CoAP port. By default this is the standard COAP Port - 5683
packet - A packet-header structure. TODO - this needs to be expanded to
       a full packet.
"
  (if (or (eq packet nil) (not (typep packet 'packet-header)))
      nil
      (let ((socket (usocket:socket-connect host port
					    :protocol :datagram
					    :element-type '(unsigned-byte 8)))
	    (buffer (packet->sequence packet)))
	(usocket:socket-send socket buffer (length buffer)))))

(defun ping-server (&key (host "127.0.0.1")
		      (port +COAP-DEFAULT-PORT+))
  "Send an empty confirmable message to a server.

host - IP Address of server. By default this is 127.0.0.1
port - The CoAP port on which the server is listening. By default 5683

returns - the ping response as a packet-header object if there is a response
from the server.
        - nil on timeout. 
"
  (let* ((packet (make-packet-header 
		 :version +COAP-VERSION-1+
		 :type +COAP-TYPE-CONFIRMABLE+
		 :token-length 0
		 :code 0
		 :message-id 32))
	 (socket (usocket:socket-connect 
			host port
			:protocol :datagram
			:element-type '(unsigned-byte 8)))
	 (send-buffer (packet->sequence packet))
	 (receive-buffer (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
	 (packet nil)) ; Once the send-buffer is initialized, we can set packet to nil.
    (unwind-protect
	 (progn
	   (usocket:socket-send socket send-buffer (length send-buffer))
	   (multiple-value-bind (stream time-remaining)
	       (usocket:wait-for-input socket :timeout +COAP-DEFAULT-ACK-TIMEOUT+)
	     (when time-remaining
	       (usocket:socket-receive socket receive-buffer (length receive-buffer))
	       (setf packet (sequence->packet receive-buffer)))))
      (usocket:socket-close socket))
    packet))
