;;;; hobnob-coap-client.lisp

(in-package #:coap)

(defconstant +GET+ #x1)
(defconstant +POST+ #x2)
(defconstant +PUT+ #x3)
(defconstant +DELETE+ #x4)

(defun send-message-to-server (&key (host "127.0.0.1") 
				 (port +COAP-DEFAULT-PORT+)
				 (packet nil))
  (if (or (eq packet nil) (not (typep packet 'packet-header)))
      nil
      (let ((socket (usocket:socket-connect host port
					    :protocol :datagram
					    :element-type '(unsigned-byte 8)))
	    (buffer (packet->sequence packet)))
	(usocket:socket-send socket buffer (length buffer)))))
