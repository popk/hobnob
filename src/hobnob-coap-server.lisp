;;;; hobnob-coap-client.lisp

(in-package #:coap)

(defun udp-handler (buffer)  ; echo
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  (format t "recv: ~A~%" buffer)	; no show in new thread.
  buffer)

(defvar port 14001)

(defun start-server ()
  (let ((socket (usocket:socket-server "127.0.0.1" port nil)))
    (unwind-protect
	 (loop (with-open-stream (stream (usocket:socket-accept socket)))
	    (format t "received~%"))
      (usocket:socket-close socket))))
