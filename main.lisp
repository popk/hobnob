;;;
;;; Create a toplevel function for the standalone hobnob
;;; server. Keep Swank Port open for debuggability.
;;;

(defun main ()
  (swank:create-server :dont-close t)
  (hobnob:start-server :port 3000)
  (loop for x = (read-line)
   when (string= x "q") do (quit)
   do (format t "Type q to quit~%" x)))
