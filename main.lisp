(defun main ()
  (swank:create-server :dont-close t)
  (hobnob:start-server :port 3000)
  (loop for x = (read-line)
   when (string= x "q") do (quit)
   do (format t "Type q to quit~%" x)))


(defun main ()
  (swank:create-server :dont-close t)
  (my-app:start-server) ; Essentially creates a thread and returns
  (loop for x = (read-line)
     when (string= x "q") do (quit)
     do (format t "Type q to quit~%" x)))
