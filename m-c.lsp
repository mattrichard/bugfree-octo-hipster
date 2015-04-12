(defun m-c-usage ()
  (format t "Usage: (m-c m c)")
)

(defun m-c (m c)
)

(defun main ()
  (when (not (= (length *args*) 2)) (m-c-usage) (return-from main NIL))
  (m-c (first *args*) (second *args*))
)

(main)
