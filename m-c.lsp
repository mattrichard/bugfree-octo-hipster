(defun m-c-usage ()
  (format t "Usage: (m-c m c)")
)

#|This is the recursive depth first search
 | params:  int leftside
 | 	    int rightside
 | 	    int boat? |#
(defun DFS (leftSide rightSide boat)
  (when (= rightSide (+ m c ))); base case of when all m and c are on the right side

)

;Define start state of DFS - 3m 3c on left side 0 on right, boat is on -1 (left bank)
(defun startState () (3 3 0 0 -1))

;Define a goal state where 3m and 3c are on right side
(defun goalState? (state) 
  (when (and(= (third state) 3)(= (fourth state) 3) ); third is # of m on right side
						     ; fourth is # of c on right side
)

(defun m-c (m c)
)

(defun main ()
  (when (not (= (length *args*) 2)) (m-c-usage) (return-from main NIL))
  (m-c (first *args*) (second *args*))
)

(main)
