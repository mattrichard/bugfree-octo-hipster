; print a usage statement
(defun m-c-usage ()
  (format t "Usage: (m-c m c)")
)

; prints a statement if initial state is invalid
(defun m<c-usage ()
  (format t "Start state had less missionaries than cannibals. No solution")
)

; prints the formatted solution list
(defun print-result (result)
        (format t "left bank      right bank     canoe     last move~%")
        (format t "---------      ----------     -----     ---------~%")

        (dolist (r result)
                (format t "~d M, ~d C      ~d M, ~d C       ~a~%"
                        (first r) (second r) (third r) (fourth r) (if (= 1 (fifth r)) "left" "right"))
        )
)

; checks if the paramater state is a valid state
(defun is-state-valid (state)
        "Checks if the given state is valid. The state is invalid if there is more
         cannibals than missionaries on either bank."
        (cond
                ((or (minusp (first state)) (minusp (second state)) (minusp (third state)) (minusp (fourth state))) NIL)
                ((and
                        (or (>= (first state) (second state)) (zerop (first state)))
                        (or (>= (third state) (fourth state)) (zerop (third state))))
                        t)
                (t NIL)
        )
)

; moves n amount of missionaries to the other side of the river
(defun move-m (state n)
	"Moves n amount of missionaries to the other side of the river."
	(let ((next-state (copy-list state)))
		(if (= (fifth state) 1)
			; Boat is on the right bank
			(let ()
				(setf (first next-state) (- (first next-state) n))
				(setf (third next-state) (+ (third next-state) n))
			)
			; Boat is on the left bank
			(let ()
				(setf (first next-state) (+ (first next-state) n))
				(setf (third next-state) (- (third next-state) n))
			)
		)
		; Move the boat to the other side of the river
		(setf (fifth next-state) (* (fifth next-state) -1))
		(return-from move-m next-state)
	)
)

(defun move-c (state n)
	"Moves n amount of cannibals to the other side of the river."
	(let ((next-state (copy-list state)))
		(if (= (fifth state) 1)
			; Boat is on the right bank
			(let ()
				(setf (second next-state) (- (second next-state) n))
				(setf (fourth next-state) (+ (fourth next-state) n))
			)
			; Boat is on the left bank
			(let ()
				(setf (second next-state) (+ (second next-state) n))
				(setf (fourth next-state) (- (fourth next-state) n))
			)
		)
		; Move the boat to the other side of the river
		(setf (fifth next-state) (* (fifth next-state) -1))
		(return-from move-c next-state)
	)
)


(defun move-m-c (state)
	"Move one cannibal and one missionary to the other side of the river."
	(let ((next-state (copy-list state)))
		(if (= (fifth state) 1)
			; Boat is on the right bank
			(let ()
				(setf (first next-state) (1- (first next-state)))
				(setf (second next-state) (1- (second next-state)))
				(setf (third next-state) (1+ (third next-state)))
				(setf (fourth next-state) (1+ (fourth next-state)))
			)
			; Boat is on the left bank
			(let ()
				(setf (first next-state) (1+ (first next-state)))
				(setf (second next-state) (1+ (second next-state)))
				(setf (third next-state) (1- (third next-state)))
				(setf (fourth next-state) (1- (fourth next-state)))
			)
		)
		; Move the boat to the other side of the river
		(setf (fifth next-state) (* (fifth next-state) -1))
		(return-from move-m-c next-state)
	)
)


(defun generate-next-states (state)
	"Generates a list of all possible next states."
	(let ((next-states ()))
		(append (list (move-m state 1))
			(list (move-m state 2))
			(list (move-c state 1))
			(list (move-c state 2))
			(list (move-m-c state))
		)
	)
)

#|This is the recursive depth first search |#
(defun DFS (current-state visited-path solution-path)

	(push current-state visited-path)
	(push current-state solution-path)


	;GOAL STATE
 	(when (and (= (first current-state) 0) (= (second current-state) 0)); base case of when all m and c are on the right side
		(return-from dfs solution-path))

	(let ((next-states (generate-next-states current-state))
	      (result ()))
		
		;get rid of invalid states and already visited states
		(do ((i 0 (1+ i)))
		    ((>= i (length next-states)))

			(if (member (nth i next-states) visited-path :test #'equal); if state has already been visited
				(let()
					;(format t "removed state already visited: ~a" (nth i next-states))
					(setf next-states (remove (nth i next-states) next-states))
					(decf i)
				)
			
			
				(when (not (is-state-valid (nth i next-states))); if state is invalid
					;(format t "removed invalid state: ~a"(nth i next-states))
					(setf next-states (remove (nth i next-states) next-states))
					(decf i))
			)
		)
		(dolist (state next-states)
			(setf result (dfs state visited-path solution-path))
			(when (not (null result))
				(return-from dfs result))
		)
	)
)

;Define start state of DFS - 3m 3c on left side 0 on right, boat is on -1 (left bank)
;(defun startState () (3 3 0 0 -1))

;Define a goal state where 3m and 3c are on right side
(defun goalState? (state) 
 	(when (and(= (third state) m)(= (fourth state) c))); third is # of m on right side
						     ; fourth is # of c on right side
)

; the real main function where all the local solution lists and visted are set. calls DFS
(defun m-c (m c)

	(if (< m c) (m<c-usage))
	(let ((state (list m c 0 0 1))
		(solution  (list))
		(visitedPath (list)))
		(setf solution (reverse (dfs state visitedPath solution)))
		(print-result solution)
		(print solution)
	)
)

; main function that checks command line arguments and calls the m-c function
(defun main ()
 	(when (not (= (length *args*) 2)) (m-c-usage) (return-from main NIL))
  	(m-c (parse-integer(first *args*)) (parse-integer(second *args*)))

	
)

(main)
