(defun m-c-usage ()
  (format t "Usage: (m-c m c)")
)

(defun m<c-usage ()
  (format t "Start state had less missionaries than cannibals. No solution")
)

(defun is-state-valid (state)
	"Checks if the given state is valid. The state is invalid if there is more
         cannibals than missionaries on either bank."
	(if (and (>= (first state) (second state)) (>= (third state) (fourth state))) t NIL)
)

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
(defun DFS (leftSide)
 	(when (= leftSide 0)); base case of when all m and c are on the right side

	(let ((next-states (generate-next-states current-state)))
		(do-list (state next-states)
			;TODO: remove invalid states and states that have already been visited
		)
		(do-list (state next-states)
			(dfs state)
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

(defun m-c (m c)

	(if (< m c) (m<c-usage))
	
)

(defun main ()
 	(when (not (= (length *args*) 2)) (m-c-usage) (return-from main NIL))
  	(m-c (parse-integer(first *args*)) (parse-integer(second *args*)))

	
)

(main)
