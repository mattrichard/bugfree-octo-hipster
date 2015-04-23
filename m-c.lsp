#|-----------------Missionaries and Cannibals-------------------
 |
 | Authors:	Matthew Richard, Mackenzie Smith
 | Class:	CSC 461 - Programming Languages
 | Professor: 	Dr. John Weiss  	
 |
 | Description: This program is writen in Lisp and uses a 
 | 		recursive depth first search to find a solution
 | 		path of the missionaries and cannibals problem.
 |
 | 		At the command line the user is asked to input
 | 		a number of missionaries and cannibals.  The
 | 		resulting output is the first solution path
 | 		the DFS discovers.
 |
 | Usage:	clisp m-c.lsp x y
 | 		x = number of missionaries
 | 		y = number of cannibals
 |
 | |#

; print a usage statement
(defun m-c-usage () (format t "Usage: (m-c m c)"))

; prints a statement if initial state is invalid
(defun m<c-usage ()
	(format t "Start state had less missionaries than cannibals. No solution")
)

; prints the formatted solution list
(defun print-result (result)
	"Given a solution path, prints the results of the missionaries and cannibals problem."
	(format t "~d Missionaries and ~d Cannibals:~%~%" (first (first result)) (second (first result)))

        (format t "left bank      right bank     canoe     last move~%")
        (format t "---------      ----------     -----     ---------~%")

	(let ((moves ()))
		; create a list of moves to be used to generate the last move string
		(do ((i 0 (1+ i)))
		    ((>= i (length result)))
			(if (= i 0)
				(setf moves (append moves (list NIL)))
				(let ()
					(setf moves (append moves (list (mapcar #'- (nth i result) (nth (1- i) result)))))
				)
			)
		)

		; output results in a nice format
	        (do ((i 0 (1+ i)))
		    ((>= i (length result)))
                	(format t "~15,a~15,a~10,a~5,a~%"
        	                (format nil "~d M, ~d C" (first (nth i result)) (second (nth i result)))
        	                (format nil "~d M, ~d C" (third (nth i result)) (fourth (nth i result)))
				(if (= 1 (fifth (nth i result))) "left" "right")
				(if (null (nth i moves))
					"start position"
					; generate last move string
					(format nil "move ~a M, ~a C, ~a"
						(abs (first (nth i moves)))
						(abs (second (nth i moves)))
						(if (minusp (fifth (nth i moves))) "left to right" "right to left")
					)
				)
			)
	        )
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
			; Boat is on the left bank
			(let ()
				(setf (first next-state) (- (first next-state) n))
				(setf (third next-state) (+ (third next-state) n))
			)
			; Boat is on the right bank
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
			; Boat is on the left bank
			(let ()
				(setf (second next-state) (- (second next-state) n))
				(setf (fourth next-state) (+ (fourth next-state) n))
			)
			; Boat is on the right bank
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
			; Boat is on the left bank
			(let ()
				(setf (first next-state) (1- (first next-state)))
				(setf (second next-state) (1- (second next-state)))
				(setf (third next-state) (1+ (third next-state)))
				(setf (fourth next-state) (1+ (fourth next-state)))
			)
			; Boat is on the right bank
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
	"Finds the first solution path of a given start state of the m-c problem
	 Parameters:
		current-state = state that is currently being processed
		visited-path  = list of all states already visited
		solution-path = list containing the solution path"

	(push current-state visited-path)
	(push current-state solution-path)


	;GOAL STATE
 	(when (and (= (first current-state) 0) (= (second current-state) 0)); base case of when all m and c are on the right side
		(return-from dfs solution-path))

	; generates a list "next-states" containing all states in the next iteration
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
		; loop through list of next-states calling dfs on each state
		(dolist (state next-states)
			(setf result (dfs state visited-path solution-path))
			(when (not (null result))
				(return-from dfs result))
		)
	)
)

; the real main function where all the local solution lists and visted are set. calls DFS
(defun m-c (m c)
	"Initializes solution and visited path as well as start state.  Calls DFS. Prints result"
	(if (< m c) (m<c-usage))
	
	;Initializes start state to command line values
	(let ((state (list m c 0 0 1))
		(solution  (list)); solution path
		(visitedPath (list))); visited path
		(setf solution (reverse (dfs state visitedPath solution))); sets solution to the result of DFS
		(print-result solution)
		(print solution)
	)
)

; main function that checks command line arguments and calls the m-c function
(defun main ()
	"Reads command line args, outputs usage if invalid. Passes them to m-c function."
 	(when (not (= (length *args*) 2)) (m-c-usage) (return-from main NIL))
  	(m-c (parse-integer(first *args*)) (parse-integer(second *args*)))
)

(main)
