;;; CLIPS rule based system playing chess endgame
;;   white rook and king against black king
;;  by Andrey Kotlarski m00naticus@gmail.com

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defglobals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defglobal ?*N* = 8)			; *N* x *N* board, >5 for now


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deftemplates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate figure
  "Will be used for current pieces on board
in this program, white king, white rook and black king."
  (slot sortof
	(type SYMBOL)
	(allowed-symbols king rook)
	(default king))
  (slot colour
	(type SYMBOL)
	(allowed-symbols white black)
	(default white))
  (slot vertical
	(type INTEGER)
	;; (range 1 ?*N*)
	)
  (slot horizontal
	(type INTEGER)
	;; (range 1 ?*N*)
	))

(deftemplate status
  "Only one instance is supposed to circulate
and keep track of current search and general condition
as well as current move number."
  (slot value
	(type SYMBOL))
  (slot move-number
	(type INTEGER)
	(default 1)))

(deftemplate play-move
  "Only one instance is supposed to flow arround at a time,
indicating just chosen move; after played, instance is retracted."
  (slot piece
	(type SYMBOL)
	(allowed-symbols king rook)
	(default rook))
  (slot colour
	(type SYMBOL)
	(allowed-symbols white black)
	(default white))
  (slot vertical
	(type INTEGER)
	;; (range 1 ?*N*)
	)
  (slot horizontal
	(type INTEGER)
	;; (range 1 ?*N*)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; usual convention:
;; wkv -> white king vertical
;; wkh -> white king horizontal
;; wrv -> white rook vertical
;; wrh -> white rook horizontal
;; bkv -> black king vertical
;; bkh -> black king horizontal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction vertical-to-letter (?v)
  "Return corresponding latin letter."
  (switch ?v
	  (case 1 then "a")
	  (case 2 then "b")
	  (case 3 then "c")
	  (case 4 then "d")
	  (case 5 then "e")
	  (case 6 then "f")
	  (case 7 then "g")
	  (case 8 then "h")
	  (default ?v)))

(deffunction letter-to-number (?v)
  "Return corresponding number."
  (switch ?v
	  (case a then 1)
	  (case b then 2)
	  (case c then 3)
	  (case d then 4)
	  (case e then 5)
	  (case f then 6)
	  (case g then 7)
	  (case h then 8)
	  (default ?v)))

(deffunction piece-to-letter (?p)
  "Return letter of corresponding piece."
  (switch ?p
	  (case rook then "R")
	  (case king then "K")
	  (default ?p)))

(deffunction read-pos (?str)
  "Read single position value."
  (printout t "enter " ?str ": ")
  (bind ?num (letter-to-number (read t)))
  (if (eq ?num X) then (return FALSE))
  (if (or (not (integerp ?num)) (< ?num 1) (> ?num ?*N*))
      then (read-pos ?str)
      else ?num))

(deffunction controlled-square (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Check whether given coordinates (black king)
are attacked either by white rook or white king
(ignoring possible intersection between them)."
  (or (= ?wrv ?bkv) (= ?wrh ?bkh)
      (and (< (abs (- ?wkv ?bkv)) 2) (< (abs (- ?wkh ?bkh)) 2))))

(deffunction attacked-square (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Check whether given coordinates (black king) are attacked by
either white rook or white king."
  (or (and (< (abs (- ?wkv ?bkv)) 2)
	   (< (abs (- ?wkh ?bkh)) 2))
      (and (= ?wrv ?bkv)
	   (or (<> ?wrv ?wkv)
	       (> ?wkh (max ?wrh ?bkh))
	       (< ?wkh (min ?wrh ?bkh))))
      (and (= ?wrh ?bkh)
	   (or (<> ?wrh ?wkh)
	       (> ?wkv (max ?wrv ?bkv))
	       (< ?wkv (min ?wrv ?bkv))))))

(deffunction enter-start-pos ()
  "Enter start position, if illegal (black king must not be under
attack) - reenter black king coordinates"
  (printout t "Starting a game " ?*N* " x " ?*N*
	    ". For exit - enter 'X' at any point." crlf)
  (if (not (and (bind ?wkv (read-pos "white king vertical position"))
		(bind ?wkh (read-pos
			    "white king horizontal position"))
		(bind ?wrv (read-pos "white rook vertical position"))
		(bind ?wrh (read-pos
			    "white rook horizontal position"))))
      then (return FALSE))
  (while
      (and (= ?wkv ?wrv) (= ?wkh ?wrh))
    do
    (printout t "Illegal rook coordinates!" crlf)
    (if (not (and (bind ?wrv (read-pos
			      "white rook vertical position"))
		  (bind ?wrh (read-pos
			      "white rook horizontal position"))))
	then (return FALSE)))
  (if (not (and (bind ?bkv (read-pos "black king vertical position"))
		(bind ?bkh (read-pos
			    "black king horizontal position"))))
      then (return FALSE))
  (while		;(or (stalemate ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
      (or (and (= ?wrv ?bkv) (= ?wrh ?bkv)) ;black king's coordinates must differ from rook's
	  (attacked-square ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)) ;and black king must not be attacked
    do
    (printout t "Illegal black king coordinates!" crlf)
    (if (not (and (bind ?bkv (read-pos
			      "black king vertical position"))
		  (bind ?bkh (read-pos
			      "black king horizontal position"))))
	then (return FALSE)))
  (assert (figure (sortof king) (colour white) (vertical ?wkv)
		  (horizontal ?wkh)))
  (assert (figure (sortof rook) (colour white) (vertical ?wrv)
		  (horizontal ?wrh)))
  (assert (figure (sortof king) (colour black) (vertical ?bkv)
		  (horizontal ?bkh)))
  TRUE)

(deffunction regular-square (?v ?h)
  "Determine whether given coordinates correspond to a square within
the chess board *N* x *N*."
  (and (> ?v 0) (<= ?v ?*N*) (> ?h 0) (<= ?h ?*N*)))

(deffunction get-distance (?v1 ?h1 ?v2 ?h2)
  "Return maximum of vertical and horizontal distances."
  (max (abs (- ?v1 ?v2)) (abs (- ?h1 ?h2))))

(deffunction get-abs-distance (?v1 ?h1 ?v2 ?h2)
  "Return vertical + horizontal distance."
  (+ (abs (- ?v1 ?v2)) (abs (- ?h1 ?h2))))

(deffunction find-move-in-sequence (?v ?h $?moves)
  "Determine whether given coordinates are present within
list of moves."
  (if (= (length$ ?moves) 0)
      then return FALSE
      else (or (and (= ?v (nth$ 1 ?moves))
		    (= ?h (nth$ 2 ?moves)))
	       (find-move-in-sequence ?v ?h
				      (subseq$ ?moves 3
					       (length$ ?moves))))))

(deffunction on-edge (?v ?h)
  "Check whether given coordinates correspond to an edge square."
  (or (= ?h 1) (= ?h ?*N*) (= ?v 1) (= ?v ?*N*)))

(deffunction in-corner (?v ?h)
  "Check whether given coordinates correspond to a corner square."
  (and (or (= ?h 1) (= ?h ?*N*))
       (or (= ?v 1) (= ?v ?*N*))))

(deffunction king-moves (?v ?h)
  "Return all possible king moves on an empty board."
  (bind $?moves (create$))
  (loop-for-count (?delta-v -1 1) do
		  (loop-for-count (?delta-h -1 1) do
				  (if (and (or (<> ?delta-v 0)
					       (<> ?delta-h 0))
					   (regular-square
					    (+ ?v ?delta-v)
					    (+ ?h ?delta-h))) then
				    (bind ?moves
					  (insert$ ?moves 1
						   (create$ (+ ?v
							       ?delta-v)
							    (+ ?h
							       ?delta-h)))))))
  ?moves)

(deffunction rook-moves (?v ?h)
  "Return all possible rook moves on an empty board."
  (bind $?moves (create$))
  (loop-for-count (?vert 1 (- ?v 1)) do
		  (bind ?moves (insert$ ?moves 1 (create$ ?vert ?h))))
  (loop-for-count (?vert (+ ?v 1) ?*N*) do
		  (bind ?moves (insert$ ?moves 1 (create$ ?vert ?h))))
  (loop-for-count (?hor 1 (- ?h 1)) do
		  (bind ?moves (insert$ ?moves 1 (create$ ?v ?hor))))
  (loop-for-count (?hor (+ ?h 1) ?*N*) do
		  (bind ?moves (insert$ ?moves 1 (create$ ?v ?hor))))
  ?moves)

(deffunction restrict-moves (?rv ?rh ?rtype $?moves)
  "Filter such moves that are not under attack by a given piece
(depending on type of piece)."
  (bind $?res-moves ?moves)
  (bind $?restrict-moves (create$))
  (if (= 0 (str-compare ?rtype "king"))
      then (bind $?restrict-moves (king-moves ?rv ?rh))
      else (bind $?restrict-moves (rook-moves ?rv ?rh)))
  (loop-for-count (?rm-count 1 (div (length$ ?restrict-moves) 2)) do
     (loop-for-count (?curr 1 (div (length$ ?moves) 2))	do
	(if (and (= (nth$ (- (* 2 ?curr) 1) ?moves)
		    (nth$ (- (* 2 ?rm-count) 1) ?restrict-moves))
		 (= (nth$ (* 2 ?curr) ?moves)
		    (nth$ (* 2 ?rm-count) ?restrict-moves)))
	    then (bind ?res-moves (replace$ ?res-moves
				       (- (* 2 ?curr) 1)
				       (* 2 ?curr) 0 0)))))
  (delete-member$ ?res-moves 0))

(deffunction rook-attacked (?wrv ?wrh ?bkv ?bkh)
  "Determine whether white rook is nearby black king."
  (and (< (abs (- ?bkv ?wrv)) 2) (< (abs (- ?bkh ?wrh)) 2)))

(deffunction rook-must-move (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Determine whether rook must move - rook is attacked and white king
is not nearby."
  (and (rook-attacked ?wrv ?wrh ?bkv ?bkv)
       (or (> (abs (- ?wkv ?wrv)) 2) (> (abs (- ?wkh ?wrh)) 2))))

(deffunction king-defending-moves (?wrv ?wrh $?wk-moves)
  "Filter such moves that are within 1 square of white rook."
  (if (= (length$ ?wk-moves) 0)
      then (create$)
      else
      (create$ (if (and (< (abs (- (nth$ 1 ?wk-moves) ?wrv)) 2)
			(< (abs (- (nth$ 2 ?wk-moves) ?wrh)) 2))
		   then (create$ (nth$ 1 ?wk-moves)
				 (nth$ 2 ?wk-moves))
		   else (create$))
	       (king-defending-moves ?wrv ?wrh
				     (subseq$ ?wk-moves 3
					      (length$ ?wk-moves))))))

(deffunction check-white-king-moves (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Check possible white king moves
restricted by black king or rook if nearby.
If rook is attacked - leave only defending moves if such."
  (bind ?wk-moves (king-moves ?wkv ?wkh))
  (if (and (< (abs (- ?bkh ?wkh)) 3) (< (abs (- ?bkv ?wkv)) 3))
      then (bind ?wk-moves (restrict-moves ?bkv ?bkh "king"
					   ?wk-moves)))
  (if (and (< (abs (- ?wkh ?wrh)) 2) (< (abs (- ?wkv ?wrv)) 2))
      then (loop-for-count (?curr 1 (div (length$ ?wk-moves) 2)) do
			   (if (<= (* 2 ?curr) (length$ ?wk-moves))
			       then (if (and (= (nth$ (- (* 2 ?curr)
							 1) ?wk-moves)
						?wrv)
					     (= (nth$ (* 2 ?curr)
						      ?wk-moves)
						?wrh))
					then
				      (bind ?wk-moves
					    (delete$ ?wk-moves
						     (- (* 2 ?curr) 1)
						     (* 2 ?curr)))
				      (break)))))
  (if (rook-attacked ?wrv ?wrh ?bkv ?bkh)
      then (bind ?wk-moves (king-defending-moves ?wrv ?wrh
						 ?wk-moves)))
  ?wk-moves)

(deffunction restrict-rook (?wkv ?wkh ?wrv ?wrh $?moves)
  "Restrict possible moves by white rook in cases
when white king is on the same vertical or horizontal."
  (bind $?ret-moves (create$))
  (if (= ?wkv ?wrv)
      then (if (> ?wkh ?wrh)
	       then (loop-for-count (?curr 1 (div (length$ ?moves) 2))
				    do
				    (if (< (nth$ (* 2 ?curr) ?moves)
					   ?wkh)
					then (bind ?ret-moves
						   (insert$ ?ret-moves 1
							    (create$
							     (nth$
							      (- (* 2 ?curr) 1)
							      ?moves)
							     (nth$
							      (* 2 ?curr)
							      ?moves))))))
	       else (loop-for-count (?curr 1 (div (length$ ?moves) 2))
				    do
				    (if (> (nth$ (* 2 ?curr) ?moves) ?wkh)
					then (bind ?ret-moves
						   (insert$ ?ret-moves 1
							    (create$
							     (nth$
							      (- (* 2 ?curr) 1)
							      ?moves)
							     (nth$
							      (* 2 ?curr)
							      ?moves)))))))
      else (if (> ?wkv ?wrv)
	       then (loop-for-count (?curr 1 (div (length$ ?moves) 2))
				    do
				    (if (< (nth$ (- (* 2 ?curr) 1) ?moves) ?wkv)
					then (bind ?ret-moves
						   (insert$ ?ret-moves 1
							    (create$
							     (nth$
							      (- (* 2 ?curr) 1)
							      ?moves)
							     (nth$
							      (* 2 ?curr)
							      ?moves))))))
	       else (loop-for-count (?curr 1 (div (length$ ?moves) 2))
				    do
				    (if (> (nth$ (- (* 2 ?curr) 1)
						 ?moves)
					   ?wkv)
					then (bind ?ret-moves
						   (insert$ ?ret-moves 1
							    (create$
							     (nth$ (- (* 2 ?curr) 1)
								   ?moves)
							     (nth$ (* 2 ?curr)
								   ?moves))))))))
  ?ret-moves)

(deffunction secure-rook-moves (?wkv ?wkh ?bkv ?bkh $?moves)
  "Filter possible rook moves which are safe - either not attacked by
black king or defended by white king."
  (if (= (length$ ?moves) 0)
      then (create$)
      else
      (create$ (if (or (> (abs (- ?bkv (nth$ 1 ?moves))) 1)
		       (> (abs (- ?bkh (nth$ 2 ?moves))) 1)
		       (and (< (abs (- ?wkv (nth$ 1 ?moves))) 2)
			    (< (abs (- ?wkh (nth$ 2 ?moves))) 2)))
		   then (create$ (nth$ 1 ?moves) (nth$ 2 ?moves))
		   else (create$))
	       (secure-rook-moves ?wkv ?wkh ?bkv ?bkh
				  (subseq$ ?moves 3
					   (length$ ?moves))))))

(deffunction plausible-rook-moves (?wkv ?wkh ?bkv ?bkh $?moves)
  "Filter plausible rook moves - away from black king or at least as
close to the white king."
  (bind ?ret-moves (create$))
  (loop-for-count (?curr 1 (div (length$ ?moves) 2)) do
		  (bind ?wr-bk-dist (get-distance ?bkv ?bkh
						  (nth$ (- (* 2 ?curr)
							   1)
							?moves)
						  (nth$ (* ?curr 2)
							?moves)))
		  (if (or (> ?wr-bk-dist 2)
			  (>= ?wr-bk-dist (get-distance ?wkv ?wkh
							(nth$
							 (- (* 2 ?curr) 1)
							 ?moves)
							(nth$
							 (* ?curr 2)
							 ?moves))))
		      then (bind ?ret-moves (insert$ ?ret-moves 1
						     (create$
						      (nth$ (- (* 2 ?curr) 1)
							    ?moves)
						      (nth$ (* 2 ?curr)
							    ?moves))))))
  ?ret-moves)

(deffunction possible-rook-moves (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Find all possible (not plausible!) rook moves."
  (if (or (= ?wkv ?wrv) (= ?wkh ?wrh))
      then (restrict-rook ?wkv ?wkh ?wrv ?wrh (rook-moves ?wrv ?wrh))
      else (rook-moves ?wrv ?wrh)))

(deffunction check-rook-moves (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Return all plausible white rook moves."
  (plausible-rook-moves ?wkv ?wkh ?bkv ?bkh
			(if (or (< (abs (- ?bkh ?wrh)) 2)
				(< (abs (- ?bkv ?wkv)) 2))
			    then (secure-rook-moves ?wkv ?wkh
						    ?bkv ?bkh
						    (possible-rook-moves ?wkv ?wkh
									 ?wrv ?wrh
									 ?bkv ?bkh))
			    else (possible-rook-moves ?wkv ?wkh
						      ?wrv ?wrh
						      ?bkv ?bkh))))

(deffunction check-black-moves (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Return possible moves by black king by restricting it by white king
and white rook."
  (bind $?bk-moves (king-moves ?bkv ?bkh))
  (if (and (< (abs (- ?bkh ?wkh)) 3) (< (abs (- ?bkv ?wkv)) 3))
      then (bind ?bk-moves (restrict-moves ?wkv ?wkh "king"
					   ?bk-moves)))
  (if (or (< (abs (- ?bkh ?wrh)) 2) (< (abs (- ?bkv ?wrv)) 2))
      then	  ; (bind ?bk-moves (restrict-moves ?wrv ?wrh "rook"))
    (bind ?rook-moves (possible-rook-moves ?wkv ?wkh ?wrv ?wrh
					   ?bkv ?bkh))
    (bind ?i 1)
    (while (>= (length$ ?bk-moves) (* ?i 2)) do
	   (if (find-move-in-sequence (nth$ (- (* ?i 2) 1) ?bk-moves)
				      (nth$ (* ?i 2) ?bk-moves)
				      ?rook-moves)
	       then (bind ?bk-moves (replace$ ?bk-moves (- (* ?i 2) 1)
					      (* ?i 2) 0 0)))
	   (bind ?i (+ ?i 1)))
    (bind ?bk-moves (delete-member$ ?bk-moves 0)))
  ?bk-moves)

(deffunction play-only-black-move (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Play only possible black king move."
  (bind ?moves (check-black-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  (if (= (length$ ?moves) 2)
      then (assert (play-move (piece king) (colour black)
			      (vertical (nth$ 1 ?moves))
			      (horizontal (nth$ 2 ?moves))))
      TRUE
      else FALSE))

(deffunction king-against-king (?wkv ?wkh ?bkv ?bkh)
  "Return number of squares that white king takes from black
maximum is 3 (king opposition), if less or equal to 0 => 0."
  (if (= 2 (abs (- ?wkv ?bkv)))
      then (- 3 (abs (- ?wkh ?bkh)))
      else (if (= 2 (abs (- ?wkh ?bkh)))
	       then (- 3 (abs (- ?wkv ?bkv)))
	       else 0)))

(deffunction check-for-mate (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Check for mate position i.e. black king is attacked by white rook
and has no possible moves."
  (and (or (= ?bkv ?wrv) (= ?bkh ?wrh))
       (= (length$ (check-black-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
	  0)))

(deffunction check-for-stalemate (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Check for stalemate position i.e. black king is not attacked but
has no possible moves."
  (and (and (<> ?bkv ?wrv) (<> ?bkh ?wrh))
       (= (length$ (check-black-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
	  0)))

(deffunction find-region (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Find a continuous region arround given square (black king)
of unattacked squares."
  (bind ?region (create$ ?bkv ?bkh (king-moves ?bkv ?bkh)))
  (bind ?i 2)
  (while (>= (length$ ?region) (* ?i 2)) do
	 (if (or (attacked-square ?wkv ?wkh ?wrv ?wrh
				  (nth$ (- (* ?i 2) 1) ?region)
				  (nth$ (* ?i 2) ?region))
		 (and (controlled-square ?wkv ?wkh ?wrv ?wrh
					 (nth$ (- (* ?i 2) 1) ?region)
					 (nth$ (* ?i 2) ?region))
		      (> (get-distance ?bkv ?bkh
				       (nth$ (- (* ?i 2) 1) ?region)
				       (nth$ (* ?i 2) ?region)) 1)))
	     then (bind ?region (delete$ ?region (- (* ?i 2) 1)
					 (* ?i 2)))
	     else
	     (bind ?new-moves (king-moves (nth$ (- (* ?i 2) 1)
						?region)
					  (nth$ (* ?i 2) ?region)))
	     (bind ?j 1)
	     (while (>= (length$ ?new-moves) (* ?j 2)) do
		    (if (not (find-move-in-sequence
			      (nth$ (- (* ?j 2) 1) ?new-moves)
			      (nth$ (* ?j 2) ?new-moves)
			      ?region))
			then (bind ?region
				   (insert$ ?region
					    (+ 1 (length$ ?region))
					    (nth$ (- (* ?j 2) 1)
						  ?new-moves)
					    (nth$ (* ?j 2)
						  ?new-moves))))
		    (bind ?j (+ ?j 1)))
	     (bind ?i (+ ?i 1))))
  ?region)

(deffunction estimation (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Estimate non-mating position - the lower number the better."
  (bind ?estimate (div (length$ (find-region ?wkv ?wkh ?wrv ?wrh
					     ?bkv ?bkh))
		       2))
  (if (= ?estimate 1) then (return (* ?*N* ?*N*))) ;to avoid stalemate
  ;;  (bind ?estimate (- ?estimate
  ;;		     (* (length$ (check-white-king-moves ?wkv ?wkh ?wrv ?wrh
  ;;							 ?bkv ?bkh)) 7)))
  (if (controlled-square ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
      then (bind ?estimate (* (div ?estimate 3) 2))) ;a bit heuristic
  (bind ?wr-bk-dist (get-distance ?wrv ?wrh ?bkv ?bkh))
  (bind ?wr-wk-dist (get-distance ?wrv ?wrh ?wkv ?wkh))
  (bind ?diff (- ?wr-wk-dist ?wr-bk-dist))
  (if (> ?diff 0)		   ;if rook is closer to black king...
      then			   ;the closer the worse
    (+ ?estimate (* ?wr-bk-dist (div ?*N* 2))
       (* ?diff (div ?*N* 2)))
    else ;else - at least as close to white king, award nearness to kings
    (+ ?estimate (- ?wr-bk-dist ?*N*)
       (- ?wr-wk-dist ?*N*))))

(deffunction find-best-white-king-move (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Find best possible move for white king - one that is closer to
black king and not on edge (edge moves are not plausible)
1st criteria is estimation, 2nd closeness."
  (bind ?moves (check-white-king-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  (if (= (length$ ?moves) 0) then return (create$))
  (bind ?best-v (nth$ 1 ?moves))
  (bind ?best-h (nth$ 2 ?moves))
  (bind ?best-diff (get-abs-distance ?bkv ?bkh ?best-v ?best-h))
  (bind ?best-dist (get-distance ?bkv ?bkh ?best-v ?best-h))
  (bind ?best-est (estimation ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  (bind ?best-edge (on-edge ?best-v ?best-h))
  (bind ?i 2)
  (while (>= (length$ ?moves) (* ?i 2)) do
	 (if (and ?best-edge (not (on-edge (nth$ (- (* ?i 2) 1) ?moves)
					   (nth$ (* ?i 2) ?moves))))
	     then
	   (bind ?best-v (nth$ (- (* ?i 2) 1) ?moves))
	   (bind ?best-h (nth$ (* ?i 2) ?moves))
	   (bind ?best-edge FALSE)
	   (bind ?best-diff (get-abs-distance ?bkv ?bkh ?best-v ?best-h))
	   (bind ?best-dist (get-distance ?bkv ?bkh ?best-v ?best-h))
	   (bind ?best-est (estimation ?best-v ?best-h ?wrv ?wrh ?bkv ?bkh))
	   else
	   (if (or ?best-edge (not (on-edge (nth$ (- (* ?i 2) 1) ?moves)
					    (nth$ (* ?i 2) ?moves))))
	       then
	     (bind ?curr-diff (get-abs-distance ?bkv ?bkh
						(nth$ (- (* ?i 2) 1) ?moves)
						(nth$ (* ?i 2) ?moves)))
	     (if (<= ?curr-diff (+ ?best-diff 1)) then
	       (bind ?curr-est (estimation (nth$ (- (* ?i 2) 1) ?moves)
					   (nth$ (* ?i 2) ?moves)
					   ?wrv ?wrh ?bkv ?bkh))
	       (if (> ?best-est ?curr-est)
		   then
		 (bind ?best-v (nth$ (- (* ?i 2) 1) ?moves))
		 (bind ?best-h (nth$ (* ?i 2) ?moves))
		 (bind ?best-edge (on-edge ?best-v ?best-h))
		 (bind ?best-diff ?curr-diff)
		 (bind ?best-est ?curr-est)
		 (bind ?best-dist (get-distance ?bkv ?bkh
						?best-v ?best-h))
		 else
		 (if (= ?best-est ?curr-est) then
		   (bind ?curr-dist (get-distance ?bkv ?bkh
						  (nth$ (- (* ?i 2) 1)
							?moves)
						  (nth$ (* ?i 2)
							?moves)))
		   (if (or (< ?curr-diff ?best-diff)
			   (< ?curr-dist ?best-dist)) then
		     (bind ?best-v (nth$ (- (* ?i 2) 1) ?moves))
		     (bind ?best-h (nth$ (* ?i 2) ?moves))
		     (bind ?best-edge (on-edge ?best-v ?best-h))
		     (bind ?best-diff ?curr-diff)
		     (bind ?best-dist ?curr-dist)
		     (bind ?best-est ?curr-est)))))))
	 (bind ?i (+ ?i 1)))
  (create$ ?best-v ?best-h))

(deffunction find-best-rook-move (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Find best estimated rook move."
  (bind ?rook-moves (check-rook-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  (bind ?best-v (nth$ 1 ?rook-moves))
  (bind ?best-h (nth$ 2 ?rook-moves))
  (bind ?best-est (estimation ?wkv ?wkh ?best-v ?best-h ?bkv ?bkh))
  (bind ?i 2) ;there certainly are more than 2 plausible rook moves on 8x8
  (while (>= (length$ ?rook-moves) (* ?i 2)) do
	 (bind ?new-est (estimation ?wkv ?wkh
				    (nth$ (- (* ?i 2) 1) ?rook-moves)
				    (nth$ (* ?i 2) ?rook-moves)
				    ?bkv ?bkh))
	 (if (< ?new-est ?best-est) then
	   (bind ?best-est ?new-est)
	   (bind ?best-v (nth$ (- (* ?i 2) 1) ?rook-moves))
	   (bind ?best-h (nth$ (* ?i 2) ?rook-moves)))
	 (bind ?i (+ ?i 1)))
  (create$ ?best-v ?best-h))

(deffunction play-best-move (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Choose best move in a non-mate situation.
Return best rook move only if it is better than the best king move."
  (bind ?best-move (find-best-white-king-move ?wkv ?wkh ?wrv ?wrh
					      ?bkv ?bkh))
  (bind ?best-rook-move (find-best-rook-move ?wkv ?wkh
					     ?wrv ?wrh ?bkv ?bkh))
  (if (< (estimation ?wkv ?wkh (nth$ 1 ?best-rook-move)
		     (nth$ 2 ?best-rook-move) ?bkv ?bkh)
	 (estimation (nth$ 1 ?best-move) (nth$ 2 ?best-move)
		     ?wrv ?wrh ?bkv ?bkh))
      then
    (assert (play-move (piece rook)
		       (vertical (nth$ 1 ?best-rook-move))
		       (horizontal (nth$ 2 ?best-rook-move))))
    else
    (assert (play-move (piece king) (vertical (nth$ 1 ?best-move))
		       (horizontal (nth$ 2 ?best-move))))))

(deffunction search-mate (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Search for a mating move within all plausible white rook moves."
  (bind ?rook-moves (check-rook-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  (bind ?i 1)
  (while (>= (length$ ?rook-moves) (* ?i 2)) do
	 (if (and (or (= ?bkv (nth$ (- (* ?i 2) 1) ?rook-moves))
		      (= ?bkh (nth$ (* ?i 2) ?rook-moves)))
		  (check-for-mate ?wkv ?wkh (nth$ (- (* ?i 2) 1)
						  ?rook-moves)
				  (nth$ (* ?i 2) ?rook-moves)
				  ?bkv ?bkh))
	     then
	   (assert (play-move (piece rook)
			      (vertical (nth$ (- (* ?i 2) 1)
					      ?rook-moves))
			      (horizontal (nth$ (* ?i 2)
						?rook-moves))))
	   (return TRUE))
	 (bind ?i (+ ?i 1)))
  FALSE)

(deffunction print-move (?p ?v ?h ?delim)
  "Print move in short notation."
  (if (not (eq ?delim ";")) then (printout t crlf))
  (printout t ?delim " " (piece-to-letter ?p)
	    (vertical-to-letter ?v) ?h))

(deffunction enter-black-move (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Enter black king move, on errors print a list of possible moves."
  (printout t crlf)
  (if (not (and
	    (bind ?v (read-pos "black king vertical"))
	    (bind ?h (read-pos "black king horizontal"))))
      then (return FALSE))
  (bind ?moves (check-black-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  (while (not (find-move-in-sequence ?v ?h ?moves)) do
	 (printout t "Incorrect move! Available squares are ")
	 (loop-for-count (?i 1 (div (length$ ?moves) 2))
			 (printout t (vertical-to-letter
				      (nth$ (- (* ?i 2) 1) ?moves))
				   (nth$ (* ?i 2) ?moves) " "))
	 (printout t crlf)
	 (if (not (and
		   (bind ?v (read-pos "black king vertical"))
		   (bind ?h (read-pos "black king horizontal"))))
	     then (return FALSE)))
  (assert (play-move (piece king) (colour black)
		     (vertical ?v) (horizontal ?h)))
  TRUE)

(deffunction make-move (?piece ?colour $?coord)
  "Execute a move by inserting a fact."
  (assert (play-move (piece ?piece) (colour ?colour)
		     (vertical (nth$ 1 ?coord))
		     (horizontal (nth$ 2 ?coord)))))

(deffunction no-mating-condition (?wkv ?wkh ?bkv ?bkh)
  "Check if there are no conditions for immediate mating move."
  (and (or (< (king-against-king ?wkv ?wkh ?bkv ?bkh) 3)
	   (on-edge ?wkv ?wkh)
	   (not (on-edge ?bkv ?bkh)))
       (or (< (king-against-king ?wkv ?wkh ?bkv ?bkh) 2)
	   (not (in-corner ?bkv ?bkh)))))

(deffunction rook-between (?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  "Check if rook is a square away from both kings."
  (and (< (abs (- ?wrv ?wkv)) 2)
       (< (abs (- ?wrh ?wkh)) 2)
       (< (abs (- ?wrv ?bkv)) 2)
       (< (abs (- ?wrh ?bkh)) 2)))

(deffunction start-game ()
  "Start a game."
  (reset)
  (assert (status (value start)))
  (run))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule start-pos
  "Enter start position."
  (initial-fact)
  ?s <- (status (value start) (move-number ?))
  =>
  (retract ?s)
  (if (enter-start-pos)
      then (modify ?s (value white-move))
      else (reset)))

(defrule too-many-moves
  "Check 50-move rule."
  (status (value ?) (move-number ?n&:(> ?n 50)))
  =>
  (printout t "=" crlf)
  (reset))

(defrule check-for-mate
  "Check current position for mate."
  ?s <- (status (value check-mate) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  =>
  (if (check-for-mate ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
      then (printout t "#" crlf)
      (reset)
      else (modify ?s (value black-move))))

(defrule black-only-move
  "Check and play black king move if there is a single possibility."
  ?s <- (status (value black-move) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (= (length$ (check-black-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
	   2))
  =>
  (play-only-black-move ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  (modify ?s (value black-move-done)))

(defrule on-black-move-done
  "Execute black move."
  ?s <- (status (value black-move-done) (move-number ?n))
  ?move <- (play-move (piece ?p) (colour black)
		      (vertical ?v) (horizontal ?h))
  ?f <- (figure (sortof ?p) (colour black))
  =>
  (print-move ?p ?v ?h ";")
  (modify ?f (vertical ?v) (horizontal ?h))
  (modify ?s (value white-move) (move-number (+ ?n 1)))
  (retract ?move))

(defrule enter-black-move
  "Manually enter black move if more than one possibility."
  ?s <- (status (value black-move) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (> (length$ (check-black-moves ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
	   2))
  =>
  (if (enter-black-move ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
      then (modify ?s (value black-move-done))
      else (reset)))

(defrule on-white-move
  "Execute white move."
  ?s <- (status (value white-move-done) (move-number ?n))
  ?move <- (play-move (piece ?p) (colour white)
		      (vertical ?v) (horizontal ?h))
  ?f <- (figure (sortof ?p) (colour white))
  =>
  (print-move ?p ?v ?h ?n)
  (modify ?f (vertical ?v) (horizontal ?h))
  (modify ?s (value check-mate))
  (retract ?move))

(defrule mating-attempt-corner
  "If conditions for mating move (black king is in
corner and there is king opposition) - check if such."
  ?s <- (status (value white-move) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (and (in-corner ?bkv ?bkh)
	     (> (king-against-king ?wkv ?wkh ?bkv ?bkh) 1)))
  =>
  (if (search-mate ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
      then (modify ?s (value white-move-done))
      else (modify ?s (value white-move-ponder))))

(defrule mating-attempt-edge
  "If conditions for mating move (black king is on
edge and there is full king opposition) - search for mate."
  ?s <- (status (value white-move) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (and (> (king-against-king ?wkv ?wkh ?bkv ?bkh) 2)
	     (on-edge ?bkv ?bkh)
	     (not (on-edge ?wkv ?wkh))))
  =>
  (if (search-mate ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
      then (modify ?s (value white-move-done))
      else (modify ?s (value white-move-ponder))))

(defrule mating-attempt-fail
  "If no mating conditions - think of other continuations."
  ?s <- (status (value white-move) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (no-mating-condition ?wkv ?wkh ?bkv ?bkh))
  =>
  (modify ?s (value white-move-ponder)))

(defrule break-king-opposition
  "If king opposition and rook in between
- execute waiting move with white king."
  ?s <- (status (value white-move-ponder) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (and (> (king-against-king ?wkv ?wkh ?bkv ?bkh) 2)
	     (rook-between ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
	     (> (length$ (find-region ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)) 4)))
  =>
  (make-move king white (find-best-white-king-move ?wkv ?wkh ?wrv ?wrh
						   ?bkv ?bkh))
  (modify ?s (value white-move-done)))

(defrule keep-king-opposition
  "Normally when king opposition, white king should not leave it
so play best rook move."
  ?s <- (status (value white-move-ponder) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (and (> (king-against-king ?wkv ?wkh ?bkv ?bkh) 2)
	     (or (not (rook-between ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
		 (= (length$ (find-region ?wkv ?wkh ?wrv ?wrh
					  ?bkv ?bkh))
		    4))))
  =>
  (make-move rook white (find-best-rook-move ?wkv ?wkh ?wrv ?wrh
					     ?bkv ?bkh))
  (modify ?s (value white-move-done)))

(defrule release-white-king
  "If white king is on edge and under nasty opposition
but has a move to leave edge - play it."
  ?s <- (status (value white-move-ponder) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (and (on-edge ?wkv ?wkh)
	     (not (on-edge ?wrv ?wrh))
	     (= (king-against-king ?wkv ?wkh ?bkv ?bkh) 2)
	     (not (rook-must-move ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))))
  =>
  (make-move king white (find-best-white-king-move ?wkv ?wkh ?wrv ?wrh
						   ?bkv ?bkh))
  (modify ?s (value white-move-done)))

(defrule rook-must-move
  "If rook is attacked and white king is away - play best rook move."
  ?s <- (status (value white-move-ponder) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (rook-must-move ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
  =>
  (make-move rook white (find-best-rook-move ?wkv ?wkh ?wrv ?wrh
					     ?bkv ?bkh))
  (modify ?s (value white-move-done)))

(defrule king-deffending-move
  "If rook is attacked and white king can defend it - play king move."
  ?s <- (status (value white-move-ponder) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  (test (and (rook-attacked ?wrv ?wrh ?bkv ?bkh)
	     (not (rook-must-move ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh))
	     (not (on-edge ?wrv ?wrh))
	     (> (length$ (check-white-king-moves ?wkv ?wkh ?wrv ?wrh
						 ?bkv ?bkh))
		0)))
  =>
  (make-move king white (find-best-white-king-move ?wkv ?wkh ?wrv ?wrh
						   ?bkv ?bkh))
  (modify ?s (value white-move-done)))

(defrule play-best-move
  "If none of the other pondering rules is applicable
choose a move among king and rook best moves."
  (declare (salience -100))
  ?s <- (status (value white-move-ponder) (move-number ?))
  (figure (sortof king) (colour white) (vertical ?wkv)
	  (horizontal ?wkh))
  (figure (sortof rook) (colour white) (vertical ?wrv)
	  (horizontal ?wrh))
  (figure (sortof king) (colour black) (vertical ?bkv)
	  (horizontal ?bkh))
  =>
  (play-best-move ?wkv ?wkh ?wrv ?wrh ?bkv ?bkh)
  (modify ?s (value white-move-done)))
