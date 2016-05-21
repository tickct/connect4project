;;connectfour.lisp
;;Sean Gray
;;Vladimir Kolmakov
;;Last Edit: April 22th 2016

(defconstant empty 0 "An empty square")
(defconstant red  1 "A red piece")
(defconstant yellow 2 "A yellow piece")
(defconstant height 6 "height of the board")
(defconstant width 7 "width of the board")


;; aux fns
;; board
(defun board-array (&optional (init-list
                               (make-list height :initial-element
                                          (make-list width :initial-element 0))))
  (make-array (list height width) :initial-contents init-list))

(defun bref (board row col) (aref board row col))

(defun copy-board (board)
  (copy-array board))

(defun print-board (board)
  (loop for i from height downto 1 do
       (progn (loop for j from 1 to width do
                   (format t "~a " (bref board (- i 1) (- j 1))))
              (format t "~%"))))

;; players
(defun opponent (player)
  (if (eql player red) yellow red))

;; game
(defun start (&optional comcolor )
  (let ((board (board-array)))
    ;;switched order to have the computer play first
    (if (eql comcolor 2)
	(play board red)
	(play board yellow))))

(defun play (board player)
  (print-board board)
  ;;victory check
  (if (four-in-a-row-p board)
      (format t "~a has won~%" (opponent player) )
      ;; turn taking
      (if (eql player yellow)
	  (play (player-move board yellow) (opponent player))
	  (play (ai-move board red) (opponent player)))))

;;return if board is full at that location
(defun legal-p (board row col)
  (if (not (eql (bref board row col) 0)) nil 1)
)

(defun min-height (board col)
  ;;set at -1 so the first loop sets it to 0
  (loop for row from 0 to 5
       when (legal-p board row col) return row))

(defun ai-move (board player)
  (print 'Ai_turn>)
  (format t "~%")
  (move board player (brd_search board 'defense-cost-function))) 

(defun player-move (board player)
  (print 'your_turn>)
  (let ((pcol (read)))
    ;; make sure data is legal
;;TODO: filter out non number input
    (cond ((< pcol width)
	     (move board player pcol))
	  ;;iligal move catch
	  (t  (print 'illegal_move)
              (player-move board player)))))

(defun move (board player col)
  (setf (aref board (min-height board col) col) player) 
  board)

;; victory checks
(defun four-in-a-row-p (board)
  (loop for x from 0 to 5 thereis
       (loop for y from 0 to 6 thereis (or
                                        (four-row-p board x y)
                                        (four-right-dia-p board x y)
                                        (four-col-p board x y)
                                        (four-left-dia-p board x y)))))

(defun four-row-p (board row col)
  (if (< row 3)
      (loop for x from row to (+ 3 row)
	 always (and (eql (aref board row col) (aref board x col))
		     (not (eql (aref board row col) 0))))
      nil))

(defun four-col-p (board row col)
  (if (< col 4)
      (loop for x from col to (+ 3 col)
	 always (and
		 (eql (aref board row col) (aref board row x))
		 (not (eql (aref board row col) 0))))))


(defun four-left-dia-p (board row col)
  (if (and (< row 3) (> col 2))
      (loop for x from 0 to 3
	 always (and
		 (eql (aref board row col) (aref board (+ row x) (- col x)))
		 (not (eql (aref board row col) 0))))))

(defun four-right-dia-p (board row col)
  (if (and (< row 3) (< col 4))
      (loop for x from 0 to 3
	 always (and
                 (eql (aref board row col) (aref board (+ row x) (+ col x)))
                 (not (eql (aref board row col) 0))))))


(defun brd_search (board cost-function)
  
  (let ((costs (loop for x from 0 to 6  collect (funcall cost-function 
						 board yellow x))        ))
    ;;need the car because the cdr returns a list of one, car turns it into an atom, I am sure there is a

    (car (cdr (reduce #'(lambda (x y) (if (> (car x) (car y)) x y))
		      costs)))))
  
  
  ;; cost function
;;returns a list of a value and a col
(defun defense-cost-function (board player col)
  "Takes in a board and puts a score on it for a given player"
;;making sure we don't go out of bounds
  (if (min-height board col)
      (cond ((four-in-a-row-p (move (copy-board board)
				    player col))
	     (list 1000 col))
	    ((four-in-a-row-p (move (copy-board board)
				    (opponent player) col))
	     (list 2000 col))
	    (t
	     (list (numbernext board player col (min-height board col)) col))
      )
      (list -2 col)
  ))

;;returns a list of a value and a col
(defun mix-cost-function (board player col)
  "Takes in a board and puts a score on it for a given player"
;;making sure we don't go out of bounds
  (if (min-height board col) 
      (cond ((four-in-a-row-p (move (copy-board board)
						   player col))
	     (list 2000 col))
	    ((four-in-a-row-p (move (copy-board board)
				    (opponent player) col))
	     (list 1000 col))
	    (t
	     (list (numbernext board player col (min-height board col) ) col))
      )
      (list -2 col)
  ))

;;counts the number of the given players pieces touching the square
(defun numbernext (board player col row)
  (let ((count 0)) (loop for x from -1 to 1 do 
			(loop for y from -1 to 1 do
			   ;; keeps from out of bounds problems
			     (if (and (>= (+ col x) 0) (>= (+ row y) 0) (<= (+ col x) 6) (<= (+ row y) 5))
				 (if (eql (aref board (+ row y) (+ col x)) player)
				     (setf count (+ count 1)) )
				 )))
       count))
(defun play-x (brd player col depth move)
  (let ((board (copy-board brd)))
    (loop for x from 0 to depth collect (if (and (< (list-length move) (- depth 1)))
					    (progn
					      (format t "~a:~a:~a~%" (list-length move) col x)
					      (move board player (+ col x))
					      (play-x board player (+ col 0) depth  (append move `(,(+ col x)))))
					    (progn
					      (format t "~a~%" move)													             (list (four-in-a-row-p brd) move)))))

  ;; debug tools
)
(defun make-test-board ()
  "Returns a board for tests"
  (let ((brd `((,empty ,empty ,empty ,empty ,empty ,empty ,empty)
	       (,empty ,empty ,empty ,empty ,empty ,empty ,empty)
	       (,empty ,empty ,empty ,empty ,empty ,empty ,empty)
	       (,empty ,empty ,empty ,empty ,empty ,empty ,empty)
	       (,empty ,empty ,empty ,empty ,empty ,empty ,empty)
	       (,empty ,empty ,empty ,empty ,empty ,empty ,empty))))
    (board-array brd)))

;; from alexandria library of quicklisp
;;alows you to copy 2d arrays
(defun copy-array (array &key
			   (element-type (array-element-type array))
			   (fill-pointer (and (array-has-fill-pointer-p array)
					      (fill-pointer array)))
			   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
	 (new-array (make-array dimensions
				:element-type element-type
				:adjustable adjustable
				:fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
	    (row-major-aref array i)))
    new-array))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

