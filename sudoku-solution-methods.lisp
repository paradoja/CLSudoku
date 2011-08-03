(in-package #:sudoku)

(defun clean-row (sudoku r c)
  "Cleans given row of the value in (r,c)."
  (let ((val (coordinates sudoku r c)))
    (dorow (v i j sudoku r)
      (when (consp v)
        (remove-number sudoku i j val)))))

(defun clean-column (sudoku r c)
  "Cleans given column of the value in (r,c)."
  (let ((val (coordinates sudoku r c)))
    (docolumn (v i j sudoku c)
      (when (consp v)
        (remove-number sudoku i j val)))))

(defun clean-quadrant (sudoku r c)
  "Cleans the quadrant of the given coordinate (r,c)."
  (let ((val (coordinates sudoku r c)))
    (doquadrant (v i j sudoku r c)
      (when (consp v)
        (remove-number sudoku i j val)))))

(defun clean (sudoku r c)
  "Puts clean functions of row, column and quadrant on given
  coordinate on the stack."
  (mapc #'(lambda (f) (push (list f (list r c)) (stack sudoku)))
        (list #'clean-row #'clean-column #'clean-quadrant)))

(defun simple-backtracker (sudoku)
  "A simple backtracker to solve everything left. It will just try
  possibilities until the sudoku is finished. Won't try to simplify
  board cleaning anything at all."
  (labels ((backtracker (coordinates solutions)
             "Tries a new position in the backtracker"
             (if (null coordinates)
                 (throw :solution solutions)
                 (destructuring-bind ((r c &rest vs) &rest rest) coordinates
                   (dolist (v vs)
                     (when (fitsp sudoku r c v solutions)
                       (backtracker rest
                                    (cons (list r c v) solutions)))))))
           (set-solutions (solutions)
             "Fills the sudoku with the given variables"
             (dolist (el solutions)
               (destructuring-bind (r c n) el
                 (setf (coordinates sudoku r c) n)))))
    (set-solutions ; once we have the solutions, fill in the sudoku
     (catch :solution
       (let (coordinates)
         (doboard (v r c sudoku)
           (when (consp v)
             ; We get the coord. with their position...
             (push (cons r (cons c v)) coordinates)))
         (backtracker (sort coordinates #'< :key #'length) ; ...sort them
                      nil))))))
