(in-package #:sudoku)

(defun simple-solver (sudoku)
  "It tries to solve a sudoku simply by looking for solved positions,
and eliminating options in rows and quadrants related to said
positions."
  (doboard (v r c sudoku)
    (unless (consp v)
      (clean sudoku r c)))
  (do ((action (pop (stack sudoku))
               (pop (stack sudoku))))
      ((null (stack sudoku)) nil)
    (let ((f (first action))
          (pars (second action)))
      (apply f sudoku pars))))

(defun backtracker-solver (sudoku)
  "It will try to solve with simpler methods first, and then will
backtrack."
  (simple-solver sudoku)
  (simple-backtracker sudoku))
