;;;; sudoku-basic.lisp

(in-package #:sudoku)

(defclass sudoku ()
  ((board :initform (make-array '(9 9)
                                :initial-element
                                '(1 2 3 4 5 6 7 8 9))
          :reader board :type array)
   (stack :accessor stack :type list :initform nil)))

(defmacro coordinates (sudoku row column)
  "Gives value of said coordinates"
  `(aref (board ,sudoku) ,row ,column))

(defmacro dorow ((var r c sudoku row &optional column) &body body)
  "Iterates over given row. If column is given, that position won't be
iterated over."
  (once-only (sudoku row)
    (with-unique-names (col)
      `(let ((,col ,column)
             (,r   ,row))
         (do* ((,c 0
                   (if (and ,col (= (1- ,col) ,c))
                       (1+ ,col)
                       (1+ ,c))))
             ((>= ,c 9) nil)
           (symbol-macrolet ((,var (coordinates ,sudoku ,row ,c)))
             (declare (ignorable ,var ,r ,c ,sudoku))
             ,@body))))))

(defmacro docolumn ((var r c sudoku column-or-row &optional column) &body body)
  "Iterates over given column. The given column will be variable
  column if given or column-or-row if not. If variable column is
  given, said coordinate is not iterated over."
  (once-only (sudoku column column-or-row)
    (with-unique-names (row col)
      `(let* ((,row (and ,column ,column-or-row))
              (,col (or  ,column ,column-or-row))
              (,c   ,col))
         (do* ((,r 0
                   (if (and ,row (= (1- ,row) ,r))
                       (1+ ,row)
                       (1+ ,r))))
              ((>= ,r 9) nil)
           (symbol-macrolet ((,var (coordinates ,sudoku ,r ,col)))
             (declare (ignorable ,var ,r ,c ,sudoku))
             ,@body))))))

(defmacro doquadrant ((var r c sudoku row column &optional treat-self-p) &body body)
  "Iterates over quadrant of given coordinate. Doesn't iterate over
  given coordinate unless treat-self-p is true."
  (once-only (sudoku row column)
    (with-unique-names (from-r from-c)
      `(let ((,from-r (* 3 (floor ,row    3)))
             (,from-c (* 3 (floor ,column 3))))
         (do* ((,r ,from-r (1+ ,r)))
              ((<= (+ 3 ,from-r) ,r) nil)
           (do* ((,c ,from-c (1+ ,c)))
                ((<= (+ 3 ,from-c) ,c) nil)
             (symbol-macrolet ((,var (coordinates ,sudoku ,r ,c)))
               (declare (ignorable ,var ,r ,c ,sudoku))
               ,@(unless treat-self-p
                         `((when (and (= ,r ,row)
                                      (= ,c ,column))
                             (go :end))))
               ,@body)
            :end))))))

(defmacro doboard ((var r c sudoku) &body body)
  "Iterates over all the board."
  (once-only (sudoku)
    `(do ((,r 0 (1+ ,r)))
         ((<= 9 ,r) nil)
       (do ((,c 0 (1+ ,c)))
           ((<= 9 ,c) nil)
         (symbol-macrolet ((,var (coordinates ,sudoku ,r ,c)))
           ,@body)))))

(defun remove-number (sudoku r c n)
  "Removes a number n from coordinate (r,c) of sudoku. If only one
number is left, it is set as the number for said coordinate and its
row, column and quadrant are set to be cleared."
  (let ((new-list (remove n (coordinates sudoku r c))))
    (setf (coordinates sudoku r c)
          (cond ((null (cadr new-list))
                 (push (list #'clean (list r c))
                       (stack sudoku))
                 (car new-list))
                (t new-list)))))

(defun fitsp (sudoku r c n &optional extra)
  "Indicates if a number n «fits» in coordinate (r,c) of sudoku. To
  fit, here, means that no other coordinate in the same row, column or
  quadrant shares said number. If extra is supplied, it will also look
  there for possible values stored as (r,c,n)."
  (labels ((return-if-found (v i j)
             (when (or (eql v n)
                       (some (lambda (l)
                               (let ((r (first l))
                                     (c (second l))
                                     (v (third l)))
                                 (and (eql r i)
                                      (eql c j)
                                      (eql v n))))
                        extra))
               (return-from fitsp nil))))
   (dorow (v i j sudoku r)
     (return-if-found v i j))
   (docolumn (v i j sudoku c)
     (return-if-found v i j))
   (doquadrant (v i j sudoku r c)
     (return-if-found v i j))
   t))
