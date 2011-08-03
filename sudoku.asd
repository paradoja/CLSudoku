;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :components ((:file "package")
               (:file "sudoku-basic")
               (:file "sudoku-solution-methods")
               (:file "sudoku-solver")
               (:file "sudoku-io")))

