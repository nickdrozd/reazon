;;; reazon-sudoku.el --- Sudoku solvers written in Reazon  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Drozd

;; Author: Nick Drozd <nicholasdrozd@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Some Sudoku solvers. These are considerably slower than specialized
;; Sudoku programs, but they have the advantage of being incredibly
;; simple to write and use.

;;; Code:

(require 'reazon)

(defmacro reazon-sudoku-solve-4x4 (&rest coordinate-value-pairs)
  "Solve 4x4 sudoku puzzles, given constraints COORDINATE-VALUE-PAIRS.

Rows are named with letters, and columns are named with numbers. For example, b4
is the square in the fourth column from the left of the second row from the top.
The items in COORDINATE-VALUE-PAIRS must be lists of the form (COORDINATE
VALUE). For example, (a3 1) puts the value 1 in the third column of the first
row. Answers are in the form of lists of coordinates ordered by row and then by
column (so that, for example, c4 comes immediately after c3 but immediately
before d1).

Example call:
    (reazon-sudoku-solve-4x4 (a2 3) (b1 4) (b4 2) (c4 1) (d3 2))
        => ((2 3 1 4 4 1 3 2 3 2 4 1 1 4 2 3))

If there are multiple solutions satisfying the given constraints, all of them
will be generated."

  `(let ((range '(1 2 3 4)))
     (reazon-run* (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)

       ,@(mapcar
          (lambda (cvp) `(reazon-== ,@cvp))
          coordinate-value-pairs)

       (reazon-subset-o range `(,a1 ,a2 ,a3 ,a4))
       (reazon-subset-o range `(,b1 ,b2 ,b3 ,b4))
       (reazon-subset-o range `(,c1 ,c2 ,c3 ,c4))
       (reazon-subset-o range `(,d1 ,d2 ,d3 ,d4))

       (reazon-subset-o range `(,a1 ,b1 ,c1 ,d1))
       (reazon-subset-o range `(,a2 ,b2 ,c2 ,d2))
       (reazon-subset-o range `(,a3 ,b3 ,c3 ,d3))
       (reazon-subset-o range `(,a4 ,b4 ,c4 ,d4))

       (reazon-subset-o range `(,a1 ,a2 ,b1 ,b2))
       (reazon-subset-o range `(,a3 ,a4 ,b3 ,b4))
       (reazon-subset-o range `(,c1 ,c2 ,d1 ,d2))
       (reazon-subset-o range `(,c3 ,c4 ,d3 ,d4)))))

(defun reazon-sudoku-generate-4x4 (n)
  "Generate N solved 4x4 Sudoku instances from scratch.
This is slow, so be patient!"
  (let ((range '(1 2 3 4)))
    (reazon-run n (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)

      (reazon-subset-o range `(,a1 ,a2 ,a3 ,a4))
      (reazon-subset-o range `(,b1 ,b2 ,b3 ,b4))
      (reazon-subset-o range `(,c1 ,c2 ,c3 ,c4))
      (reazon-subset-o range `(,d1 ,d2 ,d3 ,d4))

      (reazon-subset-o range `(,a1 ,b1 ,c1 ,d1))
      (reazon-subset-o range `(,a2 ,b2 ,c2 ,d2))
      (reazon-subset-o range `(,a3 ,b3 ,c3 ,d3))
      (reazon-subset-o range `(,a4 ,b4 ,c4 ,d4))

      (reazon-subset-o range `(,a1 ,a2 ,b1 ,b2))
      (reazon-subset-o range `(,a3 ,a4 ,b3 ,b4))
      (reazon-subset-o range `(,c1 ,c2 ,d1 ,d2))
      (reazon-subset-o range `(,c3 ,c4 ,d3 ,d4)))))


(provide 'reazon-sudoku)
;;; reazon-sudoku.el ends here
