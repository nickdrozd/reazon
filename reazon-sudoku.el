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

(defmacro reazon--one-of-the-following (s perms)
  "Assert that S is one of PERMS."
  `(reazon-disj
    ,@(mapcar
       (lambda (perm)
         `(reazon-== ,s ',perm))
       perms)))

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
will be generated. In particular, if no constraints are specified, all 4x4
solved instances will be generated."

  (let ((perms (reazon-run* q
                 (reazon-fresh (a b c d)
                   (reazon-== q `(,a ,b ,c ,d)))
                 (reazon-set-equalo q '(1 2 3 4)))))
   `(reazon-run* (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)

     ,@(mapcar
        (lambda (cvp) `(reazon-== ,@cvp))
        coordinate-value-pairs)

     (reazon--one-of-the-following `(,a1 ,a2 ,a3 ,a4) ,perms)
     (reazon--one-of-the-following `(,a1 ,b1 ,c1 ,d1) ,perms)
     (reazon--one-of-the-following `(,a1 ,a2 ,b1 ,b2) ,perms)
     (reazon--one-of-the-following `(,b1 ,b2 ,b3 ,b4) ,perms)
     (reazon--one-of-the-following `(,a2 ,b2 ,c2 ,d2) ,perms)
     (reazon--one-of-the-following `(,a3 ,b3 ,c3 ,d3) ,perms)
     (reazon--one-of-the-following `(,a3 ,a4 ,b3 ,b4) ,perms)
     (reazon--one-of-the-following `(,a4 ,b4 ,c4 ,d4) ,perms)
     (reazon--one-of-the-following `(,c1 ,c2 ,c3 ,c4) ,perms)
     (reazon--one-of-the-following `(,c1 ,c2 ,d1 ,d2) ,perms)
     (reazon--one-of-the-following `(,d1 ,d2 ,d3 ,d4) ,perms)
     (reazon--one-of-the-following `(,c3 ,c4 ,d3 ,d4) ,perms))))


(reazon-defrel reazon--in-range-9 (s)
  "Assert that numbers 1-9 are in S."
  (reazon-membero 1 s)
  (reazon-membero 2 s)
  (reazon-membero 3 s)
  (reazon-membero 4 s)
  (reazon-membero 5 s)
  (reazon-membero 6 s)
  (reazon-membero 7 s)
  (reazon-membero 8 s)
  (reazon-membero 9 s))

(defmacro reazon-sudoku-solve-9x9 (&rest coordinate-value-pairs)
  "Solve 9x9 sudoku puzzles, given constraints COORDINATE-VALUE-PAIRS.

NOTE: This is currently untenably slow.

Rows are named with letters, and columns are named with numbers. For example, b4
is the square in the fourth column from the left of the second row from the top.
The items in COORDINATE-VALUE-PAIRS must be lists of the form (COORDINATE
VALUE). For example, (a3 1) puts the value 1 in the third column of the first
row. Answers are in the form of lists of coordinates ordered by row and then by
column (so that, for example, c9 comes immediately after c8 but immediately
before d1).

If there are multiple solutions satisfying the given constraints, all of them
will be generated. In particular, if no constraints are specified, all 9x9
solved instances will be generated (but don't rely on it; the universe will
expire before the computation completes)."

  `(let (reazon-occurs-check)
     (reazon-run* (a1 a2 a3 a4 a5 a6 a7 a8 a9
                      b1 b2 b3 b4 b5 b6 b7 b8 b9
                      c1 c2 c3 c4 c5 c6 c7 c8 c9
                      d1 d2 d3 d4 d5 d6 d7 d8 d9
                      e1 e2 e3 e4 e5 e6 e7 e8 e9
                      f1 f2 f3 f4 f5 f6 f7 f8 f9
                      g1 g2 g3 g4 g5 g6 g7 g8 g9
                      h1 h2 h3 h4 h5 h6 h7 h8 h9
                      i1 i2 i3 i4 i5 i6 i7 i8 i9)

       ,@(mapcar
          (lambda (cvp) `(reazon-== ,@cvp))
          coordinate-value-pairs)

       (reazon--in-range-9 (list a1 a2 a3 a4 a5 a6 a7 a8 a9))
       (reazon--in-range-9 (list a1 b1 c1 d1 e1 f1 g1 h1 i1))
       (reazon--in-range-9 (list a1 a2 a3 b1 b2 b3 c1 c2 c3))
       (reazon--in-range-9 (list a2 b2 c2 d2 e2 f2 g2 h2 i2))
       (reazon--in-range-9 (list b1 b2 b3 b4 b5 b6 b7 b8 b9))
       (reazon--in-range-9 (list a3 b3 c3 d3 e3 f3 g3 h3 i3))
       (reazon--in-range-9 (list c1 c2 c3 c4 c5 c6 c7 c8 c9))
       (reazon--in-range-9 (list a4 a5 a6 b4 b5 b6 c4 c5 c6))
       (reazon--in-range-9 (list a4 b4 c4 d4 e4 f4 g4 h4 i4))
       (reazon--in-range-9 (list a5 b5 c5 d5 e5 f5 g5 h5 i5))
       (reazon--in-range-9 (list d1 d2 d3 d4 d5 d6 d7 d8 d9))
       (reazon--in-range-9 (list d4 d5 d6 e4 e5 e6 f4 f5 f6))
       (reazon--in-range-9 (list d1 d2 d3 e1 e2 e3 f1 f2 f3))
       (reazon--in-range-9 (list e1 e2 e3 e4 e5 e6 e7 e8 e9))
       (reazon--in-range-9 (list f1 f2 f3 f4 f5 f6 f7 f8 f9))
       (reazon--in-range-9 (list a6 b6 c6 d6 e6 f6 g6 h6 i6))
       (reazon--in-range-9 (list g4 g5 g6 h4 h5 h6 i4 i5 i6))
       (reazon--in-range-9 (list i1 i2 i3 i4 i5 i6 i7 i8 i9))
       (reazon--in-range-9 (list g1 g2 g3 g4 g5 g6 g7 g8 g9))
       (reazon--in-range-9 (list h1 h2 h3 h4 h5 h6 h7 h8 h9))
       (reazon--in-range-9 (list a7 b7 c7 d7 e7 f7 g7 h7 i7))
       (reazon--in-range-9 (list a8 b8 c8 d8 e8 f8 g8 h8 i8))
       (reazon--in-range-9 (list a9 b9 c9 d9 e9 f9 g9 h9 i9))
       (reazon--in-range-9 (list a7 a8 a9 b7 b8 b9 c7 c8 c9))
       (reazon--in-range-9 (list d7 d8 d9 e7 e8 e9 f7 f8 f9))
       (reazon--in-range-9 (list g1 g2 g3 h1 h2 h3 i1 i2 i3))
       (reazon--in-range-9 (list g7 g8 g9 h7 h8 h9 i7 i8 i9)))))


(provide 'reazon-sudoku)
;;; reazon-sudoku.el ends here
