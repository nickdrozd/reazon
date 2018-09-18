;;; reazon-test-sudoku.el ---                        -*- lexical-binding: t; -*-

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

;; Tests for Sudoku solvers. There aren't as many of these as there
;; should be, since Sudoku queries can take a long time to finish.

;;; Code:

(require 'reazon-test-utils)
(require 'reazon-sudoku)

(ert-deftest reazon-test-sudoku-solve-4x4 ()
  (reazon--should-equal '((2 3 1 4 4 1 3 2 3 2 4 1 1 4 2 3))
    (reazon-sudoku-solve-4x4 (a2 3) (b1 4) (b4 2) (c4 1) (d3 2)))
  (reazon--should-equal '()
    (reazon-sudoku-solve-4x4 (a1 1) (b2 1))
    (reazon-sudoku-solve-4x4 (a3 2) (a4 2))
    (reazon-sudoku-solve-4x4 (a1 3) (b1 3))))

(ert-deftest reazon-test-sudoku-solve-9x9 ()
  (reazon--should-equal '()
    ;; Reject invalid boards
    (reazon-sudoku-solve-9x9
     (a1 1) (a2 1) (a3 1) (a4 1) (a5 1)))
  (reazon--should-equal '((4 3 5 2 6 9 7 8 1
                             6 8 2 5 7 1 4 9 3
                             1 9 7 8 3 4 5 6 2
                             8 2 6 1 9 5 3 4 7
                             3 7 4 6 8 2 9 1 5
                             9 5 1 7 4 3 6 2 8
                             5 1 9 3 2 6 8 7 4
                             2 4 8 9 5 7 1 3 6
                             7 6 3 4 1 8 2 5 9))
    ;; Verify a solved board
    (reazon-sudoku-solve-9x9
     (a1 4) (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7) (a8 8) (a9 1)
     (b1 6) (b2 8) (b3 2) (b4 5) (b5 7) (b6 1) (b7 4) (b8 9) (b9 3)
     (c1 1) (c2 9) (c3 7) (c4 8) (c5 3) (c6 4) (c7 5) (c8 6) (c9 2)
     (d1 8) (d2 2) (d3 6) (d4 1) (d5 9) (d6 5) (d7 3) (d8 4) (d9 7)
     (e1 3) (e2 7) (e3 4) (e4 6) (e5 8) (e6 2) (e7 9) (e8 1) (e9 5)
     (f1 9) (f2 5) (f3 1) (f4 7) (f5 4) (f6 3) (f7 6) (f8 2) (f9 8)
     (g1 5) (g2 1) (g3 9) (g4 3) (g5 2) (g6 6) (g7 8) (g8 7) (g9 4)
     (h1 2) (h2 4) (h3 8) (h4 9) (h5 5) (h6 7) (h7 1) (h8 3) (h9 6)
     (i1 7) (i2 6) (i3 3) (i4 4) (i5 1) (i6 8) (i7 2) (i8 5) (i9 9))
    ;; Same as the previous one, but solving for the last row
    (reazon-sudoku-solve-9x9
     (a1 4) (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7) (a8 8) (a9 1)
     (b1 6) (b2 8) (b3 2) (b4 5) (b5 7) (b6 1) (b7 4) (b8 9) (b9 3)
     (c1 1) (c2 9) (c3 7) (c4 8) (c5 3) (c6 4) (c7 5) (c8 6) (c9 2)
     (d1 8) (d2 2) (d3 6) (d4 1) (d5 9) (d6 5) (d7 3) (d8 4) (d9 7)
     (e1 3) (e2 7) (e3 4) (e4 6) (e5 8) (e6 2) (e7 9) (e8 1) (e9 5)
     (f1 9) (f2 5) (f3 1) (f4 7) (f5 4) (f6 3) (f7 6) (f8 2) (f9 8)
     (g1 5) (g2 1) (g3 9) (g4 3) (g5 2) (g6 6) (g7 8) (g8 7) (g9 4)
     (h1 2) (h2 4) (h3 8) (h4 9) (h5 5) (h6 7) (h7 1) (h8 3) (h9 6))
    ;; The same board again, but solving for the last column
    (reazon-sudoku-solve-9x9
     (a1 4) (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7) (a8 8)
     (b1 6) (b2 8) (b3 2) (b4 5) (b5 7) (b6 1) (b7 4) (b8 9)
     (c1 1) (c2 9) (c3 7) (c4 8) (c5 3) (c6 4) (c7 5) (c8 6)
     (d1 8) (d2 2) (d3 6) (d4 1) (d5 9) (d6 5) (d7 3) (d8 4)
     (e1 3) (e2 7) (e3 4) (e4 6) (e5 8) (e6 2) (e7 9) (e8 1)
     (f1 9) (f2 5) (f3 1) (f4 7) (f5 4) (f6 3) (f7 6) (f8 2)
     (g1 5) (g2 1) (g3 9) (g4 3) (g5 2) (g6 6) (g7 8) (g8 7)
     (h1 2) (h2 4) (h3 8) (h4 9) (h5 5) (h6 7) (h7 1) (h8 3)
     (i1 7) (i2 6) (i3 3) (i4 4) (i5 1) (i6 8) (i7 2) (i8 5))
    ;; Missing TL-BR diagonal
    (reazon-sudoku-solve-9x9
            (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7) (a8 8) (a9 1)
     (b1 6)        (b3 2) (b4 5) (b5 7) (b6 1) (b7 4) (b8 9) (b9 3)
     (c1 1) (c2 9)        (c4 8) (c5 3) (c6 4) (c7 5) (c8 6) (c9 2)
     (d1 8) (d2 2) (d3 6)        (d5 9) (d6 5) (d7 3) (d8 4) (d9 7)
     (e1 3) (e2 7) (e3 4) (e4 6)        (e6 2) (e7 9) (e8 1) (e9 5)
     (f1 9) (f2 5) (f3 1) (f4 7) (f5 4)        (f7 6) (f8 2) (f9 8)
     (g1 5) (g2 1) (g3 9) (g4 3) (g5 2) (g6 6)        (g8 7) (g9 4)
     (h1 2) (h2 4) (h3 8) (h4 9) (h5 5) (h6 7) (h7 1)        (h9 6)
     (i1 7) (i2 6) (i3 3) (i4 4) (i5 1) (i6 8) (i7 2) (i8 5))
    ;; Missing BL-TR diagonal
    (reazon-sudoku-solve-9x9
     (a1 4) (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7) (a8 8)
     (b1 6) (b2 8) (b3 2) (b4 5) (b5 7) (b6 1) (b7 4)        (b9 3)
     (c1 1) (c2 9) (c3 7) (c4 8) (c5 3) (c6 4)        (c8 6) (c9 2)
     (d1 8) (d2 2) (d3 6) (d4 1) (d5 9)        (d7 3) (d8 4) (d9 7)
     (e1 3) (e2 7) (e3 4) (e4 6)        (e6 2) (e7 9) (e8 1) (e9 5)
     (f1 9) (f2 5) (f3 1)        (f5 4) (f6 3) (f7 6) (f8 2) (f9 8)
     (g1 5) (g2 1)        (g4 3) (g5 2) (g6 6) (g7 8) (g8 7) (g9 4)
     (h1 2)        (h3 8) (h4 9) (h5 5) (h6 7) (h7 1) (h8 3) (h9 6)
            (i2 6) (i3 3) (i4 4) (i5 1) (i6 8) (i7 2) (i8 5) (i9 9))
    ;; Missing the middle of every box
    (reazon-sudoku-solve-9x9
     (a1 4) (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7) (a8 8) (a9 1)
     (b1 6)        (b3 2) (b4 5)        (b6 1) (b7 4)        (b9 3)
     (c1 1) (c2 9) (c3 7) (c4 8) (c5 3) (c6 4) (c7 5) (c8 6) (c9 2)
     (d1 8) (d2 2) (d3 6) (d4 1) (d5 9) (d6 5) (d7 3) (d8 4) (d9 7)
     (e1 3)        (e3 4) (e4 6)        (e6 2) (e7 9)        (e9 5)
     (f1 9) (f2 5) (f3 1) (f4 7) (f5 4) (f6 3) (f7 6) (f8 2) (f9 8)
     (g1 5) (g2 1) (g3 9) (g4 3) (g5 2) (g6 6) (g7 8) (g8 7) (g9 4)
     (h1 2)        (h3 8) (h4 9)        (h6 7) (h7 1)        (h9 6)
     (i1 7) (i2 6) (i3 3) (i4 4) (i5 1) (i6 8) (i7 2) (i8 5) (i9 9))))


(provide 'reazon-test-sudoku)
;;; reazon-test-sudoku.el ends here
