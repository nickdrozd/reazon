;;; reazon-perf.el --- Performance utilities for Reazon  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'profiler)
(require 'reazon)
(require 'reazon-sudoku)

(defun reazon-profile-memory ()
  "Profile Reazon's memory usage.
Keep an eye out for recursive functions!"
  (interactive)
  (profiler-start 'mem)
  (dotimes (_ 2)
    ;; dummy value to silence compiler warnings
    (let ((dummy
           (reazon-sudoku-solve-4x4)))
      (null dummy)))
  (profiler-report)
  (profiler-stop))

(defun reazon-benchmark ()
  "Benchmark Reazon performance."
  (interactive)
  (benchmark
   1
   '(let ((board
           (reazon-sudoku-solve-9x9
                   (a2 3) (a3 5) (a4 2) (a5 6) (a6 9) (a7 7)
            (b1 6)        (b3 2) (b4 5)        (b6 1) (b7 4)        (b9 3)
            (c1 1) (c2 9)        (c4 8) (c5 3) (c6 4)        (c8 6) (c9 2)
            (d1 8) (d2 2) (d3 6)        (d5 9)        (d7 3)        (d9 7)
                   (e2 7) (e3 4) (e4 6)        (e6 2) (e7 9) (e8 1) (e9 5)
            (f1 9) (f2 5) (f3 1)        (f5 4)        (f7 6) (f8 2)
                   (g2 1)        (g4 3) (g5 2) (g6 6)        (g8 7)
            (h1 2)        (h3 8) (h4 9) (h5 5) (h6 7) (h7 1)
                   (i2 6) (i3 3))))
      (cl-assert
       (equal
        board
        '((4 3 5 2 6 9 7 8 1
             6 8 2 5 7 1 4 9 3
             1 9 7 8 3 4 5 6 2
             8 2 6 1 9 5 3 4 7
             3 7 4 6 8 2 9 1 5
             9 5 1 7 4 3 6 2 8
             5 1 9 3 2 6 8 7 4
             2 4 8 9 5 7 1 3 6
             7 6 3 4 1 8 2 5 9)))))))


(provide 'reazon-perf)
;;; reazon-perf.el ends here
