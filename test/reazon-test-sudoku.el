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
    (reazon-sudoku-solve-4x4 (a2 3) (b1 4) (b4 2) (c4 1) (d3 2))))


(provide 'reazon-test-sudoku)
;;; reazon-test-sudoku.el ends here
