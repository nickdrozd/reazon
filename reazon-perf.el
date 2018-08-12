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
(require 'profiler)
(require 'reazon)
(require 'reazon-sudoku)

(defun reazon-profile-memory ()
  "Profile Reazon's memory usage.
Keep an eye out for recursive functions!"
  (interactive)
  (profiler-start 'mem)
  (dotimes (_ 1)
    ;; dummy value to silence compiler warnings
    (let ((dummy
           (reazon-run 8 q
             (reazon-set-equal-o q '(1 2 3 4)))))
      (null dummy)))
  (profiler-report)
  (profiler-stop))

(defun reazon-benchmark ()
  "Benchmark Reazon performance."
  (interactive)
  (benchmark 1 '(reazon-sudoku-solve-4x4)))


(provide 'reazon-perf)
;;; reazon-perf.el ends here
