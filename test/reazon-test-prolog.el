;;; reazon-test-prolog.el ---                        -*- lexical-binding: t; -*-

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

;; Tests for Prolog-style database queries.

;;; Code:

(require 'reazon-test-utils)

(reazon-defrel reazon--test-likes (a b)
  (reazon-conde
   ((reazon-== a 'kim) (reazon-== b 'robin))
   ((reazon-== a 'sandy) (reazon-== b 'lee))
   ((reazon-== a 'sandy) (reazon-== b 'kim))
   ((reazon-== a 'robin) (reazon-== b 'cats))
   ;; ((reazon-== a 'sandy) (reazon--test-likes b 'cats))
   ((reazon-fresh (x)
      (reazon-== a 'sandy)
      (reazon-== b x)
      (reazon--test-likes x 'cats)))
   ;; ((reazon-== a 'kim) (reazon--test-likes b 'lee) (reazon--test-likes b 'kim))
   ((reazon-fresh (x)
      (reazon-== a 'kim)
      (reazon-== b x)
      (reazon--test-likes x 'lee)
      (reazon--test-likes x 'kim)))
   ;; ((reazon-== a b))
   ((reazon-fresh (x)
      (reazon-== a x)
      (reazon-== b x)))))

(ert-deftest reazon-test-prolog-likes ()
  "This is an example from the chapter on Prolog in Norvig's PAIP."
  (reazon--should-equal '(lee kim sandy robin sandy cats)
    (reazon-run* who
      (reazon--test-likes 'sandy who)))
  (reazon--should-equal '(sandy sandy kim)
    (reazon-run* who
      (reazon--test-likes who 'sandy)))
  (reazon--should-equal '()
    (reazon-run* q
      (reazon--test-likes 'robin 'lee)))
  (reazon--should-equal
      '((sandy kim)
        (_0 _0)
        (sandy sandy)
        (sandy sandy)
        (sandy sandy)
        (kim sandy))
    (reazon-run* (x y)
      (reazon--test-likes x y)
      (reazon--test-likes y x))))


(provide 'reazon-test-prolog)
;;; reazon-test-prolog.el ends here
