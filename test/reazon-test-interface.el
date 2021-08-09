;;; reazon-test-interface.el ---                     -*- lexical-binding: t; -*-

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

;; Tests for the Reazon query interface and various means of query
;; combination. Most of these come from TRS.

;;; Code:

(require 'reazon-test-utils)

(ert-deftest reazon-test-interface-run ()
  (reazon--should-equal '()
    (reazon-run* q #'reazon-!U))
  (reazon--should-equal '(t)
    (reazon-run* q (reazon-== t q)))
  (reazon--should-equal '()
    (reazon-run* q #'reazon-!U (reazon-== t q)))
  (reazon--should-equal '(t)
    (reazon-run* q #'reazon-!S (reazon-== t q)))
  (reazon--should-equal '(corn)
    (reazon-run* r #'reazon-!S (reazon-== 'corn r)))
  (reazon--should-equal '(olive oil)
    (reazon-run* x (reazon-disj (reazon-== 'olive x) (reazon-== 'oil x))))
  (reazon--should-equal '(oil olive)
    (reazon-run* x (reazon-disj (reazon-== 'oil x) (reazon-== 'olive x))))
  (reazon--should-equal '(oil)
    (reazon-run* x (reazon-disj (reazon--conj-2 (reazon-== 'olive x) #'reazon-!U) (reazon-== 'oil x))))
  (reazon--should-equal '(olive _0 oil)
    (reazon-run* x (reazon-disj (reazon-conj (reazon-== 'virgin x) #'reazon-!U) (reazon-disj (reazon-== 'olive x) (reazon-disj #'reazon-!S (reazon-== 'oil x)))))))

(reazon-defrel reazon-test--alwayso ()
  "Infinite successful goals."
  (reazon-disj #'reazon-!S (reazon-test--alwayso)))

(reazon-defrel reazon-test--nevero ()
  "Infinite unsuccessful goals."
  (reazon-disj #'reazon-!U (reazon-test--nevero)))

(ert-deftest reazon-test-interface-timeout ()
  (reazon--should-equal '()
    (let ((reazon-timeout 0.01))
      (reazon-run* q (reazon-test--nevero))))
  ;; This test might fail if your computer is REALLY slow.
  (should (> (length (let ((reazon-timeout 0.01)) (reazon-run* q (reazon-test--alwayso)))) 3)))

(ert-deftest reazon-test-interface-fresh ()
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-fresh (x)
        (reazon-== t x)
        (reazon-== t q))))
  (reazon--should-equal '((_0 _1))
    (reazon-run* s
      (reazon-fresh (x)
        (reazon-fresh (y)
          (reazon-== `(,x ,y) s)))))
  (reazon--should-equal '((_0 _1 _0))
    (reazon-run* s
      (reazon-fresh (x y)
        (reazon-== `(,x ,y ,x) s))))
  (reazon--should-equal '((split pea))
    (reazon-run* r
      (reazon-fresh (x)
        (reazon-fresh (y)
          (reazon-== 'split x)
          (reazon-== 'pea y)
          (reazon-== `(,x ,y) r))))
    (reazon-run* r
      (reazon-fresh (x)
        (reazon-fresh (y)
          (reazon-== 'split x)
          (reazon-== 'pea y)
          (reazon-== `(,x ,y) r))))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-== 'split x)
        (reazon-== 'pea y)
        (reazon-== `(,x ,y) r)))
    (reazon-run* (x y)
      (reazon-== 'split x)
      (reazon-== 'pea y)))
  (reazon--should-equal '(((split pea) split pea))
    (reazon-run* (r x y)
      (reazon-== 'split x)
      (reazon-== 'pea y)
      (reazon-== `(,x ,y) r)))
  (reazon--should-equal '((_0 _1) (_0 _1))
    (reazon-run* (x y)
      (reazon-fresh (z)
        (reazon-conde
         ((reazon-== x z) (reazon-fresh (z) (reazon-== y z)))
         ((reazon-fresh (z) (reazon-== x z)) (reazon-== y z))))))
  (reazon--should-equal '((nil _0) (_0 nil))
    (reazon-run* (x y)
      (reazon-fresh (z)
        (reazon-conde
         ((reazon-== x z) (reazon-fresh (z) (reazon-== y z)))
         ((reazon-fresh (z) (reazon-== x z)) (reazon-== y z)))
        (reazon-== nil z)))))

(ert-deftest reazon-test-interface-project ()
  (reazon--should-equal '(25)
    (reazon-run* q
      (reazon-fresh (x)
        (reazon-== x 5)
        (reazon-project (x)
          (reazon-== q (* x x))))))
  (should-error
   (reazon-run* q
     (reazon-fresh (x)
       (reazon-project (x)
         (reazon-== q (* x x)))
       (reazon-== x 5)))))

(reazon-defrel reazon--test-teacupo (x)
  "X is tea or cup."
  (reazon-disj (reazon-== x 'tea) (reazon-== x 'cup)))

(reazon-defrel reazon--test-empty-relo (_x))

(ert-deftest reazon-test-interface-defrel ()
  (reazon--should-equal '(tea cup)
    (reazon-run* x (reazon--test-teacupo x)))
  (reazon--should-equal "X is tea or cup."
    (documentation #'reazon--test-teacupo))
  ;; (reazon--should-equal '(_0)
  ;;   (reazon-run* x (reazon--test-empty-relo x)))
  (reazon--should-equal '((nil t) (tea t) (cup t))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon--test-teacupo x) (reazon-== y t))
       ((reazon-== x nil) (reazon-== y t)))))
  (reazon--should-equal '((tea tea) (tea cup) (cup tea) (cup cup))
    (reazon-run* (x y)
      (reazon--test-teacupo x)
      (reazon--test-teacupo y)))
  (reazon--should-equal '(tea cup)
    (reazon-run* x
      (reazon--test-teacupo x)
      (reazon--test-teacupo x)))
  (reazon--should-equal '((nil tea) (nil cup) (tea _0) (cup _0))
    (reazon-run* (x y)
      (reazon--disj-2
       (reazon--conj-2 (reazon--test-teacupo x) (reazon--test-teacupo x))
       (reazon--conj-2 (reazon-== nil x) (reazon--test-teacupo y)))))
  (reazon--should-equal '((t tea) (t cup) (tea _0) (cup _0))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon--test-teacupo x) (reazon--test-teacupo x))
       ((reazon-== x t) (reazon--test-teacupo y))))))

(ert-deftest reazon--test-conde ()
  (reazon--should-equal '()
    (reazon-run* q (reazon-conde)))
  (reazon--should-equal '((split pea) (navy bean) (red lentil))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon-== x 'split) (reazon-== y 'pea))
       ((reazon-== x 'navy) (reazon-== y 'bean))
       ((reazon-== x 'red) (reazon-== y 'lentil)))))
  (reazon--should-equal '(oil)
    (reazon-run* x
      (reazon-conde
       ((reazon-== x 'olive) #'reazon-!U)
       ((reazon-== x 'oil))))))

(ert-deftest reazon-test-interface-empty-conj-disj ()
  (reazon--should-equal '(_0)
    (reazon-run* q)
    (reazon-run* q (reazon-conj)))
  (reazon--should-equal '()
    (reazon-run* q (reazon-disj))))

(ert-deftest reazon-test-interface-conda ()
  (reazon--should-equal '()
    (reazon-run* q (reazon-conda (#'reazon-!U #'reazon-!S) (#'reazon-!U)))
    (reazon-run* q (reazon-conda (#'reazon-!S #'reazon-!U) (#'reazon-!S)))
    (reazon-run* x
      (reazon-conda
       ((reazon-== 'virgin x) #'reazon-!U)
       ((reazon-== 'olive x) #'reazon-!S)
       ((reazon-== 'oil x))))
    (reazon-run* q
      (reazon-fresh (x y)
        (reazon-== 'split x)
        (reazon-== 'pea y)
        (reazon-conda
         ((reazon-== 'split x) (reazon-== x y))
         (#'reazon-!S)))))
  (reazon--should-equal '(_0)
    (reazon-run* q (reazon-conda (#'reazon-!U #'reazon-!S) (#'reazon-!S)))
    (reazon-run* q (reazon-conda (#'reazon-!S #'reazon-!S) (#'reazon-!U)))
    (reazon-run* q
      (reazon-fresh (x y)
        (reazon-== 'split x)
        (reazon-== 'pea y)
        (reazon-conda
         ((reazon-== x y) (reazon-== 'split x))
         (#'reazon-!S)))))
  (reazon--should-equal '(olive)
    (reazon-run* x
      (reazon-conda
       ((reazon-== 'olive x) #'reazon-!S)
       ((reazon-== 'oil x))))))

(reazon-defrel reazon--not-pasta (x)
  "X is not pasta."
  (reazon-conda
   ((reazon-== 'pasta x) #'reazon-!U)
   (#'reazon-!S)))

(ert-deftest reazon-test-interface-not-pasta ()
  (reazon--should-equal '(spaghetti)
    (reazon-run* x
      (reazon-conda
       ((reazon--not-pasta x) #'reazon-!U)
       ((reazon-== 'spaghetti x) #'reazon-!S)
       (#'reazon-!U))))
  (reazon--should-equal '()
    (reazon-run* x
      (reazon-== 'spaghetti x)
      (reazon-conda
       ((reazon--not-pasta x) #'reazon-!U)
       ((reazon-== 'spaghetti x) #'reazon-!S)
       (#'reazon-!U)))))

(reazon-defrel reazon--test-onceo (goal)
  "???"
  (reazon-condu
   (goal #'reazon-!S)
   (#'reazon-!U)))

(ert-deftest reazon-test-interface-condu ()
  (reazon--should-equal '(tea)
    (reazon-run* x
      (reazon--test-onceo
       (reazon--test-teacupo x)))))

(ert-deftest reazon-test-interface-condeau ()
  (reazon--should-equal '(5 tea cup)
    (reazon-run* q
      (reazon-conde
       ((reazon--test-teacupo q) (reazon-== 0 0))
       ((reazon-== q 5) (reazon-== 0 0)))))
  (reazon--should-equal '(tea cup)
    (reazon-run* q
      (reazon-conda
       ((reazon--test-teacupo q) (reazon-== 0 0))
       ((reazon-== q 5)))))
  (reazon--should-equal '(tea)
    (reazon-run* q
      (reazon-condu
       ((reazon--test-teacupo q) (reazon-== 0 0))
       ((reazon-== q 5)))))
  (reazon--should-equal '()
    (reazon-run* q (reazon-conde))
    (reazon-run* q (reazon-conda))
    (reazon-run* q (reazon-condu))))


(provide 'reazon-test-interface)
;;; reazon-test-interface.el ends here
