;;; reazon-test-unit.el ---                          -*- lexical-binding: t; -*-

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

;; Tests for Reazon's internal components. These tests do not use the
;; Reazon query interface, and can therefore be changed relatively
;; freely. They are mainly here as a guide for modifying the language
;; implementation.

;;; Code:

(require 'reazon-test-utils)

(ert-deftest reazon-test-unit-variable ()
  (should (reazon--variable-p (reazon--make-variable 'x)))
  (should-not (reazon--variable-p 'x))
  (reazon--with-variables (x y z)
    (should (reazon--variable-p x))
    (should (reazon--variable-p y))
    (should (reazon--variable-p z))))

(ert-deftest reazon-test-unit-walk ()
  (reazon--with-variables (u w x y z)
    (let ((sub-1 `((,z . a) (,x . ,w) (,y . ,z)))
          (sub-2 `((,x . b) (,z . ,y) (,w . (,x e ,z)) (,u . ,w))))
      (reazon--should-equal 'a
        (reazon--walk z sub-1)
        (reazon--walk y sub-1))
      (reazon--should-equal w
        (reazon--walk x sub-1)
        (reazon--walk w sub-1))
      (reazon--should-equal 'b (reazon--walk x sub-2))
      (reazon--should-equal `(,x e ,z) (reazon--walk u sub-2))
      (reazon--should-equal `(b e ,y) (reazon--walk* u sub-2)))))

(ert-deftest reazon-test-unit-occurs ()
  (reazon--with-variables (x y)
    (should (reazon--occurs-p x x '()))
    (should (reazon--occurs-p x `(,y) `((,y . ,x))))))

(ert-deftest reazon-test-unit-extend ()
  (reazon--with-variables (x y z)
    (reazon--should-equal reazon--false
      (reazon--extend x x '())
      (reazon--extend x `(,x) '())
      (reazon--extend x `(,y) `((,y . ,x))))
    (reazon--should-equal 'e
      (reazon--walk y (reazon--extend x 'e `((,z . ,x) (,y . ,z)))))))

(ert-deftest reazon-test-unit-unify ()
  (reazon--with-variables (x y z)
    (reazon--should-equal `((,y . 1) (,x . 2))
      (reazon--unify `(,x + 1) `(2 + ,y) '()))
    (reazon--should-equal `((,x . ,y))
      (reazon--unify x y '()))
    (reazon--should-equal `((,x . ,y))
      (reazon--unify `(,x ,x) `(,y ,y) '())
      (reazon--unify `(,x ,x ,x) `(,y ,y ,y) '())
      (reazon--unify `(,x ,y) `(,y ,x) '()))
    (reazon--should-equal `((,y . a) (,x . ,y))
      (reazon--unify `(,x ,y a) `(,y ,x ,x) '()))
    (reazon--should-equal reazon--false
      (reazon--unify x `(f ,x) '())
      (reazon--unify `(,x ,y) `((f ,y) (f ,x)) '())
      (reazon--unify `(,x ,y ,z) `((,y ,z) (,x ,z) (,x ,y)) '()))

    ;; PAIP ex 11.5

    ;; At least six books (Abelson and Sussman 1985, ...) present
    ;; unification algorithms with a common error. They all have
    ;; problems unifying `(?x ?y a)' with `(?y ?x ?x)'. Some of these
    ;; texts assume that `unify' will be called in a context where no
    ;; variables are shared between the two arguments. However, they
    ;; are still suspect to the bug, as the following example points
    ;; out:

    (reazon--should-equal `((,x . a) (,y . ,x) (,z ,x ,y a))
      (reazon--unify `(f (,x ,y a) (,y ,x ,x)) `(f ,z ,z) '()))

    ;; Despite this subtle bug, I highly recommend each of the books
    ;; to the reader. It is interesting to compare different
    ;; implementations of the same algorithm. It turns out that there
    ;; are more similarities than differences. This indicates two
    ;; things: (1) there is a generally agreed-upon style for writing
    ;; these functions, and (2) good programmers sometimes take
    ;; advantage of opportunities to look at others' code.

    ;; The question is: Can you give an informal proof of the
    ;; correctness of the algorithm presented in this chapter? Start
    ;; by making a clear statement of the specification. Apply that to
    ;; the other algorithms, and show where they go wrong. Then see if
    ;; you can prove that the unify function in this chapter is
    ;; correct. Failing a complete proof, can you at least prove that
    ;; the algorithm will always terminate?
    ))

(ert-deftest reazon-test-unit-primitives ()
  (reazon--should-equal '(())
    (reazon-!S '())
    (funcall (reazon-== 4 4) '()))
  (reazon--should-equal '()
    (reazon-!U '())
    (funcall (reazon-== 4 5) '())))

(ert-deftest reazon-test-unit-reify ()
  (reazon--with-variables (u v w x y z)
    (let ((a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
          (a2 `(,y . corn))
          (a3 `(,w .(,v ,u))))
      (reazon--should-equal `(_0 (_1 _0) corn _2 ((ice) _2))
        (funcall (reazon--reify x) `(,a1 ,a2 ,a3))))))

(ert-deftest reazon-test-unit-stream ()
  (let ((s1 '(a b c d))
        (s2 `(e f . (lambda () '(g h))))
        (s3 (lambda () '(i j k l))))
    (reazon--should-equal '(a b c d i j k l)
      (reazon--pull (reazon--append s3 s1)))
    (reazon--should-equal '(e f g h)
      (reazon--take nil s2))))

(ert-deftest reazon-test-unit-goal ()
  (reazon--with-variables (x)
    (let* ((g (reazon--disj-2 (reazon-== 'olive x) (reazon-== 'oil x)))
           (s (reazon--run-goal g))
           (l (reazon--take 5 s))
           (k (length l)))
      (reazon--should-equal k 2)
      (reazon--should-equal '(1 1)
        (mapcar #'length l))
      (reazon--should-equal '(olive oil)
        (mapcar (reazon--reify x) s))
      (reazon--should-equal l
        (reazon--take nil s)))))

(ert-deftest reazon-test-unit-impure ()
  (reazon--with-variables (x y)
    (reazon--should-equal `(((,y . 6)))
      (funcall (reazon--ifte #'reazon-!U
                 (reazon-== 5 y)
                 (reazon-== 6 y))
               '()))
    (reazon--should-equal `(((,y . 5) (,x . 6)))
      (funcall (reazon--ifte (reazon-== 6 x)
                 (reazon-== 5 y)
                 (reazon-== 6 y))
               '()))
    (reazon--should-equal `(((,y . 5) (,x . 6)) ((,y . 5) (,x . 5)))
      (funcall (reazon--ifte (reazon--disj-2
                              (reazon-== x 6)
                              (reazon-== x 5))
                 (reazon-== 5 y)
                 (reazon-== 6 y))
               '()))
    (reazon--should-equal `(((,y . 5) (,x . 6)))
      (funcall (reazon--ifte (reazon--once
                              (reazon--disj-2
                               (reazon-== x 6)
                               (reazon-== x 5)))
                 (reazon-== 5 y)
                 (reazon-== 6 y))
               '()))))

(defun reazon--test-unproductive ()
  "Produce nothing...forever."
  (lambda (s)
    (lambda ()
      (funcall (reazon--test-unproductive) s))))

(defun reazon--test-productive ()
  "Produce something...forever."
  (lambda (s)
    (lambda ()
      (funcall (reazon--disj-2 #'reazon-!S (reazon--test-productive)) s))))

(ert-deftest reazon-test-unit-productivity ()
  (reazon--with-variables (x)
    (let ((s (funcall (reazon--disj-2
                       (reazon-== 'olive x)
                       (reazon--test-unproductive))
                      nil)))
      (reazon--should-equal `((,x . olive))
        (car s)))
    (let ((s (funcall (reazon--disj-2
                       (reazon--test-unproductive)
                       (reazon-== 'olive x))
                      nil)))
      (reazon--should-equal `((,x . olive))
        (car (funcall s)))
      (reazon--should-equal `((,x . olive))
        (car (reazon--pull s))))
    (reazon--should-equal '(() () ())
      (reazon--take 3 (reazon--run-goal (reazon--test-productive))))))


(provide 'reazon-test-unit)
;;; reazon-test-unit.el ends here
