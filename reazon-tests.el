;;; reazon-tests.el --- Tests for reazon             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Drozd

;; Author: Nick Drozd <nicholasdrozd@gmail.com>

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

;; Tests that use reazon-run(*) are the ultimate arbiter of truth.
;; Call such tests "integration tests" and others "unit tests". Unit
;; tests can be rewritten freely as the implementation changes, but
;; integration tests should not be altered unless the API is
;; deliberately being changed (or, obviously, if bugs are fixed).

;;; Code:

(require 'reazon)
(require 'ert)

;; Utilities

(defmacro reazon--with-variables (variables &rest body)
  "Evaluate BODY with VARIABLES as reazon--variables."
  (declare (indent 1))
  (let ((reazon--vars
         (mapcar (lambda (var)
                   `(,var (reazon--make-variable ',var)))
                 variables)))
    `(let (,@reazon--vars)
       ,@body)))

(defmacro reazon--should-equal(expected &rest forms)
  "Assert that each form in FORMS equals EXPECTED."
  (declare (indent 1))
  (let ((assertions
         (mapcar
          (lambda (form) `(should (equal ,expected ,form)))
          forms)))
    `(progn ,@assertions)))

;; Internal functions

(ert-deftest reazon--variable-test ()
  (should (reazon--variable-p (reazon--make-variable 'x)))
  (should-not (reazon--variable-p 'x))
  (reazon--with-variables (x y z)
    (should (reazon--variable-p x))
    (should (reazon--variable-p y))
    (should (reazon--variable-p z))))

(ert-deftest reazon--walk-test ()
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

(ert-deftest reazon--occurs-test ()
  (reazon--with-variables (x y)
    (should (reazon--occurs-p x x '()))
    (should (reazon--occurs-p x `(,y) `((,y . ,x))))))

(ert-deftest reazon--extend-test ()
  (reazon--with-variables (x y z)
    (reazon--should-equal reazon--false
      (reazon--extend x x '())
      (reazon--extend x `(,x) '())
      (reazon--extend x `(,y) `((,y . ,x))))
    (reazon--should-equal 'e
      (reazon--walk y (reazon--extend x 'e `((,z . ,x) (,y . ,z)))))))

(ert-deftest reazon--unification-test ()
  (reazon--should-equal '(())
    (reazon-!S '())
    (funcall (reazon-== 4 4) '()))
  (reazon--should-equal '()
    (reazon-!U '())
    (funcall (reazon-== 4 5) '())))

(ert-deftest reazon--reify-test ()
  (reazon--with-variables (u v w x y z)
    (let ((a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
          (a2 `(,y . corn))
          (a3 `(,w .(,v ,u))))
      (reazon--should-equal `(_0 (_1 _0) corn _2 ((ice) _2))
        (funcall (reazon--reify x) `(,a1 ,a2 ,a3))))))

(ert-deftest reazon--stream-test ()
  (let ((s1 '(a b c d))
        (s2 `(e f . (lambda () '(g h))))
        (s3 (lambda () '(i j k l))))
    (reazon--should-equal '(a b c d i j k l)
      (reazon--pull (reazon--append s3 s1)))
    (reazon--should-equal '(e f g h)
      (reazon--take nil s2))))

(ert-deftest reazon--goal-test ()
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

(ert-deftest reazon--productivity-test ()
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

;; Macros

(ert-deftest reazon--test-run-basic ()
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

(ert-deftest reazon--test-fresh ()
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

(ert-deftest reazon--test-project ()
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

(reazon-defrel reazon--test-teacup-o (x)
  (reazon-disj (reazon-== x 'tea) (reazon-== x 'cup)))

(ert-deftest reazon--test-defrel ()
  (reazon--should-equal '(tea cup)
    (reazon-run* x (reazon--test-teacup-o x)))
  (reazon--should-equal '((nil t) (tea t) (cup t))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon--test-teacup-o x) (reazon-== y t))
       ((reazon-== x nil) (reazon-== y t)))))
  (reazon--should-equal '((tea tea) (tea cup) (cup tea) (cup cup))
    (reazon-run* (x y)
      (reazon--test-teacup-o x)
      (reazon--test-teacup-o y)))
  (reazon--should-equal '(tea cup)
    (reazon-run* x
      (reazon--test-teacup-o x)
      (reazon--test-teacup-o x)))
  (reazon--should-equal '((nil tea) (nil cup) (tea _0) (cup _0))
    (reazon-run* (x y)
      (reazon--disj-2
       (reazon--conj-2 (reazon--test-teacup-o x) (reazon--test-teacup-o x))
       (reazon--conj-2 (reazon-== nil x) (reazon--test-teacup-o y)))))
  (reazon--should-equal '((t tea) (t cup) (tea _0) (cup _0))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon--test-teacup-o x) (reazon--test-teacup-o x))
       ((reazon-== x t) (reazon--test-teacup-o y))))))

(ert-deftest reazon--test-conde ()
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

(ert-deftest reazon--test-car-o ()
  (reazon--should-equal '(a)
    (reazon-run* p
      (reazon-car-o '(a c o r n) p)))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-car-o '(a c o r n) 'a)
      (reazon-== q t)))
  (reazon--should-equal '(pear)
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-car-o `(,r ,y) x)
        (reazon-== x 'pear))))
  (reazon--should-equal '((grape a))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-fresh (d) (reazon-== (cons x d) '(grape raisin pear)))
        (reazon-fresh (d) (reazon-== (cons y d) '((a) (b) (c))))
        (reazon-== r (cons x y))))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-car-o '(grape raisin pear) x)
        (reazon-car-o '((a) (b) (c)) y)
        (reazon-== r (cons x y))))))

(ert-deftest reazon--test-cdr-o ()
  (reazon--should-equal '(c)
    (reazon-run* r
      (reazon-fresh (v)
        (reazon-cdr-o '(a c o r n) v)
        (reazon-car-o v r))))
  (reazon--should-equal '(((raisin pear) a))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-cdr-o '(grape raisin pear) x)
        (reazon-car-o '((a) (b) (c)) y)
        (reazon-== r (cons x y)))))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-cdr-o '(a c o r n) '(c o r n))
      (reazon-== q t)))
  (reazon--should-equal '(o)
    (reazon-run* x
      (reazon-cdr-o '(c o r n) `(,x r n))))
  (reazon--should-equal '((a c o r n))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-cdr-o l '(c o r n))
        (reazon-car-o l x)
        (reazon-== x 'a)))))

(ert-deftest reazon--test-cons-o ()
  (reazon--should-equal '(((a b c) d e f))
    (reazon-run* l
      (reazon-cons-o '(a b c) '(d e f) l)))
  (reazon--should-equal '(d)
    (reazon-run* x
      (reazon-cons-o x '(a b c) '(d a b c))))
  (reazon--should-equal '((e a d c ))
    (reazon-run* r
      (reazon-fresh (x y z)
        (reazon-== r `(e a d ,x))
        (reazon-cons-o y `(a ,z c) r))))
  (reazon--should-equal '((d a d c))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-== l `(d a ,x c))
        (reazon-cons-o x `(a ,x c) l)))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-cons-o x `(a ,x c) l)
        (reazon-== l `(d a ,x c)))))
  (reazon--should-equal '((b o n u s))
    (reazon-run* l
      (reazon-fresh (d p x y w)
        (reazon-cons-o w '(n u s) p)
        (reazon-cdr-o l p)
        (reazon-car-o l x)
        (reazon-== x 'b)
        (reazon-cdr-o l d)
        (reazon-car-o d y)
        (reazon-== y 'o)))))

(ert-deftest reazon--test-null-o ()
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-null-o '(grape raisin pear))
      (reazon-== q t)))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-null-o '())
      (reazon-== q t)))
  (reazon--should-equal '(())
    (reazon-run* x
      (reazon-null-o x))))

(ert-deftest reazon--test-pair-o ()
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-pair-o (cons q q))
      (reazon-== q t)))
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-pair-o '())
      (reazon-== q t))
    (reazon-run* q
      (reazon-pair-o 'pair)
      (reazon-== q t)))
  (reazon--should-equal '((_0 . _1))
    (reazon-run* x
      (reazon-pair-o x))))

(ert-deftest reazon--test-append-o ()
  (reazon--should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reazon-run 5 x
      (reazon-fresh (y z)
        (reazon-append-o x y z))))
  (reazon--should-equal '(_0 _0 _0 _0 _0)
    (reazon-run 5 y
      (reazon-fresh (x z)
        (reazon-append-o x y z))))
  (reazon--should-equal '(_0 (_0 . _1) (_0 _1 . _2) (_0 _1 _2 . _3) (_0 _1 _2 _3 . _4))
    (reazon-run 5 z
      (reazon-fresh (x y)
        (reazon-append-o x y z))))
  (reazon--should-equal '((cake tastes yummy))
    (reazon-run* x
      (reazon-append-o
       '(cake)
       '(tastes yummy)
       x)))
  (reazon--should-equal '((cake with ice _0 tastes yummy))
    (reazon-run* x
      (reazon-fresh (y)
        (reazon-append-o
         `(cake with ice ,y)
         '(tastes yummy)
         x))))
  (reazon--should-equal '((cake with ice cream . _0))
    (reazon-run* x
      (reazon-fresh (y)
        (reazon-append-o
         '(cake with ice cream)
         y
         x))))
  (reazon--should-equal '((cake with ice d t)
                   (cake with ice _0 d t)
                   (cake with ice _0 _1 d t)
                   (cake with ice _0 _1 _2 d t)
                   (cake with ice _0 _1 _2 _3 d t))
    (reazon-run 5 x
      (reazon-fresh (y)
        (reazon-append-o `(cake with ice . ,y) '(d t) x))))
  (reazon--should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reazon-run 5 y
      (reazon-fresh (x)
        (reazon-append-o `(cake with ice . ,y) '(d t) x))))
  (reazon--should-equal '((() (cake with ice d t))
                   ((cake) (with ice d t))
                   ((cake with) (ice d t))
                   ((cake with ice) (d t))
                   ((cake with ice d) (t))
                   ((cake with ice d t) ()))
    (reazon-run 6 (x y)
      (reazon-append-o x y '(cake with ice d t)))
    (reazon-run* (x y)
      (reazon-append-o x y '(cake with ice d t)))))

(ert-deftest reazon--test-list-o ()
  (reazon--should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reazon-run 5 q
      (reazon-list-o q))
    (reazon-run 5 q
      (reazon-list-o `(a b c . ,q)))))

(ert-deftest reazon--test-lol-o ()
  (reazon--should-equal '(nil (nil) ((_0)) (nil nil) ((_0 _1)))
    (reazon-run 5 q
      (reazon-lol-o q))
    (reazon-run 5 q
      (reazon-lol-o `((a b) (c d) . ,q)))))

(ert-deftest reazon--test-twin-o ()
  (reazon--should-equal '(tofu)
    (reazon-run* q (reazon-twin-o `(,q tofu)))))

(ert-deftest reazon--test-lot-o ()
  (reazon--should-equal '(nil ((_0 _0)) ((_0 _0) (_1 _1)))
    (reazon-run 3 q
      (reazon-lot-o q))
    (reazon-run 3 q
      (reazon-lot-o `((g g) . ,q)))))

(ert-deftest reazon--test-member-o ()
  (reazon--should-equal '(e)
    (reazon-run* x
      (reazon-member-o 'e `(pasta ,x fagioli))))
  (reazon--should-equal '((e _0) (_0 e))
    (reazon-run* (x y)
      (reazon-member-o 'e `(pasta ,x fagioli ,y))))
  (reazon--should-equal '((pasta e fagioli _0) (pasta _0 fagioli e))
    (reazon-run* q
      (reazon-fresh (x y)
        (reazon-== q `(pasta ,x fagioli ,y))
        (reazon-member-o 'e q))))
  (reazon--should-equal '((tofu . _0) (_0 tofu . _1) (_0 _1 tofu . _2))
    (reazon-run 3 s
      (reazon-member-o 'tofu s))))

(ert-deftest reazon--test-proper-member-o ()
  ;; The ordering here is different from what's in the book.
  (reazon--should-equal '((tofu) (tofu _0) (_0 tofu) (tofu _0 _1)
                          (tofu _0 _1 _2) (_0 tofu _1)
                          (tofu _0 _1 _2 _3)
                          (tofu _0 _1 _2 _3 _4)
                          (_0 _1 tofu) (_0 tofu _1 _2)
                          (tofu _0 _1 _2 _3 _4 _5)
                          (tofu _0 _1 _2 _3 _4 _5 _6))
    (reazon-run 12 s
      (reazon-proper-member-o 'tofu s))))

(ert-deftest reazon--test-mem-o ()
  (reazon--should-equal '((tofu d tofu e) (tofu e))
    (reazon-run* out
      (reazon-fresh (x)
        (reazon-mem-o 'tofu `(a b ,x d tofu e) out))))
  (reazon--should-equal '(((tofu d tofu e . _0) _0)
                          ((tofu e . _0) _0)
                          ((tofu . _0) (tofu . _0))
                          ((tofu . _0) (_1 tofu . _0))
                          ((tofu . _0) (_1 _2 tofu . _0))
                          ((tofu . _0) (_1 _2 _3 tofu . _0))
                          ((tofu . _0) (_1 _2 _3 _4 tofu . _0))
                          ((tofu . _0) (_1 _2 _3 _4 _5 tofu . _0))
                          ((tofu . _0) (_1 _2 _3 _4 _5 _6 tofu . _0)))
    (reazon-run 9 (x y)
      (reazon-mem-o 'tofu `(a b tofu d tofu e . ,y) x))))


(provide 'reazon-tests)
;;; reazon-tests.el ends here
