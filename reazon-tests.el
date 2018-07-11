;;; reazon--tests.el --- Tests for reazon             -*- lexical-binding: t; -*-

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

;;; Code:

(require 'reazon)
(require 'ert)

;; utilities

(defmacro reazon--with-variables (variables &rest body)
  "Evaluate BODY with VARIABLES as reazon--variables."
  (declare (indent 1))
  (let ((reazon--vars
         (mapcar (lambda (var)
                   `(,var (reazon--make-variable ',var)))
                 variables)))
    `(let (,@reazon--vars)
       ,@body)))

(defmacro reazon--should-equal(expected form)
  "Assert that FORM evaluates equal to EXPECTED."
  (declare (indent 1))
  `(should (equal ,expected ,form)))

(defmacro reazon--should-not (&rest forms)
  ""
  (let ((should-nots (mapcar (lambda (form)
                               `(reazon--should-equal ,form reazon--false))
                             forms)))
    `(progn ,@should-nots)))

;; variables

(ert-deftest reazon--variable-test ()
  (should (reazon--variable-p (reazon--make-variable 'x)))
  (should-not (reazon--variable-p 'x))
  (reazon--with-variables (x y z)
    (should (reazon--variable-p x))
    (should (reazon--variable-p y))
    (should (reazon--variable-p z))))

;; substitutions

(ert-deftest reazon--walk-test ()
  (reazon--with-variables (u v w x y z)
    (let ((sub-1 `((,z . a) (,x . ,w) (,y . ,z)))
          (sub-2 `((,x . b) (,z . ,y) (,w . (,x e ,z)) (,u . ,w))))
      (reazon--should-equal 'a (reazon--walk z sub-1))
      (reazon--should-equal 'a (reazon--walk y sub-1))
      (reazon--should-equal w (reazon--walk x sub-1))
      (reazon--should-equal w (reazon--walk w sub-1))
      (reazon--should-equal 'b (reazon--walk x sub-2))
      (reazon--should-equal `(,x e ,z) (reazon--walk u sub-2))
      (reazon--should-equal `(b e ,y) (reazon--walk* u sub-2)))))

(ert-deftest reazon--occurs-test ()
  (reazon--with-variables (x y)
    (should (reazon--occurs-p x x '()))
    (should (reazon--occurs-p x `(,y) `((,y . ,x))))))

(ert-deftest reazon--extend-test ()
  (reazon--with-variables (x y z)
    (reazon--should-not
     (reazon--extend x x '())
     (reazon--extend x `(,x) '())
     (reazon--extend x `(,y) `((,y . ,x))))
    (reazon--should-equal 'e
      (reazon--walk y (reazon--extend x 'e `((,z . ,x) (,y . ,z)))))))

;; unification

(ert-deftest reazon--unification-test ()
  (reazon--should-equal '(())
    (funcall (||| 4 4) '()))
  (reazon--should-equal '(())
    (!S '()))
  (reazon--should-equal '()
    (funcall (||| 4 5) '()))
  (reazon--should-equal '()
    (!U '())))

;; reification

(ert-deftest reazon--reify-test ()
  (reazon--with-variables (u v w x y z)
    (let ((a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
          (a2 `(,y . corn))
          (a3 `(,w .(,v ,u))))
      (reazon--should-equal `(_0 (_1 _0) corn _2 ((ice) _2))
        (funcall (reazon--reify x) `(,a1 ,a2 ,a3))))))

;; streams

(ert-deftest reazon--stream-test ()
  (let ((s1 '(a b c d))
        (s2 `(e f . (lambda () '(g h))))
        (s3 (lambda () '(i j k l))))
    (reazon--should-equal '(a b c d i j k l)
      (reazon--pull (reazon--append s3 s1)))
    (reazon--should-equal '(e f g h)
      (reazon--take nil s2))))

;; goals

(ert-deftest reazon--goal-test ()
  (reazon--with-variables (x)
    (let* ((g (reazon--disj-2 (||| 'olive x) (||| 'oil x)))
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
  ""
  (lambda (s)
    (lambda ()
      (funcall (reazon--test-unproductive) s))))

(defun reazon--test-productive ()
  ""
  (lambda (s)
    (lambda ()
      (funcall (reazon--disj-2 #'!S (reazon--test-productive)) s))))

(ert-deftest reazon--productivity-test ()
  (reazon--with-variables (x)
    (let ((s (funcall (reazon--disj-2
                       (||| 'olive x)
                       (reazon--test-unproductive))
                      nil)))
      (reazon--should-equal `((,x . olive))
        (car s)))
    (let ((s (funcall (reazon--disj-2
                       (reazon--test-unproductive)
                       (||| 'olive x))
                      nil)))
      (reazon--should-equal `((,x . olive))
        (car (funcall s)))
      (reazon--should-equal `((,x . olive))
        (car (reazon--pull s))))
    (reazon--should-equal '(() () ())
      (reazon--take 3 (reazon--run-goal (reazon--test-productive))))))

;; macros

(ert-deftest reazon--test-run-basic ()
  (reazon--should-equal '()
    (reazon-run* q #'!U))
  (reazon--should-equal '(t)
    (reazon-run* q (||| t q)))
  (reazon--should-equal '()
    (reazon-run* q #'!U (||| t q)))
  (reazon--should-equal '(t)
    (reazon-run* q #'!S (||| t q)))
  (reazon--should-equal '(corn)
    (reazon-run* r #'!S (||| 'corn r)))
  (reazon--should-equal '(olive oil)
    (reazon-run* x (reazon-disj (||| 'olive x) (||| 'oil x))))
  (reazon--should-equal '(oil olive)
    (reazon-run* x (reazon-disj (||| 'oil x) (||| 'olive x))))
  (reazon--should-equal '(oil)
    (reazon-run* x (reazon-disj (reazon--conj-2 (||| 'olive x) #'!U) (||| 'oil x))))
  (reazon--should-equal '(olive _0 oil)
    (reazon-run* x (reazon-disj (reazon-conj (||| 'virgin x) #'!U) (reazon-disj (||| 'olive x) (reazon-disj #'!S(||| 'oil x)))))))

(ert-deftest reazon--test-fresh ()
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-fresh (x)
        (||| t x)
        (||| t q))))
  (reazon--should-equal '((_0 _1))
    (reazon-run* s
      (reazon-fresh (x)
        (reazon-fresh (y)
          (||| `(,x ,y) s)))))
  (reazon--should-equal '((_0 _1 _0))
    (reazon-run* s
      (reazon-fresh (x y)
        (||| `(,x ,y ,x) s))))
  (reazon--should-equal '((split pea))
    (reazon-run* r
      (reazon-fresh (x)
        (reazon-fresh (y)
          (||| 'split x)
          (||| 'pea y)
          (||| `(,x ,y) r)))))
  (reazon--should-equal '((split pea))
    (reazon-run* r
      (reazon-fresh (x)
        (reazon-fresh (y)
          (||| 'split x)
          (||| 'pea y)
          (||| `(,x ,y) r)))))
  (reazon--should-equal '((split pea))
    (reazon-run* r
      (reazon-fresh (x y)
        (||| 'split x)
        (||| 'pea y)
        (||| `(,x ,y) r))))
  (reazon--should-equal '((split pea))
    (reazon-run* (x y)
      (||| 'split x)
      (||| 'pea y)))
  (reazon--should-equal '(((split pea) split pea))
    (reazon-run* (r x y)
      (||| 'split x)
      (||| 'pea y)
      (||| `(,x ,y) r)))
  (reazon--should-equal '((_0 _1) (_0 _1))
    (reazon-run* (x y)
      (reazon-fresh (z)
        (reazon-conde
         ((||| x z) (reazon-fresh (z) (||| y z)))
         ((reazon-fresh (z) (||| x z)) (||| y z))))))
  (reazon--should-equal '((nil _0) (_0 nil))
    (reazon-run* (x y)
      (reazon-fresh (z)
        (reazon-conde
         ((||| x z) (reazon-fresh (z) (||| y z)))
         ((reazon-fresh (z) (||| x z)) (||| y z)))
        (||| nil z)))))

(reazon-defrel reazon--test-teacup-o (x)
  (reazon-disj (||| x 'tea) (||| x 'cup)))

(ert-deftest reazon--test-defrel ()
  (reazon--should-equal '(tea cup)
    (reazon-run* x (reazon--test-teacup-o x)))
  (reazon--should-equal '((nil t) (tea t) (cup t))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon--test-teacup-o x) (||| y t))
       ((||| x nil) (||| y t)))))
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
       (reazon--conj-2 (||| nil x) (reazon--test-teacup-o y)))))
  (reazon--should-equal '((t tea) (t cup) (tea _0) (cup _0))
    (reazon-run* (x y)
      (reazon-conde
       ((reazon--test-teacup-o x) (reazon--test-teacup-o x))
       ((||| x t) (reazon--test-teacup-o y))))))

(ert-deftest reazon--test-conde ()
  (reazon--should-equal '((split pea) (navy bean) (red lentil))
    (reazon-run* (x y)
      (reazon-conde
       ((||| x 'split) (||| y 'pea))
       ((||| x 'navy) (||| y 'bean))
       ((||| x 'red) (||| y 'lentil)))))
  (reazon--should-equal '(oil)
    (reazon-run* x
      (reazon-conde
       ((||| x 'olive) #'!U)
       ((||| x 'oil))))))

(ert-deftest reazon--test-car-o ()
  (reazon--should-equal '(a)
    (reazon-run* p
      (reazon-car-o '(a c o r n) p)))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-car-o '(a c o r n) 'a)
      (||| q t)))
  (reazon--should-equal '(pear)
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-car-o `(,r ,y) x)
        (||| x 'pear))))
  (reazon--should-equal '((grape a))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-fresh (d) (||| (cons x d) '(grape raisin pear)))
        (reazon-fresh (d) (||| (cons y d) '((a) (b) (c))))
        (||| r (cons x y)))))
  (reazon--should-equal '((grape a))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-car-o '(grape raisin pear) x)
        (reazon-car-o '((a) (b) (c)) y)
        (||| r (cons x y))))))

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
        (||| r (cons x y)))))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-cdr-o '(a c o r n) '(c o r n))
      (||| q t)))
  (reazon--should-equal '(o)
    (reazon-run* x
      (reazon-cdr-o '(c o r n) `(,x r n))))
  (reazon--should-equal '((a c o r n))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-cdr-o l '(c o r n))
        (reazon-car-o l x)
        (||| x 'a)))))

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
        (||| r `(e a d ,x))
        (reazon-cons-o y `(a ,z c) r))))
  (reazon--should-equal '((d a d c))
    (reazon-run* l
      (reazon-fresh (x)
        (||| l `(d a ,x c))
        (reazon-cons-o x `(a ,x c) l))))
  (reazon--should-equal '((d a d c))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-cons-o x `(a ,x c) l)
        (||| l `(d a ,x c)))))
  (reazon--should-equal '((b o n u s))
    (reazon-run* l
      (reazon-fresh (d p x y w)
        (reazon-cons-o w '(n u s) p)
        (reazon-cdr-o l p)
        (reazon-car-o l x)
        (||| x 'b)
        (reazon-cdr-o l d)
        (reazon-car-o d y)
        (||| y 'o)))))

(ert-deftest reazon--test-null-o ()
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-null-o '(grape raisin pear))
      (||| q t)))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-null-o '())
      (||| q t)))
  (reazon--should-equal '(())
    (reazon-run* x
      (reazon-null-o x))))

(ert-deftest reazon--test-pair-o ()
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-pair-o (cons q q))
      (||| q t)))
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-pair-o '())
      (||| q t)))
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-pair-o 'pair)
      (||| q t)))
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
      (reazon-append-o x y '(cake with ice d t))))
  (reazon--should-equal '((() (cake with ice d t))
                   ((cake) (with ice d t))
                   ((cake with) (ice d t))
                   ((cake with ice) (d t))
                   ((cake with ice d) (t))
                   ((cake with ice d t) ()))
    (reazon-run* (x y)
      (reazon-append-o x y '(cake with ice d t)))))

(provide 'reazon--tests)
;;; reazon--tests.el ends here
