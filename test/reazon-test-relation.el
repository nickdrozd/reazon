;;; reazon-test-relations.el ---                     -*- lexical-binding: t; -*-

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

;; Tests for builtin Reazon relations. Many of them come from TRS, and
;; it's probably obvious which ones.

;;; Code:

(require 'reazon-test-utils)

(ert-deftest reazon-test-relation-caro ()
  (reazon--should-equal '(a)
    (reazon-run* p
      (reazon-caro '(a c o r n) p)))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-caro '(a c o r n) 'a)
      (reazon-== q t)))
  (reazon--should-equal '(pear)
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-caro `(,r ,y) x)
        (reazon-== x 'pear))))
  (reazon--should-equal '((grape a))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-fresh (d) (reazon-== (cons x d) '(grape raisin pear)))
        (reazon-fresh (d) (reazon-== (cons y d) '((a) (b) (c))))
        (reazon-== r (cons x y))))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-caro '(grape raisin pear) x)
        (reazon-caro '((a) (b) (c)) y)
        (reazon-== r (cons x y))))))

(ert-deftest reazon-test-relation-cdro ()
  (reazon--should-equal '(c)
    (reazon-run* r
      (reazon-fresh (v)
        (reazon-cdro '(a c o r n) v)
        (reazon-caro v r))))
  (reazon--should-equal '(((raisin pear) a))
    (reazon-run* r
      (reazon-fresh (x y)
        (reazon-cdro '(grape raisin pear) x)
        (reazon-caro '((a) (b) (c)) y)
        (reazon-== r (cons x y)))))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-cdro '(a c o r n) '(c o r n))
      (reazon-== q t)))
  (reazon--should-equal '(o)
    (reazon-run* x
      (reazon-cdro '(c o r n) `(,x r n))))
  (reazon--should-equal '((a c o r n))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-cdro l '(c o r n))
        (reazon-caro l x)
        (reazon-== x 'a)))))

(ert-deftest reazon-test-relation-conso ()
  (reazon--should-equal '(((a b c) d e f))
    (reazon-run* l
      (reazon-conso '(a b c) '(d e f) l)))
  (reazon--should-equal '(d)
    (reazon-run* x
      (reazon-conso x '(a b c) '(d a b c))))
  (reazon--should-equal '((e a d c ))
    (reazon-run* r
      (reazon-fresh (x y z)
        (reazon-== r `(e a d ,x))
        (reazon-conso y `(a ,z c) r))))
  (reazon--should-equal '((d a d c))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-== l `(d a ,x c))
        (reazon-conso x `(a ,x c) l)))
    (reazon-run* l
      (reazon-fresh (x)
        (reazon-conso x `(a ,x c) l)
        (reazon-== l `(d a ,x c)))))
  (reazon--should-equal '((b o n u s))
    (reazon-run* l
      (reazon-fresh (d p x y w)
        (reazon-conso w '(n u s) p)
        (reazon-cdro l p)
        (reazon-caro l x)
        (reazon-== x 'b)
        (reazon-cdro l d)
        (reazon-caro d y)
        (reazon-== y 'o)))))

(ert-deftest reazon-test-relation-nullo ()
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-nullo '(grape raisin pear))
      (reazon-== q t)))
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-nullo '())
      (reazon-== q t)))
  (reazon--should-equal '(())
    (reazon-run* x
      (reazon-nullo x))))

(ert-deftest reazon-test-relation-pairo ()
  (reazon--should-equal '(t)
    (reazon-run* q
      (reazon-pairo (cons q q))
      (reazon-== q t)))
  (reazon--should-equal '()
    (reazon-run* q
      (reazon-pairo '())
      (reazon-== q t))
    (reazon-run* q
      (reazon-pairo 'pair)
      (reazon-== q t)))
  (reazon--should-equal '((_0 . _1))
    (reazon-run* x
      (reazon-pairo x))))

(ert-deftest reazon-test-relation-appendo ()
  (reazon--should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reazon-run 5 x
      (reazon-fresh (y z)
        (reazon-appendo x y z))))
  (reazon--should-equal '(_0 _0 _0 _0 _0)
    (reazon-run 5 y
      (reazon-fresh (x z)
        (reazon-appendo x y z))))
  (reazon--should-equal '(_0 (_0 . _1) (_0 _1 . _2) (_0 _1 _2 . _3) (_0 _1 _2 _3 . _4))
    (reazon-run 5 z
      (reazon-fresh (x y)
        (reazon-appendo x y z))))
  (reazon--should-equal '((cake tastes yummy))
    (reazon-run* x
      (reazon-appendo
       '(cake)
       '(tastes yummy)
       x)))
  (reazon--should-equal '((cake with ice _0 tastes yummy))
    (reazon-run* x
      (reazon-fresh (y)
        (reazon-appendo
         `(cake with ice ,y)
         '(tastes yummy)
         x))))
  (reazon--should-equal '((cake with ice cream . _0))
    (reazon-run* x
      (reazon-fresh (y)
        (reazon-appendo
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
        (reazon-appendo `(cake with ice . ,y) '(d t) x))))
  (reazon--should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reazon-run 5 y
      (reazon-fresh (x)
        (reazon-appendo `(cake with ice . ,y) '(d t) x))))
  (reazon--should-equal '((() (cake with ice d t))
                          ((cake) (with ice d t))
                          ((cake with) (ice d t))
                          ((cake with ice) (d t))
                          ((cake with ice d) (t))
                          ((cake with ice d t) ()))
    (reazon-run 6 (x y)
      (reazon-appendo x y '(cake with ice d t)))
    (reazon-run* (x y)
      (reazon-appendo x y '(cake with ice d t)))))

(ert-deftest reazon-test-relations-assqo ()
  ;; Verify that assqo matches assq
  (let ((results
         (reazon-run 100 (x s out)
           (reazon-assqo x s out))))
    (dolist (res results)
      (pcase res
        (`(,x ,s ,out)
         (reazon--should-equal out
           (assq x s)))))))

(ert-deftest reazon-test-relation-listo ()
  (reazon--should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reazon-run 5 q
      (reazon-listo q))
    (reazon-run 5 q
      (reazon-listo `(a b c . ,q)))))

(ert-deftest reazon-test-relation-membero ()
  (reazon--should-equal '(e)
    (reazon-run* x
      (reazon-membero 'e `(pasta ,x fagioli))))
  (reazon--should-equal '((e _0) (_0 e))
    (reazon-run* (x y)
      (reazon-membero 'e `(pasta ,x fagioli ,y))))
  (reazon--should-equal '((pasta e fagioli _0) (pasta _0 fagioli e))
    (reazon-run* q
      (reazon-fresh (x y)
        (reazon-== q `(pasta ,x fagioli ,y))
        (reazon-membero 'e q))))
  (reazon--should-equal '((tofu . _0) (_0 tofu . _1) (_0 _1 tofu . _2))
    (reazon-run 3 s
      (reazon-membero 'tofu s))))

(ert-deftest reazon-test-relation-precedeso ()
  "Tests for precedeso, adjacento, and immediately-precedeso."
  (let ((s '(1 2 3 4 5)))
    (reazon--should-equal '(_0)
      (reazon-run* q
        (reazon-immediately-precedeso 1 2 s)
        (reazon-immediately-precedeso 2 3 s)
        (reazon-immediately-precedeso 3 4 s)
        (reazon-immediately-precedeso 4 5 s)
        (reazon-precedeso 1 2 s)
        (reazon-precedeso 1 3 s)
        (reazon-precedeso 1 4 s)
        (reazon-precedeso 1 5 s)
        (reazon-precedeso 2 3 s)
        (reazon-precedeso 2 4 s)
        (reazon-precedeso 2 5 s)
        (reazon-adjacento 1 2 s)
        (reazon-adjacento 2 1 s)
        (reazon-adjacento 2 3 s)
        (reazon-adjacento 3 2 s)
        (reazon-adjacento 3 4 s)
        (reazon-adjacento 4 3 s)
        (reazon-adjacento 4 5 s)
        (reazon-adjacento 5 4 s)))
    (reazon--should-equal '()
      (reazon-run* q (reazon-immediately-precedeso 1 3 s))
      (reazon-run* q (reazon-immediately-precedeso 1 4 s))
      (reazon-run* q (reazon-immediately-precedeso 1 5 s))
      (reazon-run* q (reazon-immediately-precedeso 3 2 s))
      (reazon-run* q (reazon-precedeso 3 2 s))
      (reazon-run* q (reazon-precedeso 4 2 s))
      (reazon-run* q (reazon-precedeso 5 2 s))
      (reazon-run* q (reazon-adjacento 1 4 s))
      (reazon-run* q (reazon-adjacento 5 3 s)))))


(provide 'reazon-test-relations)
;;; reazon-test-relations.el ends here
