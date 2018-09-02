;;; reazon-test-trs.el ---                           -*- lexical-binding: t; -*-

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

;; Tests for some of the relations from The Reasoned Schemer (TRS).
;; These aren't generally useful enough to be included as builtin
;; Reazon relations.

;;; Code:

(require 'reazon-test-utils)

(reazon-defrel reazon--test-lolo (s)
  (reazon-conde
   ((reazon-nullo s))
   ((reazon-fresh (a)
      (reazon-caro s a)
      (reazon-listo a))
    (reazon-fresh (d)
      (reazon-cdro s d)
      (reazon--test-lolo d)))))

(ert-deftest reazon-test-trs-lolo ()
  (reazon--should-equal '(nil (nil) ((_0)) (nil nil) ((_0 _1)))
    (reazon-run 5 q
      (reazon--test-lolo q))
    (reazon-run 5 q
      (reazon--test-lolo `((a b) (c d) . ,q)))))

(reazon-defrel reazon--test-twino (s)
  (reazon-fresh (x)
    (reazon-== s `(,x ,x))))

(ert-deftest reazon-test-trs-twino ()
  (reazon--should-equal '(tofu)
    (reazon-run* q (reazon--test-twino `(,q tofu)))))

(reazon-defrel reazon--test-loto (s)
  (reazon-conde
   ((reazon-nullo s))
   ((reazon-fresh (a)
      (reazon-caro s a)
      (reazon--test-twino a))
    (reazon-fresh (d)
      (reazon-cdro s d)
      (reazon--test-loto d)))))

(ert-deftest reazon-test-trs-loto ()
  (reazon--should-equal '(nil ((_0 _0)) ((_0 _0) (_1 _1)))
    (reazon-run 3 q
      (reazon--test-loto q))
    (reazon-run 3 q
      (reazon--test-loto `((g g) . ,q)))))

(reazon-defrel reazon--test-proper-membero (x s)
  (reazon-conde
   ((reazon-caro s x)
    (reazon-fresh (d)
      (reazon-cdro s d)
      (reazon-listo d)))
   ((reazon-fresh (d)
      (reazon-cdro s d)
      (reazon--test-proper-membero x d)))))

(ert-deftest reazon-test-trs-proper-membero ()
  ;; The ordering here is different from what's in the book.
  (reazon--should-equal '((tofu) (tofu _0) (_0 tofu) (tofu _0 _1)
                          (tofu _0 _1 _2) (_0 tofu _1)
                          (tofu _0 _1 _2 _3)
                          (tofu _0 _1 _2 _3 _4)
                          (_0 _1 tofu) (_0 tofu _1 _2)
                          (tofu _0 _1 _2 _3 _4 _5)
                          (tofu _0 _1 _2 _3 _4 _5 _6))
    (reazon-run 12 s
      (reazon--test-proper-membero 'tofu s))))

(reazon-defrel reazon--test-memo (x s out)
  (reazon-conde
   ((reazon-caro s x) (reazon-== out s))
   ((reazon-fresh (d)
      (reazon-cdro s d)
      (reazon--test-memo x d out)))))

(ert-deftest reazon-test-trs-memo ()
  (reazon--should-equal '((tofu d tofu e) (tofu e))
    (reazon-run* out
      (reazon-fresh (x)
        (reazon--test-memo 'tofu `(a b ,x d tofu e) out))))
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
      (reazon--test-memo 'tofu `(a b tofu d tofu e . ,y) x))))

(reazon-defrel reazon--test-rembero (x s out)
  (reazon-conde
   ((reazon-nullo s) (reazon-nullo out))
   ((reazon-caro s x) (reazon-cdro s out))
   ((reazon-fresh (a d rec)
      (reazon-conso a d s)
      (reazon-conso a rec out)
      (reazon--test-rembero x d rec)))))

(ert-deftest reazon-test-trs-rembero ()
  ;; These tests confirm some behavior that seems pathological. If all
  ;; the other tests pass and these ones don't, that might be good.
  (reazon--should-equal '((a b d peas e))
    (reazon-run 1 out
      (reazon-fresh (y)
        (reazon--test-rembero 'peas `(a b ,y d peas e) out))))
  (reazon--should-equal '((b a d _0 e) (a b d _0 e) (a b d _0 e)
                          (a b d _0 e) (a b _0 d e)
                          (a b e d _0) (a b _0 d _1 e))
    (reazon-run* out
      (reazon-fresh (y z)
        (reazon--test-rembero y `(a b ,y d ,z e) out))))
  (reazon--should-equal '(_0 _0 _0 _0 _0
                             nil (_0 . _1) (_0)
                             (_0 _1 . _2) (_0 _1)
                             (_0 _1 _2 . _3))
    (reazon-run 11 w
      (reazon-fresh (y z out)
        (reazon--test-rembero y `(a b ,y d ,z . ,w) out))))
  (reazon--should-equal '((peas a peas c) (a peas peas c) (a peas peas c)
                          (a peas c) (a peas c peas))
    (reazon-run* q
      (reazon--test-rembero 'peas q `(a peas c))))
  (reazon--should-equal '(b)
    (reazon-run* r
      (reazon--test-rembero r '(a b c) '(a b c))
      (reazon-== r 'b))
    (reazon-run* r
      (reazon-== r 'b)
      (reazon--test-rembero r '(a b c) '(a b c)))))


(provide 'reazon-test-trs)
;;; reazon-test-trs.el ends here
