;;; reason.el --- miniKanren implementation for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Drozd

;; Author: Nick Drozd <nicholasdrozd@gmail.com>
;; Keywords: languages, extensions, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; variables

(defun reason-make-variable (name)
  "Create a unique variable from NAME."
  (vector name))

(defun reason-variable-p (var)
  "Check whether VAR is a variable."
  (vector-or-char-table-p var))

(defmacro reason-with-variables (variables &rest body)
  "Evaluate BODY with VARIABLES as reason-variables."
  (declare (indent 1))
  (let ((reason-vars
         (mapcar (lambda (var)
                   `(,var (reason-make-variable ',var)))
                 variables)))
    `(let (,@reason-vars)
       ,@body)))

(ert-deftest reason-variable-test ()
  (should (reason-variable-p (reason-make-variable 'x)))
  (should-not (reason-variable-p 'x))
  (reason-with-variables (x y z)
    (should (reason-variable-p x))
    (should (reason-variable-p y))
    (should (reason-variable-p z))))

;; substitutions

(defun reason-walk (variable substitution)
  "Return the value associated with VARIABLE in
SUBSTITUTION if there is one, else VARIABLE."
  (let ((association (and (reason-variable-p variable)
                          (assoc variable substitution))))
    (cond
     ((consp association)
      (reason-walk (cdr association) substitution))
     (t variable))))

(defun reason-walk* (v s)
  ""
  (let ((v (reason-walk v s)))
    (cond
     ((reason-variable-p v)
      v)
     ((consp v)
      (cons
       (reason-walk* (car v) s)
       (reason-walk* (cdr v) s)))
     (t
      v))))

(defmacro reason-should-equal(expected form)
  "Assert that FORM evaluates equal to EXPECTED."
  (declare (indent 1))
  `(should (equal ,expected ,form)))

(ert-deftest reason-walk-test ()
  (reason-with-variables (u v w x y z)
    (let ((sub-1 `((,z . a) (,x . ,w) (,y . ,z)))
          (sub-2 `((,x . b) (,z . ,y) (,w . (,x e ,z)) (,u . ,w))))
      (reason-should-equal 'a (reason-walk z sub-1))
      (reason-should-equal 'a (reason-walk y sub-1))
      (reason-should-equal w (reason-walk x sub-1))
      (reason-should-equal w (reason-walk w sub-1))
      (reason-should-equal 'b (reason-walk x sub-2))
      (reason-should-equal `(,x e ,z) (reason-walk u sub-2))
      (reason-should-equal `(b e ,y) (reason-walk* u sub-2)))))

(defun reason-occurs-p (x v s)
  ""
  (let ((v (reason-walk v s)))
    (cond
     ((reason-variable-p v)
      (equal v x))
     ((consp v)
      (or (reason-occurs-p x (car v) s)
          (reason-occurs-p x (cdr v) s)))
     (t nil))))

(ert-deftest reason-occurs-test ()
  (reason-with-variables (x y)
    (should (reason-occurs-p x x '()))
    (should (reason-occurs-p x `(,y) `((,y . ,x))))))

(defvar reason-false '!F "")

(defun reason-extend (x v s)
  ""
  (if (reason-occurs-p x v s)
      reason-false
    (cons `(,x . ,v) s)))

(defmacro reason-should-not (&rest forms)
  ""
  (let ((should-nots (mapcar (lambda (form)
                               `(reason-should-equal ,form reason-false))
                             forms)))
    `(progn ,@should-nots)))

(ert-deftest reason-extend-test ()
  (reason-with-variables (x y z)
    (reason-should-not
     (reason-extend x x '())
     (reason-extend x `(,x) '())
     (reason-extend x `(,y) `((,y . ,x))))
    (reason-should-equal 'e
      (reason-walk y (reason-extend x 'e `((,z . ,x) (,y . ,z)))))))

;; unification

(defun reason-unify (u v s)
  ""
  (let ((u (reason-walk u s))
        (v (reason-walk v s)))
    (cond
     ((equal u v)
      s)
     ((reason-variable-p u)
      (reason-extend u v s))
     ((reason-variable-p v)
      (reason-extend v u s))
     ((and (consp u) (consp v))
      (let ((s (reason-unify (car u) (car v) s)))
        (if (equal s reason-false)
            reason-false
          (reason-unify (cdr u) (cdr v) s))))
     (t reason-false))))

(defun ||| (u v)
  ""
  (lambda (s)
    (let ((s (reason-unify u v s)))
      (if (equal s reason-false)
          '()
        `(,s)))))

(defun !S (s)
  ""
  `(,s))

(defun !U (_s)
  ""
  '())

(ert-deftest reason-unification-test ()
  (reason-should-equal '(())
    (funcall (||| 4 4) '()))
  (reason-should-equal '(())
    (!S '()))
  (reason-should-equal '()
    (funcall (||| 4 5) '()))
  (reason-should-equal '()
    (!U '())))

;; reification

(defun reason-reify-name (number)
  "Return the symbol '_$NUMBER."
  (intern (concat "_" (number-to-string number))))

(defun reason-reify-s (v r)
  ""
  (let ((v (reason-walk v r)))
    (cond
     ((reason-variable-p v)
      (let ((rn (reason-reify-name (length r))))
        (reason-extend v rn r)))
     ((consp v)
      (let ((r (reason-reify-s (car v) r)))
        (reason-reify-s (cdr v) r)))
     (t
      r))))

(defun reason-reify (v)
  ""
  (lambda (s)
    (let ((v (reason-walk* v s)))
      (let ((r (reason-reify-s v '())))
        (reason-walk* v r)))))

(ert-deftest reason-reify-test ()
  (reason-with-variables (u v w x y z)
    (let ((a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
          (a2 `(,y . corn))
          (a3 `(,w .(,v ,u))))
      (reason-should-equal `(_0 (_1 _0) corn _2 ((ice) _2))
        (funcall (reason-reify x) `(,a1 ,a2 ,a3))))))

(defun reason-call/fresh (name f)
  "Returns a goal that has access to a variable created from NAME.
f: variable -> goal, e.g. (lambda (fruit) (||| 'plum fruit))"
  (declare (indent 1))
  (funcall f (reason-make-variable name)))

;; streams

;; A STREAM is
;;   * the empty list,
;;   * a cons pair whose cdr is a stream, or
;;   * a function of no arguments whose body is a stream.
;;
;; The last of these is called a SUSPENSION.

(defun reason-append (s1 s2)
  ""
  (cond
   ((null s1) s2)
   ((functionp s1) (lambda () (reason-append s2 (funcall s1))))
   (t (cons (car s1)
            (reason-append (cdr s1) s2)))))

(defun reason-pull (s)
  ""
  (cond
   ((null s) nil)
   ((functionp s) (reason-pull (funcall s)))
   (t s)))

(defun reason-take (n s)
  ""
  (declare (indent 1))
  (if (null s)
      nil
    (cons (car s)
          (if (and n (= n 1))
              nil
            (reason-take (and n (1- n))
              (reason-pull (cdr s)))))))

(ert-deftest reason-stream-test ()
  (let ((s1 '(a b c d))
        (s2 `(e f . (lambda () '(g h))))
        (s3 (lambda () '(i j k l))))
    (reason-should-equal '(a b c d i j k l)
      (reason-pull (reason-append s3 s1)))
    (reason-should-equal '(e f g h)
      (reason-take nil s2))))

;; goals

(defun reason-disj-2 (g1 g2)
  ""
  (lambda (s)
    (reason-append (funcall g1 s) (funcall g2 s))))

(defun reason-append-map (g s)
  ""
  (cond
   ((null s) nil)
   ((functionp s) (lambda () (reason-append-map g (funcall s))))
   (t (reason-append (funcall g (car s))
               (reason-append-map g (cdr s))))))

(defun reason-conj-2 (g1 g2)
  ""
  (lambda (s)
    (reason-append-map g2 (funcall g1 s))))

(defun reason-run-goal (g)
  ""
  (reason-pull (funcall g nil)))

(ert-deftest reason-goal-test ()
  (reason-with-variables (x)
    (let* ((g (reason-disj-2 (||| 'olive x) (||| 'oil x)))
           (s (reason-run-goal g))
           (l (reason-take 5 s))
           (k (length l)))
      (reason-should-equal k 2)
      (reason-should-equal '(1 1)
        (mapcar #'length l))
      (reason-should-equal '(olive oil)
        (mapcar (reason-reify x) s))
      (reason-should-equal l
        (reason-take nil s)))))

(defun reason--test-unproductive ()
  ""
  (lambda (s)
    (lambda ()
      (funcall (reason--test-unproductive) s))))

(defun reason--test-productive ()
  ""
  (lambda (s)
    (lambda ()
      (funcall (reason-disj-2 #'!S (reason--test-productive)) s))))

(ert-deftest reason-productivity-test ()
  (reason-with-variables (x)
    (let ((s (funcall (reason-disj-2
                       (||| 'olive x)
                       (reason--test-unproductive))
                      nil)))
      (reason-should-equal `((,x . olive))
        (car s)))
    (let ((s (funcall (reason-disj-2
                       (reason--test-unproductive)
                       (||| 'olive x))
                      nil)))
      (reason-should-equal `((,x . olive))
        (car (funcall s)))
      (reason-should-equal `((,x . olive))
        (car (reason-pull s))))
    (reason-should-equal '(() () ())
      (reason-take 3 (reason-run-goal (reason--test-productive))))))

;; macros

(defmacro reason-disj (&rest goals)
  ""
  (pcase (length goals)
    (0 `!U)
    (1 (car goals))
    (_ `(reason-disj-2 ,(car goals) (reason-disj ,@(cdr goals))))))

(defmacro reason-conj (&rest goals)
  ""
  (pcase (length goals)
    (0 `!S)
    (1 (car goals))
    (_ `(reason-conj-2 ,(car goals) (reason-conj ,@(cdr goals))))))

(defmacro reason-fresh (vars &rest goals)
  ""
  (declare (indent 1))
  (if (null vars)
      `(reason-conj ,@goals)
    (let ((var (car vars)))
      `(reason-call/fresh (gensym)
         (lambda (,var)
           (reason-fresh ,(cdr vars)
             ,@goals))))))

(defmacro reason-run (n var &rest goals)
  ""
  (declare (indent 2))
  (if (listp var)
      (let ((q (gensym)))
        `(reason-run ,n ,q
           (reason-fresh ,var
             (||| (list ,@var) ,q)
             ,@goals)))
    `(let ((,var (reason-make-variable ',var)))
       (mapcar
        (reason-reify ,var)
        (reason-take ,n
          (reason-run-goal (reason-conj ,@goals)))))))

(defmacro reason-run* (q &rest goals)
  ""
  (declare (indent 1))
  `(reason-run nil ,q
     ,@goals))

(defmacro reason-conde (&rest goal-lists)
  ""
  `(reason-disj ,@(mapcar (lambda (arm) `(reason-conj ,@arm)) goal-lists)))

(defmacro reason-defrel (name varlist &rest goals)
  ""
  (declare (indent 2))
  (let ((s (gensym)))
    `(defun ,name ,varlist
       (lambda (,s)
         (lambda ()
           (funcall (reason-conj ,@goals) ,s))))))

(ert-deftest reason-test-run-basic ()
  (reason-should-equal '()
    (reason-run* q #'!U))
  (reason-should-equal '(t)
    (reason-run* q (||| t q)))
  (reason-should-equal '()
    (reason-run* q #'!U (||| t q)))
  (reason-should-equal '(t)
    (reason-run* q #'!S (||| t q)))
  (reason-should-equal '(corn)
    (reason-run* r #'!S (||| 'corn r)))
  (reason-should-equal '(olive oil)
    (reason-run* x (reason-disj (||| 'olive x) (||| 'oil x))))
  (reason-should-equal '(oil olive)
    (reason-run* x (reason-disj (||| 'oil x) (||| 'olive x))))
  (reason-should-equal '(oil)
    (reason-run* x (reason-disj (reason-conj-2 (||| 'olive x) #'!U) (||| 'oil x))))
  (reason-should-equal '(olive _0 oil)
    (reason-run* x (reason-disj (reason-conj (||| 'virgin x) #'!U) (reason-disj (||| 'olive x) (reason-disj #'!S(||| 'oil x)))))))

(ert-deftest reason-test-fresh ()
  (reason-should-equal '(t)
    (reason-run* q
      (reason-fresh (x)
        (||| t x)
        (||| t q))))
  (reason-should-equal '((_0 _1))
    (reason-run* s
      (reason-fresh (x)
        (reason-fresh (y)
          (||| `(,x ,y) s)))))
  (reason-should-equal '((_0 _1 _0))
    (reason-run* s
      (reason-fresh (x y)
        (||| `(,x ,y ,x) s))))
  (reason-should-equal '((split pea))
    (reason-run* r
      (reason-fresh (x)
        (reason-fresh (y)
          (||| 'split x)
          (||| 'pea y)
          (||| `(,x ,y) r)))))
  (reason-should-equal '((split pea))
    (reason-run* r
      (reason-fresh (x)
        (reason-fresh (y)
          (||| 'split x)
          (||| 'pea y)
          (||| `(,x ,y) r)))))
  (reason-should-equal '((split pea))
    (reason-run* r
      (reason-fresh (x y)
        (||| 'split x)
        (||| 'pea y)
        (||| `(,x ,y) r))))
  (reason-should-equal '((split pea))
    (reason-run* (x y)
      (||| 'split x)
      (||| 'pea y)))
  (reason-should-equal '(((split pea) split pea))
    (reason-run* (r x y)
      (||| 'split x)
      (||| 'pea y)
      (||| `(,x ,y) r)))
  (reason-should-equal '((_0 _1) (_0 _1))
    (reason-run* (x y)
      (reason-fresh (z)
        (reason-conde
         ((||| x z) (reason-fresh (z) (||| y z)))
         ((reason-fresh (z) (||| x z)) (||| y z))))))
  (reason-should-equal '((nil _0) (_0 nil))
    (reason-run* (x y)
      (reason-fresh (z)
        (reason-conde
         ((||| x z) (reason-fresh (z) (||| y z)))
         ((reason-fresh (z) (||| x z)) (||| y z)))
        (||| nil z)))))

(reason-defrel reason--test-teacup-o (x)
  (reason-disj (||| x 'tea) (||| x 'cup)))

(ert-deftest reason-test-defrel ()
  (reason-should-equal '(tea cup)
    (reason-run* x (reason--test-teacup-o x)))
  (reason-should-equal '((nil t) (tea t) (cup t))
    (reason-run* (x y)
      (reason-conde
       ((reason--test-teacup-o x) (||| y t))
       ((||| x nil) (||| y t)))))
  (reason-should-equal '((tea tea) (tea cup) (cup tea) (cup cup))
    (reason-run* (x y)
      (reason--test-teacup-o x)
      (reason--test-teacup-o y)))
  (reason-should-equal '(tea cup)
    (reason-run* x
      (reason--test-teacup-o x)
      (reason--test-teacup-o x)))
  (reason-should-equal '((nil tea) (nil cup) (tea _0) (cup _0))
    (reason-run* (x y)
      (reason-disj-2
       (reason-conj-2 (reason--test-teacup-o x) (reason--test-teacup-o x))
       (reason-conj-2 (||| nil x) (reason--test-teacup-o y)))))
  (reason-should-equal '((t tea) (t cup) (tea _0) (cup _0))
    (reason-run* (x y)
      (reason-conde
       ((reason--test-teacup-o x) (reason--test-teacup-o x))
       ((||| x t) (reason--test-teacup-o y))))))

(ert-deftest reason-test-conde ()
  (reason-should-equal '((split pea) (navy bean) (red lentil))
    (reason-run* (x y)
      (reason-conde
       ((||| x 'split) (||| y 'pea))
       ((||| x 'navy) (||| y 'bean))
       ((||| x 'red) (||| y 'lentil)))))
  (reason-should-equal '(oil)
    (reason-run* x
      (reason-conde
       ((||| x 'olive) #'!U)
       ((||| x 'oil))))))

(reason-defrel reason-car-o (p a)
  (reason-fresh (d)
    (reason-cons-o a d p)))

(ert-deftest reason-test-car-o ()
  (reason-should-equal '(a)
    (reason-run* p
      (reason-car-o '(a c o r n) p)))
  (reason-should-equal '(t)
    (reason-run* q
      (reason-car-o '(a c o r n) 'a)
      (||| q t)))
  (reason-should-equal '(pear)
    (reason-run* r
      (reason-fresh (x y)
        (reason-car-o `(,r ,y) x)
        (||| x 'pear))))
  (reason-should-equal '((grape a))
    (reason-run* r
      (reason-fresh (x y)
        (reason-fresh (d) (||| (cons x d) '(grape raisin pear)))
        (reason-fresh (d) (||| (cons y d) '((a) (b) (c))))
        (||| r (cons x y)))))
  (reason-should-equal '((grape a))
    (reason-run* r
      (reason-fresh (x y)
        (reason-car-o '(grape raisin pear) x)
        (reason-car-o '((a) (b) (c)) y)
        (||| r (cons x y))))))

(reason-defrel reason-cdr-o (p d)
  (reason-fresh (a)
    (reason-cons-o a d p)))

(ert-deftest reason-test-cdr-o ()
  (reason-should-equal '(c)
    (reason-run* r
      (reason-fresh (v)
        (reason-cdr-o '(a c o r n) v)
        (reason-car-o v r))))
  (reason-should-equal '(((raisin pear) a))
    (reason-run* r
      (reason-fresh (x y)
        (reason-cdr-o '(grape raisin pear) x)
        (reason-car-o '((a) (b) (c)) y)
        (||| r (cons x y)))))
  (reason-should-equal '(t)
    (reason-run* q
      (reason-cdr-o '(a c o r n) '(c o r n))
      (||| q t)))
  (reason-should-equal '(o)
    (reason-run* x
      (reason-cdr-o '(c o r n) `(,x r n))))
  (reason-should-equal '((a c o r n))
    (reason-run* l
      (reason-fresh (x)
        (reason-cdr-o l '(c o r n))
        (reason-car-o l x)
        (||| x 'a)))))

(reason-defrel reason-cons-o (a d p)
  (||| p (cons a d)))

(ert-deftest reason-test-cons-o ()
  (reason-should-equal '(((a b c) d e f))
    (reason-run* l
      (reason-cons-o '(a b c) '(d e f) l)))
  (reason-should-equal '(d)
    (reason-run* x
      (reason-cons-o x '(a b c) '(d a b c))))
  (reason-should-equal '((e a d c ))
    (reason-run* r
      (reason-fresh (x y z)
        (||| r `(e a d ,x))
        (reason-cons-o y `(a ,z c) r))))
  (reason-should-equal '((d a d c))
    (reason-run* l
      (reason-fresh (x)
        (||| l `(d a ,x c))
        (reason-cons-o x `(a ,x c) l))))
  (reason-should-equal '((d a d c))
    (reason-run* l
      (reason-fresh (x)
        (reason-cons-o x `(a ,x c) l)
        (||| l `(d a ,x c)))))
  (reason-should-equal '((b o n u s))
    (reason-run* l
      (reason-fresh (d p x y w)
        (reason-cons-o w '(n u s) p)
        (reason-cdr-o l p)
        (reason-car-o l x)
        (||| x 'b)
        (reason-cdr-o l d)
        (reason-car-o d y)
        (||| y 'o)))))

(reason-defrel reason-null-o (x)
  (||| x '()))

(ert-deftest reason-test-null-o ()
  (reason-should-equal '()
    (reason-run* q
      (reason-null-o '(grape raisin pear))
      (||| q t)))
  (reason-should-equal '(t)
    (reason-run* q
      (reason-null-o '())
      (||| q t)))
  (reason-should-equal '(())
    (reason-run* x
      (reason-null-o x))))

(reason-defrel reason-pair-o (p)
  (reason-fresh (a d)
    (reason-cons-o a d p)))

(ert-deftest reason-test-pair-o ()
  (reason-should-equal '(t)
    (reason-run* q
      (reason-pair-o (cons q q))
      (||| q t)))
  (reason-should-equal '()
    (reason-run* q
      (reason-pair-o '())
      (||| q t)))
  (reason-should-equal '()
    (reason-run* q
      (reason-pair-o 'pair)
      (||| q t)))
  (reason-should-equal '((_0 . _1))
    (reason-run* x
      (reason-pair-o x))))

(reason-defrel reason-append-o (l p out)
  (reason-conde
   ((reason-null-o l) (||| p out))
   ((reason-fresh (a d res)
      (reason-car-o l a)
      (reason-cdr-o l d)
      (reason-append-o d p res)
      (reason-cons-o a res out)))))

(ert-deftest reason-test-append-o ()
  (reason-should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reason-run 5 x
      (reason-fresh (y z)
        (reason-append-o x y z))))
  (reason-should-equal '(_0 _0 _0 _0 _0)
    (reason-run 5 y
      (reason-fresh (x z)
        (reason-append-o x y z))))
  (reason-should-equal '(_0 (_0 . _1) (_0 _1 . _2) (_0 _1 _2 . _3) (_0 _1 _2 _3 . _4))
    (reason-run 5 z
      (reason-fresh (x y)
        (reason-append-o x y z))))
  (reason-should-equal '((cake tastes yummy))
    (reason-run* x
      (reason-append-o
       '(cake)
       '(tastes yummy)
       x)))
  (reason-should-equal '((cake with ice _0 tastes yummy))
    (reason-run* x
      (reason-fresh (y)
        (reason-append-o
         `(cake with ice ,y)
         '(tastes yummy)
         x))))
  (reason-should-equal '((cake with ice cream . _0))
    (reason-run* x
      (reason-fresh (y)
        (reason-append-o
         '(cake with ice cream)
         y
         x))))
  (reason-should-equal '((cake with ice d t)
                   (cake with ice _0 d t)
                   (cake with ice _0 _1 d t)
                   (cake with ice _0 _1 _2 d t)
                   (cake with ice _0 _1 _2 _3 d t))
    (reason-run 5 x
      (reason-fresh (y)
        (reason-append-o `(cake with ice . ,y) '(d t) x))))
  (reason-should-equal '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
    (reason-run 5 y
      (reason-fresh (x)
        (reason-append-o `(cake with ice . ,y) '(d t) x))))
  (reason-should-equal '((() (cake with ice d t))
                   ((cake) (with ice d t))
                   ((cake with) (ice d t))
                   ((cake with ice) (d t))
                   ((cake with ice d) (t))
                   ((cake with ice d t) ()))
    (reason-run 6 (x y)
      (reason-append-o x y '(cake with ice d t)))))


(provide 'reason)
;;; reason.el ends here
