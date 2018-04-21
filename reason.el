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

(defmacro reason-should-equal(f1 f2)
  "Assert that f1 and f2 are equal."
  `(should (equal ,f1 ,f2)))

(ert-deftest reason-walk-test ()
  (reason-with-variables (u v w x y z)
    (let ((sub-1 `((,z . a) (,x . ,w) (,y . ,z)))
          (sub-2 `((,x . b) (,z . ,y) (,w . (,x e ,z)) (,u . ,w))))
      (reason-should-equal (reason-walk z sub-1) 'a)
      (reason-should-equal (reason-walk y sub-1) 'a)
      (reason-should-equal (reason-walk x sub-1) w)
      (reason-should-equal (reason-walk w sub-1) w)
      (reason-should-equal (reason-walk x sub-2) 'b)
      (reason-should-equal (reason-walk u sub-2) `(,x e ,z))
      (reason-should-equal (reason-walk* u sub-2) `(b e ,y)))))

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
    (reason-should-equal
     (reason-walk y (reason-extend x 'e `((,z . ,x) (,y . ,z))))
     'e)))

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
  (reason-should-equal (funcall (||| 4 4) '()) '(()))
  (reason-should-equal (!S '()) '(()))
  (reason-should-equal (funcall (||| 4 5) '()) '())
  (reason-should-equal (!U '()) '()))

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
      (reason-should-equal
       (funcall (reason-reify x) `(,a1 ,a2 ,a3))
       `(_0 (_1 _0) corn _2 ((ice) _2))))))

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
          (if (and n (zerop (1- n)))
              nil
            (reason-take (and n (1- n))
              (reason-pull (cdr s)))))))

(ert-deftest reason-stream-test ()
  (let ((s1 '(a b c d))
        (s2 `(e f ,(lambda () '(g h))))
        (s3 (lambda () '(i j k l))))
    (reason-should-equal (reason-pull (reason-append s3 s1)) '(a b c d i j k l))))

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
      (reason-should-equal (mapcar #'length l) '(1 1))
      (reason-should-equal (mapcar (reason-reify x) s) '(olive oil))
      (reason-should-equal l (reason-take nil s)))))

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
      (reason-should-equal (car s) `((,x . olive))))
    (let ((s (funcall (reason-disj-2
                       (reason--test-unproductive)
                       (||| 'olive x))
                      nil)))
      (reason-should-equal (car (funcall s)) `((,x . olive)))
      (reason-should-equal (car (reason-pull s)) `((,x . olive))))
    (reason-should-equal (reason-take 3 (reason-run-goal (reason--test-productive))) '(() () ()))))


(provide 'reason)
;;; reason.el ends here
