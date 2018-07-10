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

(defvar reason-false '!F "")

(defun reason-extend (x v s)
  ""
  (if (reason-occurs-p x v s)
      reason-false
    (cons `(,x . ,v) s)))

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

;; do all the goal lists get a conj, or just the first one?
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

(reason-defrel reason-car-o (p a)
  (reason-fresh (d)
    (reason-cons-o a d p)))

(reason-defrel reason-cdr-o (p d)
  (reason-fresh (a)
    (reason-cons-o a d p)))

(reason-defrel reason-cons-o (a d p)
  (||| p (cons a d)))

(reason-defrel reason-null-o (x)
  (||| x '()))

(reason-defrel reason-pair-o (p)
  (reason-fresh (a d)
    (reason-cons-o a d p)))

(reason-defrel reason-append-o (l p out)
  (reason-conde
   ((reason-null-o l) (||| p out))
   ((reason-fresh (a d res)
      (reason-cons-o a d l)
      (reason-cons-o a res out)
      (reason-append-o d p res)))))


(provide 'reason)
;;; reason.el ends here
