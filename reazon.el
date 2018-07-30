;;; reazon.el --- miniKanren implementation for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Drozd

;; Author: Nick Drozd <nicholasdrozd@gmail.com>
;; URL: https://github.com/nickdrozd/reazon
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
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

;; Reazon is an implmentation of the miniKanren language for Emacs. It
;; provides an interface for writing an running relational programs.
;; That interface consists of the following macros:

;; * reazon-defrel
;; * reazon-conde
;; * reazon-fresh
;; * reazon-run
;; * reazon-run*

;;; Code:

(when (< emacs-major-version 26)
  (require 'cl-lib)
  (defalias 'gensym 'cl-gensym))

;; variables

(defun reazon--make-variable (name)
  "Create a unique variable from NAME."
  (vector name))

(defun reazon--variable-p (var)
  "Check whether VAR is a variable."
  (vector-or-char-table-p var))

;; substitutions

(defun reazon--walk (var sub)
  "Return the value associated with VAR in SUB if there is one, else VAR."
  (let ((val (and (reazon--variable-p var)
                  (assoc var sub))))
    (cond
     ((consp val)
      (reazon--walk (cdr val) sub))
     (t var))))

(defun reazon--walk* (var sub)
  ""
  (let ((var (reazon--walk var sub)))
    (cond
     ((reazon--variable-p var)
      var)
     ((consp var)
      (cons
       (reazon--walk* (car var) sub)
       (reazon--walk* (cdr var) sub)))
     (t
      var))))

(defun reazon--occurs-p (var val sub)
  ""
  (let ((val (reazon--walk val sub)))
    (cond
     ((reazon--variable-p val)
      (equal val var))
     ((consp val)
      (or (reazon--occurs-p var (car val) sub)
          (reazon--occurs-p var (cdr val) sub)))
     (t nil))))

(defvar reazon--false '!F "")

(defun reazon--extend (var val sub)
  ""
  (if (reazon--occurs-p var val sub)
      reazon--false
    (cons `(,var . ,val) sub)))

;; unification

(defun reazon--unify (u v sub)
  ""
  (let ((u (reazon--walk u sub))
        (v (reazon--walk v sub)))
    (cond
     ((equal u v)
      sub)
     ((reazon--variable-p u)
      (reazon--extend u v sub))
     ((reazon--variable-p v)
      (reazon--extend v u sub))
     ((and (consp u) (consp v))
      (let ((sub (reazon--unify (car u) (car v) sub)))
        (if (equal sub reazon--false)
            reazon--false
          (reazon--unify (cdr u) (cdr v) sub))))
     (t reazon--false))))

(defun reazon-== (u v)
  ""
  (lambda (sub)
    (let ((sub (reazon--unify u v sub)))
      (if (equal sub reazon--false)
          '()
        `(,sub)))))

(defun reazon-!S (sub)
  ""
  `(,sub))

(defun reazon-!U (_sub)
  ""
  '())

;; reification

(defun reazon--reify-name (number)
  "Return the symbol '_$NUMBER."
  (intern (concat "_" (number-to-string number))))

(defun reazon--reify-sub (var sub)
  ""
  (let ((var (reazon--walk var sub)))
    (cond
     ((reazon--variable-p var)
      (let ((rn (reazon--reify-name (length sub))))
        (reazon--extend var rn sub)))
     ((consp var)
      (let ((sub (reazon--reify-sub (car var) sub)))
        (reazon--reify-sub (cdr var) sub)))
     (t
      sub))))

(defun reazon--reify (var)
  ""
  (lambda (sub)
    (let ((var (reazon--walk* var sub)))
      (let ((r (reazon--reify-sub var '())))
        (reazon--walk* var r)))))

(defun reazon--call-with-fresh (name function)
  "Returns a goal that has access to a variable created from NAME.
function: variable -> goal, e.g. (lambda (fruit) (reazon-== 'plum fruit))"
  (declare (indent 1))
  (funcall function (reazon--make-variable name)))

;; streams

;; A STREAM is
;;   * the empty list,
;;   * a cons pair whose cdr is a stream, or
;;   * a function of no arguments whose body is a stream.
;;
;; The last of these is called a SUSPENSION.

(defun reazon--append (s1 s2)
  ""
  (cond
   ((null s1) s2)
   ((functionp s1) (lambda () (reazon--append s2 (funcall s1))))
   (t (cons (car s1)
            (reazon--append (cdr s1) s2)))))

(defun reazon--pull (stream)
  ""
  (cond
   ((null stream) nil)
   ((functionp stream) (reazon--pull (funcall stream)))
   (t stream)))

(defun reazon--take (n stream)
  ""
  (declare (indent 1))
  (if (null stream)
      nil
    (cons (car stream)
          (if (and n (= n 1))
              nil
            (reazon--take (and n (1- n))
              (reazon--pull (cdr stream)))))))

;; goals

(defun reazon--disj-2 (goal-1 goal-2)
  ""
  (lambda (stream)
    (reazon--append
     (funcall goal-1 stream)
     (funcall goal-2 stream))))

(defun reazon--append-map (goal stream)
  ""
  (cond
   ((null stream) nil)
   ((functionp stream) (lambda () (reazon--append-map goal (funcall stream))))
   (t (reazon--append
       (funcall goal (car stream))
       (reazon--append-map goal (cdr stream))))))

(defun reazon--conj-2 (goal-1 goal-2)
  ""
  (lambda (stream)
    (reazon--append-map
     goal-2
     (funcall goal-1 stream))))

(defun reazon--run-goal (goal)
  ""
  (reazon--pull (funcall goal nil)))

;; macros

(defmacro reazon-disj (&rest goals)
  ""
  (pcase (length goals)
    (0 `reazon-!U)
    (1 (car goals))
    (_ `(reazon--disj-2 ,(car goals) (reazon-disj ,@(cdr goals))))))

(defmacro reazon-conj (&rest goals)
  ""
  (pcase (length goals)
    (0 `reazon-!S)
    (1 (car goals))
    (_ `(reazon--conj-2 ,(car goals) (reazon-conj ,@(cdr goals))))))

(defmacro reazon-fresh (vars &rest goals)
  ""
  (declare (indent 1))
  (if (null vars)
      `(reazon-conj ,@goals)
    (let ((var (car vars)))
      `(reazon--call-with-fresh (gensym)
         (lambda (,var)
           (reazon-fresh ,(cdr vars)
             ,@goals))))))

(defmacro reazon-run (n var/list &rest goals)
  ""
  (declare (indent 2))
  (if (listp var/list)
      (let ((vars var/list)
            (q (gensym)))
        `(reazon-run ,n ,q
           (reazon-fresh ,vars
             (reazon-== (list ,@vars) ,q)
             ,@goals)))
    (let ((var var/list))
      `(let ((,var (reazon--make-variable ',var)))
         (mapcar
          (reazon--reify ,var)
          (reazon--take ,n
            (reazon--run-goal (reazon-conj ,@goals))))))))

(defmacro reazon-run* (query-var &rest goals)
  ""
  (declare (indent 1))
  `(reazon-run nil ,query-var
     ,@goals))

;; do all the goal lists get a conj, or just the first one?
(defmacro reazon-conde (&rest goal-lists)
  ""
  `(reazon-disj ,@(mapcar (lambda (arm) `(reazon-conj ,@arm)) goal-lists)))

(defmacro reazon-defrel (name varlist &rest goals)
  ""
  (declare (indent 2))
  (let ((stream (gensym)))
    `(defun ,name ,varlist
       (lambda (,stream)
         (lambda ()
           (funcall (reazon-conj ,@goals) ,stream))))))

(reazon-defrel reazon-car-o (p a)
  (reazon-fresh (d)
    (reazon-cons-o a d p)))

(reazon-defrel reazon-cdr-o (p d)
  (reazon-fresh (a)
    (reazon-cons-o a d p)))

(reazon-defrel reazon-cons-o (a d p)
  (reazon-== p (cons a d)))

(reazon-defrel reazon-null-o (x)
  (reazon-== x '()))

(reazon-defrel reazon-pair-o (p)
  (reazon-fresh (a d)
    (reazon-cons-o a d p)))

(reazon-defrel reazon-append-o (l p out)
  (reazon-conde
   ((reazon-null-o l) (reazon-== p out))
   ((reazon-fresh (a d res)
      (reazon-cons-o a d l)
      (reazon-cons-o a res out)
      (reazon-append-o d p res)))))


(provide 'reazon)
;;; reazon.el ends here
