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

;; -- Variables --

;; Reazon variables need to be distinct from Lisp symbols. They are
;; implemented here as vectors.

(defun reazon--make-variable (name)
  "Create a unique variable from NAME."
  (vector name))

(defun reazon--variable-p (var)
  "Check whether VAR is a variable."
  (vector-or-char-table-p var))

;; -- Substitutions --

;; An ASSOCIATION is a cons pair whose car is a Reazon variable.

;; A SUBSTITUTION is a list of associations such that no variable is
;; associated more than once and no variable is associated to itself,
;; either directly or through a CYCLE.

;; A variable unassociated in a substitution is FRESH.

(defun reazon--walk (var sub)
  "Return the value associated with VAR in SUB if there is one, else VAR."
  (let ((val (and (reazon--variable-p var)
                  (assoc var sub))))
    (cond
     ((consp val)
      (reazon--walk (cdr val) sub))
     (t var))))

(defun reazon--walk* (var sub)
  "Return SUB with VAR replaced by its recursively walked value."
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
  "Return whether VAL is chain-associated with VAR in SUB."
  (let ((val (reazon--walk val sub)))
    (cond
     ((reazon--variable-p val)
      (equal val var))
     ((consp val)
      (or (reazon--occurs-p var (car val) sub)
          (reazon--occurs-p var (cdr val) sub)))
     (t nil))))

(defconst reazon--false (gensym)
  "A symbol to indicate substitution failure.
In The Reasoned Schemer, several substitution-handling functions
return #f (false) in certain circumstances instead of a substitution
to indicate that the operation failed. In Emacs, false is nil, which
is also the empty list, which happens to be a substitution. To avoid
confusing these functions, we pick an arbitrary dummy symbol to
indicate substitution failure.")

(defun reazon--extend (var val sub)
  "Associate VAR and VAL in SUB."
  (if (reazon--occurs-p var val sub)
      reazon--false
    (cons `(,var . ,val) sub)))

;; -- Unification --

(defun reazon--unify (u v sub)
  "Attempt to extend SUB with recursive associations between U and V."
  (let ((u (reazon--walk u sub))
        (v (reazon--walk v sub)))
    (cond
     ((equal u v)
      ;; The vars are already associated, so do nothing.
      sub)
     ((reazon--variable-p u)
      ;; u is fresh, so associate it with v.
      (reazon--extend u v sub))
     ((reazon--variable-p v)
      ;; v is fresh and u is not, so associate v with u.
      (reazon--extend v u sub))
     ((and (consp u) (consp v))
      ;; Destructure the vars and attempts to recursively unify them.
      (let ((sub (reazon--unify (car u) (car v) sub)))
        (if (equal sub reazon--false)
            reazon--false
          (reazon--unify (cdr u) (cdr v) sub))))
     (t
      ;; Unification failed.
      reazon--false))))

(defun reazon-== (u v)
  "Attempt to unify U and V in the provided substitution.
If unification succeeds, return a stream containing the
resulting substitution, else return the empty stream.

This primitive goal succeeds if U and V can be unified."
  (lambda (sub)
    (let ((sub (reazon--unify u v sub)))
      (if (equal sub reazon--false)
          '()
        `(,sub)))))

(defun reazon-!S (sub)
  "Return a stream containing SUB.
This primitive goal always succeeds."
  `(,sub))

(defun reazon-!U (_sub)
  "Return the empty stream.
This primitive goal always fails."
  '())

;; -- Reification --

;; A REIFIED NAME is a concrete identifier assigned to a fresh
;; variable when values are presented.

(defun reazon--reify-name (number)
  "Return the symbol '_$NUMBER."
  ;; Should this use `make-symbol' instead of `intern'?
  (intern (concat "_" (number-to-string number))))

(defun reazon--reify-sub (var sub)
  "Replace VAR in SUB with its reified name."
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
  "Return a function that takes a substitution and reifies VAR therein.
This is the reification entrypoint."
  (lambda (sub)
    (let ((var (reazon--walk* var sub)))
      (let ((r (reazon--reify-sub var '())))
        (reazon--walk* var r)))))

(defun reazon--call-with-fresh (name function)
  "Call FUNCTION with a variable created from NAME.
function: variable -> goal, e.g. (lambda (fruit) (reazon-== 'plum fruit))"
  (declare (indent 1))
  (funcall function (reazon--make-variable name)))

;; -- Streams --

;; A STREAM is
;;   * the empty list,
;;   * a cons pair whose cdr is a stream, or
;;   * a function of no arguments whose body is a stream.
;;
;; The last of these is called a SUSPENSION.

(defun reazon--append (stream-1 stream-2)
  "Join STREAM-1 and STREAM-2 into a new stream.
If STREAM-1 is a suspsension, force it and append the result to
STREAM-2, else append them as usual."
  (cond
   ((null stream-1) stream-2)
   ((functionp stream-1) (lambda () (reazon--append stream-2 (funcall stream-1))))
   (t (cons (car stream-1)
            (reazon--append (cdr stream-1) stream-2)))))

(defun reazon--pull (stream)
  "Force STREAM and repull if it is a suspension, else just return it."
  (cond
   ((null stream) nil)
   ((functionp stream) (reazon--pull (funcall stream)))
   (t stream)))

(defun reazon--take (n stream)
  "Pull N values from STREAM if N is non-nil, else pull it without stopping."
  (declare (indent 1))
  (if (null stream)
      nil
    (cons (car stream)
          (if (and n (= n 1))
              nil
            (reazon--take (and n (1- n))
              (reazon--pull (cdr stream)))))))

;; -- Goals --

;; A GOAL is a function that takes a substitution and returns a stream
;; of substitutions.

(defun reazon--disj-2 (goal-1 goal-2)
  "Join GOAL-1 and GOAL-2 into a new goal containing them both.
This primitive goal succeeds if either of them do."
  (lambda (stream)
    (reazon--append
     (funcall goal-1 stream)
     (funcall goal-2 stream))))

(defun reazon--append-map (goal stream)
  "Run GOAL with every element of STREAM."
  (cond
   ((null stream) nil)
   ((functionp stream) (lambda () (reazon--append-map goal (funcall stream))))
   (t (reazon--append
       (funcall goal (car stream))
       (reazon--append-map goal (cdr stream))))))

(defun reazon--conj-2 (goal-1 goal-2)
  "Run GOAL-2 with the result of running GOAL-1 with the provided stream.
This primitive goal succeeds if they both do."
  (lambda (stream)
    (reazon--append-map
     goal-2
     (funcall goal-1 stream))))

(defun reazon--run-goal (goal)
  "Pull GOAL with the empty stream."
  (reazon--pull (funcall goal nil)))

;; -- Macros --

;; Reazon is implemented in such a way that everything could in
;; principle be run without using any macros. No "work" is being done
;; by the macros; they are strictly wrappers around the functions
;; defined above.

(defmacro reazon-disj (&rest goals)
  "Chain together GOALS with `reazon--disj-2' if there are any, else fail."
  (pcase (length goals)
    (0 `reazon-!U)
    (1 (car goals))
    (_ `(reazon--disj-2 ,(car goals) (reazon-disj ,@(cdr goals))))))

(defmacro reazon-conj (&rest goals)
  "Chain together GOALS with `reazon--conj-2' if there are any, else succeed."
  (pcase (length goals)
    (0 `reazon-!S)
    (1 (car goals))
    (_ `(reazon--conj-2 ,(car goals) (reazon-conj ,@(cdr goals))))))

(defmacro reazon-fresh (vars &rest goals)
  "Bind each of VARS as a fresh variable and run the conjunction of GOALS."
  (declare (indent 1))
  (if (null vars)
      `(reazon-conj ,@goals)
    (let ((var (car vars)))
      `(reazon--call-with-fresh (gensym)
         (lambda (,var)
           (reazon-fresh ,(cdr vars)
             ,@goals))))))

(defmacro reazon-run (n var/list &rest goals)
  "Run GOALS against VAR/LIST for at most N values.
If N is nil, run for as many values as possible. VAR/LIST can be
either a symbol or a list."
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
  "Run GOALS against QUERY-VAR for as many values as possible.
This will raise an error if the query has infinitely many solutions."
  (declare (indent 1))
  `(reazon-run nil ,query-var
     ,@goals))

(defmacro reazon-conde (&rest goal-lists)
  "Chain together each GOAL-LISTS as a disjunction of conjunctions."
  `(reazon-disj ,@(mapcar (lambda (arm) `(reazon-conj ,@arm)) goal-lists)))

(defmacro reazon-defrel (name varlist &rest goals)
  "Define relation NAME with args VARLIST and body GOALS."
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
