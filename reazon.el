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
  (let* ((next var)
         (val (and (reazon--variable-p next) (assoc next sub))))
    (while (consp val)
      (setq next (cdr val))
      (setq val (and (reazon--variable-p next) (assoc next sub))))
    next))

(defun reazon--walk* (var sub)
  "Return SUB with VAR replaced by its recursively walked value."
  (let ((walked (reazon--walk var sub)))
    (if (not (consp walked))
        walked
      (cons
       (reazon--walk* (car walked) sub)
       (reazon--walk* (cdr walked) sub)))))

(defun reazon--occurs-p (var val sub)
  "Return whether VAL is chain-associated with VAR in SUB."
  (let ((walked (reazon--walk val sub)))
    (cond
     ((reazon--variable-p walked)
      (equal walked var))
     ((consp walked)
      (or (reazon--occurs-p var (car walked) sub)
          (reazon--occurs-p var (cdr walked) sub)))
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

(defun reazon--unify (u v sub)
  "Attempt to extend SUB with recursive associations between U and V."
  (let ((u-walked (reazon--walk u sub))
        (v-walked (reazon--walk v sub)))
    (cond
     ((equal u-walked v-walked)
      ;; The vars are already associated, so do nothing.
      sub)
     ((reazon--variable-p u-walked)
      ;; u-walked is fresh, so associate it with v-walked.
      (reazon--extend u-walked v-walked sub))
     ((reazon--variable-p v-walked)
      ;; v-walked is fresh and u-walked is not, so associate v-walked
      ;; with u-walked.
      (reazon--extend v-walked u-walked sub))
     ((and (consp u-walked) (consp v-walked))
      ;; Destructure the vars and attempts to recursively unify them.
      (let ((sub (reazon--unify (car u-walked) (car v-walked) sub)))
        (if (equal sub reazon--false)
            reazon--false
          (reazon--unify (cdr u-walked) (cdr v-walked) sub))))
     (t
      ;; Unification failed.
      reazon--false))))

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
  (let (result
        (rest stream-1))
    (while (and rest (not (functionp rest)))
      (setq result (cons (car rest) result))
      (setq rest (cdr rest)))
    ;; rest is nil or a suspension
    (append
     (nreverse result)
     (if (null rest) stream-2
       ;; In the recursive call, STREAM-1 is forced and swapped with
       ;; STREAM-2. This swap is critical; without it, the search
       ;; would be depth-first and incomplete, whereas with it the
       ;; search is complete. See "microKanren: A Lucid Little Logic
       ;; Language with a Simple Complete Search".
       (lambda ()
         (reazon--append
          stream-2
          (funcall rest)))))))

(defun reazon--pull (stream)
  "Force STREAM until it isn't a suspension, then return it."
  (let ((result stream))
    (while (functionp result)
      (setq result (funcall result)))
    result))

(defun reazon--take (n stream)
  "Pull N values from STREAM if N is positive else pull it without stopping."
  (declare (indent 1))
  (if (null stream)
      nil
    (let ((count (if n (1- n) -1))
          (result (list (car stream)))
          (rest (reazon--pull (cdr stream))))
      (while (and rest (not (zerop count)))
        (setq count (1- count))
        (setq result (cons (car rest) result))
        (setq rest (reazon--pull (cdr rest))))
      (nreverse result))))

;; -- Goals --

;; A GOAL is a function that takes a substitution and returns a stream
;; of substitutions.

(defun reazon-== (u v)
  "Attempt to unify U and V in the provided substitution.
If unification succeeds, return a stream containing the
resulting substitution, else return the empty stream.

This primitive goal succeeds if U and V can be unified."
  (lambda (sub)
    (let ((unified (reazon--unify u v sub)))
      (if (equal unified reazon--false)
          '()
        `(,unified)))))

(defun reazon-!S (sub)
  "Return a stream containing SUB.
This primitive goal always succeeds."
  `(,sub))

(defun reazon-!U (_sub)
  "Return the empty stream.
This primitive goal always fails."
  '())

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

;; -- Reification --

;; A REIFIED NAME is a concrete identifier assigned to a fresh
;; variable when values are presented.

(defun reazon--reify-name (number)
  "Return the symbol '_$NUMBER."
  ;; Should this use `make-symbol' instead of `intern'?
  (intern (concat "_" (number-to-string number))))

(defun reazon--reify-sub (var sub)
  "Replace VAR in SUB with its reified name."
  (let ((walked (reazon--walk var sub)))
    (cond
     ((reazon--variable-p walked)
      (let ((name (reazon--reify-name (length sub))))
        (reazon--extend walked name sub)))
     ((consp walked)
      (let ((sub (reazon--reify-sub (car walked) sub)))
        (reazon--reify-sub (cdr walked) sub)))
     (t
      sub))))

(defun reazon--reify (var)
  "Return a function that takes a substitution and reifies VAR therein.
This is the reification entrypoint."
  (lambda (sub)
    (let* ((walked-var (reazon--walk* var sub))
           (reified-sub (reazon--reify-sub walked-var '())))
      (reazon--walk* walked-var reified-sub))))

(defun reazon--call-with-fresh (name function)
  "Call FUNCTION with a variable created from NAME.
function: variable -> goal, e.g. (lambda (fruit) (reazon-== 'plum fruit))"
  (declare (indent 1))
  (funcall function (reazon--make-variable name)))

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

(defmacro reazon-project (vars &rest goals)
  "Run GOALS with the values associated with VARS lexically bound.
This is an impure, non-relational operator, and its correct use
depends on the ordering of clauses. For example,

  (reazon-run* q
    (reazon-fresh (x)
      (reazon-== x 5)
      (reazon-project (x)
        (reazon-== q (* x x)))))

succeeds with a value of '(25), while

  (reazon-run* q
    (reazon-fresh (x)
      (reazon-project (x)
        (reazon-== q (* x x)))
      (reazon-== x 5)))

raises an error. This is because in the second instance, the variable
`x' is still fresh in the substitution, so the multiplication fails
when applied to it."
  (declare (indent 1))
  (if (null vars)
      `(reazon-conj ,@goals)
    (let* ((stream (gensym))
           (walked-vars
            (mapcar (lambda (var) `(,var (reazon--walk* ,var ,stream))) vars)))
      `(lambda (,stream)
         (let ,walked-vars
           (funcall (reazon-conj ,@goals) ,stream))))))

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

(reazon-defrel reazon-list-o (s)
  (reazon-conde
   ((reazon-null-o s))
   ((reazon-pair-o s)
    (reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-list-o d)))))

(reazon-defrel reazon-lol-o (s)
  (reazon-conde
   ((reazon-null-o s))
   ((reazon-fresh (a)
      (reazon-car-o s a)
      (reazon-list-o a))
    (reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-lol-o d)))))

(reazon-defrel reazon-twin-o (s)
  (reazon-fresh (x)
    (reazon-== s `(,x ,x))))

(reazon-defrel reazon-lot-o (s)
  (reazon-conde
   ((reazon-null-o s))
   ((reazon-fresh (a)
      (reazon-car-o s a)
      (reazon-twin-o a))
    (reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-lot-o d)))))

(reazon-defrel reazon-member-o (x s)
  (reazon-conde
   ((reazon-car-o s x))
   ((reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-member-o x d)))))

(reazon-defrel reazon-proper-member-o (x s)
  (reazon-conde
   ((reazon-car-o s x)
    (reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-list-o d)))
   ((reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-proper-member-o x d)))))

(reazon-defrel reazon-mem-o (x s out)
  (reazon-conde
   ((reazon-car-o s x) (reazon-== out s))
   ((reazon-fresh (d)
      (reazon-cdr-o s d)
      (reazon-mem-o x d out)))))

(reazon-defrel reazon-rember-o (x s out)
  (reazon-conde
   ((reazon-null-o s) (reazon-null-o out))
   ((reazon-car-o s x) (reazon-cdr-o s out))
   ((reazon-fresh (a d rec)
      (reazon-cons-o a d s)
      (reazon-cons-o a rec out)
      (reazon-rember-o x d rec)))))

;; -- Utilities --

(require 'profiler)

(defun reazon-profile-memory ()
  "Profile Reazon's memory usage.
Keep an eye out for recursive functions!"
  (interactive)
  (profiler-start 'mem)
  (dotimes (_ 10)
    ;; dummy value to silence compiler warnings
    (let ((dummy
           (let ((names '(mary-anne gabrielle lorna rosalind melissa)))
             (reazon-run* q
               (reazon-fresh (a b c d)
                 (reazon-== q `((barnacle-hood melissa gabrielle) ,a ,b ,c ,d))
                 (reazon-fresh (daughter)
                   (reazon-== a `(moore ,daughter lorna))
                   (reazon-member-o daughter names))
                 (reazon-fresh (daughter)
                   (reazon-== b `(downing ,daughter melissa))
                   (reazon-member-o daughter names))
                 (reazon-fresh (daughter)
                   (reazon-== c `(hall ,daughter rosalind))
                   (reazon-member-o daughter names))
                 (reazon-fresh (common-name yacht other other-father)
                   (reazon-== d `(parker ,common-name ,yacht))
                   (reazon-== other `(,other-father gabrielle ,common-name))
                   (reazon-member-o common-name names)
                   (reazon-member-o other q)))))))
      (null dummy)))
  (profiler-report)
  (profiler-stop))


(provide 'reazon)
;;; reazon.el ends here
