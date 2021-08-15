;;; reazon.el --- miniKanren for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Drozd

;; Author: Nick Drozd <nicholasdrozd@gmail.com>
;; URL: https://github.com/nickdrozd/reazon
;; Version: 0.3
;; Package-Requires: ((emacs "26"))
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
;; provides an interface for writing and running relational programs.
;; That interface consists of the following macros:

;; * reazon-defrel
;; * reazon-run*
;; * reazon-run
;; * reazon-fresh
;; * reazon-conde
;; * reazon-conj
;; * reazon-disj
;; * reazon-project

;; Besides these, there is a single primitive goal, reazon-==.

;;; Code:

;; -- Configuration --

(defvar reazon-timeout nil
  "The maximum amount of time in seconds that a query can take.

If a query is interrupted, it will return any results it has
collected so far.

Consider let-binding this around your call to `reazon-run'
instead of setqing it.")

(defvar reazon-occurs-check t
  "Whether to run the `occurs' check during unification.
Circular queries are bad. It is possible to detect them, but it isn't
cheap. Setting the flag to nil grants the programmer faster queries in
exchange for assuming the responsibility of ensuring correctness on
their own.")

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

(define-error 'reazon-circular-query
  "Circular query detected")

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
  (if (and reazon-occurs-check (reazon--occurs-p var val sub))
      (signal 'reazon-circular-query `(,var ,val ,sub))
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

(defvar reazon--stop-time nil
  "The time from epoch in seconds that a query should halt computation.
Should not be set manually; its value is derived from `reazon-timeout'.")

(defun reazon--pull (stream)
  "Force STREAM until it isn't a suspension or `reazon--stop-time' is reached."
  (let ((result stream))
    (while (and (functionp result)
                (or (not reazon--stop-time)
                    (> reazon--stop-time (float-time))))
      (setq result (funcall result)))
    result))

(defun reazon--take (n stream)
  "Pull N values from STREAM if N is positive else pull it without stopping.

If `stream' is a function, then `reazon--pull' ended due to a
timeout, and the values collected so far will be returned."
  (declare (indent 1))
  (if (or (functionp stream) (null stream))
      nil
    (let ((count (if n (1- n) -1))
          (result (list (car stream)))
          (rest (reazon--pull (cdr stream))))
      (while (and rest (not (zerop count)) (not (functionp rest)))
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

(defun reazon--ifte (test consq alt)
  "Run CONSQ if TEST succeeds, else ALT."
  (declare (indent 1))
  (lambda (s)
    (reazon--ifte-help (funcall test s) consq alt)))

(defun reazon--ifte-help (stream consq alt)
  "Run ALT with STREAM if it's nil, else CONSQ."
  (cond
   ((null stream) (funcall alt stream))
   ((functionp stream)
    (lambda () (reazon--ifte-help (funcall stream) consq alt)))
   (t (reazon--append-map consq stream))))

(defun reazon--once (goal)
  "Run GOAL for just one value (if there is one)."
  (lambda (s)
    (reazon--once-help (funcall goal s))))

(defun reazon--once-help (stream)
  "Pull at most one value out of STREAM."
  (cond
   ((null stream) '())
   ((functionp stream)
    (lambda () (reazon--once-help (funcall stream))))
   (t `(,(car stream)))))

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

(defun reazon--call-with-fresh (names function)
  "Call FUNCTION with a variable created from NAMES.
names: list of symbols
function: variables -> goal, e.g. (lambda (fruit) (reazon-== 'plum fruit))"
  (declare (indent 1))
  (apply function (mapcar #'reazon--make-variable names)))

;; -- Macros --

;; Reazon is implemented in such a way that everything could in
;; principle be run without using any macros. No "work" is being done
;; by the macros; they are strictly wrappers around the functions
;; defined above.

(defmacro reazon-disj (&rest goals)
  "Chain together GOALS with `reazon--disj-2' if there are any, else fail."
  (pcase goals
    ('() '#'reazon-!U)
    (`(,goal) goal)
    (`(,goal . ,rest)
     `(reazon--disj-2 ,goal (reazon-disj ,@rest)))))

(defmacro reazon-conj (&rest goals)
  "Chain together GOALS with `reazon--conj-2' if there are any, else succeed."
  (pcase goals
    ('() '#'reazon-!S)
    (`(,goal) goal)
    (`(,goal . ,rest)
     `(reazon--conj-2 ,goal (reazon-conj ,@rest)))))

(defmacro reazon-fresh (vars &rest goals)
  "Bind each of VARS as a fresh variable and run the conjunction of GOALS."
  (declare (indent 1))
  (if (null vars)
      `(reazon-conj ,@goals)
    `(reazon--call-with-fresh (mapcar #'gensym ',vars)
       (lambda (,@vars)
         (reazon-conj ,@goals)))))

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
      `(let ((,var (reazon--make-variable ',var))
             (reazon--stop-time (and reazon-timeout (+ reazon-timeout (float-time)))))
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

(defmacro reazon-conde (&rest clauses)
  "Chain together CLAUSES as a disjunction of conjunctions."
  `(reazon-disj ,@(mapcar (lambda (arm) `(reazon-conj ,@arm)) clauses)))

(defmacro reazon-conda (&rest clauses)
  "Run only the first clause in CLAUSES whose head succeeds.
Also known as committed choice. This operator is impure."
  (pcase clauses
    ('() '#'reazon-!U)
    (`(,clause) `(reazon-conj ,@clause))
    (`((,head . ,body) . ,rest)
     `(reazon--ifte ,head
        (reazon-conj ,@body)
        (reazon-conda ,@rest)))))

(defmacro reazon-condu (&rest clauses)
  "Run for just one value the first clause in CLAUSES whose head succeeds.
Also known as committed choice. This operator is impure."
  (pcase clauses
    ('() '#'reazon-!U)
    (`((,head . ,body) . ,rest)
     `(reazon-conda
       ((reazon--once ,head) ,@body)
       ,@rest))))

(defmacro reazon-defrel (name varlist &optional docstring &rest goals)
  "Define relation NAME with args VARLIST and body GOALS, and a DOCSTRING."
  (declare (indent 2) (doc-string 3))

  ;; keep this nasty docstring logic
  ;; away from the relation definition
  (if (stringp docstring)
      (setq docstring `(,docstring))
    (setq goals `(,docstring . ,goals)
          docstring nil))

  (let ((stream (gensym)))
    `(defun ,name ,varlist
       ,@docstring
       (lambda (,stream)
         (lambda ()
           (funcall (reazon-conj ,@goals) ,stream))))))

(reazon-defrel reazon-caro (p a)
  "A is the car of P."
  (reazon-fresh (d)
    (reazon-conso a d p)))

(reazon-defrel reazon-cdro (p d)
  "D is the cdr of P."
  (reazon-fresh (a)
    (reazon-conso a d p)))

(reazon-defrel reazon-conso (a d p)
  "P is a cons of A and D."
  (reazon-== p (cons a d)))

(reazon-defrel reazon-nullo (x)
  "X is null."
  (reazon-== x '()))

(reazon-defrel reazon-pairo (p)
  "P is a pair."
  (reazon-fresh (a d)
    (reazon-conso a d p)))

(reazon-defrel reazon-listo (s)
  "S is a proper list."
  (reazon-conde
   ((reazon-nullo s))
   ((reazon-pairo s)
    (reazon-fresh (d)
      (reazon-cdro s d)
      (reazon-listo d)))))

(reazon-defrel reazon-appendo (l p out)
  "L appended to P produces OUT."
  (reazon-conde
   ((reazon-nullo l) (reazon-== p out))
   ((reazon-fresh (a d res)
      (reazon-conso a d l)
      (reazon-conso a res out)
      (reazon-appendo d p res)))))

(reazon-defrel reazon-assqo (x s out)
  "Retrieving X from the alist S produces OUT."
  (reazon-conde
   ((reazon-nullo s) (reazon-nullo out))
   ((reazon-fresh (key val rest)
      (reazon-== s `((,key . ,val) ,@rest))
      (reazon-conde
       ((reazon-== x key) (reazon-== out `(,key . ,val)))
       ((reazon-assqo x rest out)))))))

(reazon-defrel reazon-membero (x s)
  "X exists in the list S."
  (reazon-fresh (a d)
    (reazon-== s `(,a . ,d))
    (reazon-disj
     (reazon-== a x)
     (reazon-membero x d))))

(reazon-defrel reazon-precedeso (x y s)
  "X is the list of elements preceding Y in the list S."
  (reazon-fresh (a d)
    (reazon-conso a d s)
    (reazon-conde
     ((reazon-== x a) (reazon-membero y d))
     ((reazon-precedeso x y d)))))

(reazon-defrel reazon-immediately-precedeso (x y s)
  "X is the element immediately preceding Y in the list S."
  (reazon-fresh (a d)
    (reazon-conso a d s)
    (reazon-conde
     ((reazon-== x a) (reazon-caro d y))
     ((reazon-immediately-precedeso x y d)))))

(reazon-defrel reazon-adjacento (x y s)
  "X is an adjacent element to Y in the list S."
  (reazon-disj
   (reazon-immediately-precedeso x y s)
   (reazon-immediately-precedeso y x s)))

(reazon-defrel reazon-subseto (subset set)
  "SUBSET is a subset of SET."
  (reazon-disj
   (reazon-== subset '())
   (reazon-fresh (a d)
     (reazon-== subset `(,a . ,d))
     (reazon-membero a set)
     (reazon-subseto d set))))

(reazon-defrel reazon-set-equalo (s1 s2)
  "S1 contains the same set of elements as S2."
  (reazon-subseto s1 s2)
  (reazon-subseto s2 s1))


(provide 'reazon)
;;; reazon.el ends here
