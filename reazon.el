;;; reazon.el --- miniKanren implementation for Emacs  -*- lexical-binding: t; -*-

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

;; Reazon is an implmentation of the miniKanren language for Emacs. It
;; provides an interface for writing an running relational programs.
;; That interface consists of the following macros:

;; * reazon-defrel
;; * reazon-conde
;; * reazon-fresh
;; * reazon-run
;; * reazon-run*

;;; Code:

;; variables

(defun reazon--make-variable (name)
  "Create a unique variable from NAME."
  (vector name))

(defun reazon--variable-p (var)
  "Check whether VAR is a variable."
  (vector-or-char-table-p var))

;; substitutions

(defun reazon--walk (variable substitution)
  "Return the value associated with VARIABLE in
SUBSTITUTION if there is one, else VARIABLE."
  (let ((association (and (reazon--variable-p variable)
                          (assoc variable substitution))))
    (cond
     ((consp association)
      (reazon--walk (cdr association) substitution))
     (t variable))))

(defun reazon--walk* (v s)
  ""
  (let ((v (reazon--walk v s)))
    (cond
     ((reazon--variable-p v)
      v)
     ((consp v)
      (cons
       (reazon--walk* (car v) s)
       (reazon--walk* (cdr v) s)))
     (t
      v))))

(defun reazon--occurs-p (x v s)
  ""
  (let ((v (reazon--walk v s)))
    (cond
     ((reazon--variable-p v)
      (equal v x))
     ((consp v)
      (or (reazon--occurs-p x (car v) s)
          (reazon--occurs-p x (cdr v) s)))
     (t nil))))

(defvar reazon--false '!F "")

(defun reazon--extend (x v s)
  ""
  (if (reazon--occurs-p x v s)
      reazon--false
    (cons `(,x . ,v) s)))

;; unification

(defun reazon--unify (u v s)
  ""
  (let ((u (reazon--walk u s))
        (v (reazon--walk v s)))
    (cond
     ((equal u v)
      s)
     ((reazon--variable-p u)
      (reazon--extend u v s))
     ((reazon--variable-p v)
      (reazon--extend v u s))
     ((and (consp u) (consp v))
      (let ((s (reazon--unify (car u) (car v) s)))
        (if (equal s reazon--false)
            reazon--false
          (reazon--unify (cdr u) (cdr v) s))))
     (t reazon--false))))

(defun ||| (u v)
  ""
  (lambda (s)
    (let ((s (reazon--unify u v s)))
      (if (equal s reazon--false)
          '()
        `(,s)))))

(defun !S (s)
  ""
  `(,s))

(defun !U (_s)
  ""
  '())

;; reification

(defun reazon--reify-name (number)
  "Return the symbol '_$NUMBER."
  (intern (concat "_" (number-to-string number))))

(defun reazon--reify-s (v r)
  ""
  (let ((v (reazon--walk v r)))
    (cond
     ((reazon--variable-p v)
      (let ((rn (reazon--reify-name (length r))))
        (reazon--extend v rn r)))
     ((consp v)
      (let ((r (reazon--reify-s (car v) r)))
        (reazon--reify-s (cdr v) r)))
     (t
      r))))

(defun reazon--reify (v)
  ""
  (lambda (s)
    (let ((v (reazon--walk* v s)))
      (let ((r (reazon--reify-s v '())))
        (reazon--walk* v r)))))

(defun reazon--call/fresh (name f)
  "Returns a goal that has access to a variable created from NAME.
f: variable -> goal, e.g. (lambda (fruit) (||| 'plum fruit))"
  (declare (indent 1))
  (funcall f (reazon--make-variable name)))

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

(defun reazon--pull (s)
  ""
  (cond
   ((null s) nil)
   ((functionp s) (reazon--pull (funcall s)))
   (t s)))

(defun reazon--take (n s)
  ""
  (declare (indent 1))
  (if (null s)
      nil
    (cons (car s)
          (if (and n (= n 1))
              nil
            (reazon--take (and n (1- n))
              (reazon--pull (cdr s)))))))

;; goals

(defun reazon--disj-2 (g1 g2)
  ""
  (lambda (s)
    (reazon--append (funcall g1 s) (funcall g2 s))))

(defun reazon--append-map (g s)
  ""
  (cond
   ((null s) nil)
   ((functionp s) (lambda () (reazon--append-map g (funcall s))))
   (t (reazon--append (funcall g (car s))
               (reazon--append-map g (cdr s))))))

(defun reazon--conj-2 (g1 g2)
  ""
  (lambda (s)
    (reazon--append-map g2 (funcall g1 s))))

(defun reazon--run-goal (g)
  ""
  (reazon--pull (funcall g nil)))

;; macros

(defmacro reazon-disj (&rest goals)
  ""
  (pcase (length goals)
    (0 `!U)
    (1 (car goals))
    (_ `(reazon--disj-2 ,(car goals) (reazon-disj ,@(cdr goals))))))

(defmacro reazon-conj (&rest goals)
  ""
  (pcase (length goals)
    (0 `!S)
    (1 (car goals))
    (_ `(reazon--conj-2 ,(car goals) (reazon-conj ,@(cdr goals))))))

(defmacro reazon-fresh (vars &rest goals)
  ""
  (declare (indent 1))
  (if (null vars)
      `(reazon-conj ,@goals)
    (let ((var (car vars)))
      `(reazon--call/fresh (gensym)
         (lambda (,var)
           (reazon-fresh ,(cdr vars)
             ,@goals))))))

(defmacro reazon-run (n var &rest goals)
  ""
  (declare (indent 2))
  (if (listp var)
      (let ((q (gensym)))
        `(reazon-run ,n ,q
           (reazon-fresh ,var
             (||| (list ,@var) ,q)
             ,@goals)))
    `(let ((,var (reazon--make-variable ',var)))
       (mapcar
        (reazon--reify ,var)
        (reazon--take ,n
          (reazon--run-goal (reazon-conj ,@goals)))))))

(defmacro reazon-run* (q &rest goals)
  ""
  (declare (indent 1))
  `(reazon-run nil ,q
     ,@goals))

;; do all the goal lists get a conj, or just the first one?
(defmacro reazon-conde (&rest goal-lists)
  ""
  `(reazon-disj ,@(mapcar (lambda (arm) `(reazon-conj ,@arm)) goal-lists)))

(defmacro reazon-defrel (name varlist &rest goals)
  ""
  (declare (indent 2))
  (let ((s (gensym)))
    `(defun ,name ,varlist
       (lambda (,s)
         (lambda ()
           (funcall (reazon-conj ,@goals) ,s))))))

(reazon-defrel reazon-car-o (p a)
  (reazon-fresh (d)
    (reazon-cons-o a d p)))

(reazon-defrel reazon-cdr-o (p d)
  (reazon-fresh (a)
    (reazon-cons-o a d p)))

(reazon-defrel reazon-cons-o (a d p)
  (||| p (cons a d)))

(reazon-defrel reazon-null-o (x)
  (||| x '()))

(reazon-defrel reazon-pair-o (p)
  (reazon-fresh (a d)
    (reazon-cons-o a d p)))

(reazon-defrel reazon-append-o (l p out)
  (reazon-conde
   ((reazon-null-o l) (||| p out))
   ((reazon-fresh (a d res)
      (reazon-cons-o a d l)
      (reazon-cons-o a res out)
      (reazon-append-o d p res)))))

;; Bookkeeping

(defvar reazon--public-prefix "reazon-")

(defun reazon/install-aliases ()
  "Bind alias \"blarg\" to \"reazon-blarg\" for every other public reazon callable.
The implementation assumes that the Reazon namespace marker is the
typical \"reazon-\". To avoid binding itself, this function is named
with \"reazon/\"."
  (interactive)
  (mapatoms
   (lambda (sym)
     (let* ((public-prefix reazon--public-prefix)
            (public-prefix-regexp (concat public-prefix "[^-]"))
            (prefixed-name (symbol-name sym)))
       (when (string-match public-prefix-regexp prefixed-name)
         (reazon--defalias sym))))))

(defun reazon--defalias (sym)
  (let* ((base-name (string-remove-prefix reazon--public-prefix (symbol-name sym)))
         (alias (intern base-name)))
    (defalias alias sym)))


(provide 'reazon)
;;; reazon.el ends here
