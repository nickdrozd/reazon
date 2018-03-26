;; -*- lexical-binding: t -*-

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
      (reason-walk (cadr association) substitution))
     (t variable))))

(defmacro reason-should-equal(f1 f2)
  "Assert that f1 and f2 are equal."
  `(should (equal ,f1 ,f2)))

(ert-deftest reason-walk-test ()
  (reason-with-variables (u v w x y z)
    (let ((sub-1 `((,z a) (,x ,w) (,y ,z)))
          (sub-2 `((,x b) (,z ,y) (,w (,x e ,z)) (,u ,w))))
      (reason-should-equal (reason-walk z sub-1) 'a)
      (reason-should-equal (reason-walk y sub-1) 'a)
      (reason-should-equal (reason-walk x sub-1) w)
      (reason-should-equal (reason-walk w sub-1) w)
      (reason-should-equal (reason-walk x sub-2) 'b)
      (reason-should-equal (reason-walk u sub-2) `(,x e ,z)))))

(defun reason-occurs-p (x v s)
  ""
  (let ((v (reason-walk v s)))
    (cond
     ((reason-variable-p v)
      (equal v x))
     ((consp v)
      (or (reason-occurs-p x (car v) s)
          (reason-occurs-p x (cadr v) s)))
     (t nil))))

(ert-deftest reason-occurs-test ()
  (reason-with-variables (x y)
    (should (reason-occurs-p x x '()))
    (should (reason-occurs-p x `(,y) `((,y ,x))))))

(defvar reason-false '!F "")

(defun reason-extend (x v s)
  ""
  (if (reason-occurs-p x v s)
      reason-false
    (cons `(,x ,v) s)))

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
     (reason-extend x `(,y) `((,y ,x))))
    (reason-should-equal
     (reason-walk y (reason-extend x 'e `((,z ,x) (,y ,z))))
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
          (reason-unify (cadr u) (cadr v) s))))
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
