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

(ert-deftest reason-walk-test ()
  (reason-with-variables (u v w x y z)
    (let ((sub-1 `((,z a) (,x ,w) (,y ,z)))
          (sub-2 `((,x b) (,z ,y) (,w (,x e ,z)) (,u ,w))))
      (should (equal (reason-walk z sub-1) 'a))
      (should (equal (reason-walk y sub-1) 'a))
      (should (equal (reason-walk x sub-1) w))
      (should (equal (reason-walk w sub-1) w))
      (should (equal (reason-walk x sub-2) 'b))
      (should (equal (reason-walk u sub-2) `(,x e ,z))))))
