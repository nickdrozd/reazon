;; -*- lexical-binding: t -*-

;; variables

(defun reason-make-variable (name)
  "Create a unique variable from NAME."
  (vector name))

(defun reason-variable-p (var)
  "Check whether VAR is a variable."
  (vector-or-char-table-p var))

(ert-deftest reason-variable-test ()
  (should (reason-variable-p (reason-make-variable 'x)))
  (should-not (reason-variable-p 'x)))
