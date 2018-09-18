;;; reazon-test-utils.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Drozd

;; Author: Nick Drozd <nicholasdrozd@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for Reazon tests. `reazon--with-variables' could
;; conceivably be moved to and used in the main Reazon file, while
;; something like `reazon--should-equal' would already be included in
;; ERT and I wouldn't have to implement it myself.

;;; Code:

(require 'ert)
(require 'reazon)

(defmacro reazon--with-variables (variables &rest body)
  "Evaluate BODY with VARIABLES as reazon--variables."
  (declare (indent 1))
  (let ((reazon--vars
         (mapcar (lambda (var)
                   `(,var (reazon--make-variable ',var)))
                 variables)))
    `(let (,@reazon--vars)
       ,@body)))

(defun  reazon--should-equal(expected &rest forms)
  "Assert that each form in FORMS equals EXPECTED."
  (declare (indent 1))
  (dolist (form forms)
    (should (equal form expected))))


(provide 'reazon-test-utils)
;;; reazon-test-utils.el ends here
