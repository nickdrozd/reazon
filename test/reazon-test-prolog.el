;;; reazon-test-prolog.el ---                        -*- lexical-binding: t; -*-

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

;; Tests for Prolog-style database queries.

;;; Code:

(require 'reazon-test-utils)

(reazon-defrel reazon--test-likes (a b)
  (reazon-conde
   ((reazon-== a 'kim) (reazon-== b 'robin))
   ((reazon-== a 'sandy) (reazon-== b 'lee))
   ((reazon-== a 'sandy) (reazon-== b 'kim))
   ((reazon-== a 'robin) (reazon-== b 'cats))
   ;; ((reazon-== a 'sandy) (reazon--test-likes b 'cats))
   ((reazon-fresh (x)
      (reazon-== a 'sandy)
      (reazon-== b x)
      (reazon--test-likes x 'cats)))
   ;; ((reazon-== a 'kim) (reazon--test-likes b 'lee) (reazon--test-likes b 'kim))
   ((reazon-fresh (x)
      (reazon-== a 'kim)
      (reazon-== b x)
      (reazon--test-likes x 'lee)
      (reazon--test-likes x 'kim)))
   ;; ((reazon-== a b))
   ((reazon-fresh (x)
      (reazon-== a x)
      (reazon-== b x)))))

(ert-deftest reazon-test-prolog-likes ()
  "This is an example from the chapter on Prolog in Norvig's PAIP."
  (reazon--should-equal '(lee kim sandy robin sandy cats)
    (reazon-run* who
      (reazon--test-likes 'sandy who)))
  (reazon--should-equal '(sandy sandy kim)
    (reazon-run* who
      (reazon--test-likes who 'sandy)))
  (reazon--should-equal '()
    (reazon-run* q
      (reazon--test-likes 'robin 'lee)))
  (reazon--should-equal
      '((sandy kim)
        (_0 _0)
        (sandy sandy)
        (sandy sandy)
        (sandy sandy)
        (kim sandy))
    (reazon-run* (x y)
      (reazon--test-likes x y)
      (reazon--test-likes y x))))

;; From the SICP section on Prolog (4.4), the personnel records
;; database for "Microshaft, a thriving high-technology company in the
;; Boston area".

(reazon-defrel reazon--microshaft-address (name address)
  (reazon-membero
   `(,name ,address)
   '(((Bitdiddle Ben) (Slumerville (Ridge Road) 10))
     ((Hacker Alyssa P) (Cambridge (Mass Ave) 78))
     ((Fect Cy D) (Cambridge (Ames Street) 3))
     ((Tweakit Lem E) (Boston (Bay State Road) 22))
     ((Reasoner Louis) (Slumerville (Pine Tree Road) 80))
     ((Warbucks Oliver) (Swellesley (Top Heap Road)))
     ((Scrooge Eben) (Weston (Shady Lane) 10))
     ((Cratchet Robert) (Allston (N Harvard Street) 16))
     ((Aull DeWitt) (Slumerville (Onion Square) 5)))))

(reazon-defrel reazon--microshaft-job (name job)
  (reazon-membero
   `(,name ,job)
   '(((Bitdiddle Ben) (computer wizard))
     ((Hacker Alyssa P) (computer programmer))
     ((Fect Cy D) (computer programmer))
     ((Tweakit Lem E) (computer technician))
     ((Reasoner Louis) (computer programmer trainee))
     ((Warbucks Oliver) (administration big wheel))
     ((Scrooge Eben) (accounting chief accountant))
     ((Cratchet Robert) (accounting scrivener))
     ((Aull DeWitt) (administration secretary)))))

(reazon-defrel reazon--microshaft-salary (name salary)
  (reazon-membero
   `(,name ,salary)
   '(((Bitdiddle Ben) 60000)
     ((Hacker Alyssa P) 40000)
     ((Fect Cy D) 35000)
     ((Tweakit Lem E) 25000)
     ((Reasoner Louis) 30000)
     ((Warbucks Oliver) 150000)
     ((Scrooge Eben) 75000)
     ((Cratchet Robert) 18000)
     ((Aull DeWitt) 25000))))

(reazon-defrel reazon--microshaft-supervisor (name supervisor)
  (reazon-membero
   `(,name ,supervisor)
   '(((Bitdiddle Ben) (Warbucks Oliver))
     ((Hacker Alyssa P) (Bitdiddle Ben))
     ((Fect Cy D) (Bitdiddle Ben))
     ((Tweakit Lem E) (Bitdiddle Ben))
     ((Reasoner Louis) (Hacker Alyssa P))
     ((Scrooge Eben) (Warbucks Oliver))
     ((Cratchet Robert) (Scrooge Eben))
     ((Aull DeWitt) (Warbucks Oliver)))))

(reazon-defrel reazon--microshaft-lives-near (a b)
  (reazon-fresh (town rest-a rest-b)
    (reazon--microshaft-address a `(,town . ,rest-a))
    (reazon--microshaft-address b `(,town . ,rest-b))
    (reazon-project (a b)
      (reazon-== nil (equal a b)))))

(reazon-defrel reazon--microshaft-wheel (wheel)
  (reazon-fresh (middle-manager underling)
    (reazon--microshaft-supervisor middle-manager wheel)
    (reazon--microshaft-supervisor underling middle-manager)))

(reazon-defrel reazon--microshaft-meeting (division time)
  (reazon-membero
   `(,division ,time)
   '((accounting (Monday 9am))
     (administration (Monday 10am))
     (computer (Wednesday 3pm))
     (administration (Friday 1pm))
     (whole-company (Wednesday 4pm)))))

(reazon-defrel reazon--microshaft-meeting-time (name day-and-time)
  (reazon-fresh (division job)
    (reazon-disj
     (reazon-== division 'whole-company)
     (reazon--microshaft-job name `(,division . ,job)))
    (reazon--microshaft-meeting division day-and-time)))

(ert-deftest reazon-test-prolog-microshaft ()
  ;; The addresses of all the computer programmers
  (reazon--should-equal '(((Hacker Alyssa P) (Cambridge (Mass Ave) 78))
                          ((Fect Cy D) (Cambridge (Ames Street) 3)))
    (reazon-run* (name address)
      (reazon--microshaft-job name '(computer programmer))
      (reazon--microshaft-address name address)))
  ;; Everyone supervised by Ben Bitdiddle or Alyssa P Hacker
  (reazon--should-equal '(((Hacker Alyssa P) (Bitdiddle Ben))
                          ((Fect Cy D) (Bitdiddle Ben))
                          ((Tweakit Lem E) (Bitdiddle Ben))
                          ((Reasoner Louis) (Hacker Alyssa P)))
    (reazon-run* (name supervisor)
      (reazon-disj
       (reazon-== supervisor '(Bitdiddle Ben))
       (reazon-== supervisor '(Hacker Alyssa P)))
      (reazon--microshaft-supervisor name supervisor)))
  ;; Everyone supervised by Ben Bitdiddle who is not a computer programmer
  (reazon--should-equal '(((Tweakit Lem E) (computer technician)))
    (reazon-run* (name job)
      (reazon--microshaft-supervisor name '(Bitdiddle Ben))
      (reazon--microshaft-job name job)
      (reazon-project (job)
        (reazon-== nil (equal job '(computer programmer))))))
  ;; Everyone who makes more than 30000
  (reazon--should-equal '(((Bitdiddle Ben) 60000)
                          ((Hacker Alyssa P) 40000)
                          ((Fect Cy D) 35000)
                          ((Warbucks Oliver) 150000)
                          ((Scrooge Eben) 75000))
    (reazon-run* (name salary)
      (reazon--microshaft-salary name salary)
      (reazon-project (salary)
        (reazon-== t (> salary 30000)))))
  ;; Everyone who lives near Ben Bitdiddle
  (reazon--should-equal '((Reasoner Louis) (Aull DeWitt))
    (reazon-run* name
      (reazon--microshaft-lives-near name '(Bitdiddle Ben))))
  ;; All the wheels (see SICP exercise 4.65)
  (reazon--should-equal '((Warbucks Oliver)
                          (Warbucks Oliver)
                          (Warbucks Oliver)
                          (Bitdiddle Ben)
                          (Warbucks Oliver))
    (reazon-run* name
      (reazon--microshaft-wheel name)))
  ;; All the Friday meetings (SICP ex 4.59a)
  (reazon--should-equal '((administration (Friday 1pm)))
    (reazon-run* (division time)
      (reazon-caro time 'Friday)
      (reazon--microshaft-meeting division time)))
  ;; The times of Alyssa's Wednesday meetings (SICP ex 4.59c)
  (reazon--should-equal '((4pm) (3pm))
    (reazon-run* (time)
      (reazon--microshaft-meeting-time '(Hacker Alyssa P) `(Wednesday ,time)))))


(provide 'reazon-test-prolog)
;;; reazon-test-prolog.el ends here
