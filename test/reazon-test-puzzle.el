;;; reazon-test-puzzle.el ---                         -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'reazon-test-utils)

(ert-deftest reazon-test-puzzle-yachts ()
  ;; Mary Ann's father has a yacht and so has each of his four
  ;; friends. They are: Mr. Moore, Colonel Downing, Mr. Hall, Sir
  ;; Barnacle Hood, and Dr. Parker. Each of the five also has one
  ;; daughter and each has named his yacht after a daughter of one of
  ;; the others. Sir Barnacle's yacht is the Gabrielle, Mr. Moore owns
  ;; the Lorna; Mr. Hall the Rosalind. The Melissa, owned by Colonel
  ;; Downing, is named after Sir Barnacle's daughter. Gabrielle's
  ;; father owns the yacht that is named after Dr. Parker's daughter.
  ;; Who is Lorna's father?
  (let ((names '(mary-anne gabrielle lorna rosalind melissa)))
    (reazon-run* q
      (reazon-fresh (a b c d)
        (reazon-== q `((barnacle-hood melissa gabrielle) ,a ,b ,c ,d))
        (reazon-fresh (daughter)
          (reazon-== a `(moore ,daughter lorna))
          (reazon-membero daughter names))
        (reazon-fresh (daughter)
          (reazon-== b `(downing ,daughter melissa))
          (reazon-membero daughter names))
        (reazon-fresh (daughter)
          (reazon-== c `(hall ,daughter rosalind))
          (reazon-membero daughter names))
        (reazon-fresh (common-name yacht other other-father)
          (reazon-== d `(parker ,common-name ,yacht))
          (reazon-== other `(,other-father gabrielle ,common-name))
          (reazon-membero common-name names)
          (reazon-membero other q))))))

(ert-deftest reazon-test-puzzle-zebra ()
  (reazon--should-equal '(((yel nrw wat koo fox)
                           (blu ukr tea chs hrs)
                           (red eng mlk olg snl)
                           (ivr spn ojj lks dog)
                           (grn jap cof prl zeb)))
    (reazon-run* q
      ;; Represent houses as quintuples: (color nationality drink smoke pet)

      ;; 1 There are five houses.
      (reazon-fresh (a b c d e)
        (reazon-== q `(,a ,b ,c ,d ,e))

        ;; Ordering-related clues

        ;; 10 The Norwegian lives in the first house.
        (reazon-fresh (col drn smk pet)
          (reazon-== a `(,col nrw ,drn ,smk ,pet)))

        ;; 15 The Norwegian lives next to the blue house.
        (reazon-fresh (nat drn smk pet)
          (reazon-== b `(blu ,nat ,drn ,smk ,pet)))

        ;; 9 Milk is drunk in the middle house.
        (reazon-fresh (col nat smk pet)
          (reazon-== c `(,col ,nat mlk ,smk ,pet)))

        ;; 6 The green house is immediately to the right of the ivory house.
        ;; 4 Coffee is drunk in the green house.
        (reazon-fresh (ho1 ho2 nt1 nt2 dr1 sm1 sm2 pt1 pt2)
          (reazon-== ho1 `(ivr ,nt1 ,dr1 ,sm1 ,pt1))
          (reazon-== ho2 `(grn ,nt2 cof ,sm2 ,pt2))
          (reazon-immediately-precedeso ho1 ho2 q))

        ;; 11 The man who smokes Chesterfields lives in the house next to the man with the fox.
        (reazon-fresh (ho1 ho2 co1 co2 nt1 nt2 dr1 dr2 sm2 pt1)
          (reazon-== ho1 `(,co1 ,nt1 ,dr1 chs ,pt1))
          (reazon-== ho2 `(,co2 ,nt2 ,dr2 ,sm2 fox))
          (reazon-adjacento ho1 ho2 q))

        ;; 12 Kools are smoked in the house next to the house where the horse is kept.
        ;; 8 Kools are smoked in the yellow house.
        (reazon-fresh (ho1 ho2 co2 nt1 nt2 dr1 dr2 sm2 pt1)
          (reazon-== ho1 `(yel ,nt1 ,dr1 koo ,pt1))
          (reazon-== ho2 `(,co2 ,nt2 ,dr2 ,sm2 hrs))
          (reazon-adjacento ho1 ho2 q)))

      ;; General clues

      ;; 2 The Englishman lives in the red house.
      (reazon-fresh (hou drn smk pet)
        (reazon-== hou `(red eng ,drn ,smk ,pet))
        (reazon-membero hou q))

      ;; 3 The Spaniard owns the dog.
      (reazon-fresh (hou col drn smk)
        (reazon-== hou `(,col spn ,drn ,smk dog))
        (reazon-membero hou q))

      ;; 5 The Ukrainian drinks tea.
      (reazon-fresh (hou col smk pet)
        (reazon-== hou `(,col ukr tea ,smk ,pet))
        (reazon-membero hou q))

      ;; 7 The Old Gold smoker owns snails.
      (reazon-fresh (hou col nat drn)
        (reazon-== hou `(,col ,nat ,drn olg snl))
        (reazon-membero hou q))

      ;; 13 The Lucky Strike smoker drinks orange juice.
      (reazon-fresh (hou col nat pet)
        (reazon-== hou `(,col ,nat ojj lks ,pet))
        (reazon-membero hou q))

      ;; 14 The Japanese smokes Parliaments.
      (reazon-fresh (hou col drn pet)
        (reazon-== hou `(,col jap ,drn prl ,pet))
        (reazon-membero hou q))

      ;; Now, who drinks water?
      (reazon-fresh (hou col nat smk pet)
        (reazon-== hou `(,col ,nat wat ,smk ,pet))
        (reazon-membero hou q))

      ;; Who owns the zebra?
      (reazon-fresh (hou col nat drn smk)
        (reazon-== hou `(,col ,nat ,drn ,smk zeb))
        (reazon-membero hou q)))))


(provide 'reazon-test-puzzle)
;;; reazon-test-puzzle.el ends here
