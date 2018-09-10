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

(ert-deftest reazon-test-puzzle-interrogation ()
  ;; Three men were once arrested for a crime which beyond a shadow of
  ;; a doubt had been committed by one of them. Preliminary
  ;; questioning disclosed the curious fact that one of the suspects
  ;; was a highly respected judge, one just an average citizen, and
  ;; one a notorious crook. In what follows they will be referred to
  ;; as Brown, Jones, and Smith, though not necessarily respectively.
  ;; Each man made two statements to the police, which were in effect
  ;;
  ;; Brown:
  ;;   I didn't do it.
  ;;   Jones didn't do it.
  ;;
  ;; Jones:
  ;;   Brown didn't do it.
  ;;   Smith did it.
  ;;
  ;; Smith:
  ;;   I didn't do it.
  ;;   Brown did it.
  ;;
  ;; Further investigation showed, as might have been expected, that
  ;; both statements made by the judge were true, both statements made
  ;; by the criminal were false, and of the two statements made by the
  ;; average man one was true and one was false.
  ;;
  ;; Which of the three men was the judge, the average citizen, the
  ;; crook? And who committed the crime?

  (reazon--should-equal '(((brown a y) (jones c n) (smith j n)))
    (reazon-run* q
      ;; s(tatus): j(udge), a(verage citizen), c(rook)
      ;; v(erdict): y(es), n(o)
      (reazon-fresh (bs bv js jv ss sv)

        (reazon-== q `((brown ,bs ,bv) (jones ,js ,jv) (smith ,ss ,sv)))

        (reazon-subseto '(j a c) `(,bs ,js ,ss))

        ;; This feels inelegant.
        (reazon-membero `(,bv ,jv ,sv) '((y n n) (n y n) (n n y)))

        (reazon-conde
         ;; Brown is the judge.
         ((reazon-== `(,bs ,bv jv) '(j n n)))
         ;; Brown is the crook.
         ((reazon-== `(,bs ,bv ,jv) '(c y y)))
         ;; Brown is the average citizen.
         ((reazon-== bs 'a)
          (reazon-disj
           (reazon-== `(,bv ,jv) '(y n))
           (reazon-== `(,bv ,jv) '(n y)))))

        (reazon-conde
         ;; Jones is the judge.
         ((reazon-== `(,js ,bv ,sv) '(j n y)))
         ;; Jones is the crook.
         ((reazon-== `(,js ,bv ,sv) '(c y n)))
         ;; Jones is the average citizen.
         ((reazon-== js 'a)
          (reazon-disj
           (reazon-== `(,bv ,sv) '(n n))
           (reazon-== `(,bv ,sv) '(y y)))))

        (reazon-conde
         ;; Smith is the judge.
         ((reazon-== `(,ss ,sv ,bv) '(j n y)))
         ;; Smith is the crook.
         ((reazon-== `(,ss ,sv ,bv) '(c y n)))
         ;; Smith is the average citizen.
         ((reazon-== ss 'a)
          (reazon-disj
           (reazon-== `(,sv ,bv) '(n n))
           (reazon-== `(,sv ,bv) '(y n)))))))))

(ert-deftest reazon-test-puzzle-exam ()
  ;; Five schoolgirls sat for an examination. Their parents -- so they
  ;; thought -- showed an undue degree of interest in the result. They
  ;; therefore agreed that, in writing home about the examination,
  ;; each girl should make one true statement and one untrue one. The
  ;; following are the relevant passages from their letters:

  ;; > Betty: "Kitty was second in the examination. I was only third."
  ;; > Ethel: "You'll be glad to hear that I was on top. Joan was second."
  ;; > Joan: "I was third, and poor old Ethel was bottom."
  ;; > Kitty: "I came out second. Mary was only fourth."
  ;; > Mary: "I was fourth. Top place was taken by Betty."

  ;; What in fact was the order in which the five girls were placed?
  (reazon--should-equal '((kitty joan betty mary ethel))
    (reazon-run* q
      (reazon-fresh (a b c d e)
        ;; Betty: "Kitty was second in the examination. I was only third."
        (reazon-disj
         (reazon-== q `(,a kitty ,c ,d ,e))
         (reazon-== q `(,a ,b betty ,d ,e)))
        ;; Ethel: "You'll be glad to hear that I was on top. Joan was second."
        (reazon-disj
         (reazon-== q `(ethel ,b ,c ,d ,e))
         (reazon-== q `(,a joan ,c ,d ,e)))
        ;; Joan: "I was third, and poor old Ethel was bottom."
        (reazon-disj
         (reazon-== q `(,a ,b joan ,d ,e))
         (reazon-== q `(,a ,b ,c ,d ethel)))
        ;; Kitty: "I came out second. Mary was only fourth."
        (reazon-disj
         ;; Explicity enumerate possibilities to simulate negation.
         (reazon-disj
          (reazon-== q `(mary kitty ,c ,d ,e))
          (reazon-== q `(,a kitty mary ,d ,e))
          (reazon-== q `(,a kitty ,c ,d mary)))
         (reazon-disj
          (reazon-== q `(kitty ,b ,c mary ,e))
          (reazon-== q `(,a ,b kitty mary ,e))
          (reazon-== q `(,a ,b ,c mary kitty))))
        ;; Mary: "I was fourth. Top place was taken by Betty."
        (reazon-disj
         (reazon-== q `(,a ,b ,c mary ,e))
         (reazon-== q `(betty ,b ,c ,d ,e))))
      (reazon-subseto '(betty ethel joan kitty mary) q))))

(ert-deftest reazon-test-puzzle-ages ()
  ;; Tom and Betty have the same birthday and are both in their
  ;; twenties. He is four times as old as she was when he was three
  ;; times as old as she was when he was twice as old as she was. How
  ;; old are they?
  (reazon--should-equal '((24 21))
    (reazon-run* (tom betty)
      (reazon-subseto `(,tom ,betty) (number-sequence 20 29))
      (reazon-project (tom betty)
        (reazon-fresh (diff)
          (reazon-== diff (- tom betty))
          (reazon-project (diff)
            (reazon-fresh (b1)
              (reazon-== t (= 0 (mod tom 4)))
              (reazon-== b1 (/ tom 4))
              (reazon-project (b1)
                (reazon-fresh (t1)
                  (reazon-== t1 (+ b1 diff))
                  (reazon-project (t1)
                    (reazon-fresh (b2)
                      (reazon-== t (= 0 (mod t1 3)))
                      (reazon-== b2 (/ t1 3))
                      (reazon-project (b2)
                        (reazon-fresh (t2)
                          (reazon-== t2 (+ b2 diff))
                          (reazon-== t2 (* b2 2)))))))))))))
    ;; Same as the last one, but with the fresh variables hoisted.
    (reazon-run* (tom betty)
      (reazon-subseto `(,tom ,betty) (number-sequence 20 29))
      (reazon-fresh (diff b1 t1 b2 t2)
        (reazon-project (tom betty)
          (reazon-== diff (- tom betty))
          (reazon-project (diff)
            (reazon-== t (= 0 (mod tom 4)))
            (reazon-== b1 (/ tom 4))
            (reazon-project (b1)
              (reazon-== t1 (+ b1 diff))
              (reazon-project (t1)
                (reazon-== t (= 0 (mod t1 3)))
                (reazon-== b2 (/ t1 3))
                (reazon-project (b2)
                  (reazon-== t2 (+ b2 diff))
                  (reazon-== t2 (* b2 2)))))))))))


(provide 'reazon-test-puzzle)
;;; reazon-test-puzzle.el ends here
