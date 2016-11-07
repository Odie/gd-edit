;; -------------------------------------------------------------------
;; clj-fuzzy Double Metaphone
;; -------------------------------------------------------------------
;;
;;
;;   Author: PLIQUE Guillaume (Yomguithereal)
;;   Version: 0.1
;;
(ns clj-fuzzy.double-metaphone
  (:require clojure.string)
  (:use [clj-fuzzy.helpers :only [re-test?
                                  slice
                                  in?]]))

;; Utilities
(defn- slavo-germanic? [string] (re-test? #"W|K|CZ|WITZ" string))

(defn- vowel? [string] (re-test? #"^A|E|I|O|U|Y$" string))

(defn- conj-if-not-nil [array value]
  (if (nil? value)
    array
    (conj array value)))

(def ^:private not-re-test? (complement re-test?))

;; Lookup functions
(defn- lookup-vowel [pos] (if (zero? pos) [:A :A 1] [nil nil 1]))

(defn- lookup-B [string pos]
  [:P :P (if (= "B" (slice string (inc pos) 1)) 2 1)])

(defn- lookup-C-cedilla [] [:S :S 1])

(defn- lookup-C-1 [] [:K :K 2])

(defn- lookup-C-2 [] [:S :S 2])

(defn- lookup-C-3 [string pos]
  (cond (and (pos? pos)
             (= "CHAE" (slice string pos 4))) [:K :X 2]
        (and (zero? pos)
             (or (in? (slice string (inc pos) 5) ["HARAC" "HARIS"])
                 (in? (slice string (inc pos) 3) ["HOR" "HYM" "HIA" "HEM"]))
             (not= "CHORE" (slice string 0 5))) [:K :K 2]
        (or (in? (slice string 0 4) ["VAN " "VON "])
            (= "SCH" (slice string 0 3))
            (in? (slice string (- pos 2) 6) ["ORCHES" "ARCHIT" "ORCHID"])
            (in? (slice string (+ pos 2) 1) ["T" "S"])
            (and (or (zero? pos)
                     (in? (slice string (dec pos) 1) ["A" "O" "U" "E"]))
                 (in? (slice string (+ pos 2) 1) ["L" "R" "N" "M" "B" "H" "F" "V" "W" " "]))) [:K :K 2]
        (pos? pos) [(if (= "MC" (slice string 0 2)) :K :X) :K 2]
        :else [:X :X 2]))

(defn- lookup-C-4 [] [:S :X 2])

(defn- lookup-C-5 [] [:X :X 3])

(defn- lookup-C-6 [string pos]
  (if (and (re-test? #"^I|E|H$" (slice string (+ pos 2) 1))
           (not= "HU" (slice string (+ pos 2) 2)))
    (if (or (and (= pos 1)
                 (= "A" (slice string (dec pos) 1)))
            (re-test? #"^UCCE(E|S)$" (slice string (dec pos) 5)))
      [:KS :KS 3]
      [:X :X 3])
    [:K :K 2]))

(defn- lookup-C-7 [] [:K :K 2])

(defn- lookup-C-8 [string pos]
  [:S (if (re-test? #"^CI(O|E|A)$" (slice string pos 3)) :X :S) 2])

(defn- lookup-C-9 [string pos]
  (if (re-test? #"^ (C|Q|G)$" (slice string (inc pos) 2))
    [:K :K 3]
    [:K :K (if (and (re-test? #"^C|K|Q$" (slice string (inc pos) 1))
                    (not (in? (slice string (inc pos) 2) ["CE" "CI"]))) 2 1)]))

(defn- lookup-C [string pos]
  (cond (and (> pos 1)
             (vowel? (slice string (- pos 2) 1))
             (= "ACH" (slice string (dec pos) 3))
             (not= "I" (slice string (+ pos 2) 1))
             (or (not= "E" (slice string (+ pos 2) 1))
                 (re-test? #"^(B|M)ACHER$" (slice string (- pos 2) 6)))) (lookup-C-1)
        (and (zero? pos) (= "CAESAR" (slice string pos 6))) (lookup-C-2)
        (= "CHIA" (slice string pos 4)) (lookup-C-1)
        (= "CH" (slice string pos 2)) (lookup-C-3 string pos)
        (and (= "CZ" (slice string pos 2)) (not= "WICZ" (slice string (- pos 2) 4))) (lookup-C-4)
        (= "CIA" (slice string (inc pos) 3)) (lookup-C-5)
        (and (= "CC" (slice string pos 2))
             (not (or (= pos 1) (= "M" (slice string 0 1))))) (lookup-C-6 string pos)
        (re-test? #"^C(K|G|Q)$" (slice string pos 2)) (lookup-C-7)
        (re-test? #"^C(I|E|Y)$" (slice string pos 2)) (lookup-C-8 string pos)
        :else (lookup-C-9 string pos)))

(defn- lookup-D [string pos]
  (if (= "DG" (slice string pos 2))
    (if (re-test? #"^I|E|Y$" (slice string (+ pos 2) 1))
      [:J :J 3]
      [:TK :TK 2])
    [:T :T (if (re-test? #"^D(T|D)$" (slice string pos 2)) 2 1)]))

(defn- lookup-F [string pos]
 [:F :F (if (= "F" (slice string (inc pos) 1)) 2 1)])

(defn lookup-G-1-bis [string pos]
  (cond (and (> pos 2)
             (= "U" (slice string (dec pos) 1))
             (re-test? #"^C|G|L|R|T$" (slice string (- pos 3) 1))) [:F :F 2]
        (and (pos? pos) (not= "I" (slice string (dec pos) 1))) [:K :K 2]
        :else [nil nil 2]))

(defn- lookup-G-1 [string pos]
  (cond (and (pos? pos)
             (not (vowel? (slice string (dec pos) 1)))) [:K :K 2]
        (zero? pos) (if (= "I" (slice string (+ pos 2) 1)) [:J :J 2] [:K :K 2])
        (or (and (> pos 1) (re-test? #"^B|H|D$" (slice string (- pos 2) 1)))
            (and (> pos 2) (re-test? #"^B|H|D$" (slice string (- pos 3) 1)))
            (and (> pos 3) (re-test? #"^B|H$" (slice string (- pos 4) 1)))) [nil nil 2]
        :else (lookup-G-1-bis string pos)))

(defn- lookup-G-2 [string pos]
  (if (and (= pos 1)
           (vowel? (slice string 0 1))
           (not (slavo-germanic? string)))
    [:KN :N 2]
    (if (and (not= "EY" (slice string (+ pos 2) 2))
             (not= "Y" (slice string (inc pos) 1))
             (not (slavo-germanic? string)))
      [:N :KN 2]
      [:KN :KN 2])))

(defn- lookup-G-3 [] [:KL :L 2])

(defn- lookup-G-4 [] [:K :J 2])

(defn- lookup-G-5 [] [:K :J 2])

(defn- lookup-G-6 [string pos]
  (if (or (re-test? #"^V(A|O)N $" (slice string 0 4))
          (= "SCH" (slice string 0 3))
          (= "ET" (slice string (inc pos) 2)))
    [:K :K 2]
    (if (= "IER " (slice string (inc pos) 4))
      [:J :J 2]
      [:J :K 2])))

(defn- lookup-G-7 [] [:K :K 2])

(defn- lookup-G-8 [] [:K :K 1])

(defn- lookup-G [string pos]
  (cond (= "H" (slice string (inc pos) 1)) (lookup-G-1 string pos)
        (= "N" (slice string (inc pos) 1)) (lookup-G-2 string pos)
        (and (= "LI" (slice string (inc pos) 2)) (not (slavo-germanic? string))) (lookup-G-3)
        (and (zero? pos)
             (or (= "Y" (slice string (inc pos) 1))
                 (re-test? #"^(E(S|P|B|L|Y|I|R)|I(B|L|N|E))$" (slice string (inc pos) 2)))) (lookup-G-4)
        (and (or (= "ER" (slice string (inc pos) 2)) (= "Y" (slice string (inc pos) 1)))
             (not-re-test? #"^(D|R|M)ANGER$" (slice string 0 6))
             (not-re-test? #"^E|I$" (slice string (dec pos) 1))
             (not-re-test? #"^(R|O)GY$" (slice string (dec pos) 3))) (lookup-G-5)
        (or (re-test? #"^E|I|Y$" (slice string (inc pos) 1))
            (re-test? #"^(A|O)GGI$" (slice string (dec pos) 4))) (lookup-G-6 string pos)
        (= "G" (slice string (inc pos) 1)) (lookup-G-7)
        :else (lookup-G-8)))

(defn- lookup-H [string pos]
  (if (and (or (zero? pos) (vowel? (slice string (dec pos) 1)))
           (vowel? (slice string (inc pos) 1)))
    [:H :H 2]
    [nil nil 1]))

(defn- lookup-J-1 [string pos]
  (if (or (and (zero? pos)
               (= " " (slice string (+ pos 4) 1)))
          (= "SAN " (slice string 0 4)))
    [:H :H 1]
    [:J :H 1]))

(defn- lookup-J-2-bis [current] [:J :H current])

(defn- lookup-J-2-ter [string pos lastp current]
  (if (= lastp pos)
    [:J nil current]
    (if (and (not-re-test? #"^L|T|K|S|N|M|B|Z$" (slice string (inc pos) 1))
             (not-re-test? #"^S|K|L$" (slice string (dec pos) 1)))
      [:J :J current]
      [nil nil current])))

(defn- lookup-J-2 [string pos lastp]
  (let [current (if (= "J" (slice string (inc pos) 1)) 2 1)]
    (if (and (zero? pos)
             (not= "JOSE" (slice string pos 4)))
      [:J :A current]
      (if (and (vowel? (slice string (dec pos) 1))
               (not (slavo-germanic? string))
               (re-test? #"^A|O$" (slice string (inc pos) 1)))
        (lookup-J-2-bis current)
        (lookup-J-2-ter string pos lastp current)))))

(defn- lookup-J [string pos lastp]
  (if (or (= "JOSE" (slice string pos 4)) (= "SAN " (slice string 0 4)))
    (lookup-J-1 string pos)
    (lookup-J-2 string pos lastp)))

(defn- lookup-K [string pos]
  [:K :K (if (= "K" (slice string (inc pos) 1)) 2 1)])

(defn- lookup-L-1 [string pos length lastp]
  (if (or (and (= (- length 3) pos) (re-test? #"^(ILL(O|A)|ALLE)$" (slice string (dec pos) 4)))
          (and (or (re-test? #"^(A|O)S$" (slice string (dec lastp) 2))
                   (re-test? #"^A|O$" (slice string lastp 1)))
               (= "ALLE" (slice string (dec pos) 4))))
    [:L nil 2]
    [:L :L 2]))

(defn- lookup-L [string pos length lastp]
  (if (= "L" (slice string (inc pos) 1))
    (lookup-L-1 string pos length lastp)
    [:L :L 1]))

(defn- lookup-M [string pos lastp]
  (if (or (and (= "UMB" (slice string (dec pos) 3))
               (or (= (dec lastp) pos)
                   (= "ER" (slice string (+ pos 2) 2))))
          (= "M" (slice string (inc pos) 1)))
    [:M :M 2]
    [:M :M 1]))

(defn- lookup-N [string pos]
  [:N :N (if (= "N" (slice string (inc pos) 1)) 2 1)])

(defn- lookup-NY [string pos]
  [:N :N 1])

(defn- lookup-P [string pos]
  (if (= "H" (slice string (inc pos) 1))
    [:F :F 2]
    [:P :P (if (re-test? #"^P|B$" (slice string (inc pos) 1)) 2 1)]))

(defn- lookup-Q [string pos]
  [:K :K (if (= "Q" (slice string (inc pos) 1)) 2 1)])

(defn- lookup-R [string pos lastp]
  (let [current (if (= "R" (slice string (inc pos) 1)) 2 1)]
    (if (and (= lastp pos)
             (not (slavo-germanic? string))
             (= "IE" (slice string (- pos 2) 2))
             (not-re-test? #"^M(E|A)$" (slice string (- pos 4) 2)))
      [nil :R current]
      [:R :R current])))

(defn- lookup-S-1 [] [nil nil 1])

(defn- lookup-S-2 [] [:X :S 1])

(defn- lookup-S-3 [string pos]
  (if (re-test? #"^H(EIM|OEK|OLM|OLZ)$" (slice string (inc pos) 4))
    [:S :S 2]
    [:X :X 2]))

(defn- lookup-S-4 [string]
  [:S (if (slavo-germanic? string) :S :X) 3])

(defn- lookup-S-5 [string pos]
  [:S :X (if (= "Z" (slice string (inc pos) 1)) 2 1)])

(defn- lookup-S-6 [string pos]
  (cond (= "H" (slice string (+ pos 2) 1)) (if (re-test? #"^OO|ER|EN|UY|ED|EM$" (slice string (+ pos 3) 2))
                                      [(if (re-test? #"^E(R|N)$" (slice string (+ pos 3) 2)) :X :SK) :SK 3]
                                      [:X (if (and (zero? pos)
                                                   (not (vowel? (slice string 3 1)))
                                                   (not= "W" (slice string (+ pos 3) 1))) :S :X) 3])
        (re-test? #"^I|E|Y$" (slice string (+ pos 2) 1)) [:S :S 3]
        :else [:SK :SK 3]))

(defn- lookup-S [string pos lastp]
  (cond (re-test? #"^(I|Y)SL$" (slice string (dec pos) 3)) (lookup-S-1)
        (and (zero? pos)
             (= "SUGAR" (slice string pos 5))) (lookup-S-2)
        (= "SH" (slice string pos 2)) (lookup-S-3 string pos)
        (or (re-test? #"^SI(O|A)$" (slice string pos 3))
            (= "SIAN" (slice string pos 4))) (lookup-S-4 string)
        (or (and (zero? pos)
                 (re-test? #"^M|N|L|W$" (slice string (inc pos) 1)))
            (= "Z" (slice string (inc pos) 1))) (lookup-S-5 string pos)
        (= "SC" (slice string pos 2)) (lookup-S-6 string pos)
        :else [(when-not (and (= lastp pos)
                              (re-test? #"^(A|O)I$" (slice string (- pos 2) 2)))
                 :S)
               :S
               (if (re-test? #"^S|Z$" (slice string (inc pos) 1)) 2 1)]))

(defn- lookup-T-1 [] [:X :X 3])

(defn- lookup-T-2 [string pos]
  (if (or (re-test? #"^(O|A)M$" (slice string (+ pos 2) 2))
          (re-test? #"^V(A|O)N " (slice string 0 4))
          (= "SCH" (slice string 0 3)))
    [:T :T 2]
    [:0 :T 2]))

(defn- lookup-T [string pos]
  (cond (= "TION" (slice string pos 4)) (lookup-T-1)
        (re-test? #"^T(IA|CH)$" (slice string pos 3)) (lookup-T-1)
        (or (= "TH" (slice string pos 2))
            (= "TTH" (slice string pos 3))) (lookup-T-2 string pos)
        :else [:T :T (if (re-test? #"^T|D$" (slice string (inc pos) 1)) 2 1)]))

(defn- lookup-V [string pos]
  [:F :F (if (= "V" (slice string (inc pos) 1)) 2 1)])

(defn- let-lookup-W [string pos]
  (if (and (zero? pos)
           (or (vowel? (slice string (inc pos) 1))
               (= "WH" (slice string pos 2))))
    ["A" (if (vowel? (slice string (inc pos) 1)) "F" "A")]
    [nil nil]))

(defn- lookup-W-1 [] [:R :R 2])

(defn- lookup-W-2 [pri sec]
  [(keyword pri) (keyword (str sec "F")) 1])

(defn- lookup-W-3 [pri sec]
  [(keyword (str pri "TS")) (keyword (str sec "FX")) 4])

(defn- lookup-W [string pos lastp]
  (if (= "WR" (slice string pos 2))
    (lookup-W-1)
    (let [[pri sec] (let-lookup-W string pos)]
      (cond (or (and (= lastp pos)
                     (vowel? (slice string (dec pos) 1)))
                (= "SCH" (slice string 0 3))
                (re-test? #"^EWSKI|EWSKY|OWSKI|OWSKY$" (slice string (dec pos) 5))) (lookup-W-2 pri sec)
            (re-test? #"^WI(C|T)Z$" (slice string pos 4)) (lookup-W-3 pri sec)
            :else [(keyword pri) (keyword sec) 1]))))

(defn- lookup-X [string pos lastp]
  (if (zero? pos)
    [:S :S 1]
    (let [current (if (re-test? #"^C|X$" (slice string (inc pos) 1)) 2 1)]
      (if-not (and (= lastp pos)
                   (or (re-test? #"^(I|E)AU$" (slice string (- pos 3) 3))
                       (re-test? #"^(A|O)U$" (slice string (- pos 2) 2))))
        [:KS :KS current]
        [nil nil current]))))

(defn- lookup-Z-1 [] [:J :J 2])

(defn- lookup-Z-2 [current] [:S :TS current])

(defn- lookup-Z-3 [current] [:S :S current])

(defn- lookup-Z [string pos]
  (if (="H" (slice string (inc pos) 1))
    (lookup-Z-1)
    (let [current (if (= "Z" (slice string (inc pos) 1)) 2 1)]
      (if (or (re-test? #"^Z(O|I|A)$" (slice string (inc pos) 2))
              (and (slavo-germanic? string)
                   (pos? pos)
                   (not= "T" (slice string (dec pos) 1))))
        (lookup-Z-2 current)
        (lookup-Z-3 current)))))

(defn lookup [string pos length lastp]
  (let [char-to-lookup (slice string pos 1)]
    (cond (vowel? char-to-lookup) (lookup-vowel pos)
          (= "B" char-to-lookup) (lookup-B string pos)
          (= "Ç" char-to-lookup) (lookup-C-cedilla)
          (= "C" char-to-lookup) (lookup-C string pos)
          (= "D" char-to-lookup) (lookup-D string pos)
          (= "F" char-to-lookup) (lookup-F string pos)
          (= "G" char-to-lookup) (lookup-G string pos)
          (= "H" char-to-lookup) (lookup-H string pos)
          (= "J" char-to-lookup) (lookup-J string pos lastp)
          (= "K" char-to-lookup) (lookup-K string pos)
          (= "L" char-to-lookup) (lookup-L string pos length lastp)
          (= "M" char-to-lookup) (lookup-M string pos lastp)
          (= "N" char-to-lookup) (lookup-N string pos)
          (= "Ñ" char-to-lookup) (lookup-NY string pos)
          (= "P" char-to-lookup) (lookup-P string pos)
          (= "Q" char-to-lookup) (lookup-Q string pos)
          (= "R" char-to-lookup) (lookup-R string pos lastp)
          (= "S" char-to-lookup) (lookup-S string pos lastp)
          (= "T" char-to-lookup) (lookup-T string pos)
          (= "V" char-to-lookup) (lookup-V string pos)
          (= "W" char-to-lookup) (lookup-W string pos lastp)
          (= "X" char-to-lookup) (lookup-X string pos lastp)
          (= "Z" char-to-lookup) (lookup-Z string pos)
          :else [nil nil 1])))

;; Main functions
(defn- prep-string
  "Prepare the [string] before its passage through the Double Metaphone
  Algorithm."
  [string]
  (str (clojure.string/upper-case string) "     "))

(defn- define-start-position [pstring] (if (re-test? #"^GN|KN|PN|WR|PS$" (slice pstring 0 2)) 1 0))

(defn- symbols->string [symbols] (slice (apply str (map name symbols)) 0 4))

(defn process
  "Applying the Double Metaphone algorithm to a single [string]."
  [string]
  (let [pstring (prep-string string)
        current (define-start-position pstring)
        length (count string)
        lastp (dec length)]
    (loop [pos current
           primary []
           secondary []]
      (if (or (> pos length)
              (and (>= (count primary) 4)
                   (>= (count secondary) 4)))
        [(symbols->string primary) (symbols->string secondary)]
        (let [[newp news offset] (lookup pstring pos length lastp)]
          (recur (+ offset pos)
                 (conj-if-not-nil primary newp)
                 (conj-if-not-nil secondary news)))))))
