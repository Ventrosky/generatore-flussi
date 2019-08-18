(ns app.soggetto  
  (:require [clojure.string :refer [split-lines split upper-case blank?]]
            [app.state :refer [app-anagrafe]]))

(def mesi {"01" "a"
           "02" "b"
           "03" "c"
           "04" "d"
           "05" "e"
           "06" "h"
           "07" "l"
           "08" "m"
           "09" "p"
           "10" "r"
           "11" "s"
           "12" "t"})

(def char-ctrl {"0" '(1,0)
                "A" '(1,0)
                "1" '(0,1)
                "B" '(0,1)
                "2" '(5,2)
                "C" '(5,2)
                "3" '(7,3)
                "D" '(7,3)
                "4" '(9,4)
                "E" '(9,4)
                "5" '(13,5)
                "F" '(13,5)
                "6" '(15,6)
                "G" '(15,6)
                "7" '(17,7)
                "H" '(17,7)
                "8" '(19,8)
                "I" '(19,8)
                "9" '(21,9)
                "J" '(21,9)
                "K" '(2,10)
                "L" '(4,11)
                "M" '(18,12)
                "N" '(20,13)
                "O" '(11,14)
                "P" '(3,15)
                "Q" '(6,16)
                "R" '(8,17)
                "S" '(12,18)
                "T" '(14,19)
                "U" '(16,20)
                "V" '(10,21)
                "W" '(22,22)
                "X" '(25,23)
                "Y" '(24,24)
                "Z" '(23,25)})

(defn tre-cons
  [parola isNome]
  (let [pad-x (fn [nv]
                (loop [lst nv]
                  (if (> (count lst) 3)
                    lst
                    (recur (conj lst "x")))))
        estrai #(concat (re-seq %2 %1) (re-seq %3 %1))
        lettere (pad-x (estrai parola #"[^aeiou'\s]" #"[aeiou]"))
        indici (if isNome 
                 [0 2 3]
                 [0 1 2])]
    (apply str (map #(nth lettere %) indici))))

(defn cognome-nome
  [cognome nome]
  (str (tre-cons cognome false) (tre-cons nome true)))

(defn calc-giorno
  [day sex]
  (case sex
    "M" day
    "F" (str (+ (js/parseInt day) 40))))

(defn calc-dtnas
  [data sex]
  (let [dati (if (blank? data) 
               ["01" "01" "1970"]
               (split data "/"))
        anno (subs (dati 2) 2)
        mese (mesi (dati 1))
        giorno (calc-giorno (dati 0) sex)]
    (str anno mese giorno)))

(defn add-summ
  [c x]
  (char (+ (int c) x)))

(defn get-controllo
  [parziale]
  (let [cdfisc (upper-case parziale)
        codd (map #(first (char-ctrl %)) (take-nth 2 cdfisc))
        ceven (map #(second (char-ctrl %)) (take-nth 2 (rest cdfisc)))
        csum (reduce + (concat codd ceven))]
    (char (+ (mod csum 26) 65))))

(defn gen-cdfisc
  ([nome cognome dtnasc sesso]
   (let [rand-ch #(char (+ (rand-int 11) 65))
         loc (str (rand-ch) (+ (rand-int 900) 101))
         parziale (upper-case (str (cognome-nome cognome nome) (calc-dtnas dtnasc sesso) loc))]
     (str parziale (get-controllo parziale))))
  ([nome cognome dtnasc]
   (gen-cdfisc nome cognome dtnasc (rand-nth ["M" "F"]))))
