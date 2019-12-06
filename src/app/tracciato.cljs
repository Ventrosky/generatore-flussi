(ns app.tracciato
  (:require [goog.string :as gstring]
            [goog.string.format]
            [clojure.walk]
            [app.state :refer [app-anagrafe app-generated app-setting censimento]]
            [app.soggetto :refer [gen-cdfisc]]
            [clojure.string :refer [upper-case]]))

(def cognomi (:cognomi @app-anagrafe))

(def nomi (:nomi @app-anagrafe))

;;"This function returns js/Date from a clj String"
(defn string-to-date-obj
  [date-str]
  (js/Date. (clj->js (reverse (clojure.string/split date-str #"[^\d]")))))

(defn left-pad
  [d]
  (gstring/format "%02d" d))

;;"This function returns formatted dd/MM/yyy string from js/Date"
(defn date-obj-to-str
  [date-obj]
  (str (left-pad (.getDate date-obj)) "/" (left-pad (inc (.getMonth date-obj))) "/" (.getFullYear date-obj)))

;;"This function returns new random js/Date from intervals js/Date"
(defn random-date 
  [start end] 
  (let [time-start (.getTime start)
        time-end (.getTime end)
        time-new (+ time-start (* (Math/random) (- time-end time-start)))]
    (js/Date. time-new)))

;;"This function returns new random   from intervals clj String"

(defn rng-date-str-interval
  ([date-str-from date-str-to]
   (date-obj-to-str (random-date (string-to-date-obj date-str-from) (string-to-date-obj date-str-to))))
  ([date-str]
     (rng-date-str-interval date-str date-str)))

(defn rng-date-in
  [date-str]
  (if (clojure.string/blank? date-str)
    ""
    (apply rng-date-str-interval (clojure.string/split date-str #";"))))

(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(defn rng-importo
  ([imp-min imp-max]
   (let [n-min (js/parseInt imp-min)
         n-max (js/parseInt imp-max)]
     (str (+ n-min (rand-int (- n-max n-min))))))
  ([imp]
   (rng-importo imp imp)))

(defn rng-importo-in
  [str-imp]
  (if (clojure.string/blank? str-imp)
    ""
    (apply rng-importo (clojure.string/split str-imp #";"))))

(defn no-nill-str
  [str]
  (if (clojure.string/blank? str)
    ""
    str))

(def fu-gen-record
  {:cdoper #(repeat (no-nill-str %))
   :nupoliz #(repeat (no-nill-str %))
   :cdfisccontr #(repeat (no-nill-str %))
   :cdsesso (fn [seq-sex] (repeatedly #(rand-nth seq-sex)));["M", "F"]
   :dtnasc (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :prgriga #(positive-numbers)
   :imptcm  (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impci (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impre (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impdd (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :dtentrata (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dtuscita (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dsazade #(repeat (no-nill-str %))
   :cfazade #(repeat (no-nill-str %))
   :categoria (fn [categorie] (repeatedly #(rand-nth categorie)))
   :dsmotivo #(repeat " ")
   })

(defn genera-carta
  [dati-form]
  (let [{:keys [cdoper nupoliz cdfisccontr cdsesso dtnasc prgriga imptcm impci impre impdd dtentrata dtuscita dsazade cfazade categoria dsmotivo]
         :or {cdoper "E" dsmotivo "" cdsesso ["M" "F"] categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"]}} dati-form
        gen-single-seq (fn [nkey nval] ((nkey fu-gen-record) nval))]
    (map vector
         (gen-single-seq :cdoper cdoper)
         (gen-single-seq :nupoliz nupoliz)
         (gen-single-seq :cdfisccontr cdfisccontr)
         (gen-single-seq :cdsesso ["M" "F"])
         (gen-single-seq :dtnasc dtnasc)
         ((:prgriga fu-gen-record))
         (gen-single-seq :imptcm imptcm)
         (gen-single-seq :impci impci)
         (gen-single-seq :impre impre)
         (gen-single-seq :impdd impdd)
         (gen-single-seq :dtentrata dtentrata)
         (gen-single-seq :dtuscita dtuscita)
         (gen-single-seq :dsazade dsazade)
         (gen-single-seq :cfazade cfazade)
         (gen-single-seq :categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"])
         (gen-single-seq :dtnasc dsmotivo))))

(def fu-gen-record-carbp
  {:cdoper #(repeat (no-nill-str %))
   :nupoliz #(repeat (no-nill-str %))
   :dsnome (fn [rlst] (repeatedly #(rand-nth rlst)))
   :dscognome (fn [rlst] (repeatedly #(rand-nth rlst)))
   :cdfisc #(repeat "")
   :dtnasc (fn [dt-str] (let [val (if (clojure.string/blank? dt-str)
                                    "01/01/1970;31/12/1984"
                                    dt-str)]
                          (repeatedly #(rng-date-in val))))
   :dtentrata (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dtuscita (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :imptcm  (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impci (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impre (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impdd (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :categoria (fn [categorie] (repeatedly #(rand-nth categorie)))
   :domanda1 (fn [answers] (repeatedly #(rand-nth answers)))
   :domanda2 (fn [answers] (repeatedly #(rand-nth answers)))
   :domanda3 (fn [answers] (repeatedly #(rand-nth answers)))
   :domanda4 (fn [answers] (repeatedly #(rand-nth answers)))
   :domanda5 (fn [answers] (repeatedly #(rand-nth answers)))
   :aws (fn [answers] (repeatedly #(rand-nth answers)))})

(defn genera-carbp
  [dati-form]
  (let [{:keys [cdoper nupoliz dsnome dscognome cdfisc dtnasc dtentrata dtuscita imptcm impci impre impdd categoria domanda1 domanda2 domanda3 domanda4 domanda5 aaw]
         :or {cdoper "E" categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"]}} dati-form
        gen-single-seq (fn [nkey nval] ((nkey fu-gen-record-carbp) nval))]
    (map vector
         (gen-single-seq :cdoper cdoper)
         (gen-single-seq :nupoliz nupoliz)
         (gen-single-seq :dsnome nomi)
         (gen-single-seq :dscognome cognomi)
         (gen-single-seq :cdfisc cdfisc)
         (gen-single-seq :dtnasc dtnasc)
         (gen-single-seq :dtentrata dtentrata)
         (gen-single-seq :dtuscita dtuscita)
         (gen-single-seq :imptcm imptcm)
         (gen-single-seq :impci impci)
         (gen-single-seq :impre impre)
         (gen-single-seq :impdd impdd)
         (gen-single-seq :categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"])
         (gen-single-seq :domanda1 ["S" "N"])
         (gen-single-seq :domanda2 ["S" "N"])
         (gen-single-seq :domanda3 ["S" "N"])
         (gen-single-seq :domanda4 ["S" "N"])
         (gen-single-seq :domanda5 ["S" "N" ""])
         (gen-single-seq :aws ["S" "N"]))))

(def fu-gen-record-carti
  {:cdoper #(repeat (no-nill-str %))
   :cdprod #(repeat (no-nill-str %))
   :nupoliz #(repeat (no-nill-str %))
   :cdfisccontr #(repeat (no-nill-str %))
   :cdfiscmaster #(repeat "")
   :cdidmaster #(repeat "")
   :cdfisc #(repeat "")
   :cdidslave #(repeat "")
   :cdparent (fn [parentela] (repeatedly #(rand-nth parentela)))
   :dsnome (fn [rlst] (repeatedly #(rand-nth rlst)))
   :dscognome (fn [rlst] (repeatedly #(rand-nth rlst)))
   :dtnasc (fn [dt-str] (let [val (if (clojure.string/blank? dt-str)
                                    "01/01/1970;31/12/1984"
                                    dt-str)]
                          (repeatedly #(rng-date-in val))))
   :imptcm  (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impci (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impre (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :perquota #(repeat (no-nill-str %))
   :dsstato #(repeat "")
   :dsprov #(repeat "")
   :dscap #(repeat "")
   :dscomune #(repeat "")
   :dsloc #(repeat "")
   :dsaddress #(repeat "")
   :dspresso #(repeat "")
   :dtentrata (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dtuscita (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dtvaria (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dsscarto #(repeat "")
   :cdqualifica (fn [qualifiche] (repeatedly #(rand-nth qualifiche)))
   :imsovrapre (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :dtpriade (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dsazade #(repeat (no-nill-str %))
   :impdd (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :cdinabile #(repeat "")
   :dsbenef #(repeat "")
   :cdfamiliare #(repeat "")
   :categoria (fn [categorie] (repeatedly #(rand-nth categorie)))
})

(defn genera-carti
  [dati-form]
  (let [{:keys [cdoper cdprod nupoliz cdfisccontr dtnasc dtentrata dtuscita dtvaria imptcm
                impci impre impdd perquota imsovrapre dsazade categoria cdfiscmaster
                cdidmaster cdfisc cdidslave dtpriade]
         :or {cdoper "E" categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"]}} dati-form
        gen-single-seq (fn [nkey nval] ((nkey fu-gen-record-carti) nval))]
    (map vector
         (gen-single-seq :cdoper cdoper)
         (gen-single-seq :cdprod cdprod)
         (gen-single-seq :nupoliz nupoliz)
         (gen-single-seq :cdfisccontr cdfisccontr)
         (gen-single-seq :cdfiscmaster cdfiscmaster)
         (gen-single-seq :cdidmaster cdidmaster)
         (gen-single-seq :cdfisc cdfisc)
         (gen-single-seq :cdidslave cdidslave)
         (gen-single-seq :cdparent ["00"])
         (gen-single-seq :dsnome nomi)
         (gen-single-seq :dscognome cognomi)
         (gen-single-seq :dtnasc dtnasc)
         (gen-single-seq :imptcm imptcm)
         (gen-single-seq :impci impci)
         (gen-single-seq :impre impre)
         (gen-single-seq :perquota perquota)
         (gen-single-seq :dsstato "")
         (gen-single-seq :dsprov "")
         (gen-single-seq :dscap "")
         (gen-single-seq :dscomune "")
         (gen-single-seq :dsloc "")
         (gen-single-seq :dsaddress "")
         (gen-single-seq :dspresso "")
         (gen-single-seq :dtentrata dtentrata)
         (gen-single-seq :dtuscita dtuscita)
         (gen-single-seq :dtvaria dtvaria)
         (gen-single-seq :dsscarto "")
         (gen-single-seq :cdqualifica ["01" "02" "03" "04" "99"])
         (gen-single-seq :imsovrapre imsovrapre)
         (gen-single-seq :dtpriade dtpriade)
         (gen-single-seq :dsazade dsazade)
         (gen-single-seq :impdd impdd)
         (gen-single-seq :cdinabile "")
         (gen-single-seq :dsbenef "")
         (gen-single-seq :cdfamiliare "")
         (gen-single-seq :categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"]))))


(def fu-gen-record-adica
  {:cdoper #(repeat (no-nill-str %))
   :cdprod #(repeat (no-nill-str %))
   :nupoliz #(repeat (no-nill-str %))
   :cdfisccontr #(repeat (no-nill-str %))
   :cdfiscmaster #(repeat "")
   :cdidmaster #(repeat "")
   :cdfisc #(repeat "")
   :cdidslave #(repeat "")
   :cdparent (fn [parentela] (repeatedly #(rand-nth parentela)))
   :dsnome (fn [rlst] (repeatedly #(rand-nth rlst)))
   :dscognome (fn [rlst] (repeatedly #(rand-nth rlst)))
   :dtnasc (fn [dt-str] (let [val (if (clojure.string/blank? dt-str)
                                    "01/01/1970;31/12/1984"
                                    dt-str)]
                          (repeatedly #(rng-date-in val))))
   :imptcm  (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impci (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impre (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :perquota #(repeat (no-nill-str %))
   :dsstato #(repeat "")
   :dsprov #(repeat "")
   :dscap #(repeat "")
   :dscomune #(repeat "")
   :dsloc #(repeat "")
   :dsaddress #(repeat "")
   :dspresso #(repeat "")
   :dtentrata (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dtuscita (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dtvaria (fn [dt-str] (repeatedly #(rng-date-in dt-str)))
   :dsscarto #(repeat "")
   :cdqualifica (fn [qualifiche] (repeatedly #(rand-nth qualifiche)))
   :imsovrapre (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :dtpriade #(repeat "")
   :dsazade #(repeat (no-nill-str %))
   :impdd (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :cdinabile #(repeat "")
   :dsbenef #(repeat "")
   :cdfamiliare #(repeat "")
   :categoria (fn [categorie] (repeatedly #(rand-nth categorie)))
   :tpquota #(repeat (no-nill-str %))
   :imptcmagg (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impciagg (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impreagg (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))
   :impddagg (fn [imp-str] (repeatedly #(rng-importo-in imp-str)))})

(defn genera-adica
  [dati-form]
  (let [{:keys [cdoper cdprod nupoliz cdfisccontr dtnasc dtentrata dtuscita dtvaria imptcm
                impci impre impdd perquota imsovrapre dsazade categoria cdfiscmaster
                cdidmaster cdfisc cdidslave dtpriade tpquota imptcmagg impciagg impreagg impddagg]
         :or {cdoper "E" categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"]}} dati-form
        gen-single-seq (fn [nkey nval] ((nkey fu-gen-record-adica) nval))]
    (map vector
         (gen-single-seq :cdoper cdoper)
         (gen-single-seq :cdprod cdprod)
         (gen-single-seq :nupoliz nupoliz)
         (gen-single-seq :cdfisccontr cdfisccontr)
         (gen-single-seq :cdfiscmaster cdfiscmaster)
         (gen-single-seq :cdidmaster cdidmaster)
         (gen-single-seq :cdfisc cdfisc)
         (gen-single-seq :cdidslave cdidslave)
         (gen-single-seq :cdparent ["00"])
         (gen-single-seq :dsnome nomi)
         (gen-single-seq :dscognome cognomi)
         (gen-single-seq :dtnasc dtnasc)
         (gen-single-seq :imptcm imptcm)
         (gen-single-seq :impci impci)
         (gen-single-seq :impre impre)
         (gen-single-seq :perquota perquota)
         (gen-single-seq :dsstato "")
         (gen-single-seq :dsprov "")
         (gen-single-seq :dscap "")
         (gen-single-seq :dscomune "")
         (gen-single-seq :dsloc "")
         (gen-single-seq :dsaddress "")
         (gen-single-seq :dspresso "")
         (gen-single-seq :dtentrata dtentrata)
         (gen-single-seq :dtuscita dtuscita)
         (gen-single-seq :dtvaria dtvaria)
         (gen-single-seq :dsscarto "")
         (gen-single-seq :cdqualifica ["01" "02" "03" "04" "99"])
         (gen-single-seq :imsovrapre imsovrapre)
         (gen-single-seq :dtpriade dtpriade)
         (gen-single-seq :dsazade dsazade)
         (gen-single-seq :impdd impdd)
         (gen-single-seq :cdinabile "")
         (gen-single-seq :dsbenef "")
         (gen-single-seq :cdfamiliare "")
         (gen-single-seq :categoria ["A00" "A01" "A02" "P00" "P01" "P02" "P03"])
         (gen-single-seq :tpquota tpquota)
         (gen-single-seq :imptcmagg imptcmagg)
         (gen-single-seq :impciagg impciagg)
         (gen-single-seq :impreagg impreagg)
         (gen-single-seq :impddagg impddagg))))

(def numbers (iterate inc 1))

(def titles (map #(str "gen-" %) numbers))

(defn nuclei-seq
  [n]
  (let [family [["00" "00"] ["00" "00" "00"] ["00" "01" "02"] ["00" "01"] ["00" "02"] ["00" "01" "02" "02"]]]
    (take n (into [] cat (take n (repeatedly #(rand-nth family)))))))

(defn genera-carta-n
  [n spec-in key]
  (let [{:keys [pos-cdfisc pos-nome pos-cognome pos-nasc pos-master pos-parent]} (:anag (key @censimento))
        do-cdfisc (fn [rec]
                    (gen-cdfisc (nth rec pos-nome) (nth rec pos-cognome) (nth rec pos-nasc)))
        with-cdfisc (fn [rec]
                      (assoc rec pos-cdfisc (do-cdfisc rec)))
        integrate-data (fn [f m]
                         (into {} (for [[k v] m] [k (f v)])))
        add-parent (fn [rec cdsogg mastercf]
                     (let []
                       (case cdsogg
                         "00" (assoc rec pos-master (rec pos-cdfisc))
                         (assoc rec pos-master mastercf pos-parent cdsogg))))
        add-nucleo (fn [n no-cf-records]
                     (let [cdseq (nuclei-seq n)
                           records (map with-cdfisc (take n no-cf-records))]
                       (loop [x 0
                              master (nth (nth records x) pos-cdfisc)
                              new-coll []]
                         (if (< x (count records))
                           (let [new-record (add-parent (nth records x) (nth cdseq x) master)]
                             (recur (inc x) (new-record pos-master) (conj new-coll new-record)))
                           new-coll))))]
    (case key
      :carta (clojure.walk/keywordize-keys (zipmap titles (take n (genera-carta spec-in))))
      :carbp (clojure.walk/keywordize-keys (integrate-data with-cdfisc (zipmap titles (take n (genera-carbp spec-in)))))
      :carti (clojure.walk/keywordize-keys (zipmap titles (add-nucleo n (genera-carti spec-in))))
      :adica (clojure.walk/keywordize-keys (zipmap titles (add-nucleo n (genera-adica spec-in)))))))

(defn nome-flusso
  [rec trac]
  (let [[cdoper _] rec]
    (str cdoper "_" trac "_" (upper-case (#(.toString % 16) (.getTime (js/Date.)))) ".csv")))

(defn download-trc
  []
  (let [record-text (vals @app-generated)
        trac-name (:name ((:selected @app-setting) @censimento))
        name (nome-flusso (first record-text) trac-name)
        link (.createElement js/document "a")
        text (upper-case (clojure.string/join "\n" (map #(clojure.string/join "|" %) (sort-by #(nth % 5) record-text))))
        blob (new js/Blob
                  (clj->js [text])
                  (clj->js {:type "text/plain;charset=utf-8"}))]
    (when-not (empty? record-text)
      (do
        (set! (.-href link) (.createObjectURL js/URL blob))
        (.setAttribute link "download" name)
        (.appendChild (.-body js/document) link)
        (.click link)
        (.removeChild (.-body js/document) link)))))
