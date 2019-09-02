(ns app.views
  (:require [app.state :refer [app-state app-generated censimento app-setting app-inputato]]
            [app.events :refer [increment decrement]]
            [cljsjs.semantic-ui-react]
            [reagent.core :as r :refer [atom]]
            [app.tracciato :refer [genera-carta-n download-trc download-xls]]))

(def semantic-ui js/semanticUIReact)

(defn component
  "Get a component from sematic-ui-react:
    (component \"Button\")
    (component \"Menu\" \"Item\")"
  [k & ks]
  (if (seq ks)
    (apply goog.object/getValueByKeys semantic-ui k ks)
    (goog.object/get semantic-ui k)))

;;to-do remove unused components
(def container        (component "Container"))
(def button           (component "Button"))
(def segment          (component "Segment"))
(def dimmer           (component "Dimmer"))
(def loader           (component "Loader"))
(def message          (component "Message"))
(def message-header   (component "Message" "Header"))
(def form             (component "Form"))
(def form-field       (component "Form" "Field"))
(def table            (component "Table"))
(def table-header     (component "Table" "Header"))
(def table-body       (component "Table" "Body"))
(def table-footer     (component "Table" "Footer"))
(def table-row        (component "Table" "Row"))
(def table-headercell (component "Table" "HeaderCell"))
(def table-cell       (component "Table" "Cell"))
(def header           (component "Header"))
(def grid             (component "Grid"))
(def grid-column      (component "Grid" "Column"))
(def grid-row         (component "Grid" "Row"))
(def divider          (component "Divider"))
(def dropdown         (component "Dropdown"))
(def input            (component "Input"))

(defn note
  []
  [:div.ui.segments
   [:div.ui.segment 
    [:p "E' possibile esprimere in intervalli randomici date ed importi." ]]
   [:div.ui.secondary.segment
    [:p "Esempio data: 01/01/1990;31/12/1999"]
    [:p "Esempio importo: 500000;1000000"]]])

(def patterns
  {:tdate "[0-9]{2}[^\\d][0-9]{2}[^\\d][0-9]{4}(;[0-9]{2}[^\\d][0-9]{2}[^\\d][0-9]{4}){0,1}"
   :tnumber "\\d+(;\\d+)*"})

(defn posfield
  [satom lname pname dominio tipo obbligatorio specifiche code]
  (let [pattern (tipo patterns)
        req (= "O" obbligatorio)]
    ^{:key lname}
    [:> grid-column 
     [:> form-field 
      [:label lname]
      (if (empty? dominio)
        [:input {:placeholder pname
                 :value (code @satom)
                 :pattern pattern
                 :title specifiche
                 :required req
                 :on-change (fn [e]
                              (swap! satom assoc code (-> e .-target .-value)))
                 :key pname}]
        [:select.ui.dropdown {:on-change (fn [e]
                                           (swap! satom assoc code (-> e .-target .-value)))}
         (for [d dominio]
           [:option {:value d 
                     :key d} d])])]]))

(defn inputati
  [items]
  (filter #(:inputato %) items))

(defn content
  [items s]
  [:> grid {:columns 2 :stackable true}
   (doall
    (for [{:keys [nome descrizione dominio tipo obbligatorio specifiche code]} (inputati items)]
      ^{:key (str nome "-" (:selected @app-setting))}
      [posfield app-inputato nome descrizione dominio tipo obbligatorio specifiche code]))
   [:> grid-row {:columns 3 :style {:text-align "center"}}
    [:> grid-column 
     [:> button {:type "submit" 
                 :color "blue"
                 :on-click (fn [evt]
                             (swap! app-setting assoc :loading true))} "Genera"]]
    [:> grid-column;
     [:> button {:color "blue"
                 :on-click (fn [evt]
                             (download-trc))} "Esporta CSV"]]
    [:> grid-column;
     [:> button {:color "blue"
                 :on-click (fn [evt]
                             (download-xls))} "Esporta XLS"]]]])

(defn build-form-atom
  []
  (reduce #(assoc %1 (:code %2) (or (first (:dominio %2)) "")) {} ((:selected @app-setting) @app-state)))

(defn fl-loader
  []
  [:div.ui.inverted.dimmer {:class (when (:loading @app-setting) "active")}
   [:> loader "Preparazione Flusso"]])

(defn up-loader 
  []
  (r/create-class
   {:display-name "up-loader"
    :reagent-render
    (fn []
      [:div.ui.inverted.dimmer {:class (when (:loading @app-setting) "active")}
       [:> loader "Preparazione Flusso"]])
    :component-did-update
    (fn [comp]
      (when (:loading @app-setting) 
         (let [x (:num @app-setting)
               sel (:selected @app-setting)
               new-records (genera-carta-n x @app-inputato sel)]
           (do 
             (js/console.log "component-did-update")
             (reset! app-generated new-records)
             (swap! app-setting assoc :loading false)))))}))

(defn form-tracciato
  [campi]
  (let [s app-inputato]
    (fn []
      [:> form
       [up-loader]
       [content ((:selected @app-setting) @app-state) s]])))

(defn tabella-generata
  [records testata]
  [:> table {:celled true :id "table-flusso"}
   [:> table-header
    [:> table-row
       (for [testa testata]
         ^{:key (random-uuid)}
         [:> table-headercell testa ])]]
   [:> table-body {:id "tbody-flusso"}
    (for [riga (sort-by #(nth % (:sort ((:selected @app-setting) @censimento))) records)]
      ^{:key (random-uuid)}
      [:> table-row {:style {:white-space "nowrap"}}
       (for [campo riga]
         ^{:key (random-uuid)}
         [:> table-cell (clojure.string/upper-case (str campo))])])]
   [:> table-footer]])

(defn nomi-tracciati
  [klist]
  (map #({:key % :value % :text %}) klist))

(defn app []
  [:div ; to-do use re-frame 
   [:> container
    [:> header {:content "Generatore di Flussi" 
                :textAlign "center" 
                :as "h1" 
                :style {:color "#2185d0" 
                        :padding "1em"}}]
    [note]
    [:> grid {:columns 2 :stackable true}
     [:> grid-row {:style {:textAlign "center"}}
      [:> grid-column
       [:select.ui.dropdown {:on-change (fn [e]
                                          (do 
                                            (swap! app-setting assoc :selected (keyword (-> e .-target .-value)))
                                            (reset! app-inputato (build-form-atom))
                                            (reset! app-generated {})))}
        (for [trc (vals @censimento)]
          [:option {:value (:code trc) :key (:name trc)} (:name trc)])]]
      [:> grid-column
       [:> input { :placeholder (:num @app-setting)
                  :on-change (fn [e]
                               (swap! app-setting assoc :num (js/parseInt (-> e .-target .-value))))}]]]]
    [:> divider]
    [form-tracciato @app-state]
    [:> divider]
    [:> grid {:columns 1 :stackable true}
     [:> grid-row
      [:div {:style {:overflow-x "scroll"
                     :padding "0px"
                     :border "dashed"
                     :border-color "#2185d0"
                     :border-radius "10px"
                     :margin-top "1.5em"}}
       [tabella-generata (vals @app-generated) (map #(:nome %) ((:selected @app-setting) @app-state))]]]]]])
