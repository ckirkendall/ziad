(ns ziad-example.core
  (:require [reagent.core :as r]
            [ziad.pos :as pos]
            [ziad.common :as common]))

(def init-sent "Sally Cornelly went to there house.  She wanted to go to you're house.")

(defn by-id [id] (js/document.getElementById id))

(defn convert-sent [sent]
  (let [start-time (js/Date.now)
        tokens (pos/pos-tagger sent)
        total-time (- (js/Date.now) start-time)
        html-text (common/tokens->html tokens)]
    {:sentence sent
     :tokens tokens
     :html html-text
     :analysis-time total-time}))

(defonce state (r/atom (convert-sent init-sent)))

(defn analyze []
  (let [input (.-value (by-id "input-text"))
        new-state (convert-sent input)]
    (reset! state new-state)))

(defn update-sent [event]
  (.preventDefault event)
  (analyze))

(defn update-grammar-threshold [e]
  (js/console.log (.-target e))
  (let [value (.. e -currentTarget -value)]
    (reset! pos/grammar-threshold value)
    (analyze)))

(defn page []
  [:div
   [:div {:class "page-header"}
    [:h1 "Ziad " [:small "Grammar Checker and Part of Speech Tagger"]]
    [:div {:style {:position "absolute" :top "10px" :right "20px"}}
     [:a {:href "https://github.com/ckirkendall/ziad"} "GitHub Source"]]]
   [:form
    [:div [:textarea {:id "input-text" :class "form-control"
                      :rows 3 :defaultValue (:sentence @state)}]]
    [:div [:button {:class "btn btn-primary" :on-click update-sent} "Analyze"]]
    [:br]
    [:div {:class "form-group"}
     [:label "Grammar Threshold - " @pos/grammar-threshold ]
     [:input {:type "range"
              :style {:width 200}
              :defaultValue @pos/grammar-threshold
              :min (/ 1 1000000)
              :max (/ 1 25000)
              :step (/ 1 1000000)
              :on-change update-grammar-threshold}]]]
   [:div {:class "results"}
    [:div {:class "panel panel-default"}
     [:div {:class "panel-heading"}
      [:strong "Analysis"]
      " - "
      (:analysis-time @state)
      " millseconds"]
     [:div {:class "panel-body" :dangerouslySetInnerHTML {:__html (:html @state)}}]]
    [:div {:class "panel panel-default"}
     [:div {:class "panel-heading"} [:strong "Tokens"]]
     [:div {:class "panel-body"}
      (let [tokens (:tokens @state)]
        (map (fn [cnt tok]
               ^{:key (str tok cnt)}
               [:span
                [:span {:class (common/token->class tok)}
                 (:tok tok)]
                "/" (:word tok) " "])
          (range (count tokens))
          tokens))]]]])


(r/render-component [page] (by-id "content"))
