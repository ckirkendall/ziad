(ns ziad-example.core
  (:require [reagent.core :as r]
            [ziad.pos :as pos]
            [ziad.common :as common]))

(def init-sent "Sally Cornelly went to there house.")

(defn by-id [id] (js/document.getElementById id))

(defn convert-sent [sent]
  (let [start-time (js/Date.now)
        tokens (pos/pos-tagger sent)
        total-time (- (js/Date.now) start-time)
        html-text (common/tokens->html tokens)]
    (js/console.log html-text)
    {:sentence sent
     :tokens tokens
     :html html-text
     :analysis-time total-time}))

(defonce state (r/atom (convert-sent init-sent)))

(defn analyze [event]
  (.preventDefault event)
  (let [input (.-value (by-id "input-text"))
        new-state (convert-sent input)]
    (reset! state new-state)))

(defn page []
  [:div
   [:div {:class "page-header"}
    [:h1 "Ziad " [:small "Grammar Checker and Part of Speech Tagger"]]]
   [:form
    [:div [:textarea {:id "input-text" :class "form-control"
                      :rows 3 :defaultValue (:sentence @state)}]]
    [:div [:button {:class "btn btn-primary" :on-click analyze} "Analyze"]]]
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
