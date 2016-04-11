(ns ziad-example.graph
  (:require
   [ziad.pos :as pos]
   [ziad.common :as common]))


(defn get-word-tok-prob [model cur-word cur-tok]
  (get-in model [:word-model cur-word (:tok cur-tok)] (/ 1 10000)))

(defn get-ptok-tok-prob [model prev-tok cur-tok]
  (get-in model [:token-model (:tok cur-tok) (:tok prev-tok)] (/ 1 10000)))

(defn pos-prob-map
  ([words model]
   (pos-prob-map words 1 model (atom 0) :start))
  ([[cur-word & r-words] prior-prob model cnt prev-tok ]
   (if (nil? cur-word)
     {:tok :end :name (str @cnt (double prior-prob)) :link-label ""}
     (let [wtok-list (pos/word-toks cur-word prev-tok model)]
       (reduce (fn [graph cur-tok]
                 (let [tok-prob (get-ptok-tok-prob model prev-tok cur-tok)
                       word-prob (get-word-tok-prob model cur-word cur-tok)
                       total-prob (* tok-prob word-prob prior-prob)
                       tok (:tok cur-tok)
                       ptok (:tok prev-tok)]
                   (swap! cnt inc)
                   (assoc graph
                          {:name (str @cnt
                                     "-"
                                      cur-word "/" tok
                                      "(" (format "%.3f" (double word-prob)) ")")
                           :link-label (str ptok "->" tok "(" (format "%.3f" (double tok-prob)) ")" )
                           :word cur-word
                           :tok tok
                           :link-prob tok-prob
                           :node-prob word-prob
                           :total-prob total-prob}
                          (pos-prob-map r-words
                                        total-prob
                                        model
                                        cnt
                                        cur-tok))))
               {}
               wtok-list)))))

(defn prob-map->graph
  ([prob-map]
   (prob-map->graph {:nodes [{:index 0
                              :name "_start_"
                             :word :start
                             :tok :start
                             :node-prob 1
                             :total-prob 1}]
                     :links []}
                    prob-map
                    0))
  ([accum prob-map source-index]
   (let [index (count (:nodes accum))
         prev-node (get-in accum [:nodes source-index])]
     (if (or (= (:tok prob-map) :end)
             (nil? prob-map))
       (-> accum
           (update :nodes conj (assoc prob-map :index index))
           (update :links conj {:source source-index
                                :target index
                                :source-node prev-node
                                :target-node prob-map}))
       (reduce (fn [accum [node p-map]]
                 (let [index (count (:nodes accum))
                       prev-node (get-in accum [:nodes source-index])
                       node (assoc node :index index)]
                   (-> accum
                       (update :nodes conj node)
                       (update :links conj {:source source-index
                                            :target index
                                            :source-node prev-node
                                            :target-node node
                                            :value (:link-prob node)})
                       (prob-map->graph p-map index))))
               accum
               prob-map)))))


(defn to-digraph [graph]
  (println (str "digraph test {\n"
                (apply str
                  (map #(str \" (get-in % [:source-node :name]) \"
                             " -> "
                             \" (get-in % [:target-node :name]) \"
                             "[ label = \"" (get-in % [:target-node :link-label]) "\"]"
                             "\n")
                    (:links graph)))
                "}")))


(defn generate-probability-graph [content model]
  (let [graph (-> content
                  common/partition-sentences
                  first
                  common/partition-words
                  (pos-prob-map model)

                  #_(#(clojure.pprint/pprint %))
                  prob-map->graph
                  to-digraph)]
    (count (filter #(= (:tok %) :end) (:nodes graph)))))
