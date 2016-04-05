(ns ziad.common
  (:require [cognitect.transit :as transit]
            [clojure.java.io :as io]))

(def endings #{\. \? \!})

(defn normalize-hash-map [map-hash]
  (let [sum-vals  (apply + (vals map-hash))]
    (reduce #(assoc %1 %2 (/ (get %1 %2) sum-vals)) map-hash (keys map-hash))))

(defn normalize-full-model [model]
  (let [word-keys (keys (get model :word-model))
        tok-keys (keys (get model :token-model))
        rtok-keys (keys (get model :rev-token-model))
        word-model (get model :word-model)
        tok-model (get model :token-model)
        rtk-model (get model :rev-token-model)
        reduce-func (fn [tmodel tkey]
                      (assoc tmodel tkey
                        (normalize-hash-map (get tmodel tkey))))]
    (let [tmp-word-model (reduce reduce-func word-model word-keys)
          tmp-token-model (reduce reduce-func tok-model tok-keys)
          tmp-rtk-model (reduce reduce-func rtk-model rtok-keys)]
      (assoc model :word-model tmp-word-model
                   :token-model tmp-token-model
                   :rev-token-model tmp-rtk-model ))))


(defn load-model [model-file-name]
  (with-open [in (io/input-stream model-file-name)]
    (let [reader (transit/reader in :json)]
      (transit/read reader))))


(defn partition-words
  [sent]
  (loop [[cur-char & rest-sent] sent
         cur-word []
         words []]
    (cond
      (nil? cur-char)
      words

      (and (not (empty? cur-word))
           (= \space cur-char))
      (recur rest-sent [] (conj words (apply str cur-word)))

      (endings cur-char)
      (conj words (apply str cur-word) (str cur-char))

      (= \' cur-char)
      (recur rest-sent [cur-char] (conj words (apply str cur-word)))

      (not= \space cur-word)
      (recur rest-sent (conj cur-word cur-char) words)

      :else
      (recur rest-sent cur-word words))))
