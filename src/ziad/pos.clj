(ns ziad.pos
  (:require [clojure.string :as str]
            [ziad.common :as common]))

(declare pos-tag-max-ent)


(defn word-toks [cur-word cur-tok model]
  (let [word-model (get-in model [:word-model cur-word])]
    (map (fn [tok]
           {:tok tok})
      (filter identity
              (if word-model
                (keys word-model)
                (keys (get-in model [:rev-token-model (:tok cur-tok)])))))))


(defn max-prob-fn [prob cur-word cur-tok word-list model]
  (fn this [[tok & rtoks] tmp-accum]
    #_(println cur-tok tok rtoks)
    (if (nil? tok)
      [-1, []]
      (let [word-prob (get-in model [:word-model cur-word (:tok tok)] 1/10000)
            tok-prob (get-in model [:token-model (:tok tok) (:tok cur-tok)] 1/10000)
            x (pos-tag-max-ent (* word-prob tok-prob prob)
                               (assoc tok
                                      :word cur-word
                                      :word-prob word-prob
                                      :tok-prob tok-prob)
                               (rest word-list)
                               tmp-accum
                               model)
            y (this rtoks tmp-accum)]
        (if (>= (first x) (first y)) x y)))))


(defn pos-tag-max-ent [prob cur-tok word-list accum model]
  (if (empty? word-list) [prob, (conj accum cur-tok)]
    (let [cur-word (first word-list)
          wtok-list (word-toks cur-word cur-tok model)
          max-prob (max-prob-fn prob cur-word cur-tok word-list model)]
       (max-prob wtok-list (conj accum cur-tok)))))


(defn set-sub-vec [main-vec sub-vec idx]
  (cond
    (empty? sub-vec)
    main-vec

    (> idx (count main-vec))
    (set-sub-vec (conj main-vec (nth sub-vec 0))
                   (subvec sub-vec 1)
                   (+ idx 1))
    :else
    (set-sub-vec (assoc main-vec idx (nth sub-vec 0))
                   (subvec sub-vec 1)
                   (+ idx 1))))

(defn pos-optimized [words tokens idx depth model]
  (loop [toks tokens
         tidx idx
         tdepth depth]
    (if (> (+ tidx tdepth) (count words))
      [words, toks]
      (let [word-set (subvec words tidx (+ tidx tdepth))
            prev-tok (nth toks tidx )
            tok-set (second (pos-tag-max-ent 1 prev-tok word-set [] model))]
        (if (or (zero? tidx)
                (>= (+ tidx 1) (count toks))
                (= (nth tok-set 1) (nth toks (+ tidx 1))))
          (recur (set-sub-vec toks tok-set tidx) (+ (- tdepth depth) tidx 1) tdepth)
          (recur toks (dec tidx) (inc tdepth)))))))

(def start {:tok :start})

(defn pos-tagger [sent model]
  (let [words (-> sent
                  str/lower-case
                  (str/split #" ")
                  vec)
        [words tokens] (time (pos-optimized words [start] 0 2 model))
        final (->> tokens
                   rest
                   (map (fn [tok]
                          (if (<= (:word-prob tok 0) 1/10000)
                            (assoc tok :unknown-word true)
                            tok)))
                   (map (fn [tok]
                          (if (and (not (:unknown-word tok))
                                   (< (:tok-prob tok 0) 0.02))
                            (assoc tok :grammer-issue true)
                            tok))))]
    final))
