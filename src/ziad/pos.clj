(ns ziad.pos
  (:require [clojure.string :as str]
            [ziad.common :as common]))

(declare pos-tag-max-ent)

(def start {:tok :start})

;; below this probability a token
;; will be marked as a grammar issues
(def grammer-threshold 0.000004)

(defn word-toks
  "Given:
    cur-word - current word
    cur-token - known token prior to this word
    model - grammar/pos stats model
   Returns:
    A List of possible tokens for this word."
  [cur-word cur-tok model]
  (try
    (let [word-model (get-in model [:word-model cur-word])]
      (map (fn [tok]
             {:tok tok})
        (remove nil?
                (cond
                  (and (= cur-word "'s")
                       (#{"NNP" "NN"} (:tok cur-tok)))
                  ["POS"]

                  word-model
                  (keys word-model)

                  (and (Character/isUpperCase (first cur-word))
                       (= (last cur-word) \s))
                  ["NNPS"]

                  ;;Unknown words in caps are consider proper nouns
                  (Character/isUpperCase (first cur-word))
                  ["NNP"]

                  (common/isNumber? cur-word)
                  ["CD"]

                  (common/isHyphenated? cur-word)
                  (conj (mapcat #(word-toks % cur-tok model)
                                (str/split cur-word #"-"))
                        "JJ")

                  :else
                  (keys (get-in model [:rev-token-model (:tok cur-tok)]))))))
    (catch Throwable t
      (println "ERROR:" cur-word cur-tok)
      (throw t))))


(defn max-prob-fn
  "Given:
    prob - current probability for the word-list up to this point
    cur-word - current word we are working on
    cur-tok - last known token before the cur word
    word-list - set of words we are working on
    model - grammar/pos stats model
   Returns:
    A function that given current set of tokens to this
    point and an accumulator will return a tuple containing
    the current probability and token list up to the end
    of the word set."
  [prob cur-word cur-tok word-list model]
  (fn this [[tok & rtoks] tmp-accum]
    (if (nil? tok)
      [-1, []]
      (let [word-prob (get-in model
                              [:word-model cur-word (:tok tok)]
                              1/10000)
            tok-prob (get-in model
                             [:token-model (:tok tok) (:tok cur-tok)]
                             1/10000)
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


(defn pos-tag-max-ent
  "Given
    prob - current probability
    cur-tok - known token just prior to word list
    word-list - current set of words to calculate entropy
    accum - accumulator for the list of tokens
    model - grammer/pos stats model
   Returns:
    A tuple containing the overall probability and a
    list of annotated tokens representing the word/token
    combination with the highest entropy/probability of
    occurring based on the statistical model.
   Description:
    This is a straight max entropy algorithm that checks
    all possible valid token/word probabilities. This
    algorithm is O(n!)"
  [prob cur-tok word-list accum model]
  (if (empty? word-list)
    [prob, (conj accum cur-tok)]
    (let [cur-word (first word-list)
          wtok-list (word-toks cur-word cur-tok model)
          max-prob (max-prob-fn prob cur-word cur-tok word-list model)]
       (max-prob wtok-list (conj accum cur-tok)))))


(defn- set-sub-vec
  "Given two vectors and index it will splice the second
   vector into the first at that index."
  [main-vec sub-vec idx]
  (into (subvec main-vec 0 (min idx (count main-vec)))
        sub-vec))


(defn pos-optimized
  "Given:
    words - list of words in a sentence
    model - grammar/pos stats model
   Returns:
    An annotated list of tok/word combination that
    have the highest entropy/probability based on
    the statistical model.
   Description:
    This is an optimization of a standard max entropy
    that takes advantage of the context sensitive nature
    of language to do a step wise tree trimming. This
    brings the normal max entropy algorithm from
    O(n!) to O(n)"
  [words model]
  (loop [toks [start]
         tidx 0
         tdepth 3]
    (if (> (+ tidx tdepth) (count words))
      toks
      (let [tdepth (if (= (inc (+ tidx tdepth)) (count words))
                     (inc tdepth)
                     tdepth)
            word-set (subvec words tidx (+ tidx tdepth))
            prev-tok (nth toks tidx )
            tok-set (second (pos-tag-max-ent 1 prev-tok word-set [] model))]
        (if (or (zero? tidx)
                (>= (+ tidx 1) (count toks))
                (= (nth tok-set 1) (nth toks (+ tidx 1))))
          (recur (set-sub-vec toks tok-set tidx)
                 (+ (- tdepth 3) tidx 1)
                 tdepth)
          (recur toks (dec tidx) (inc tdepth)))))))


(defn spell-check
  "given set of tokens and model
   return a set of tokens annotated
   with unknown word if word does not
   exist in the model"
  [toks model]
  (map (fn [{:keys [word] :as tok-map}]
         (if-not (get-in model [:word-model word])
           (assoc tok-map :unknown-word true)
           tok-map))
    toks))

(defn create-semantic-tri [tri]
  (mapv (fn [token]
          (let [tok (:tok token)]
            (case tok
              "NNP" "NN"
              "NNPS" "NNS"
              tok))) tri))

(defn grammar-check
  "Given set of tokens annotate grammar errors based
   on probability of a grammar tri existing."
  [toks model]
  (if (:tri-model model)
    (reduce
     (fn [stack tok]
       (let [cnt (count stack)]
         (if (>= cnt 2)
           (let [tri (conj (subvec stack (- cnt 2) cnt) tok)
                 sem-tri (create-semantic-tri tri)
                 prob (get-in model [:tri-model sem-tri] 0.000004)]
             (if (<= prob grammer-threshold)
               (into (subvec stack 0 (- cnt 2))
                     (map #(assoc % :grammar-issue true
                                    :tr-prob prob) tri))
               (conj stack tok)))
           (conj stack tok))))
     []
     toks)
    toks))


(defn pos-tagger
  "Given a sentence and grammar/pos we will return
   a list of tokens with the highest entropy/probability
   based on the model."
  [sent model]
  (let [words (common/partition-words sent)]
    (-> (time (pos-optimized words model))
        rest
        (spell-check model)
        (grammar-check model))))
