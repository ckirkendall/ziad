(ns ziad.common
  #?(:clj
     (:import [java.io ByteArrayOutputStream]))
  (:require #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.walk :as walk])
            [cognitect.transit :as transit]
            [clojure.string :as str]))

(def endings #{\. \? \!})

(def digit? (into #{} "0123456789"))

(def upper-case? (into #{} "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn isNumber? [word]
  (every? #(or (digit? %) (#{\, \.} %)) word))

(defn isHyphenated? [word]
  (some #{\-} word))

(defn normalize-hash-map [cnt-hash]
  (when-not (empty? cnt-hash)
    (let [sum-vals  (reduce + 0 (vals cnt-hash))]
      (into {} (map (fn [[k v]]
                      [k (/ v sum-vals)]) cnt-hash)))))

(defn normalize-full-model [model]
  (let [word-keys (keys (get model :word-model))
        tok-keys (keys (get model :token-model))
        rtok-keys (keys (get model :rev-token-model))
        word-model (get model :word-model)
        tok-model (get model :token-model)
        rtk-model (get model :rev-token-model)
        tri-model (get model :tri-model)
        reduce-func (fn [tmodel tkey]
                      (assoc tmodel tkey
                        (normalize-hash-map (get tmodel tkey))))]
    (let [tmp-word-model (reduce reduce-func word-model word-keys)
          tmp-token-model (reduce reduce-func tok-model tok-keys)
          tmp-rtk-model (reduce reduce-func rtk-model rtok-keys)
          tmp-tri-model (normalize-hash-map tri-model)]
      (assoc model :word-model tmp-word-model
                   :token-model tmp-token-model
                   :rev-token-model tmp-rtk-model
                   :tri-model tmp-tri-model))))

#?(:clj
   (defn load-model [model-file-name]
     (with-open [in (io/input-stream
                     (io/resource model-file-name))]
       (let [reader (transit/reader in :json)]
         (transit/read reader)))))


#?(:clj
   (defn ratios->doubles [model]
     (walk/postwalk (fn [x]
                      (if (ratio? x)
                        (double x)
                        x))
                    model)))

#?(:clj
   (defmacro inline-model [model-file-name]
     (let [model (-> (load-model model-file-name)
                     ratios->doubles)
           out (ByteArrayOutputStream. 4096)
           writer (transit/writer out :json)]
       (transit/write writer model)
       (.toString out))))


(defn partition-sentences
  [content]
  (loop [[cur-char & rest-sent] content
         cur-sent []
         sents []]
    (cond
      (and (nil? cur-char) (empty? cur-sent))
      sents

      (nil? cur-char)
      (conj sents (apply str cur-sent))

      (or (and (= \. cur-char)
               (not (digit? (or (first rest-sent) \space))))
          (#{\! \?} cur-char))
      (recur rest-sent [] (conj sents (apply str (conj cur-sent cur-char))))

      :else
      (recur rest-sent (conj cur-sent cur-char) sents))))


(defn partition-words
  [sent]
  (loop [[cur-char & rest-sent] (str/trim sent)
         cur-word []
         words []]
    (cond
      (and (nil? cur-char)
           (empty? cur-word))
      words

      (nil? cur-char)
      (conj words (apply str cur-word))

      (= \space cur-char)
      (recur rest-sent [] (conj words (apply str cur-word)))

      (endings cur-char)
      (conj words (apply str cur-word) (str cur-char))

      (= \' cur-char)
      (recur rest-sent [cur-char] (conj words (apply str cur-word)))

      (and (#{\, \: \;} cur-char)
           (not (digit? (last cur-word))))
      (recur rest-sent [cur-char] (conj words (apply str cur-word)))

      (not= \space cur-word)
      (recur rest-sent (conj cur-word cur-char) words)

      :else
      (recur rest-sent cur-word words))))


(defn is-contraction? [{:keys [word]}]
  (= (first word) \'))

(defn token->class [{tok :tok}]
  (case tok
    "." "PERIOD"
    "," "COMMA"
    "?" "QMARK"
    "!" "EMARK"
    ":" "COLON"
    ";" "SEMICOLON"
    tok))

(defn token->html [token]
  (str "<span class='"
       (token->class token)
       " "
       (if (:unknown-word token) "unknown-word" "")
       "'>"
       (:word token)
       "</span>"))

(def grammar-start "<span class='grammar-issue'>")
(def grammar-end "</span>")

(defn tokens->html [tokens]
  (let [space? #(not (#{\. \, \'} (first (:word %))))]
    (apply str
      (first
       (reduce (fn [[tags g-issue?] tok]
                 (let [ntags (cond-> tags
                               (and (not (empty? tags))
                                    (space? tok))
                               (conj "&nbsp;")

                               (and (not g-issue?)
                                    (:grammar-issue tok))
                               (conj grammar-start)

                               (and g-issue?
                                    (not (:grammar-issue tok)))
                               (conj grammar-end)

                               :always
                               (conj (token->html tok)))]
                   [ntags (:grammar-issue tok)]))
               [[] false]
               tokens)))))
