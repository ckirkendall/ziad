(ns ziad.pos-test
  (:require [ziad.pos :refer [pos-tagger]]
            #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [is deftest testing]])))

(defn- filt-toks [tokens ky]
  (->> tokens
       (filter ky)
       (map :word)))

(defn- filt-grammar [tokens]
  (filt-toks tokens :grammar-issue))

(defn- filt-spelling [tokens]
  (filt-toks tokens :unknown-word))

(deftest simple-pos-test
  (testing "basic tagging"
    (let [tokens (pos-tagger "Bill is an amazing person.")]
      (is (= ["NNP" "VBZ" "DT" "JJ" "NN" "."]
             (map :tok tokens))))))

(deftest grammar-test
  (testing "there vs their"
    (let [bad (pos-tagger "Sally went to there house.")
          good (pos-tagger "Sally went to their house.")]
      (is (= ["there" "house" "."]
             (filt-grammar bad)))
      (is (empty? (filt-grammar good)))))
  (testing "there vs their"
    (let [bad (pos-tagger "Sally went to you're house.")
          good (pos-tagger "Sally went to your house.")]
      (is (= ["'re" "house" "."]
             (filt-grammar bad)))
      (is (empty? (filt-grammar good)))))
  (testing "everyday vs every day"
    (let [bad (pos-tagger "Sally runs with Suzan everyday.")
          good (pos-tagger "Sally runs with Suzan every day.")]
      (is (= ["Suzan" "everyday" "."]
             (filt-grammar bad)))
      (is (empty? (filt-grammar good))))))


(deftest spelling-test
  (testing "basic miss spellings"
    (let [bad (pos-tagger "Sally wnt to their houze.")
          good (pos-tagger "Sally went to their house.")]
      (is (= ["wnt" "houze"]
             (filt-spelling bad)))
      (is (empty? (filt-spelling good))))))
