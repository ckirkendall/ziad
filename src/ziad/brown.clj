(ns ziad.brown
  (:require [cognitect.transit :as transit]
            [clojure.java.io :as io]
            [ziad.common :as common])
  (:import [java.io File]))

(defn word-tokenizer [se] (re-seq #"\w+" se))
(defn sent-tokenizer [txt] (seq (. txt split "[\\.\\?\\;]\\s+")))

(defn brown-sent-tokenizer [txt]
  (let [cleanTxt (. txt replaceAll "\\n" "")]
    (seq (. cleanTxt split "(\\.\\/\\.)|(\\?\\/\\.)|(\\!\\/\\.)|(\\;\\/\\.)|\\\\n"))))

(defn brown-word-tokenizer [se] (re-seq #"\S+\/\S+" se))


(def brown-model {:word-model {} :token-model {} :rev-token-model {}})


(defn get-dist-map [model model-label word-tok token ]
  (get (get model model-label {word-tok {token 0}}) word-tok))

(defn analyze-brown-word [prevTok word-group model]
  (let  [word-seq (seq (. word-group split "/"))
         word (. (apply str (interpose "/" (butlast word-seq)))  toLowerCase)
         token (last word-seq)
         red-func (fn [model token-list]
                      (update-in model token-list #(if %1 (inc %1) 1)))]
    {:token  token
     :model (reduce red-func model [[:word-model word token]
                                    [:token-model token prevTok]
                                    [:rev-token-model prevTok token]])}))


(defn analyze-brown-sub-sent [token, word-group-list, model]
  (if (empty? word-group-list) model
    (let [word-group (first word-group-list)
          tok-model (analyze-brown-word token, word-group, model)]
        (analyze-brown-sub-sent (tok-model :token) (rest word-group-list) (tok-model :model)))))


(defn analyze-brown-sent [se, model]
  (analyze-brown-sub-sent :start , se, model))


(defn analyze-brown-sent-list [sent-list model]
  (if (empty? sent-list) model
    (let [new-model (analyze-brown-sent (first sent-list) model)]
      (analyze-brown-sent-list (rest sent-list) new-model))))


(defn analyze-brown-file [fileStr model]
  (let [brown-text (slurp fileStr)
        brown-sent-list (map brown-word-tokenizer (brown-sent-tokenizer brown-text))]
      (analyze-brown-sent-list brown-sent-list model )))


(defn load-brown-file-list [file-list, model]
  (if (empty? file-list) model
    (let [new-model (analyze-brown-file (first file-list) model)]
      (load-brown-file-list (rest file-list) new-model))))


(defn load-brown-dir [dir]
  (let [file-list (file-seq (new File dir))
        path-list (map #(. %1 getPath) (rest file-list) )]
      (load-brown-file-list path-list brown-model)))


(defn create-model [corpus-dir out-file-name]
  (let [model (common/normalize-full-model (load-brown-dir corpus-dir))]
    (with-open [out (io/output-stream out-file-name)]
      (let [writer (transit/writer out :json)]
        (transit/write writer model)))))

(defn load-model [model-file-name]
  (with-open [in (io/input-stream model-file-name)]
    (let [reader (transit/reader in :json)]
      (transit/read reader))))
