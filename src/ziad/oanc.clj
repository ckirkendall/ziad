(ns ziad.oanc
  (:require [cognitect.transit :as transit]
            [ziad.common :as common]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml]))

(def base-model {:word-model {} :token-model {} :rev-token-model {}})

(def endings #{"." "?" "!"})

(defn sentence-boundary? [{[word] :content}]
  (endings word))

(defn sentence-partioner [nodes]
  (let [words (vec (take-while #(not (sentence-boundary? %)) nodes))
        [end r-nodes] (drop-while #(not (sentence-boundary? %)) nodes)
        words (if end (conj words end) words)]
    (when-not (empty? words)
      (lazy-seq (cons words (sentence-partioner r-nodes))))))


(defn parse-word-block [word-block]
  (let [tok (get-in word-block [:attrs :msd])
        word (get-in word-block [:content 0])]
    {:tok tok :word word}))


(defn add-word [model prev-tok word-block]
  (let [{:keys [tok word]} (parse-word-block word-block)]
    [(-> model
          (update-in [:word-model word tok] (fnil inc 0))
          (update-in [:token-model tok prev-tok] (fnil inc 0))
          (update-in [:rev-token-model prev-tok tok] (fnil inc 0)))
     tok]))


(defn load-sent [model sent]
  (first (reduce (fn [[cur-model tok] word-block]
                   (add-word cur-model tok word-block))
                 [model :start]
                 sent)))


(defn file-xml [file-name]
  (let [file-data (rest (line-seq (io/reader file-name)))
      end-of-file (str (apply str file-data) "</data>")
      header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<data>"
      new-content (apply str header end-of-file)
      b-content (.getBytes new-content)
      b-stream (java.io.ByteArrayInputStream. b-content)]
  (xml/parse b-stream)))


(defn load-file [model file-name]
  (let [data-xml (file-xml file-name)
        nodes (:content data-xml)
        sents (sentence-partioner nodes)]
     (reduce load-sent model sents)))


(defn find-xml-files
  ([data-dir-file]
   (find-xml-files data-dir-file #{(.getPath data-dir-file)}))
  ([data-dir-file visited]
   (let [fseq (file-seq data-dir-file)]
     (mapcat (fn [f]
               (let [vis (conj visited (.getPath f))]
                 (cond
                   (visited (.getPath f))
                   []

                   (.endsWith (.getName f) ".xml")
                   [(.getPath f)]

                   (.isDirectory f)
                   (find-xml-files f vis)

                   :else
                   [])))
             fseq))))


(defn create-model [data-dir out-file-name]
  (let [file-dir (io/as-file data-dir)
        xml-files (find-xml-files file-dir)
        model (common/normalize-full-model
               (reduce load-file base-model xml-files))]
    (with-open [out (io/output-stream out-file-name)]
      (let [writer (transit/writer out :json)]
        (transit/write writer model)))))
