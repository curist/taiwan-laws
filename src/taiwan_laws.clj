(ns taiwan-laws
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :refer [shell]]
            [cheshire.core :as json]
            [selmer.parser :refer [render-file]]))

(def ch-laws-url "https://law.moj.gov.tw/api/data/chlaw.json.zip")
(def ch-orders-url "https://law.moj.gov.tw/api/data/chorder.json.zip")

(def en-laws-url "https://law.moj.gov.tw/api/data/enlaw.json.zip")
(def en-orders-url "https://law.moj.gov.tw/api/data/enorder.json.zip")

(defn- download-and-unzip-law [url]
  (-> (http/get url {:as :stream}) :body
      (fs/unzip "blob" {:replace-existing true})))

(defn download-and-unzip-laws []
  (let [urls [ch-laws-url ch-orders-url]]
    (run! download-and-unzip-law urls)))

(defn- jq-process-json [[in out]]
  (shell {:in (io/reader in)
          :out :write
          :out-file out} "jq ."))

(defn jq-process-jsons []
  (let [jsons [["blob/ChLaw.json" "blob/law.jq.json"]
               ["blob/ChOrder.json" "blob/order.jq.json"]]]
    (run! jq-process-json jsons)))

(defn- format-modified-date [s]
  (.format
   (java.text.SimpleDateFormat. "yyyy/MM/dd")
   (.parse
    (java.text.SimpleDateFormat. "yyyyMMdd")
    s)))

(defn- trim-line-breaks [s]
  (string/replace s #"\r\n" ""))

(defn- is-section? [s]
  (re-find #"\s.+\s節\s.+" s))

(defn- compose-chapter-link [s]
  (let [chapter (string/trim s)
        link (string/replace chapter #"\s" "-")]
    (format "[%s](#%s)" chapter link)))

(defn- compose-chapters [articles]
  (->> articles
       (filter (fn [{:keys [ArticleType]}] (= "C" ArticleType)))
       (map (fn [{:keys [ArticleContent]}]
              (str (if (is-section? ArticleContent) "  * " "* ")
                   (compose-chapter-link ArticleContent))))
       (string/join "\n")))

(defn- format-article-no [article-no]
  (if (re-find #"^\d+$" article-no) "" article-no))

(defn- numbering-articles [articles]
  (if (> (count articles) 1)
    (map #(str "1. " %) articles)
    articles))

(defn- is-sub-article-content? [s]
  (let [patterns [#"^[一二三四五六七八九十]{1,2}[　、 ]"
                  #"^（[一二三四五六七八九十]{1,2}）"
                  #"^\d\."
                  #"^[甲乙丙丁]等[：︰]"
                  #"^\s*[┌│└├]"]]
    (some #(re-find % s) patterns)))

(defn grouping-sub-articles [article]
  (let [articles (vec (string/split article #"\r\n"))
        split-indexes (->> (partition 2 1 articles)
                           (map #(let [[a b] %]
                                   (or (re-find #"[：︰]\s*$" a)
                                       (is-sub-article-content? b))))
                           (map-indexed (fn [idx item] (when (not item) (inc idx))))
                           (filter identity))
        split-indexes (concat [0] split-indexes [(count articles)])]
    (->> split-indexes
         (partition 2 1)
         (map (fn [[start end]] (subvec articles start end)))
         (map #(string/join "  \n" %)))))

(defn- format-article [article]
  (->> article grouping-sub-articles numbering-articles (string/join "\n")))

(defn- compose-law-articles [articles]
  (->> articles
       (map (fn [{:keys [ArticleType ArticleNo ArticleContent]}]
              (if (= "C" ArticleType)
                (if (is-section? ArticleContent)
                  (str "#### " (string/trim ArticleContent))
                  (str "### " (string/trim ArticleContent)))
                ;; articles
                (str "##### " (format-article-no ArticleNo) "\n"
                     (format-article ArticleContent)))))
       (string/join "\n\n")))

(defn- format-attachments [attachments]
  (->> attachments
       (map (fn [{:keys [FileName FileURL]}]
              (format "* [%s](%s)" FileName FileURL)))
       (string/join "\n")))

(defn- format-histories [history]
  (let [histories (-> history
                      (string/replace #"^\d+\." "")
                      (string/replace #"\r\n\d+\." "OXXXO")
                      (string/split #"OXXXO"))]
    (->> histories
         (mapv #(string/replace % #"\r\n  " ""))
         numbering-articles
         (string/join "\n"))))

(defn- law-json->markdown [law]
  (let [law-name (:LawName law)
        law-name-note (re-find #"（.+）$" law-name)
        law-name (string/replace law-name #"（.+）$" "")]
    (render-file
     "law.md.template"
     {:name law-name
      :name-note law-name-note
      :category (:LawCategory law)
      :level (:LawLevel law)
      :modified-date (format-modified-date (:LawModifiedDate law))
      :url (:LawURL law)
      :foreword (-> (:LawForeword law) trim-line-breaks)
      :abandoned (:LawAbandonNote law)
      :chapters (compose-chapters (:LawArticles law))
      :articles (compose-law-articles (:LawArticles law))
      :attachments (format-attachments (:LawAttachements law))
      :histories (format-histories (:LawHistories law))})))

(defn- process-law-json [law]
  (let [dir (->> law :LawCategory
                 (re-seq #"[^＞\s]+")
                 (string/join "/"))
        law-name (:LawName law)
        law-name (string/replace law-name #"（.+）$" "")
        json-path (str "json/" dir "/" law-name ".json")
        md-path (str "laws/" dir "/" law-name ".md")]
    (fs/create-dirs (str "json/" dir))
    (fs/create-dirs (str "laws/" dir))
    (spit json-path (json/encode law {:pretty true}))
    (spit md-path (law-json->markdown law))))

(defn process-law-jsons [laws]
  (run! process-law-json laws))

