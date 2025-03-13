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
  (not (nil? (or (re-find #"^.{1,2}[　、]" s)
                 (re-find #"^.（{1,2}）" s)))))

(defn- grouping-sub-articles
  ([article]
   (grouping-sub-articles [] (string/split article #"\r\n")))
  ([grouped sub-articles]
   (if (empty? sub-articles)
     grouped
     (let [[article & rest'] sub-articles]
       (if (and (nil? (re-find #"：$" article))
                (not (is-sub-article-content? article)))
         (grouping-sub-articles (conj grouped article) rest')
         (let [[article-body & [rest']]
               (split-with is-sub-article-content? rest')]
           (grouping-sub-articles
            (conj grouped
                  (string/join "  \n" (cons article article-body)))
            rest')))))))

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
        law-name (string/replace law-name #"（.+）$" "")
        foreword (:LawForeword law)
        foreword (if (empty? foreword) "" (trim-line-breaks foreword))
        articles (compose-law-articles (:LawArticles law))
        chapters (compose-chapters (:LawArticles law))
        attachments (format-attachments (:LawAttachements law))
        histories (format-histories (:LawHistories law))]
    (render-file
     "law.md.template"
     {:name law-name
      :name-note law-name-note
      :category (:LawCategory law)
      :modified-date (format-modified-date (:LawModifiedDate law))
      :url (:LawURL law)
      :foreword foreword
      :abandoned (:LawAbandonNote law)
      :chapters chapters
      :articles articles
      :attachments attachments
      :histories histories})))

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

