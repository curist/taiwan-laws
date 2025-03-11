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

(defn- compose-law-articles [articles]
  (->> articles
       (map (fn [{:keys [ArticleType ArticleNo ArticleContent]}]
              (if (= "C" ArticleType)
                (if (is-section? ArticleContent)
                  (str "#### " (string/trim ArticleContent))
                  (str "### " (string/trim ArticleContent)))
                ;; articles
                (str "##### " ArticleNo "\n" ArticleContent ;; TODO: formatting this
                     ))))
       (string/join "\n\n")))

(defn- law-json->markdown [law]
  (let [law-name (:LawName law)
        law-name-note (re-find #"（.+）$" law-name)
        law-name (string/replace law-name #"（.+）$" "")
        foreword (:LawForeword law)
        foreword (if (empty? foreword) "" (trim-line-breaks foreword))
        articles (compose-law-articles (:LawArticles law))]
    (render-file
     "law.md.template"
     {:name law-name
      :name-note law-name-note
      :category (:LawCategory law)
      :modified-date (format-modified-date (:LawModifiedDate law))
      :url (:LawURL law)
      :foreword foreword
      :abandoned (:LawAbandonNote law)
      ;; :chapters :TODO
      :articles articles
      ;; :attachments :TODO
      ;; :histories :TODO
      })))

(defn- process-law-json [law base-dir]
  (let [dir (->> law :LawCategory
                 (re-seq #"[^＞\s]+")
                 (string/join "/")
                 (str base-dir "/"))
        law-name (:LawName law)
        law-name (string/replace law-name #"（.+）$" "")
        json-path (str dir "/" law-name ".json")
        md-path (str dir "/" law-name ".md")]
    (fs/create-dirs dir)
    (spit json-path (json/encode law {:pretty true}))
    (spit md-path (law-json->markdown law))))

(defn process-law-jsons [laws base-dir]
  (run! #(process-law-json % base-dir) laws))

; (def the-law :the-law) ;; (first laws)
; (->> (:LawArticles the-law)
;      (partition-by :ArticleType)
;      (partition 2))

;; check article 27 & https://law.moj.gov.tw/LawClass/LawSingle.aspx?pcode=A0000001&flno=27
;; 30 also
;; should handle 節, like https://law.moj.gov.tw/LawClass/LawAllPara.aspx?pcode=A0000001
;; TODO: can we use chinese chapter names & use them as github markdown TOC target?
;; # LawName
;; ## laws 法條
;; ### 章 chapter
;; #### 節 chapter
;; ##### 條 article
;;
;; interesting law fields
;; LawModifiedDate 法規異動日期
;; LawLevel 法規位階
;; LawForeword 前言
;; LawHistories
;; LawURL
;; LawAttachements?

(comment
  (def laws (-> (io/reader "blob/law.jq.json") (json/parse-stream true) :Laws))
  (def orders (-> (io/reader "blob/order.jq.json") (json/parse-stream true) :Laws))

  (extract-laws laws "laws")
  (extract-laws orders "laws")
  (defn filter-laws [laws filter-txt]
    (filter (fn [law]
              (and (not= "廢" (:LawAbandonNote law))
                   (or (re-find (re-pattern filter-txt) (:LawName law))
                       (re-find (re-pattern filter-txt) (:LawHistories law))
                       (some #(re-find (re-pattern filter-txt) (:ArticleContent %))
                             (:LawArticles law)))))
            laws))

  (defn find-laws [laws law-name]
    (filter #(re-find (re-pattern law-name) (:LawName %)) laws))

  (count laws)
  (count orders)

  (-> (filter-laws orders "二級電信管制射頻器材") count)
  (find-laws laws "科學技術")

  (find-laws laws "國家通訊傳播委員會組織法")
  (find-laws laws "財團法人國家同步輻射研究中心設置條例")
  (find-laws orders "證券投資顧問事業管理規則")

  (->> (filter #(and (= "廢" (:LawAbandonNote %))
                     (re-find #"^廢止法規" (:LawCategory %))) laws) count)

  (->> (filter #(and (= "廢" (:LawAbandonNote %))
                     (re-find #"^廢止法規" (:LawCategory %))) orders) count)

  (count *1))


