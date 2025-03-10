(ns taiwan-laws
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :refer [shell]]
            [cheshire.core :as json]))

(def ch-laws-url "https://law.moj.gov.tw/api/data/chlaw.json.zip")
(def ch-orders-url "https://law.moj.gov.tw/api/data/chorder.json.zip")

(defn download-and-unzip-laws []
  (-> (http/get ch-laws-url {:as :stream}) :body
      (fs/unzip "blob" {:replace-existing true}))
  (-> (http/get ch-orders-url {:as :stream}) :body
      (fs/unzip "blob" {:replace-existing true})))

(defn jq-process-jsons []
  (shell {:in (io/reader "blob/ChLaw.json")
          :out :write
          :out-file "blob/law.jq.json"} "jq .")
  (shell {:in (io/reader "blob/ChOrder.json")
          :out :write
          :out-file "blob/order.jq.json"} "jq .")
  nil)

(defn extract-laws [laws]
  (-> (fn [law]
        (let [dir (->> law :LawCategory
                       (re-seq #"[^＞\s]+")
                       (string/join "/")
                       (str "laws/"))
              law-name (:LawName law)
              law-name (string/replace law-name #"（.+）$" "")
              file-path (str dir "/" law-name ".json")]
          (fs/create-dirs dir)
          (spit file-path (json/encode law {:pretty true}))))
      (run! laws)))

(comment
  (def laws (-> (io/reader "blob/law.jq.json") (json/parse-stream true) :Laws))
  (def orders (-> (io/reader "blob/order.jq.json") (json/parse-stream true) :Laws))

  (extract-laws laws)
  (extract-laws orders)
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


