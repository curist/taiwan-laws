(ns taiwan-laws-test
  (:require [clojure.test :refer [deftest is]]
            [taiwan-laws :as t]))

(deftest test-grouping-sub-articles
  (let [article "銀行：\r\n一、\r\n二、守法性：\r\n最近。"]
    (is (= 1 (count (t/grouping-sub-articles article))))))

