(ns day04
  (:require [clojure.string :as str]))

(defn md5sum [text]
  (let [md5 (java.security.MessageDigest/getInstance "MD5")
        _ (.update md5 (byte-array (map byte text)))
        result (.digest md5)]
    (format "%032x" (BigInteger. 1 result))))

(defn find-hash [key]
  (first (filter (fn [n]
                   (str/starts-with? (md5sum (str key n)) "000000")) (range))))

(comment
  (find-hash "abcdef")
  (find-hash "pqrstuv")
  (find-hash "input here!")
  )
