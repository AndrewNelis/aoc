(ns day05
  (:require [clojure.set] 
            [clojure.string :as str]))

(defn has-three-vowels? [word]
  (<= 3 (count (re-seq #"[aeiou]" word))))

(defn has-repeated-letter? [word]
  (boolean (re-find #"(.)\1" word)))

(defn no-forbidden-strings? [word]
  (let [absent? (complement str/includes?)]
    (and (absent? word "ab")
         (absent? word "cd")
         (absent? word "pq")
         (absent? word "xy"))))

(def puzzle-input (str/split-lines (slurp "5.input")))

(defn nice?
  "is a word nice?"
  [word]
  (and (has-three-vowels? word)
       (has-repeated-letter? word)
       (no-forbidden-strings? word)))

(defn pair-of-pairs? [word]
  (not (nil? (re-find #"(..).*?\1" word))))

(defn pair-middle? 
  "it has a pair of letters with another inbetween"
  [word]
  (not (nil? (re-find #"(.).\1" word))))

(defn nice2? [word]
  (and (pair-of-pairs? word)
       (pair-middle? word)))

(comment
  (nice? "ugknbfddgicrmopn")
  (nice? "aaa")
  (nice? "jchzalrnumimnmhp")
  (nice? "haegwjzuvuyypxyu")
  (nice? "dvszwmarrgswjxmb")
  
  ;; Part 1 - how many nice words in the list?
  (count (filter nice? puzzle-input))
  
  ;; Part 2 New rules!
  (pair-of-pairs? "aaa")
  (pair-of-pairs? "aabcdefgaa")
  (pair-middle? "aba")
  
  (count (filter nice2? puzzle-input))
  )
