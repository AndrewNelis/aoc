#!/usr/bin/env bb

(require '[clojure.string :as str])

(def test-input "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")

(def puzzle-input "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1")

(defn properties-map [ingredients]
  (->> ingredients
       (map #(str/split % #" " 2))
       (map #(vector (first %) (Integer/parseInt (second %))))
       (apply conj {})))

(defn parse-ingredient [line]
  (let [[name properties] (str/split line #": ", 2)
        ingredients (str/split properties #", ")]
    (vector name (properties-map ingredients))))

(defn just-numbers [[_ ingredients]]
  (vals ingredients))

(defn ingredient-scores [ingredients]
  (->> ingredients
       (str/split-lines)
       (map parse-ingredient)
       (map just-numbers)
      ;;; (scores calorie-count)
       (map #(list (butlast %) (last %)))
       ))

(defn mult-row [n row]
  (map #(* n %) row))

(defn calorie-count 
  "Determine calorie count for a given combination"
  [combination ingredients]
  (let [calories (map second ingredients)
        calorie-list (map * calories combination)]
    (reduce + calorie-list)))

(defn score-combination
  "Scores a given combination of ingredients
   combination - [1,2] ingredient amounts
   ingredients - [([1 -1 3] 2)  ([2 2 3] 2)] ingredient scores
   calorie-limit - 500
   "
  [combination ingredients]
  (if (> (calorie-count combination ingredients) 500)
    0
    (->> ingredients
       ;; Want the ingredient scores
         (map first)
       ;; Multiply each set of ingredients by its combination
         (map mult-row combination)
       ;; Add the rows up
         (apply map +)
       ;; Combination becomes zero if negative
         (map #(max % 0))
       ;; Multiply values together
         (reduce *))))

(defn divvy [buckets size accum]
  (if (= buckets 0)
    (concat accum [size])
    (for [n (range size -1 -1)]
      (divvy (dec buckets) (- size n) (concat accum [n])))))

(defn combinations 
  "Generate ingredient combination ratios"
  [buckets size]
  (->> []
       (divvy (dec buckets) size)
       (flatten)
       (partition buckets)))

(defn highest-score
  "Determine the best scoring ingredient combination"
  [ingredient-text]
  (let [scores (ingredient-scores ingredient-text)
        n-ingredients (count scores)
        all-combos (combinations n-ingredients 100)]
    (reduce (fn [max-score combo]
              (let [score (score-combination combo scores)]
                (if (< max-score score)
                  (do
                    (println "New High Score" score " for combo " combo)
                    score)
                  max-score)))
            0 all-combos)))

(highest-score puzzle-input)


(comment

  (def test-scores (ingredient-scores test-input))
 
  (score-combination [44 56] test-scores)
  (score-combination [40 60] test-scores)


  )
