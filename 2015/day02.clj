(require '[clojure.string :as str])

(defn area [l w h]
  (let [a (* l w)
        b (* w h)
        c (* h l)]
    (+ (* 2 a)
       (* 2 b)
       (* 2 c)
       (min a b c))))

(defn ribbon [l w h]
  (let [[a b] (sort [l w h])]
    (+ a a b b (* l w h))))

(defn parse-area [str-area]
  (map #(Integer/parseInt %1) (str/split str-area #"x")))

(def puzzle-input (slurp "2.input"))

(comment
  (area 2 3 4)
  (area 1 1 10)
  (ribbon 2 3 4)
  (ribbon 1 1 10)
  (reduce + (map #(->> %1 (parse-area) (apply ribbon)) (str/split-lines puzzle-input)))

  )
