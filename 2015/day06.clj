(ns day06
  (:require [clojure.string :as str]))

(def size 1000)

(def input (str/split-lines (slurp "./day06.input")))

(def line-re #"(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)")

(def action-map {"turn on" :on "turn off" :off "toggle" :toggle})

(defn line [text]
  (let [[_ action & coords] (re-find line-re text)
        [fx fy tx ty] (map #(Integer/parseInt %) coords)]
    [(get action-map action) [fx fy] [tx ty]]))

(def actions (map line input))

(def grid (vec (repeat (* size size) 0)))

(defn points
  "Gen list of points contained by p1-p2"
  [[x0 y0] [x1 y1]]
  (for [x (range x0 (inc x1))
        y (range y0 (inc y1))]
    (+ x (* y size))))

(defn off [val] (max (dec val) 0))
(defn on [val] (inc val))
(defn toggle [val] (+ 2 val))

(defn paint [grid action p1 p2]
  (let [action-fn ({:off off :on on :toggle toggle} action)
        coords (points p1 p2)]
    (apply assoc grid
           (flatten (map #(list % (action-fn (get grid % 0))) coords)))))

(defn count-on [grid]
  (reduce + 0 grid))

(defn run-actions [grid actions]
  (reduce (fn [grid [action p1 p2]] (paint grid action p1 p2)) grid actions))

(comment

  (count (points [0 0] [9 9]))

  (-> grid
      (paint :toggle [0 0] [999 999])
      (count-on))

  (-> grid
      (run-actions actions)
      (count-on))

  (points [0 0] [1 1])

  (first input)

  (line (first input))

  (map line
       (take 20 input))
  )
  
  