

(def moves {\^ [0  1]
            \v [0 -1]
            \> [1  0]
            \< [-1 0]})


(defn move-sleigh 
  "Move sleigh at [x y] to new position"
  [[x y] move]
  (let [[dx dy] (moves move)]
    [(+ x dx) (+ y dy)]))

(defn run-route [route] 
  (reduce 
   (fn [[pos visited] move]
     (let [next-pos (move-sleigh pos move)]
       [next-pos (conj visited next-pos)]))
   [[0 0] #{[0 0]}] route))

(defn count-deliveries
  "Count the number of distinct locations visited along the given route"
  [route]
  (->> route
       (run-route)
       (second)
       (count)))

(def puzzle-input (slurp "3.input"))

(defn split-route [route]
  (let [route-1 (take-nth 2 (rest route))
        route-2 (take-nth 2 route)]
    [route-1 route-2]))

(defn robo-santa
  "Run the route, split in 2"
  [route]
  (let [[r1 r2] (split-route route)
        s1 (run-route r1)
        s2 (run-route r2)
        all-visited (into (second s1) (second s2))]
    (count all-visited)))

(comment

  (robo-santa "^v^v^v^v^v")
  
  (count-deliveries puzzle-input)
  (robo-santa puzzle-input)

  (move-sleigh [0 0] \^)
  (into #{1 2 3} #{4 5 6} )
  )
