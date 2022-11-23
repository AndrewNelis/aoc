

(defn eval-instr [floor instr]
  (+ floor ({\( 1 \) -1} instr)))

(defn eval-elevator [instructions]
  (reduce eval-instr 0 instructions))

(def puzzle-input (slurp "1.input"))

(defn find-basement [instr]
  (loop [floor 0
         instr instr
         count 1]
    (let [next-floor (eval-instr floor (first instr))]
      (if (== next-floor -1)
        count
        (recur next-floor (rest instr) (inc count))))))

(comment
  (eval-elevator "(()))")
  (find-basement puzzle-input)
  (eval-elevator puzzle-input)
  )
