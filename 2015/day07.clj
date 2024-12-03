(ns day07
  (:require [clojure.string :as str]))

(def input (slurp "day07.input"))

(def sample-input
  "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")


(defn value [x]
  (try (Integer/parseInt x)
       (catch NumberFormatException _ (keyword x))))


(defn parse-entry
  "Parse entry into [symbol [args...] destination]"
  ; 123 -> x
  ([a _ b] [:set [(value a)] (keyword b)])
  ; NOT x -> f
  ([op a _ b] [(keyword (str/lower-case op)) [(value a)] (keyword b)])
  ; x AND y -> z
  ([a op b _ c] [(keyword (str/lower-case op)) [(value a) (value b)] (keyword c)]))


(defn parse-line [line]
  ; NNNN -> X
  ; x AND y -> z
  ; p LSHIFT 2 -> q
  ; NOT e -> f
  ; x OR y -> z
  ; p RSHIFT 2 -> q
  ; 
  (let [parts (str/split line #" ")]
    (apply parse-entry parts)))



(defn parse-instructions [instructions]
  (->> instructions
       (str/split-lines)
       (map parse-line)))


(def gate-methods
  {:set identity
   :lshift bit-shift-left
   :rshift bit-shift-right
   :not bit-not
   :and bit-and
   :or bit-or})


(defn eval-gate [^hash-map regs gate args]
  (let [method (gate-methods gate)
        values (map #(if (keyword? %) (get regs %) %) args)
        ready? (every? int? values)]
    (if ready?
      (bit-and (apply method values) 16rFFFF)
      nil)))

(defn eval-instruction [^hash-map regs [gate args dest]]
  (let [result (eval-gate regs gate args)]
    (if (some? result)
      (assoc regs dest result)
      regs)))


(defn eval-all-instructions [regs instructions]
  (loop [regs regs
         instructions instructions]
    (if (empty? instructions)
      regs
      (recur (eval-instruction regs (first instructions)) (rest instructions)))))


(defn wait-for-register [regs instructions attr]
  (loop [regs regs
         lastcount -1]
    (let [count (count (filter some? (vals regs)))]
      ;; Assert gate count is increasing!
      (assert (> count lastcount))
      (if (some? (regs attr))
        (regs attr)
        (recur (eval-all-instructions regs instructions) count)))))


(comment

  (def instructions (parse-instructions input))
  (count instructions)

  (wait-for-register {} instructions :a)

  (def a-value 16076)

  (eval-all-instructions {} instructions)

  (def not-b-instructions (filter #(not= (last %) :b) instructions))

  ; Now, take the signal you got on wire a, override wire b to that signal
  (wait-for-register {:b a-value} not-b-instructions :a)

  (eval-gate {} :and [6 2])
  (eval-gate {:a 1 :b 1} :and [:a :b])
  (eval-gate {:a 0 :b 1} :and [:a :b])
  (eval-gate {:a 0 :b 1} :and [:a :c])

  (not-any? nil? [1 2 3])

  sample-input
  (->> (str/split-lines sample-input)
       (map parse-line))
  )