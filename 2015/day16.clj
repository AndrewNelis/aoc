(require '[clojure.string :as str])

(def sender-identity-text
"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1")

(def sender
  (reduce conj {} (map #(str/split % #": " 2) (str/split-lines sender-identity-text))))

(defn parse-sue [text]
  (let [[_ sue-number properties] (re-find #"Sue (\d+): (.*)" text)
        property-list (map rest (vector (re-seq #"(.*?): (\d+)(?:, )*" properties)))
        ;; props (reduce conj {} property-list)
        ]
    (list sue-number property-list)))

;; (def all-sues
;;   (slurp "2015/day16.input"))

(comment

  sender

  (def s-line "Sue 359: samoyeds: 3, cars: 2, akitas: 4")

  (def test '[("samoyeds" "3") ("cars" "2") ("akitas" "4")])

  (into (hash-map) test)

  (into {} [(:a 1) (:b 2)])

  (parse-sue s-line)

;;   (slurp "2015/day16.input")
  )
