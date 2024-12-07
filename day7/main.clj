(require '[clojure.string :as str])

(defn read-input
  [filename]
  (->> (slurp filename)
       str/split-lines
       (map #(str/split % #":"))
       (map #(map str/trim %))
       (map #(vector (parse-long (first %))
                     (mapv parse-long (str/split (second %) #" "))))))

(defn can-be-combined?
  [test values total]
  (if (empty? values)
    (= total test)
    (or (can-be-combined? test (rest values) (* total (first values)))
        (can-be-combined? test (rest values) (+ total (first values))))))

(defn concat-longs [a b] (parse-long (str a b)))
(defn can-be-combined-concat?
  [test values total]
  (if (empty? values)
    (= total test)
    (or (can-be-combined-concat? test (rest values) (* total (first values)))
        (can-be-combined-concat? test (rest values) (+ total (first values)))
        (can-be-combined-concat? test
                                 (rest values)
                                 (concat-longs total (first values))))))


(->> (read-input "input.txt")
     (filter #(can-be-combined? (first %) (rest (second %)) (first (second %))))
     (map first)
     (apply +))

(->> (read-input "input.txt")
     (filter #(can-be-combined-concat? (first %)
                                       (rest (second %))
                                       (first (second %))))
     (map first)
     (apply +))

