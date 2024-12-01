(require '[clojure.string :as str])

(defn split-lists
  "Reads input file `filename`, splits each line by whitespace
   and constructs two sorted lists of int's"
  [filename]
  (let [pairs (map #(str/split % #"\s+") (str/split-lines (slurp filename)))
        first-list (sort (map (comp Integer/parseInt first) pairs))
        second-list (sort (map (comp Integer/parseInt second) pairs))]
    [first-list second-list]))

(defn total-distance
  "Calculates total distance between two *sorted* lists of the *same size*"
  ([first-list second-list] (total-distance first-list second-list 0))
  ([first-list second-list total]
   (if (empty? first-list)
     total
     (total-distance (rest first-list)
                     (rest second-list)
                     (+ total
                        (abs (- (first first-list) (first second-list))))))))

(defn similarity
  "Counts up total similarity between two lists"
  ([coll freq] (similarity coll freq 0))
  ([coll freq total]
   (if (empty? coll)
     total
     (similarity (rest coll)
                 freq
                 (+ total (* (first coll) (get freq (first coll) 0)))))))

; First part of Day 1's task
(defn aoc1 [filename] (apply total-distance (split-lists filename)))

; Second part of Day 1's task
(defn aoc2
  [filename]
  (let [[first-list second-list] (split-lists filename)]
    (similarity (set first-list) (frequencies second-list))))

(aoc1 "input.txt")

(aoc2 "input.txt")
