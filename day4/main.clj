(require '[clojure.string :as str])

(defn get-2d "Get value of m[x][y]" [m [x y]] ((m y) x))

(defn inbound
  [m [x y]]
  (and (>= x 0) (>= y 0) (< y (count m)) (< x (count (m 0)))))

(defn check-xmas
  [m points]
  (and (every? (partial inbound m) points)
       (= "XMAS" (apply str (map #(get-2d m %) points)))))

(defn xmas-directions
  [[x y]]
  [[[x y] [(+ x 1) y] [(+ x 2) y] [(+ x 3) y]]
   [[x y] [(- x 1) y] [(- x 2) y] [(- x 3) y]]
   [[x y] [x (+ y 1)] [x (+ y 2)] [x (+ y 3)]]
   [[x y] [x (- y 1)] [x (- y 2)] [x (- y 3)]]
   [[x y] [(- x 1) (- y 1)] [(- x 2) (- y 2)] [(- x 3) (- y 3)]]
   [[x y] [(- x 1) (+ y 1)] [(- x 2) (+ y 2)] [(- x 3) (+ y 3)]]
   [[x y] [(+ x 1) (- y 1)] [(+ x 2) (- y 2)] [(+ x 3) (- y 3)]]
   [[x y] [(+ x 1) (+ y 1)] [(+ x 2) (+ y 2)] [(+ x 3) (+ y 3)]]])

(defn count-xmas
  [m]
  (count (filter identity
           (flatten (for [x (range 0 (count (m 0)))
                          y (range 0 (count m))]
                      (map #(check-xmas m %) (xmas-directions [x y])))))))

(defn get-main-diag
  [m x y]
  [(get-2d m [(- x 1) (- y 1)]) (get-2d m [x y]) (get-2d m [(+ x 1) (+ y 1)])])
(defn get-sec-diag
  [m x y]
  [(get-2d m [(+ x 1) (- y 1)]) (get-2d m [x y]) (get-2d m [(- x 1) (+ y 1)])])
(defn is-mas
  [m [x y]]
  (let [main-diag-str (apply str (get-main-diag m x y))
        sec-diag-str (apply str (get-sec-diag m x y))]
    (and (or (= main-diag-str "MAS") (= main-diag-str "SAM"))
         (or (= sec-diag-str "MAS") (= sec-diag-str "SAM")))))

(defn for-mas
  [m]
  (map #(is-mas m %)
    (for [x (range 1 (- (count (m 0)) 1)) y (range 1 (- (count m) 1))] [x y])))

; First task of Day 4
(->> (slurp "input.txt")
     str/split-lines
     (map vec) ; convert each line to vector of character
     vec
     count-xmas)

; Second task of Day 4
(->> (slurp "input.txt")
     str/split-lines
     (map vec) ; convert each line to vector of character
     vec
     for-mas
     (filter identity)
     count)
