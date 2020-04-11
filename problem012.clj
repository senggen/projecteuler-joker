;; Problem 12
;; Highly divisible triangular number

(defn triangular [n]
  (reduce + (range (inc n))))

(defn prime? [n]
  (cond
    (= n 1) true
    (< n 4) true
    (zero? (mod n 2)) false
    (< n 9) true
    (zero? (mod n 3)) false
    :else (let [r (Math/floor (Math/sqrt n))]
      (loop [k 5]
        (cond
          (> k r) true
          (zero? (mod n k)) false
          (zero? (mod n (+ k 2))) false
          :else (recur (+ k 6)))))))

(defn factor [n]
  (loop [i 2 m n acc {}]
    (if (>= i n)
      acc 
      (if (and (zero? (mod m i)) (prime? i))
        (recur i (/ m i) (update acc i #(if (nil? %) 1 (inc %))))
        (recur (inc i) n acc)))))

(defn count-factors [factors]
  (reduce * (map inc (vals factors))))

(defn f []
  (loop [i 0 tri 0]
    (let [fs (factor tri)
          len (count-factors fs)]
      (if (>= len 500)
        (identity [tri len])
        (do (if (zero? (mod i 100)) (println i tri len)) (recur (inc i) (+ tri i)))))))

(println (f))
