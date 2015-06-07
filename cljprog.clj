;; clojure programming
(defn ldcp
  []
  (load-file "cljprog.clj"))

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn hypot
  [x y]
  (let [x2 (* x x)
        y2 (* y y)]
    (Math/sqrt (+ x2 y2))))

(def m {:a 5 :b 6
        :c [7 8 9]
        :d {:e 10 :f 11}
        "foo" 88
        42 false})
