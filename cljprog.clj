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

(defn looptest1 []
  (loop
      [x 5]
    (if (neg? x)
      x
      (recur (dec x)))))

(defn countdown
  [x]
  (if (zero? x)
    :blastoff!
    (do (println x)
        (recur (dec x)))))

(defn embedded-repl
  "a naive Clojure REPL implementation. Enter ':quit' to exit"
  []
  (print (str (ns-name *ns*) ">>> "))
  (flush)
  (let [expr (read)
        value (eval expr)]
    (when (not= :quit value)
      (println value)
      (recur))))

(defn reducetest1
  []
  (reduce (fn [m v]
            (assoc m v (* v v)))
          {}
          [1 2 3 4]))

(def only-strings (partial filter string?))

(defn nageted-sum-str1
  [& numbers]
  (str (- (apply + numbers))))

(def nageted-sum-str (comp str - +))

(require '[clojure.string :as str])

(def camel->keyword1 (comp keyword
                           str/join
                           (partial interpose \-)
                           (partial map str/lower-case)
                           #(str/split % #"(?<=[a-z])(?=[A-Z])")))

(defn camel->keyword
  [s]
  (->> (str/split s #"(?<=[a-z])(?=[A-Z])")
       (map str/lower-case)
       (interpose \-)
       str/join
       keyword))


(def camel-pairs->map (comp (partial apply hash-map)
                            (partial map-indexed (fn [i x]
                                                   (if (odd? i)
                                                     x
                                                     (camel->keyword x))))))

(defn adder
  [n]
  (fn [x] (+ n x)))

(defn doubler
  [f]
  (fn [& args]
    (* 2 (apply f args))))

(def doubler-+ (doubler +))
(defn print-logger
  [writer]
  #(binding [*out* writer]
     (println %)))

(def *out*-logger (print-logger *out*))

(def writer (java.io.StringWriter.))

(def retained-logger (print-logger writer))

(require 'clojure.java.io)

(defn file-logger
  [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
     ((print-logger f) %)))

(def log->file (file-logger "messages.log"))

(defn multi-logger
  [& logger-fns]
  #(doseq [f logger-fns]
     (f %)))

(def log (multi-logger
          (print-logger *out*)
          (file-logger "messages.log")))

(defn timestamped-logger
  [logger]
  #(logger (format "[%1$tY-%1$tm-%1$te %1$tH:%1$tM:%1$tS] %2$s" (java.util.Date.) % )))

(def log-timestamped (timestamped-logger
                      (multi-logger
                       (print-logger *out*)
                       (file-logger "messages.log"))))

(defn prime?
  [n]
  (cond
    (== 1 n) false
    (== 2 n) true
    (even? n) false
    :else (->> (range 3 (inc (Math/sqrt n)) 2)
               (filter #(zero? (rem n %)))
               empty?)))

(defn swap-pairs
  [sequential]
  (into (empty sequential)
        (interleave
         (take-nth 2 (drop 1 sequential))
         (take-nth 2 sequential))))

(defn map-map
  [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))

(defn random-ints
  "Returns a lazy seq of random integers in the range [0,limit]."
  [limit]
  (lazy-seq
   (println "realizing random number")
   (cons (rand-int limit)
         (random-ints limit))))

(def rands (take 10 (random-ints 50)))

(defn magnitude
  [x]
  (-> x Math/log10 Math/floor))

(defn compare-magnitude1
  [a b]
  (- (magnitude a) (magnitude b)))

(defn compare-magnitude
  [a b]
  (let [diff (- (magnitude a) (magnitude b))]
    (if (zero? diff)
      (compare a b)
      diff)))

(defn interpolate
  "Takes a collection of points (as [x y] tuples), returing a function which is
  a liner interpolation between those points."
  [points]
  (let [results (into (sorted-map) (map vec points))]
    (fn
      [x]
      (let [[xa ya] (first (rsubseq results <= x))
            [xb yb] (first (subseq results > x))]
        (if (and xa xb)
          (/ (+ (* ya (- xb x)) (* yb (- x xa)))
             (- xb xa))
          (or ya yb))))))

(def playlist
  [{:title "Elephant", :artist "The White Stripes", :year 2003}
   {:title "Helioself", :artist "Papas Fritas", :year 1997}
   {:title "Stories from the City, Stories from the Sea", :artist "PJ Harvey", :year 2000}
   {:title "Buildings and Grounds", :artist "Papas Fritas", :year 2002}
   {:title "Zen Rodeo", :artist "Mardi Gras BB", :year 2002}])

(defn summarize [{:keys [title artist year]}]
  (str title " / " artist " / " year))

(defn reduce-by
  [key-fn f init coll]
  (reduce (fn [summaries x]
            (let [k (key-fn x)]
              (assoc summaries k (f (summaries k init) x))))
          {} coll))

(def orders
  [{:product "Clock", :customer "Wile Coyote", :qty 6, :total 300}
   {:product "Dynamite", :customer "Wile Coyote", :qty 20, :total 5000}
   {:product "Shotgun", :customer "Elmer Fudd", :qty 2, :total 800}
   {:product "Shells", :customer "Elmer Fudd", :qty 4, :total 100}
   {:product "Hole", :customer "Wile Coyote", :qty 1, :total 1000}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Wile Coyote", :qty 6, :total 900}])
