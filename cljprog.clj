;; clojure programming
(defn ldcp
  []
  (load-file "cljprog.clj"))

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn maptest1 []
  (map
   clojure.string/lower-case
   ["Java" "Imperative" "Weeping"
    "Clojure" "Learning" "Peace"]))

(defn apply1 []
  (apply hash-map
         [:a 5 :b 6]))

(def args_1 [2 -2 10])

(defn apply2 []
  (apply * 0.5 3 args_1))

(def reducetest1
  (reduce
   (fn [m v]
     (assoc m v (* v v)))
   {}
   [1 2 3 4]))

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

(defn memoizetest1
  []
  (do
    (time (prime? 1125899906842679))
    (let [m-prime? (memoize prime?)]
      (time (m-prime? 1125899906842679))
      (time (m-prime? 1125899906842679)))))

(defn memoizetest2 []
  (do
    (repeatedly 10 (partial rand-int 10))
    (repeatedly 10 (partial (memoize rand-int) 10))))

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

(defn seqtest2 []
  (let [r (range 3) rst
        (rest r)]
    (prn (map str rst))
    (prn (map #(+ 100 %) r))
    (prn (conj r -1) (conj rst 42))))

(defn seq-vs-list []
  "list 保存元素数量，count很快返回。seq只能遍历"
  (let [s (range 1e6)]
    (prn (time (count s))))
  (let [s (apply list (range 1e6))]
    (prn (time (count s)))))

(defn seq-test3 []
  "头保持会导致内存无法释放，从而被耗尽"
  (let [[t d] (split-with #(< % 12) (range 1e8))]
    [(count d) (count t)]))

(defn seq-test4 []
  "交换处理顺序"
  (let [[t d] (split-with #(< % 12) (range 1e8))]
    [(count t) (count d)]))
(defn seq-test5 []
  "显式创建seq"
  (do
    (prn (cons 0 (range 1 5)))
    (prn (cons :a [:b :c :d]))
    (prn (cons 0 (cons 1 (cons 2 (cons 3 (range 4 10))))))
    (prn (list* 0 1 2 3 (range 4 10)))))

(defn random-ints
  "Returns a lazy seq of random integers in the range [0,limit]."
  [limit]
  (lazy-seq
   (println "realizing random number")
   (cons (rand-int limit)
         (random-ints limit))))

(defn random-ints2 []
  "使用repeatedly创建惰性序列"
  (do
    (repeatedly 10 (partial rand-int 50))))

(def rands (take 10 (random-ints 50)))

(defn rest-vs-next []
  "rest简单返回序列尾巴，不需要像next强制实例化第一个元素。
顺序结构使用next"
  (let [_ (next (random-ints 50))
        _ (prn "rest:")
        _ (rest (random-ints 50))
        _ (prn "--")
        [_ & rest] (random-ints 50)]))


(defn doall-vs-dorun []
  "dorun不关注返回值"
  (prn "doall:")
  (prn   (doall (take 5 (random-ints 50))))
  (prn "dorun:")
  (prn   (dorun (take 5 (random-ints 50)))))

(defn assoc-1 []
  "assco, dissoc, get, contains?"
  (let [m {:a 1 :b 2 :c 3 :d 4}
        v [1 2 3]]
    (prn "get:" (get m :b))
    (prn "get:"(get m :d))                    ;获取不存在的值
    (prn "get:"(get m :d "not-found"))        ;提供默认值
    (prn "assoc:"(assoc m :d 4))
    (prn "dissoc:"(dissoc m :b))
    (prn "assoc multi:" (assoc m :x 4 :y 5 :z 6))
    (prn "dissoc multi:" (dissoc m :a :c))

    (prn "vertor-test:")
    (prn "get:" (get v 1))
    (prn "get:" (get v 10))
    (prn "get:" (get v 10 "not-found"))
    (prn "assoc:" (assoc v 1 4 0 -12 2 :p)) ;使用assoc更新数组
    (prn "assoc:" (assoc v 3 10))

    (prn "set-test:")
    (prn "get:" (get #{1 2 3} 2))
    (prn "get:" (get #{1 2 3} 4))
    (prn "get:" (get #{1 2 3} 4 "not-found"))
    (prn "get:" (when (get #{1 2 3} 2)
                  (prn "it contains '2'!"))) ;set中值到本身映射

    (prn "contains-test:")
    (prn "contains?:" (contains? [1 2 3] 0))
    (prn "contains?:" (contains? {:a 5 :b 6} :b))
    (prn "contains?:" (contains? {:a 5 :b 6} 42))
    (prn "contains?:" (contains? #{1 2 3} 1))
    (prn "contains?:" (contains? [1 2 3] 3))
    (prn "contains?:" (contains? [1 2 3] 2))
    (prn "contains?:" (contains? [1 2 3] 0))

    (prn "get:" (get "Clojure" 3))
    (prn "contains?:" (contains? (java.util.HashMap.) "not-there"))
    (prn "get:" (get (into-array [1 2 3]) 0))
    (prn "get:" (get {:ethel nil} :lucy))
    (prn "get:" (get {:ethel nil} :ethel))
    (prn "find:" (find {:ethel nil} :lucy))
    (prn "find:" (find {:ethel nil} :ethel))
    (prn "find:"
         (if-let [e (find {:a 5 :b 6} :a)]
           (format "found %s => %s" (key e) (val e))
           "not-found"))
    (prn "find:"
         (if-let [[k v] (find {:a 5 :b 6} :a)]
           (format "found %s => %s" k v)
           "not-found"))
    ))

(defn indexed-1 []
  (prn "nth:" (nth [:a :b :c] 2))
  (prn "get:" (get [:a :b :c] 2))
  ;; (prn "nth:" (nth [:a :b :c] 3))
  (prn "get:" (get [:a :b :c] 3))
  ;; (prn "nth:" (nth [:a :b :c] -1))
  (prn "get:" (get [:a :b :c] -1))
  (prn "get:" (get [:a :b :c] -1 :not-found))
  (prn "nth:" (nth [:a :b :c] -1 :not-found))
  )

(defn stack-1 []
  "tests for stack"
  (prn "list:")
  (prn "conj:" (conj '() 1))
  (prn "conj:" (conj '(2 1) 3))
  (prn "peek:" (peek '(3 2 1)))
  (prn "pop:" (pop '(3 2 1)))
  (prn "pop:" (pop '(1)))
  ;; 数据添加在尾部
  (prn "vector:")
  (prn "conj:" (conj [] 1))
  (prn "conj:" (conj [1 2] 3))
  (prn "peek:" (peek [1 2 3]))
  (prn "pop:" (pop [1 2 3]))

  (prn "set:")
  (prn "get:" (get #{1 2 3} 2))
  (prn "get:" (get #{1 2 3} 4))
  (prn "get:" (get #{1 2 3} 4 "not-found"))
  (prn "disj:" (disj #{1 2 3} 3 1))
  )

(defn sorted-1 []
  (prn "sorted-1:")
  (let [sm (sorted-map :z 5 :x 9 :y 0 :b 2 :a 3 :c 4)]
    (prn sm)
    (prn "rseq:" (rseq sm))
    (prn "subseq:" (subseq sm <= :c))
    (prn "subseq:" (subseq sm > :b <= :y))
    (prn "rsubseq:" (rsubseq sm > :b <= :y))
    (prn "compare:" (compare 2 2))
    (prn "compare:" (compare "ab" "abc"))
    (prn "compare:" (compare ["a" "b" "c"] ["a" "b"]))
    (prn "compare:" (compare ["a" 2] ["a" 2 0]))
    ))
(defn sort-1 []
  (prn "sort:" (sort < (repeatedly 10 #(rand-int 100))))
  (prn "sort-by:" (sort-by first > (map-indexed vector "Clojure")))
  )

(defn sort-2 []
  (prn "sorted-map-by:" (sorted-map-by compare :z 5 :x 9 :y 0 :b 2 :a 3 :c 4))
  (prn "sorted-map-by:" (sorted-map-by (comp - compare) :z 5 :x 9 :y 0 :b 2 :a 3 :c 4))
  )

(defn assoc-2 []
  (prn "get:" ([:a :b :c] 2))
  (prn "get:" ({:a 5 :b 6} :b))
  (prn "get:" ({:a 5 :b 6} :c 7))
  (prn "get:" (#{1 2 3} 3))
  ;; (prn "get:" ([:a :b :c] -1))
  (prn "get:" (get {:a 5 :b 6} :b))
  (prn "get:" (get {:a 5 :b 6} :c 7))
  (prn "get:" (get #{:a :b :c} :d))
  )

(defn get-foo [map]
  (:foo map))
(defn get-bar [map]
  (map :bar))

(defn map-1 []
  (prn "map:" (map :name
                   [{:age 21 :name "David"}
                    {:gender :f :name "Suzanne"}
                    {:name "Sara" :location "NYC"}]))
  )

(defn some-1 []
  (prn "some:" (some #{1 3 7} [0 2 4 5 6]))
  (prn "some:" (some #{1 3 7} [0 2 3 4 5 6]))
  )

(defn filter-1 []
  (prn "filetr:" (filter :age
                         [{:age 21 :name "David"}
                          {:gender :f :name "Suzanne"}
                          {:name "Sara" :location "NYC"}]))
  (prn "filter:" (filter (comp (partial <= 25) :age) ;(partial <= 25) == (<= 25 %)
                         [{:age 21 :name "David"}
                          {:gender :f :name "Suzanne" :age 20}
                          {:name "Sara" :location "NYC" :age 34}]))
  )

(defn remove-1 []
  (prn "remove:" (remove #{5 7} (cons false (range 0 10))))
  (prn "remove:" (remove #{5 7 false} (cons false (range 0 10))))
  (prn "remove:" (remove (partial contains? #{5 7 false}) (cons false (range 0 10))))
  )

(defn list-1 []
  (prn "list:" '(1 2 3))
  (prn "list:" '(1 2 (+ 1 2)))
  (prn "list:" (list 1 2 (+ 1 2)))
  )

(defn vector-1 []
  (prn "vector:" (vector 1 2 3))
  (prn "vector:" (vec (range 5)))
  )

(defn euclidian-division [x y]
  [(quot x y) (rem x y)])
(defn euclidian-division-2 [x y]
  ((juxt quot rem) x y))

(defn set-1 []
  (prn "set:" (set [1 6 1 8 3 7 7]))
  (prn "hash-set:" (hash-set :a :b :c :d))
  )

(defn vector-2 []
  (prn "vector:" (let
                     [point-3d [42 26 -7]
                      travel-legs [["LYS" "FRA"] ["FRA" "PHL"] ["PHL" "RDU"]]]
                   (prn point-3d)
                   (prn travel-legs)))
  )

(defn numeric? [s] (every? (set "0123456789") s))

(defn set-2 []
  (prn "set:" (apply str (remove (set "aeiouy") "vowels are useless")))
  (prn "set:" (numeric? "123"))
  (prn "set:" (numeric? "42b"))
  )

(defn map-2 []
  (prn "map:" (hash-map :a 5 :b 6))
  (prn "map" (apply hash-map [:a 5 :b 6]))
  )

(defn map-3 []
  (let [m {:a 1 :b 2 :c 3}]
    (prn "keys:" (keys m))
    (prn "vals:" (vals m))
    (prn "keys:" (map key m))
    (prn "vals:" (map val m))
    ))
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

(defn reduce-by-in
  [keys-fn f init coll]
  (reduce (fn [summaries x]
            (let [ks (keys-fn x)]
              (assoc-in summaries ks
                        (f (get-in summaries ks init) x))))
          {} coll)
  )

(defn group-by-1 []
  (prn "group-by:" (group-by #(rem % 3) (range 10)))
  (prn "group-by:" (group-by :artist playlist))
  )

(def orders
  [{:product "Clock", :customer "Wile Coyote", :qty 6, :total 300}
   {:product "Dynamite", :customer "Wile Coyote", :qty 20, :total 5000}
   {:product "Shotgun", :customer "Elmer Fudd", :qty 2, :total 800}
   {:product "Shells", :customer "Elmer Fudd", :qty 4, :total 100}
   {:product "Hole", :customer "Wile Coyote", :qty 1, :total 1000}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Wile Coyote", :qty 6, :total 900}])

(defn reduce-by-1 []
  (prn "reduce-by:"
       (reduce-by :customer #(+ %1 (:total %2)) 0 orders))
  (prn "reduce-by:"
       (reduce-by :product #(conj %1 (:customer %2)) #{} orders))
  (prn "reduce-by:"
       (reduce-by (juxt :customer :product) #(+ %1 (:total %2)) 0 orders))
  )

(defn reduce-by-in-1 []
  (prn "reduce-by-in:" (reduce-by-in (juxt :customer :product)
                                     #(+ %1 (:total %2)) 0 orders))
  )


(defn update-in-1 []
  (let [version1 {:name "Chas" :info {:age 31}}
        version2 (update-in version1 [:info :age] + 3)]
    (prn "verison1:" version1 "version2:" version2)
    ))

(defn test-1 []
  "易变集合"
  (let [x (transient [])
        y (conj! x 1)]
    (prn (count y)
         (count x)))
  )


(defn naive-into
  [coll source]
  (reduce conj coll source)
  )

(defn fast-into
  [coll source]
  (persistent! (reduce conj! (transient coll) source)))

(defn naive-into-1 []
  (prn (= (into #{} (range 500))
          (naive-into #{} (range 500))))
  (prn (time (do (into #{} (range 1e6)) nil))
       (time (do (naive-into #{} (range 1e6)) nil))
       (time (do (fast-into #{} (range 1e6)) nil)))
  )

(defn transient-capable?
  [coll]
  (instance? clojure.lang.IEditableCollection coll))


(defn transient-1 []
  (prn "transient:"
       (let [tm (transient {})]
         (doseq [x (range 100)]
           (assoc! tm x 0))
         (persistent! tm)))
  )

(defn future-1 []
  (prn "future:" (let [t (transient {})]
                   @(future (get t :a))))
  )

(defn meta-1 []
  (prn "meta:" (let [a ^{:create (System/currentTimeMillis)} [1 2 3]
                     b (with-meta a
                         (assoc (meta a)
                                :modified (System/currentTimeMillis)))
                     c (vary-meta a assoc :modified (System/currentTimeMillis))]
                 (meta a)
                 (meta b)
                 (meta c)))
  (prn "meta:" (meta ^:private [1 2 3]))
  (prn "meta:" (meta ^:private ^:dynamic [1 2 3]))
  )

(defn empty-board
  "创建矩形白板"
  [w h]
  (vec (repeat w (vec (repeat h nil))))
  )

(defn populate
  ""
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))


(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]
    ))


(defn count-neighbors
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
        (let [new-liveness
              (case (count-neighbors board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (recur (assoc-in new-board [x y] new-liveness) x (inc y))
          )
        ))
    ))


(defn index-step2
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board x]
       (reduce
        (fn [new-board y]
          (let [new-liveness
                (case (count-neighbors board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil
                  )]
            (assoc-in new-board [x y] new-liveness)))
        new-board (range h)))
     board (range w))

    ))

(defn index-step3
  [board]
  (let [w (count board)
        h (count (first))]
    (reduce
     (fn [new-board [x y]]
       (let [new-liveness
             (case (count-neighbors board [x y])
               2 (get-in board [x y])
               3 :on
               nil
               )]
         (assoc-in new-board [x y] new-liveness)))
     board (for [x (range h) y (range w)] [x y]))))

(defn partition-1 []
  (prn "partition:" (partition 3 1 (range 5)))
  (prn "partition:" (partition 3 1 (concat [nil] (range 5) [nil])))
  )

(defn window
  [coll]
  (partition 3 1 (concat [nil] coll [nil])))

(defn cell-block
  [[left mid right]]
  (window (map vector
               (or left (repeat nil)) mid (or right (repeat nil)))))
