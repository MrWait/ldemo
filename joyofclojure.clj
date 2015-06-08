(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))


(defn xors [max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (rem ( bit-xor x y) 256)]))

(defn loadjoc
  []
  (load-file "joyofclojure.clj"))

;; (defn f-values [f xs ys]
;;   (for [x (range xs) y (range ys)]
;;     [x y (rem (f x y) 256)]))

;; (defn draw-values [f xs ys]
;;   (clear gfx)
;;   (.setSize frame (java.awt.Dimension. xs ys))
;;   (doseq [[x y v] (f-values f xs ys )]
;;     (.setColor gfx (java.awt.Color. v v v))
;;     (.fillRect gfx x y 1 1)))


(def a-to-j (vec (map char (range 65 75))))

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(map + yx %) deltas))))

(defn strict-map1
  [f coll]
  (loop [coll coll, acc nil]
    (if (empty? coll)
      (reverse acc)
      (recur (next coll) (cons (f (first coll)) acc)))))

(defn strict-map2
  [f coll]
  (loop [coll coll, acc []]
    (if (empty? coll)
      acc
      (recur (next coll) (conj acc (f (first coll)))))))

(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

(def schedule
  (conj clojure.lang.PersistentQueue/EMPTY
        :wake-up :shower :brush-teeth))

(defn pos1
  [e coll]
  (let [cmp (if (map? coll)
              #(= (second %1) %2)
              #(= %1 %2))]
    (loop [s coll idx 0]
      (when (seq s)
        (if (cmp (first s) e)
          (if (map? coll)
            (first (first s))
            idx)
          (recur (next s) (inc idx)))))))

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos2
  [e coll]
  (for [[i v] (index coll) :when (= e v)] i))

(defn pos
  [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))


(def baselist (list :barnabas :adam))
(def lst1 (cons :willie baselist))
(def lst2 (cons :phoenix baselist))


(defn xconj [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}
    (< v (:val t)) {:val (:val t),
                    :L (xconj (:L t) v),
                    :R (:R t)}
    :else {:val (:val t),
           :L (:L t),
           :R (xconj (:R t) v)}))

(defn if-chain [x y z]
  (if x
    (if y
      (if z
        (do
          (println "Made it!")
          :all-truthy)))))

(defn and-chain [x y z]
  (and x y z (do (println "Made it!") :all-truthy)))

(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

(def very-lazy (-> (iterate #(do (print \.) (inc %)) 1)
                   rest rest rest))

(def less-lazy (-> (iterate #(do (print \.) (inc %)) 1)
                   next next next))


(defn lz-rec-step [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-step (rest s))]
     [])))

(defn simple-range [i limit]
  (lazy-seq
   (when (< i limit)
     (cons i (simple-range (inc i) limit)))))

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(def tri-nums (map triangle (iterate inc 1)))

(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))


(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))

(def tri-nums (inf-triangles 1))

(defn taker [n l]
  (loop [t n, src l, ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

(defn nom [n] (take n (repeatedly #(rand-int n))))

(defn sort-parts
  [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
         (recur (list*
                 (filter smaller? xs)
                 pivot
                 (remove smaller? xs)
                 parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

(def fifth (comp first rest rest rest rest))

(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))

(def plays [{:band "Burial", :plays 979, :loved 9}
            {:band "Eno", :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979, :loved 9}
            {:band "Magma", :plays 2665, :loved 31}])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(defn columns [columns-names]
  (fn [row]
    (vec (map row columns-names))))


(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))

(defn manip-map [f ks m]
  (conj m (keys-apply f ks m)))

(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks) plays))

(defn slope1
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(defn slope [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(defn put-things [m]
  (into m {:meet "bee", :veggie "broccoli"}))

(defn vegan-constrains [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(def times-two
  (let [x 2]
    (fn [y] (* y x))))

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(defn times-n [n]
  (fn [y] (* y n)))

(defn divisible [denom]
  (fn [num]
    (zero? (rem num denom))))

(defn filter-divisible [demon s]
  (filter (fn [num] (zero? (rem num demon))) s))

(def bearings [{:x 0, :y 1}
               {:x 1, :y 0}
               {:x 0, :y -1}
               {:x -1, :y 0}])

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

(defn pow1 [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))

(defn gcd [x y]
  (cond
    (> x y) (gcd (- x y) y)
    (< x y) (gcd x (- y x))
    :else x))

(defn gcd1 [x y]
  (int
   (cond
     (> x y) (gcd (- x y) y)
     (< x y) (gcd x (- y x))
     :else x)))

(defn elevator [commands]
  (letfn
      [(ff-open [[cmd & r]]
         "When elevator is on first floor and close, open it"
         #(case cmd
            :close (ff-closed r)
            :done true
            false))
       (ff-closed [[cmd & r]]
         "When elevator is on first floor and open, close it"
         #(case cmd
            :open (ff-open r)
            :up (sf-closed r)
            false))
       (sf-closed [[cmd & r]]
         "When elevator is closed on the 2nd floor"
         #(case cmd
            :down (ff-closed r)
            :open (sf-open r)
            false))
       (sf-open [[cmd & r]]
         "When elevator is closed on the 2nd floor, open it"
         #(case cmd
            :close (sf-closed r)
            :done true
            :false))]
    (trampoline ff-open commands)))


(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k (kont v n)))]
         (if (accept? n)
           (k end-value)
           (recur (dec n) cont))))
     n kend)))

(def fac1 (mk-cps zero? 1 identity #(* %1 %2)))

(def tri (mk-cps zero? 1 dec #(+ %1 %2)))

(def world [[1 1 1 1 1]
            [999 999 999 999 1]
            [1 1 1 1 1]
            [1 999 999 999 999]
            [1 1 1 1 1]])

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this))  this min))
            coll)))

;; ERROR
(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-time] (first work-todo)
              rest-work-todo (disj work-todo work-time)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defmacro do-until [& clauses]
  (when clauses
    (list `when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(defn from-end [s n]
  (let [delta (dec (- (count s) n))]
    (unless (neg? delta)
            (nth s delta))))


(defmacro def-watch [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

(declare handle-things)

(defmacro domain [name & body]
  `{:tag :domain,
    :attrs {:name (str '~name)},
    :content [~@body]})

(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str '~name)},
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:commment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties, :attrs nil,
     :content (apply vector (for [p props]
                              {:tags :property,
                               :attrs {:name (str (first p))},
                               :content nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock human")
                    (Man (isa Human)
                         "A man, baby"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yes elusive creature"
                     [eat-goats?]))))

(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (when ~'it
       (do ~@body))))

(import [java.io BufferedReader InputStreamReader] [ java.net URL])
;; (import [java.io BufferedReader InputStreamReader]
;;         [java.net URL])

(defn joc-www []
  (-> "http://joyofclojure.com/hello" java.net.URL.
      .openStream java.io.InputStreamReader. java.io.BufferedReader))

(defn coll-joc-www []
  (let [stream (joc-www)]
    (with-open [page stream]
      (println (.readLine page))
      (print "The stream will now close..."))
    (println "but let's read from it anyway.")
    (.readLine stream)))

(defmacro with-resource [binding close-fn &body]
  '(let ~binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))

(declare collect-bodies)

(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) :require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) :ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception. (str "Unknow tag" (first con))))))))))

(def doubler-contract
  (contract doubler
            [x]
            (:require
             (pos? x))
            (:ensure
             (= (* 2 x) %))))
