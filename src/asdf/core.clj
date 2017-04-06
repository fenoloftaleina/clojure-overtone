(ns asdf.core)

(def fib
  ((fn inside-fib [a b]
    (lazy-seq (cons a (inside-fib b (+ a b))))) 0 1))
(stop)


(defn f [x]
  (lazy-seq (cons x (f (+ 1 x)))))
(take 5 (f 1))

(def fib-lazy-cat
  (lazy-cat [0 1] (map (fn [x y] (println x y) (+ x y)) (rest fib-lazy-cat) fib-lazy-cat)))

(take 5 fib-lazy-cat)

(take 5 (rest (iterate inc 1)))

(clojure.repl/source take)

(filter #{1 2} [1 3 4 2])

(interleave (cycle (range 1 4)) "abcdefgh")


(defn sequence-reductions
  ([f s]
   (sequence-reductions f (first s) (next s)))
  ([f i s]
   (lazy-seq
     (cons i
           (when s
             (sequence-reductions f
                                  (f i (first s))
                                  (rest s)))))))

(= (take 5 (sequence-reductions + (range))) [0 1 3 6 10])

(count
  (take-while
    #(<= % 100)
    (take
      1000
      (filter
        (fn [x]
          (or
            (= 0 (mod x 3))
            (= 0 (mod x 5))
            ))
        (range)))))

(if (seq (filter odd? [2 4])) "asdf" "fdsa")

((some-fn :a :b :c :d) {:c 3 :d 4})
(some {:c 3 :d 4} '(:a :b :c :d))

(defn f [a b]
  (lazy-seq (cons a (f b (+ a b)))))

(reduce + (filter even? (take-while (partial > 4000000) (f 0 1))))

(for [a (range 1 4)
      b (range 1 4)]
  (* a b))

(apply concat
  ;(mapcat
  (map
    (fn [x]
      (map (partial * x) (range 1 4)))
    (range 1 4)))

(declare my-odd? my-even?)

(defn my-odd? [n] (if (= n 0)
                    false
                    #(my-even? (dec n))))

(defn my-even? [n] (if (= n 0)
                     true
                     #(my-odd? (dec n))))

(trampoline my-even? 10023333)

(declare m f)

(defn m [n]
  (if (zero? n) 0
    (- n (f (m (dec n))))))

(defn f [n]
  (if (zero? n) 1
    (- n (m (f (dec n))))))

(def m (memoize m))
(def f (memoize f))

(def m-seq (map m (iterate inc 0)))
(def f-seq (map f (iterate inc 0)))

(time (nth m-seq 10000))
(take 50 m-seq)

;; male has returnal == 0
(defn hof [returnal n]
  (if (zero? n)
    returnal
    (let [opposite-returnal (if (zero? returnal) 1 0)]
      (- n (hof opposite-returnal (hof returnal (dec n)))))))

(def hof-m (memoize (fn [x] (hof 0 x))))
(def hof-m-seq (map hof-m (iterate inc 0)))

(=
 (take 50 m-seq)
 (take 50 hof-m-seq))

(apply + 1 2 [3 4 5])


(defn get-column [arr i]
  (map (fn [row] (row i)) arr))

(defn agg-indexed [fn-agg arr]
  (apply (memoize (resolve (symbol (str fn-agg "-key")))) second (map-indexed vector arr)))

(agg-indexed 'max [7 9 3])

(defn median-cut [input out-count]
  (println out-count)
  (let [input-count (count input)]
    (if (= out-count 1)
      {:avg (vec (map
                   #(int (/ % input-count))
                   (reduce
                     (fn [pixel1 pixel2] (map + pixel1 pixel2))
                     input)))
       :pixels input}
      (let [
            columns (vec (map (partial get-column input) (range 3)))
            maxes (vec (map (partial apply max) columns))
            mins [1 1 1]
            ranges (vec (map - maxes mins))
            max-indexed (fn [arr] (apply max-key second (map-indexed vector arr)))
            max-range-index (first (max-indexed ranges))
            ;; max-range-index 0
            next-out-count (int (/ out-count 2))
            two-buckets (map
                          #(median-cut % next-out-count)
                          (split-at
                            (/ input-count 2)
                            (sort-by
                              #(% max-range-index)
                              input)))]
        (if (> out-count 2)
          (apply concat two-buckets)
          two-buckets)))))

(defn more-input [times]
  (println (* times 2500))
  (apply concat (repeat times (input))))

(let [in (more-input 50)]
  (dotimes [i 5] (time (median-cut in 4))))

(* 300 400)

(let [pallette (median-cut
                 [[1 2 3] [2 4 5] [7 4 1] [3 0 2]
                  [5 2 6] [2 9 5] [6 9 1] [5 3 7]
                  [1 2 3] [2 4 5] [7 4 1] [3 0 2]
                  [5 2 6] [2 9 5] [6 9 1] [5 3 7]]
                 4)]
  (dorun (map-indexed (fn [i {x :avg}] (println x)) pallette)))

(int (/ 10 4))

(defn quantize-into [input out-count]
  (median-cut input out-count))

(apply map + '((192 184 174) (191 186 172) (192 188 172) (194 191 176)))

(let [pixels-list '((0 0) (1 0) (2 0) (3 0)
                    (0 1) (1 1) (2 1) (3 1)
                    (0 2) (1 2) (2 2) (3 2)
                    (0 3) (1 3) (2 3) (3 3))
      pixels-matrix (to-array-2d (partition 4 pixels-list))
      gege (fn [x y size arr-2d]
             (map
               (fn [x-plus]
                 (map
                   (fn [y-plus]
                     (aget arr-2d (+ x x-plus) (+ y y-plus)))
                   (range size)))
               (range size)))]
  (gege 0 1 2 pixels-matrix))

(let [pixels (to-array-2d (partition 4 '((0 0) (1 0) (2 0) (3 0)
                                 (0 1) (1 1) (2 1) (3 1)
                                 (0 2) (1 2) (2 2) (3 2)
                                 (0 3) (1 3) (2 3) (3 3))))
      m 2
      gege (fn [x y size arr-2d]
             (apply concat
               (map
                 (fn [x-plus]
                   (map
                     (fn [y-plus]
                       (aget arr-2d (+ x x-plus) (+ y y-plus)))
                     (range size)))
               (range size))))]
  (dorun (map
           (fn [[x y]]
             (let [colors (gege x y m pixels)
                   color (map #(int (/ % m)) (apply map + colors))]
               (println colors)))
           (for [x (range 0 4 m)
                 y (range 0 4 m)]
             [x y])
           )))

(defn f [[x y :as point]]
  [point, x, y])
(f [1 2])

(defn pow [a x]
  (reduce * (repeat x a)))
(pow 2 5)

(defn ease-out [a]
  (let [x (- a 1)]
    (* 5 x x x)))
(map ease-out (range 1 7))

(defn say-hello [name] (str "Hello " name))
(def say-hello-with-defaults (fnil say-hello "World"))
(say-hello-with-defaults "Sir")
(say-hello-with-defaults nil)

(def convolution-matrix [[1 2 1]
                         [2 4 2]
                         [1 2 1]])

(let [size 5
      input-range (range size)
      input (into
              []
              (map
                (fn [x]
                  (into [] (repeat size x)))
                input-range))
      matrix convolution-matrix
      matrix-size (count matrix)
      matrix-middle (int (/ matrix-size 2))
      matrix-lower-end-index (+ (- matrix-middle matrix-size) 1)
      matrix-range (range matrix-lower-end-index (+ matrix-lower-end-index matrix-size))
      get-px (fn [a x y] (get (get a y) x))
      result-range (range size)
      result (for [y result-range]
               (for [x result-range]
                 (reduce
                   +
                   (map
                     (fn [relative-y]
                       (reduce
                         +
                         (map
                           (fn [relative-x]
                             (let [matrix-px (get-px
                                               matrix
                                               (+ matrix-middle relative-x)
                                               (+ matrix-middle relative-y))
                                   input-x (+ x relative-x)
                                   input-y (+ y relative-y)
                                   input-px (if (or
                                                  (< input-x 0)
                                                  (>= input-x size)
                                                  (< input-y 0)
                                                  (>= input-y size))
                                              0
                                              (get-px
                                                input
                                                (+ x relative-x)
                                                (+ y relative-y)))]
                               (* matrix-px input-px)))
                           matrix-range)))
                     matrix-range))))]
  result)

(reduce
  (fn [a [index value]] (update-in a [index] + value))
  [3 2 1]
  (map-indexed vector [1 2 3]))

