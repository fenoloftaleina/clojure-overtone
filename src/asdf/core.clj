(ns asdf.core)

(def fib
  ((fn inside-fib [a b]
    (lazy-seq (cons a (inside-fib b (+ a b))))) 0 1))
(stop)


(defn f [x]
  (lazy-seq (cons x (f (+ 1 x)))))
(take 5 (f 1))

(def fib-lazy-cat
  (lazy-cat [0 1] (map + (rest fib-lazy-cat) fib-lazy-cat)))

(take 5 fib-lazy-cat)

(take 5 (rest (iterate inc 1)))
