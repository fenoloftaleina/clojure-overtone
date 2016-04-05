(ns asdf.core)

(def fib
  ((fn inside-fib [a b]
    (lazy-seq (cons a (inside-fib b (+ a b))))) 0 1))
(stop)

