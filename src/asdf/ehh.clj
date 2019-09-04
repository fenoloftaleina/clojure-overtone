(ns asdf.ehh
  (:require
    [asdf.sequencer :refer [se ranger]]
    [clojure.repl :refer [source]]
    [clojure.tools.logging :as log]
    ))

(use 'overtone.live)

(+ 1 2)

(definst something-fuck-you [f 440 krf 500 hf 1000 vol 1.0]
  (* vol (hpf (sin-osc (* f (sin-osc:kr krf))) hf)))
(something-fuck-you)
(do (ctl something-fuck-you :f 300 :krf 1.8 :hf 1 :vol 0.5) nil)
(stop)

(defn sese-play [x]
  #_(ctl something-fuck-you :f (midi->hz x))
  (ctl something-fuck-you :f x))

(defn sese-notes []
  #_(flatten
   (loop [cur (mapv note [:c6 :g5 :a5]) #_[440 500 600 330]
          n 10
          res []]
     (if (pos? n)
       (recur
        (->> cur
             (map #(- % 1))
             (mapv #(if (< % 78) (+ % 7) %)))
        (dec n)
        (conj res cur))
       res)))

  [440 500 600 330]

  #_(map (comp midi->hz note) [:c3 :g4 :e4 :c2]))

(defn sese []
  (se
    (metronome 50)
    (repeat
      50
      (fn []
        (se
          (metronome 200)
          (ranger sese-play
                  sese-notes
                  100
                  )
               ))
        ))
    )

(sese)
(stop)

(definst sfy-env [f 440 krf 500 hf 1000 vol 0.3]
  ;;(* (sin-osc (* f 0.5 (line:kr 1 0 2) (sin-osc:kr (* krf 0.2))))
  (log/info :sfy)
  (* (sin-osc (* f 0.5
                 #_(sin-osc:kr (* krf 0.138))
                 (env-gen (envelope [0 0.5] [0.4] :wel)
                          :action
                          FREE)
                 ))
     (env-gen (lin 0.02 0.2 0.1) :action FREE)
     vol
     ))

(defn nottt []
  (sfy-env :f 300)
  (sfy-env :f 400)
  (sfy-env :f 250))

(se
 (metronome 300)
 (ranger
  identity
  nottt
  1000))
(sfy-env)
(stop)

(defn aaa []
  (loop [cur (mapv note [:c6 :g5 :a5]) #_[440 500 600 330]
         n 10
         res []]
    (if (pos? n)
      (recur
       (->> cur
            (map #(- % 1))
            (mapv #(if (< % 78) (+ % 12) %)))
       (dec n)
       (conj res cur))
      res))

  )

(se (metronome 50)
    (repeat
     10
     (fn []
       (se (metronome 50)
           (ranger (fn [a]
                      (for [e a]
                        (ctl a :f (midi->hz e))
                        ))
                   aaa
                   200)))))



(def a (doall (for [i (range 3)] (something-fuck-you))))
(doseq [x a] (ctl x :f 545 :krf 81 :hf 100 :vol 0.3))
(stop)

(defn falling-play [vs]
  (mapv (fn [inst value]
          (ctl inst :f value))
        a
        vs))

(defn falling-notes []
  [[440 550 660]
   [300 200 100]]
  (loop [cur (mapv note [440 500 600])
         n 20
         res []]
    (if (pos? n)
      (recur
       (->> cur
            (map #(- % 25))
            (mapv #(if (< % 200) (+ % 400) %)))
       (dec n)
       (conj res cur))
      res))
  )

(se (metronome 100)
    (ranger falling-play
            falling-notes
            200))

(stop)






;; obczaic synthy, budowac sobie biblioteke rzeczy takich jak `se`, `ranger`, etc. i instrumenty
;; more emacs





(definst aa [f 440 krf 500 m 1.0 vol 1.0]
  (* vol (sin-osc (* f (sin-osc:kr (* m krf))))))
(aa)
(do (ctl aa :f 440 :krf 1.0 :m 1.0 :vol 0.5) nil)
(stop)
