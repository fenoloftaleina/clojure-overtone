(ns asdf.speed)
(use 'overtone.live)

(defonce metro (metronome 120))
(metro-bpm metro 500)
(stop)

(take 10 (iterate (fn [x] (metro)) 1))
(do (println (metro) (metro 10)))

(definst sin-wave [freq 440 attack 0.01 sustain 0.01 release 0.01 vol 0.7]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))

;; (defn cgaf [metr speed]
;;   (let [notes [:C3 :G4 :A3 :F4]
;;         start (metr)]
;;     (doseq [i (range (count notes))]
;;       (let [timing (metr (+ i start))
;;             n (get notes i)]
;;         (println timing n i)
;;         (at timing (sin-wave (midi->hz (note n)))))
;;       )
;;     ))
;; blah, it's all wrong, doseq blocks

(defn cgaf [metr start]
  (at (metr (+ 0 start)) (sin-wave (midi->hz (note :C4))))
  (at (metr (+ 1 start)) (sin-wave (midi->hz (note :G4))))
  (at (metr (+ 2 start)) (sin-wave (midi->hz (note :A4))))
  (at (metr (+ 3 start)) (sin-wave (midi->hz (note :F4))))
  (let [new-start (+ 4 start)]
    (apply-at (metr new-start) cgaf metr new-start [])))

(cgaf metro (metro))
(stop)

(demo (sin-osc 400))
(demo (sin-osc 399))
(stop)

(midi->hz (note :A4))
(midi->hz (note :C5))
(midi->hz (note :E5))

(sin-wave :freq (midi->hz (note :C3)) :vol 0.5 :sustain 10)
(sin-wave :freq (midi->hz (note :E3)) :vol 0.5 :sustain 10)
n1 = 100 n1 - 100 n2
n2 = (99 / 100) n1

n1 / (n1 - n2) = 400
n1 = 400 n1 - 400 n2
n2 = 399 / 400 n1
;; (sin-wave :freq 660 :vol 0.5 :sustain 10)
;; (sin-wave :freq 441 :vol 0.25 :sustain 10)
(sin-wave :freq 101 :vol 0.5 :sustain 10)
(sin-wave :freq 100 :vol 0.5 :sustain 10)
