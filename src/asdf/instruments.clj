(ns asdf.instruments)
(use 'overtone.live)

(definst trem [freq 440 depth 10 rate 6 length 3]
  (* 0.3
     (line:kr 0 1 length FREE)
     (saw (+ freq (* depth (sin-osc:kr rate))))))
(trem)
(stop)

(def scale-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])
(def pitches (degrees->pitches scale-degrees :dorian :C4))

(stop)

(definst a [] (sin-osc 440))
(a)

(definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))
(sin-wave :sustain 2)

(demo 10 (lpf (saw 100) (mouse-x 40 5000 EXP)))
(demo 10 (hpf (saw 100) (mouse-x 40 5000 EXP)))
(demo 30 (bpf (saw 100) (mouse-x 40 5000 EXP) (mouse-y 0.01 1 LIN)))
(let [freq 220]
   (demo (pluck (* (white-noise) (env-gen (perc 0.001 2) :action FREE)) 1 3 (/ 1 freq))))
(stop)

(use 'overtone.inst.synth)

(definst saw-wave [freq 440 attack 0.01 sustain 0.5 release 0.1 vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn saw2 [music-note length]
    (saw-wave (midi->hz (note music-note)) :sustain length))

(definst spooky-house [freq 440 width 0.2
                         attack 0.3 sustain 4 release 0.3
                         vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.5 0 width))))
     vol))
(spooky-house :width 0.7)

;; (demo (pan2 (sin-osc) -0.5))

(definst grumbles [speed 6 freq-mul 1]
  (let [sines 5]
    (* (apply +
              (map #(pan2 (* (sin-osc (* % 100 freq-mul))
                             (max 0 (+ (lf-noise1:kr speed) (line:kr 1 -1 30))))
                          (- (clojure.core/rand 2) 1))
                   (range sines)))
       (/ 1 sines))))

(grumbles 10 0.5)
(stop)

(ctl grumbles :freq-mul 2)

(definst fib-overtones [freq 440 n 5]
  (pan2 (reduce + (map (fn [x] (sin-osc (* 0.1 x))) [220 111 488 440 885 324 291]))))
(fib-overtones :freq 440 :n 5)
(stop)

(definst trem [freq 440 depth 10 rate 6 length 3]
  (* 0.3
    (line:kr 0.5 0.7 length FREE)
    (saw (+ freq (* depth (sin-osc:kr rate))))))

(trem :length 200 :depth 400 :rate 200)
(stop)
