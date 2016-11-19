(ns asdf.instruments)
(use 'overtone.live)

(for [y [4 3 3 1]]
  (for [x [1 2 3 4 10 15 25 31]]
    (do
      (demo (* 0.05 (sin-osc (* (mod y x) 210) 0)))
      (demo (* 0.05 (sin-osc (* (mod x y) 210) 0))))))

(defn white [lowpass]
  (definst w []
    (lpf (white-noise) lowpass)))
((white 1000))
((white (line:kr 5000 1000 2)))
(stop)

(demo (white-noise))
(demo (pan2 (white-noise)))
(demo (mix (pan2 (white-noise))))

(demo (pan2 (white-noise) (mouse-x)))

(demo (resonz (white-noise) 25 10))
(demo (resonz (white-noise) (line:kr 500 50 1) 10))

(defsynth sin2 [freq 440]
  (out 0 (sin-osc freq))
  (out 1 (sin-osc freq)))
(defsynth sin2a [freq 440]
  (out 0 [(sin-osc freq) (sin-osc freq)]))
(defsynth sin2b [freq 440]
  (out 0 (sin-osc [freq freq])))
(defsynth sin2c [freq 440]
  (out 0 (* 0.2 [(sin-osc freq) (sin-osc freq)])))
(defsynth sin2d [freq 440]
  (out 0 (* [0.3 0.9] (sin-osc freq))))
(sin2)
(sin2a)
(sin2b)
(sin2c)
(sin2d)
(stop)

(let [freq 220]
  (demo (* (white-noise) (env-gen (perc 0.001 4 3) :action FREE)) 1 3 (/ 1 freq)))

(defn adsr-env
  ([attack decay sustain release duration]
   (adsr-env attack decay sustain release duration 1 :linear))
  ([attack decay sustain release duration peak curve]
   (env-gen
     (adsr attack decay sustain release peak :linear)
     (line:kr 1.0 0.0 duration)
     :action FREE)))
;; Curves: :step, :linear, :exponential, :sine, :welch, float, array of floats.

(defsynth noise-in-env [freq 220]
  (out 0 (pan2 (* 0.2 (white-noise) (adsr-env 1 2 0.5 1 2)))))
(noise-in-env)
(stop)

(defsynth play-a-bell [freq 440]
  (let [freqs [0.5 1 1.19 1.56 2 2.51 2.66 3.01 4.1]
        ampls (map #(* % 0.5) [0.25 1 0.8 0.5 0.9 0.4 0.3 0.6 0.1])
        freqs-times-ampls (map * freqs ampls)
        ]
    (out 0 (pan2 (mix (sin-osc (* freq freqs-times-ampls)))))))
(play-a-bell)
(stop)

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
