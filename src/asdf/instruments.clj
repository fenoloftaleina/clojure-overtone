(ns asdf.instruments
  (:require
    [asdf.sequencer :refer [se ranger]]
    [clojure.repl :refer [source]]))
(use 'overtone.live)

;; (for [y [4 3 3 1]]
;;   (for [x [1 2 3 4 10 15 25 31]]
;;     (do
;;       (demo (* 0.05 (sin-osc (* (mod y x) 210) 0)))
;;       (demo (* 0.05 (sin-osc (* (mod x y) 210) 0))))))

(prn :asdf)

(+ 1 2)
(defn white [lowpass]
  (definst w []
    (lpf (pan2 (* 3 (white-noise))) lowpass)))
(defn white-bpf [a b]
  (definst w []
    (bpf (pan2 (* 15 (white-noise))) a b)))
((white 1000))
((white 2000))
(do (stop) ((white-bpf 1000 0.1)))
((white-bpf 500 3))
(stop)
(do
  (stop)
  ((white 1000))
  ((white 1000))
  ((white 1000))
  ((white 1000))
  ((white 1000))
  ((white 1000))
  ((white 2000))
  ((white 2000))
  ((white 2000))
  ((white 2000))
  )
(stop)
((white (line:kr 5000 1000 2)))
((white (integrator:kr (line:kr 90 100) 1)))
(demo 5 (sin-osc (integrator:kr (sin-osc:kr 1000) 1)))
(stop)
(defsynth decaying-sin []
  (out 0 (sin-osc (decay:kr (line:kr 50 100) 1))))
(decaying-sin)

(demo 5 (sin-osc (line:kr 50 100 1)))
(defn a []
  (demo 1 (sin-osc (line:kr 50 100 1))))
(defn b []
  (demo 0.2 (sin-osc (line:kr 50 100 0.2)))
  (demo 0.2 (sin-osc (line:kr 100 50 0.2)))
  (demo 0.2 (sin-osc (line:kr 50 100 0.2)))
  (demo 0.2 (sin-osc (line:kr 100 50 0.2)))
  (demo 0.2 (sin-osc (line:kr 50 100 0.2)))
  (demo 0.2 (sin-osc (line:kr 100 50 0.2)))
  (demo 0.2 (sin-osc (line:kr 50 100 0.2)))
  )
(apply-at (+ (now) 1000) a)
(apply-at (+ (now) 2000) b)

(demo 2 (sin-osc (env-gen (envelope [400 600 800 600 0] [0.2 0.2 0.2 0.2] :step))))
(stop)

(demo (pan2 (* 2 (white-noise))))
(demo 10 (pan2 (white-noise) (line:kr -1 1 10)))
(demo (mix (pan2 (white-noise))))

(demo (pan2 (white-noise) (mouse-x)))

(demo (white-noise))
(demo (resonz (white-noise) 25 10))
(demo (resonz (pink-noise) 25 10))
(demo (resonz (pink-noise) (line:kr 500 50 1) 10))
(demo 2
      ;; (mix (sin-osc [400 500 600] [0.5 0.2 0.1]))
      (+
        (sin-osc 440)
        (sin-osc 880)
      ))

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
  (out
    0
    (pan2
      (*
       0.2
       (white-noise)
       (adsr-env 1 2 5.5 1 2))
      ;; (mouse-x -1 1)
      )))
(noise-in-env)
(stop)

#_(defsynth play-a-bell [freq 440]
  (let [freqs [0.5 1 1.19 1.56 2 2.51 2.66 3.01 4.1]
        ampls (map #(* % 0.5) [0.25 1 0.8 0.5 0.9 0.4 0.3 0.6 0.1])
        freqs-times-ampls (map * freqs ampls)
        ]
    (out (pan2 (sin-osc (* freq freqs-times-ampls))))))
#_(play-a-bell)
(stop)

(definst trem [freq 440 depth 10 rate 6 length 3]
  (* 0.3
     (line:kr 0 1 length FREE)
     (sin-osc (+ freq (* depth (sin-osc:kr rate))))))
(trem)
(stop)


(do
  (stop)
  (definst a [freq 440 depth 10 rate 6 length 5]
    (mix
      [
    (* 1

       (line:kr 1 1 length FREE)
       ;; (sin-osc:kr 10)
       (sin-osc (+ freq
                   (* (sin-osc:kr 2440) 10)
                   (line:kr 50 10 3)
                   ;; (line:kr 0 50 length FREE)
                   )
                ))
    (sin-osc freq)
    ;; (saw 10)
    ]))
  (a))

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

(demo 2 (pan2
          (* 0.5
             (mix
               [
                (sin-osc (sin-osc 400))
                ]
               )
             )))

(definst kosc [freq 440]
  (* 0.2
     (sin-osc freq)))

(definst kosc2 [freq 440]
  (* 0.2
     (sin-osc freq)))

(clojure.repl/source sin-osc)

(kosc (kosc2 90))
(ctl kosc2 :freq 1332)
(stop)



(prn (sin-osc 400))

(defn hz [k]
  (when k
    (midi->hz (note k))))

(defn freqs0 [i]
  (let [notes
        (mapv
          hz
          (concat
            (repeat 5 nil)
            (repeat 5 :E5)
            (repeat 3 :F4)
            (repeat 2 :E4)
            (repeat 3 :D5)
            (repeat 2 :C4)
            (repeat 2 :C5)
            (repeat 2 :D4)
            (repeat 1 :D5)
            ))]
    (get notes (mod i (count notes)))))

(defn freqs [i]
  (let [notes
        (mapv
          hz
          (concat
            (repeat 5 nil)
            (repeat 5 :E5)
            (repeat 3 :F4)
            (repeat 2 :E4)
            (repeat 3 :D5)
            (repeat 2 :C4)
            (repeat 2 :C5)
            (repeat 2 :D5)
            (repeat 1 nil)
            ))]
    (get notes (mod i (count notes)))))

(defn freqs2 [i]
  (let [notes
        (mapv
          hz
          ;; (reverse
          (concat
            (repeat 5 nil)
            (repeat 5 :E5)
            (repeat 3 :F4)
            (repeat 2 :E4)
            (repeat 3 :D5)
            (repeat 2 :C4)
            (repeat 2 :C5)
            (repeat 2 :D5)
            ;; )
            ))]
    (get notes (mod i (count notes)))))

(defn a [i]
  (sin-wave :freq (freqs0 i) :attack 0.5 :sustain 0.1))

(defn b [i]
  (sin-wave :freq (* 10 (freqs2 i))
            :vol 0.01))

(defn c [i]
  (sin-wave :freq (* 1.5
                     (freqs i))
            :vol 0.1))

(se
  (metronome 200)
  (map (fn [freq]
         #(sin-wave :freq freq))
       (concat
         (repeat 1000 (hz :A4))))

  (metronome 200)
  (map (fn [i]
         #(a i))
       (range 1000))


  (metronome 400)
  (map (fn [i]
         #(b i))
       (range 1000))

  (metronome 200)
  (map (fn [i]
         #(c i))
       (range 1000))

  )
(stop)

;; (defn kick-notes []
;;   [400 nil 500 nil 300 400 500 nil 600])

(use 'overtone.inst.drum)

(defn instro [x]
  (let [s 0.5
        a 0.1]
    (doseq [e [
               x
               (+ x 100)
               (+ x 200)
               (+ x 255)
               (+ x 319)
               (+ x 39)
               (+ x 49)
               (+ x 490)
               ]]
      (sin-wave :freq e :sustain s :attack a :vol 0.1))))


;; (se
;;   (metronome 250)
;;   (ranger instro melody1-notes 2000))
;; (stop)

;; (use 'overtone.music.pitch)
(use 'overtone.inst.sampled-piano)

(defn melody1 [x]
  (sampled-piano :note x
                 :attack 0.5
                 :sustain 1
                 :release 1
                 :decay 0.5
                 :level 0.5))

(defn melody1-notes []
  (flatten
    (concat
      (repeat 2 (scale :Cb2 :minor [:iii :iv :v]))
      (repeat 2 (scale :Ab2 :minor [:ii :iv :ii]))
      ;; (repeat 2 (scale :Cb2 :minor [:ii :iv :v]))
      ;; (repeat 2 (scale :G2 :minor [:ii :iv :vii]))
  )))

(defn drum1 [x]
  (kick2 x))

(defn drum1-notes []
  [50 50 35 35 25 60 60 40])

(defn drum2 [x]
  (snare2 x 2))

(defn drum2-notes []
  [150 250 150 150 250]
  )

(defn bass1 [x]
  (mapv #(sin-wave :freq (* x %)
                   :attack (/ % 2)
                   :sustain 1
                   :vol 0.2)
        [2 3 5]))

(defn bass1-notes []
  [50 50 35 35 25 60 60 40])

(se
  (metronome 500)
  (ranger drum1 drum1-notes 10000)

  (metronome 500)
  (ranger drum2 drum2-notes 10000)

  (metronome 250)
  (ranger bass1 bass1-notes 10000)

  (metronome 250)
  (ranger melody1 melody1-notes 2000)

  ;; (metronome 250)
  ;; (ranger melody2 melody2-notes 200)
  )
(stop)



(definst saw-wave [lowpass 440 freq 100 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (lpf (pan2 (saw freq)) lowpass)
     vol))


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

(trem :length 100 :depth 800 :rate 100)
(trem :length 100 :depth 800 :rate 200)
(trem :length 100 :depth 800 :rate 800)
(stop)
