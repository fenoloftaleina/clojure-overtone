(ns asdf.core)
(use 'overtone.live)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; (dotimes [i 10] (foo "a") (Thread/sleep 2000))
;; (dotimes [i 10]
;;   (foo "a"))

(definst trem [freq 440 depth 10 rate 6 length 3]
  (* 0.3
    (line:kr 0.5 0.7 length FREE)
    (saw (+ freq (* depth (sin-osc:kr rate))))))

;; (trem :length 200 :depth 400 :rate 200)
(stop)
(use 'overtone.inst.drum)
(kick)
(defn looper  [nome sound]
  (let  [beat  (nome)]
    (at  (nome beat)  (sound))
    (apply-by  (nome  (inc beat)) looper nome sound  [])))
;; (looper (metronome 80) kick2)
;; (looper (metronome 100) kick3)
;; (looper (metronome 500) kick4)

(definst spooky-house [freq 440 width 0.2
                         attack 0.3 sustain 4 release 0.3
                         vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.5 0 width))))
     vol))
(spooky-house :width 0.7)

(def fib
  ((fn inside-fib [a b]
    (lazy-seq (cons a (inside-fib b (+ a b))))) 0 1))

(definst fib-overtones [freq 440 n 5]
  (pan2 (reduce + (map (fn [x] (sin-osc (* 0.1 x))) [220 111 488 440 885 324 291]))))
(fib-overtones :freq 440 :n 5)
(stop)

;; (pan2 (+ (sin-osc) (sin-osc)))
;; (def freq 220)
;; (apply + (map (fn [x] (sin-osc (+ freq (* (/ x 5) freq)))) (take 5 fib)))


(use 'overtone.inst.sampled-piano)

(defn play-one
  [metronome beat instrument [pitch dur]]
  (let [end (+ beat dur)]
    (if pitch
      (let [id (at (metronome beat) (instrument (note pitch)))]
        (at (metronome end) (ctl id :gate 0))))
    end))

(defn play
  ([metronome inst score]
     (play metronome (metronome) inst score))
  ([metronome beat instrument score]
     (let [cur-note (first score)]
       (when cur-note
         (let [next-beat (play-one metronome beat instrument cur-note)]
           (apply-at (metronome next-beat) play metronome next-beat instrument
             (next score) []))))))
(def subject [[:d4 3] [:d4 2] [nil 3] [:a4 4] [:a4 3]])
;; (dotimes [i 5]
;;   (play (metronome 600) sampled-piano subject)
;;   (Thread/sleep (+ 300 (rand-int 500))))

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

(defsynth sin-square [freq 440]
  (out 0 (* 0.125 (+ (square (* 0.5 freq)) (sin-osc freq))))
  (out 1 (* 0.05 (+ (square (* 0.5 freq)) (saw freq)))))
(sin-square)
(stop)
(haziti-clap)

(use 'overtone.inst.synth)
(mooger)

(definst saw-wave [freq 440 attack 0.01 sustain 0.5 release 0.1 vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn saw2 [music-note length]
    (saw-wave (midi->hz (note music-note)) :sustain length))

(defn play-chord
  ([a-chord]
    (play-chord a-chord 0.5))
  ([a-chord length]
    ;; (doseq [note a-chord] (saw2 note length))))
    (doseq [note a-chord] (sampled-piano note))))

(defn pp []
  (let [time (now) x 1400]
    (at time (play-chord (chord :C4 :major)))
    (at (+ time x) (play-chord (chord :G3 :minor7)))
    (at (+ time (* 2 x)) (play-chord (chord :A3 :minor)))
    (at (+ time (* 3 x)) (play-chord (chord :F3 :major7)))
    (at (+ time (* 4 x)) (play-chord (chord :F3 :major7)))))
;; (dotimes [n 5] (pp) (Thread/sleep (* 5 1400)))

(defonce metro (metronome 120))
(metro)
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major) 0))
  (at (m (+ 2 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 5 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 6 beat-num)) (play-chord (chord :F3 :major)))
  (at (m (+ 7 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor) 1))
  (at (m (+ 10 beat-num)) (play-chord (chord :F3 :major) 1.5))
  (apply-at (m (+ 13 beat-num)) chord-progression-beat m (+ 13 beat-num) [])
)

(stop)
(chord-progression-beat metro (metro))

(sampled-piano (note :C3))
(sampled-piano (note :G#2))

(def scale-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])
(def pitches (degrees->pitches scale-degrees :dorian :C4))

(defn play [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (saw (midi->hz note))))
    (let [next-time (+ time sep)]
      (apply-at next-time play [next-time (rest notes) sep]))))
(play (now) [200 400 600] 20)
(stop)

;; Pack the piano in an envelope

(defsynth schroeder-reverb-mic
  [rate 1 dec 1 del 10 out-bus 0]
  (let [input    (pan2 (allpass-c (sound-in) 10  dec del))
        delrd    (local-in 4)
        output   (/ (+ input [(first delrd) (second delrd)]) 2)
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))
        ]
    (out out-bus output)))
(schroeder-reverb-mic :rate 0.1 :dec 0.1 :del 0)
(stop)

;; It cracks

(def server (osc-server 44100 "osc-clj"))
(zero-conf-on)

(osc-listen server (fn [msg] (println msg)) :debug)
(osc-rm-listener server :debug)

(definst foo [freq 440] (sin-osc freq))
(defn control-foo
 [val]
 (let [val (scale-range val 0 1 50 1000)]
      (ctl foo :freq val)))
(foo)
(definst bar [freq 440] (sin-osc freq))
(defn control-bar
 [val]
 (let [val (scale-range val 0 1 50 1000)]
      (ctl bar :freq val)))
(bar)
(osc-handle server "/1/faderA" (fn [msg] (control-foo (first (:args msg)))))
(osc-handle server "/1/faderB" (fn [msg] (control-bar (first (:args msg)))))
(stop)

