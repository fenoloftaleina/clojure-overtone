(ns asdf.touch-osc)
(use 'overtone.live)

;; (comment
(def server (osc-server 44100 "osc-clj"))
(zero-conf-on)
;; )

(osc-rm-listener server :debug)
(osc-listen server (fn [msg] (println msg)) :debug)

(definst sin-osc-six [freq0 440 freq1 440 freq2 440 freq3 440 freq4 440 freq5 440]
  (prn 'frequencies freq0 freq1 freq2 freq3 freq4 freq5)
  (mix [(sin-osc freq0)
        (sin-osc freq1)
        (sin-osc freq2)
        (sin-osc freq3)
        (sin-osc freq4)
        (sin-osc freq5)]))

(sin-osc-six)

(defn control
 [i val]
 (let [val (scale-range val 0 1 50 1000)]
   (ctl sin-osc-six (keyword (str "freq" i)) val)))

(for [i (range 1 7)]
  (osc-handle server (str "/3/rotary" i) (fn [msg] (control (dec i) (first (:args msg))))))

(def rotaries (atom [0 0 0 0 0 0 0 0]))

(defn set-rotary [i value]
  (swap! rotaries #(assoc % i value)))

(for [i (range 1 9)]
  (osc-handle server (str "/3/rotary" i) (fn [msg] (set-rotary (dec i) (first (:args msg))))))

(stop)

;; )

(def freqs [0.27457368 0.18902497 0.26946247 0.3734846 0.29898533 0.41717643 0.32680398])

(definst idk [y 1]
  (prn 'y y)
  (mix (map #(sin-osc (* y (scale-range % 0 1 50 1000))) freqs)))

(idk)
(ctl idk :y 1.5)

(for [i (range 1 9)]
  (osc-handle server (str "/1/push" i) (fn [msg] (ctl idk :y (scale-range i 1 9 0.1 2)))))

;; lag moj czy buttony

(stop)




;; melodies below (way below - cool sequencer shit)

(def notes [1 2 3 4 1 5 3 0 0.5 1 1.5 5 4 3.6 3.2 3 2])
(def notes-cnt (count notes))
(defn melo [metro start]
  (dorun
    (map-indexed
      (fn [i f]
        (at (metro (+ start i)) (ctl idk :y f)))
      notes))
  (at (metro (+ notes-cnt start)) (melo metro (+ start notes-cnt))))
#_(melo (metronome 400) 0)

(stop)


(definst instrument [freq 440]
  (* 0.4
  (+ (* 0.5  (sin-osc freq))
     (* 0.2 (sin-osc (* 2 freq)))
     (* 0.1 (sin-osc (* 3 freq)))
     (* 0.05 (sin-osc (* 7 freq)))
     (* 0.01 (sin-osc (* 9 freq)))))
  )

(definst instrument2 [freq 440]
  (* 0.1
  (+ (* 0.6  (sin-osc freq))
     (* 0.1 (sin-osc (* 2 freq)))
     (* 0.5 (sin-osc (* 3 freq)))
     (* 0.29898533 (sin-osc (* 7 freq)))
     (* 0.01 (sin-osc (* 9 freq)))))
  )
(instrument)
(instrument2)
(defn play-melody [metro delay-freq-delay-freq]
  (for [[delay-time freq] (partition 2 delay-freq-delay-freq)]
    (at (metro delay-time) (ctl instrument :freq freq))))
(defn play-melody2 [metro delay-freq-delay-freq]
  (for [[delay-time freq] (partition 2 delay-freq-delay-freq)]
    (at (metro delay-time) (ctl instrument2 :freq freq))))
  ;; (apply-at (metro 12) play-melody2 metro delay-freq-delay-freq []))

(play-melody (metronome 600) [1 900 5 800])
(play-melody2 (metronome 600) [0 300 3 200 5 500 6 200 9.5 400])
(stop)

;; (defn meta-play-melody []
;;   ;; (prn 'meta)
;;   ;; (let [metro (metronome 60)
;;   ;;       beat (metro)]
;;     ;; (prn 'beat beat)
;;     (at (+ (now) 1000) (play-melody2 (metronome 200) [0 300 3 200 5 500 6 200 9.5 400]))
;;     (apply-at (+ (now) 2000) play-melody2 (metronome 1600) [0 300 3 200 5 500 6 200 9.5 400] [])
;;     ;; (prn 'after-melody)
;;     ;; (prn 'beat-inc (inc beat))
;;     ;; (at (+ (now) 3000) (meta-play-melody))
;;     ;; )
;; )
;; (meta-play-melody)

;; play data instead of loops

(def melody [0 300 3 200 5 500 6 200 9.5 400])

(do
(let [n 5
      d 10]
  (doall
  (play-melody2
    (metronome 800)
    (flatten
      (mapv (fn [beat-inc melody]
             (mapv (fn [[beat freq]]
                    [(+ beat beat-inc) freq])
                  (partition 2 melody)))
           (range 0 (* n d) d)
           (repeat n melody))))))
(play-melody (metronome 600) [1 900 5 800 15 900 19 800])
)

(stop)


;; cool sequencer shit

(definst sin-wave [freq 440 attack 0.2 sustain 0.4 release 0.5 vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))

(definst falling-sin-wave [from 440 to 220 in 3 vol 0.5]
  (* (env-gen (lin 0 in 0) 1 1 0 1 FREE)
     (sin-osc (line:kr from to in))
     vol))


(defn sequencer [iterator-and-sequences-triplets]
  (dorun
    (apply
      map
      (fn [i & column]
        (dorun (map (fn [[inst metro arg]]
                      (when (and metro inst)
                        (let [beat (metro i)]
                          (prn beat i)
                          (if arg
                            (at beat (inst i arg))
                            (at beat (inst i))))))
                    (partition 3 column))))
      iterator-and-sequences-triplets)))


(sequencer
  [(range 600) ;; (iterate inc 0)

   ;; (repeat #(sin-wave :freq %2 :attack 0.2 :sustain 0.2 :release 0.1 :vol 0.6))
   ;; (repeat (metronome 600))
   ;; ;; (flatten (let [something [440 660 200 100 500 400]]
   ;; (flatten (let [something [440 660 200 100 500 400]]
   ;;            (map (fn [coef]
   ;;                   (repeat 2 (map #(* coef %) something)))
   ;;                 [1 1.2 0.5 0.7 2 1.1 1.3 1.5])))

   (repeat #(sin-wave :freq %2 :attack 1 :sustain 1 :release 1 :vol 0.2))
   (repeat (metronome 200))
   (cycle [333 555])

   (repeat (fn [i]
             (sequencer
               [(range (* i 10) (+ (* i 10) 12))
                ;; tyr tyr tyr tyr
                (repeat #(sin-wave :freq %2 :attack 0 :sustain 0.1 :release 0 :vol 0.1))
                (repeat (metronome 2000))
                (cycle [133 155 200 188 120 122 140 200 300 301 320 353])
                ])))
   (repeat (metronome 60))
   (repeat nil)

   ;; falling sine wave repeating and waiting and so on
   (repeat (fn [i] (falling-sin-wave :from 1000 :to 599 :in 5 :vol 0.2)))
   (flatten (repeat 7 [(repeat 10 (metronome 200)) (repeat 5 nil)]))
   (repeat nil)

   ;; falling sine wave once
   ;; (lazy-cat [(fn [i] (falling-sin-wave :from 1000 :to 599 :in 5 :vol 0.5)] (repeat nil)))
   ;; (lazy-cat [(metronome 200)] (repeat nil))
   ;; (repeat nil)

   ])

(stop)

(let [s1-fn
      (fn [i]
        (sequencer [(range i (+ i 2))
                    (repeat (fn [i] (sin-wave :attack 0 :sustain 0.1 :release 0)))
                    (repeat (metronome 200))
                    (repeat nil)]))]
  (sequencer [(range 6)
              (cycle [s1-fn nil])
              (repeat (metronome 20))
              (repeat nil)]))

(sequencer [(range 8)
            (repeat (fn [i] (sin-wave :attack 0 :sustain 0.1 :release 0)))
            (repeat (metronome 100))
            (repeat nil)])

(stop)





(reduce (fn [metros-beats column]
          (mapv (fn [[metro beat] action]
                  (when action
                    (at (metro beat) (action)))
                  [metro (inc beat)])
                metros-beats column))
        [(let [metro (metronome 200)]
          [metro (metro)])
         (let [metro (metronome 200)]
           [metro (metro)])]
        (partition 2 (interleave
                       (for [freq (repeat 6 600)] #(sin-wave :freq freq :attack 0 :sustain 0.1 :release 0 :vol 0.5))
                       (for [freq (cycle [nil 900])] (when freq #(sin-wave :freq freq :attack 0 :sustain 0.1 :release 0 :vol 0.2))))))


(defn se [& metro-sequence-arr]
  (let [[metros sequences] (reduce (fn [[metros sequences] [m s]]
                                     [(conj metros m) (conj sequences s)])
                                   [[] []]
                                   (partition 2 metro-sequence-arr))
        metros-beats (mapv (fn [m] [m (m)]) metros)
        sequences-columns (partition (count sequences)
                                     (apply interleave sequences))]
    (reduce (fn [metros-beats sequences-column]
              (mapv (fn [[metro beat] action]
                      (when action
                        (apply-at (metro beat) action []))
                      [metro (inc beat)])
                    metros-beats sequences-column))
            metros-beats
            sequences-columns))
  nil)

(se
  (metronome 30)
  (repeat 5 (fn []
(se
  (metronome 200)
  (for [freq (repeat 6 600)] #(sin-wave :freq freq :attack 0 :sustain 0.1 :release 0 :vol 0.2))

  (metronome 200)
  (for [freq (take 6 (cycle [nil 900]))] (when freq #(sin-wave :freq freq :attack 0 :sustain 0.1 :release 0 :vol 0.2)))

  (metronome 200)
  (for [freq-add [100 200 500 300 -200 -100]]
    (fn []
      (se
        (metronome 2000)
        (for [freq (concat [100 110 120 150 230 250] (mapv #(+ freq-add %) [505 510 513]))]
          (when freq #(sin-wave :freq freq :attack 0 :sustain 0.1 :release 0 :vol 0.3))))))
  )
)))


;; it works
(stop)
