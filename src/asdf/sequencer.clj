(ns asdf.sequencer)

(use 'overtone.live)

;; Run directly from the repl to get the freesound auth working.
(use 'overtone.inst.sampled-piano)


(stop)
(sampled-piano :note 69 :sustain 3 :decay 1)

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

(def octave-diff 12)

(definst white [bandpass 1000 attack 0.2 sustain 2 release 0.5 vol 0.9]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (bpf (white-noise)
          bandpass)
     (sin-osc:kr 50)
     (* 250 (lpf (sin-osc:kr 5)
               0.2))
     vol))

(definst noise [freq 100 attack 0.2 sustain 1 release 0.5 vol 0.1]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (reduce +
             (map #(sin-osc (-> (+ freq %)
                                (* 0.3)))
                  [10 11 12 103 105 110 500 501 502]))
     vol))

(defn notes []
  (->> [:C3 :G3 :A3 :F3]
       (map note)
       cycle
       (map (fn [additional input]
              (+ additional input))
            (->> (map #(repeat 4 %)
                      [0 12 0 -12 24 12 -24 -36])
                 cycle
                 (take 8)
                 flatten
                 shuffle))))

(defn piano [notes]
  (se
    (metronome 200)
    (doall
      (map (fn [n]
             (fn []
               (sampled-piano :note n
                              :attack 0.1
                              :sustain 2
                              :release 0.1
                              :decay 5)))
           notes))))

(defn multi-piano [& notes]
  (apply
    se
    (mapcat
      identity
      (map (fn [piano-notes]
             [(metronome 200)
              [#(piano piano-notes)]])
           notes))))

(multi-piano (notes) (notes))

(defn play-white []
  (white :bandpass (+ (rand 200) 20000)
         :attack 0
         :sustain 0.1
         :release 0.3
         :vol 0.1))

(se (metronome 20)
    (concat
      (repeat
        1
        (fn []
          (multi-piano
            (map note [:g3 nil :f3 nil nil :a3 nil :d3])
            (map note [:G3 nil :E3 nil :F3 nil :A4])
            (map note [:E3 nil :F#3 nil :A#4 nil :E3])
            ;; (map note [:E6 :F#6 :A#7 :E6])
            )))
      (repeat
        1
        (fn []
          (multi-piano
            (map note [:f3 0 :f3 0 0 :a3 0 :d3])
            ;; (map note [:G3 :E3 :F3 :A4])
            ;; (map note [:E3 :F#3 :A#4 :E3])
            ;; (map note [:E6 :F#6 :A#7 :E6])
            ))))
    ;; (metronome 20)
    ;; (repeat 4
    ;;         #(se (metronome 200)
    ;;              (repeat 8 (fn [] (play-white)))))
    )

(stop)

;; Do something to be able to come up with piano melodies better.


(defn gimme-notes [shift]
  (se (metronome 400)
      (for [n (->> [:c2 :g3 :a3 :f3]
                   cycle
                   (take 50)
                   shuffle
                   (take 4))]
        (fn []
          (sampled-piano :note (+ (note n) shift)
                         :attack 0.1
                         :sustain 1
                         :release 0.1
                         :decay 1)))))

(se (metronome 30)
    (repeat
      100
      (fn []
        (se (metronome 100)
            (concat
              (repeat 1 (fn [] (gimme-notes 12)))
              (repeat 2 (fn [] (gimme-notes 0)))
              (repeat 1 #(gimme-notes -12))
              ))
        )))
(stop)

;; Ok.


;; Btw. check ugens from the cheat sheet.



(definst whiter [bandpass 1000 attack 0.2 sustain 2 release 0.5 vol 0.9]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (white-noise)
     (sin-osc:kr 19)
     (* 250 (lpf (sin-osc:kr 1)
               0.1))
     vol))

(defn play-white1 []
  (whiter :bandpass (+ (rand 100) 50)
          :attack 0.1
          :sustain (-> (rand 100)
                       (/ 100.0)
                       (* 2))
          :release 0.3
          :vol 0.05))

(defn play-white2 []
  (white :bandpass (+ (rand 100) 10000)
         :attack 0
         :sustain (-> (rand 100)
                      (/ 100.0)
                      (* 0.2))
         :release 0
         :vol 0.02))

(defn play-white3 []
  (white :bandpass 100
         :attack 0.1
         :sustain 1.9
         :release 0.1
         :vol 0.8))

(se (metronome 200)
    (repeat 999 (fn [] (play-white1)))
    (metronome 200)
    (repeat 999 (fn [] (play-white2)))
    (metronome 200)
    (repeat 999 (fn [] (play-white3)))
    )

(stop)




(defn live-notes11 [] [:g2 nil :f2 nil nil :a5 nil :d4  :g2 nil :f2 nil nil :a5 :e#3 :d4  :g2 nil :f2 nil nil :a5 :e#3 :d4  :g2 nil :f2 nil nil :a5 :e#3 :d4  :g2 nil :f2 nil nil :a5 :e#3 :d4])
(defn live-notes12 [] [nil nil nil nil nil nil nil nil  nil nil nil nil nil nil nil nil   nil :g6 nil nil :a3 nil nil nil   nil :g6 nil nil :a3 nil nil nil   nil :g6 nil nil :a3 nil nil nil])
(defn live-notes13 [] [nil nil nil nil nil nil nil nil  nil nil nil nil nil nil nil nil   nil nil nil nil nil nil nil nil   :g3 nil :f3 nil nil :a3 nil :d3   :g3 :f#3 :f3 nil nil :a3 :g#3 :d3])
(defn live-notes14 [] [:g2 nil :f3 nil nil :a1 nil :d3  :g3 nil :f1 nil nil :a3 nil :d1])

(defn live-notes21 [] [:g3 nil :a3 nil nil :a1 nil :d3])
(defn live-notes22 [] [:g3 nil :f3 nil nil :a3 nil :d3])
(defn live-notes23 [] [:g3 nil :f3 nil nil :a3 nil :d3])

;; (defn quick-inst [n]
;;   (sampled-piano :note n
;;                  :attack 0.1
;;                  :sustain 2
;;                  :release 0.1
;;                  :decay 5
;;                  :rate 1.9))
;;
;; (defn slow-inst [n]
;;   (sampled-piano :note n
;;                  :attack 1
;;                  :sustain 3
;;                  :release 1
;;                  :decay 5
;;                  :rate 3.1))
;;
;; (defn quicker-inst [n]
;;   (sampled-piano :note n
;;                  :attack 0.1
;;                  :sustain 1
;;                  :release 0.1
;;                  :decay 5
;;                  :rate 2.1))
;;
;; (defn slower-inst [n]
;;   (sampled-piano :note n
;;                  :attack 0.1
;;                  :sustain 2
;;                  :release 0.1
;;                  :decay 0.5
;;                  :level 0.7
;;                  :rate 0.9))

(defn quick-inst [n]
  (sampled-piano :note n
                 :attack 0.1
                 :sustain 1
                 :release 0.1
                 :decay 0.1
                 :rate 0.97))

(defn slow-inst [n]
  (sampled-piano :note n
                 :attack 1
                 :sustain 3
                 :release 1
                 :decay 0.5
                 :rate 0.98))

(defn quicker-inst [n]
  (sampled-piano :note n
                 :attack 0.1
                 :sustain 1
                 :release 0.1
                 :decay 1
                 :rate 0.97))

(defn slower-inst [n]
  ;; (noise :freq (* 150 n)
  ;;        :attack 0.1
  ;;        :sustain 1
  ;;        :release 0.1
  ;;        :vol 0.01)
  )
  ;; #_(sampled-piano :note n
  ;;                :attack 0.1
  ;;                :sustain 2
  ;;                :release 0.1
  ;;                :decay 10
  ;;                :level 0.2
  ;;                :rate 0.3))

(use 'overtone.inst.drum)
(kick 50)

(kick2)
(kick3)

(defn drum1 [n]
  ;; (kick2 100 0.9)
  )
(clojure.repl/source kick3)

(defn drum2 [n]
  ;; (kick n 2)
  )

(clojure.repl/source kick3)

(defn play-note-for-i-th-sound [i notes-callable inst-n]
  (let [notes (notes-callable)
        len (count notes)
        picked-note (nth notes (mod i len))
        n (note picked-note)]
    (inst-n n)))

(let [length 1000]
  (se
    (metronome 200)
    (for [i (range length)]
      (fn []
        (play-note-for-i-th-sound
          i
          live-notes11
          quick-inst)))

    (metronome 200)
    (for [i (range length)]
      (fn []
        (play-note-for-i-th-sound
          i
          live-notes12
          slow-inst)))

    (metronome 200)
    (for [i (range length)]
      (fn []
        (play-note-for-i-th-sound
          i
          live-notes13
          quicker-inst)))

    (metronome 200)
    (for [i (range length)]
      (fn []
        (play-note-for-i-th-sound
          i
          live-notes14
          slower-inst)))

    ;; (metronome 800)
    ;; (for [i (range (* length 4))]
    ;;   (fn []
    ;;     (play-note-for-i-th-sound
    ;;       i
    ;;       live-notes11
    ;;       drum1)))
    ;;
    ;; (metronome 500)
    ;; (for [i (range (* length 2))]
    ;;   (fn []
    ;;     (play-note-for-i-th-sound
    ;;       i
    ;;       live-notes11
    ;;       drum2)))
    ))

(stop)


(defmacro ranger [sound-fun notes-fun n]
  `(map (fn [i#]
          #(let [notes# (vec (~notes-fun))
                 note# (get notes# (mod i# (count notes#)))]
             (when note#
               (~sound-fun note#))))
        (range ~n)))
