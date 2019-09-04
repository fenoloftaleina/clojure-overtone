(ns asdf.maybe-song
  (:require
   [asdf.sequencer :refer [se ranger]]
   [asdf.instruments :refer [sin-wave saw-wave]]
    [clojure.repl :refer [source]]))

(use 'overtone.live)
(use 'overtone.inst.drum)
(use 'overtone.inst.sampled-piano)
;; (use 'overtone.music.pitch)
;; song 1

(defn melody1 [x]
  (sampled-piano :note x
                 :attack 0.5
                 :sustain 1
                 :release 1
                 :decay 0.5
                 :level 0.5))

(defn melody1-notes []
  #_(flatten
    (concat
      (repeat 4 (shuffle (scale :Cb2 :minor [:iii :iv :v])))
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

(defn bass1 [[x ys]]
  (mapv #(sin-wave :freq (* x %)
                   :attack (/ % 2)
                   :sustain 1
                   :vol 0.3)
        ys))

(defn bass1-notes []
  [[50 [2 3 5]]
   [50 [2 3 5 10]]
   [35 [2 3 5 6 7]]
   [35 [2 3 7 10]]
   [25 [2 7 10 15]]
   [60 [2 7 10 15]]
   [60 [2 3 5]]
   [40 [2 5]]
   ])

(defn sin-machine [sin-machine-data]
  (doseq [{:keys [freq vol attack sustain release]
           :or {freq 440 vol 0.1 attack 0.01 release 0.1 sustain 0.4}}
          sin-machine-data]
    (sin-wave :freq freq :vol vol :attack attack :sustain sustain :release release)))

(sin-wave :freq 2500)
25 - 2500


(defn sin-machine-data-list []
  #_[
   [{:freq 400 :vol 0.1} {:freq 440 :vol 0.2}]
   [{:freq 600 :vol 0.2} {:freq 500 :vol 0.1 :sustain 2}]
   [{:freq 650 :vol 0.2} {:freq 800 :vol 0.1 :sustain 3}]
   ])


(se
  (metronome 250)
  (ranger drum1 drum1-notes 10000)

  (metronome 250)
  (ranger drum2 drum2-notes 10000)

  (metronome 250)
  (ranger bass1 bass1-notes 10000)

  (metronome 150)
  (ranger sin-machine sin-machine-data-list 10000)

  (metronome 120)
  (ranger melody1 melody1-notes 2000)

  ;; (metronome 250)
  ;; (ranger melody2 melody2-notes 200)
  )
(stop)

(defn sins [freq multiplies sustain]
  (mapv
    (fn [multiply]
      {:freq (* freq multiply) :vol (/ 1 (+ 1 (* multiply (inc sustain)))) :sustain sustain})
    (cons 1 multiplies)))

(defn sin-machine-drone []
  ;; [
   ;; (sins 400 [2 3 3.5] 0.5)
   ;; (sins 600 [2 3 1.1 1.11] 1)
   ;; (sins 500 [2 2.1] 0.5)
   ;; (sins 900 [1.2 1.5] 1)
   ;;
   ;; (sins 400 [2 3 3.5] 0.5)
   ;; (sins 600 [2 3 1.1 1.11] 1)
   ;; (sins 500 [2 2.1] 0.5)
   ;; (sins 900 [1.2 1.5] 1)
   ;;
   ;; (sins 400 [2 3 3.5] 0.5)
   ;; (sins 600 [2 3 1.1 1.11] 1)
   ;; (sins 500 [2 2.1] 1.5)
   ;; (sins 900 [1.2 1.5] 1)
   ;;
   ;; (sins 400 [2 3 3.5] 0.5)
   ;; (sins 600 [2 3 1.1 1.11] 0.2)
   ;; (sins 500 [2 2.1] 0.25)
   ;; (sins 900 [1.2 1.5] 0.7)

   ;; (mapv
   ;;   #(sins % [2 3 3.5 3.1 3.2 3.3] 0.2)
   ;;   (concat
   ;;     (repeat 8 400)
   ;;     (repeat 8 500)
   ;;     (repeat 8 300)))

   ;; (sins 600 [2 3 1.1 1.11 1.15] 0.5)
   ;; (sins 500 [2 2.1 3] 0.3)
   ;; (sins 900 [1.2 1.5] 0.7)
   ;; ]

     (mapcat
       identity
       (concat
         []

       ;; (repeat
       ;;   8
       ;; (mapv
       ;;   #(sins % [2 2.5 2.501 3 50] 2)
       ;;   (concat
       ;;     (repeat 2 50)
       ;;     (repeat 2 100)
       ;;     (repeat 2 70)
       ;;     (repeat 2 85)))
       ;; )

       ;;
       ;; (repeat
       ;;   2
       ;; (mapv
       ;;   #(sins % [2 2.5 2.501 3] 2)
       ;;   (concat
       ;;     (repeat 2 50)
       ;;     (repeat 2 100)
       ;;     (repeat 2 70)
       ;;     (repeat 2 85)))
       ;; )

       ;; (repeat
       ;;   2
       ;; (mapv
       ;;   #(sins % [2 2.5 2.501 3 50] 2)
       ;;   (concat
       ;;     (repeat 2 50)
       ;;     (repeat 2 100)
       ;;     (repeat 2 70)
       ;;     (repeat 2 85)))
       ;; )

       ;; (let [rots @asdf.touch-osc/rotaries
       ;;       sin-vals (concat (mapv (partial * 10) (take 6 rots))
       ;;                         (mapv (partial * 50) (take-last 2 rots)))]
       ;;   (repeat
       ;;     2
       ;;     (mapv
       ;;       #(sins % sin-vals 2)
       ;;       (concat
       ;;         (repeat 2 50)
       ;;         (repeat 2 100)
       ;;         (repeat 2 70)
       ;;         (repeat 2 85))))
       ;; )

       ;; (prn @asdf.touch-osc/rotaries)
       ;; [0.92505515 0.41622463 0.31980377 0.92237663 0.25595835 0.3076919 0.4631501 0.76597047]

;; INFO: (2.6115229725837708 2.516292929649353 3.2631969451904297 0.5670308321714401 0.0 5.314422249794006 22.09087163209915 0.0)

;; INFO: (0.18842646852135658 1.2134305387735367 0.35571198910474777 0.10936520993709564 0.0 0.40500372648239136 0.0 0.0)
;; INFO: (0.18842646852135658 1.2134305387735367 0.35571198910474777 0.10936520993709564 0.7582515478134155 0.40500372648239136 0.0 0.0)

       ;; (repeat
       ;;   8
       ;; (mapv
       ;;   #(sins % [2 2.5 2.501 3 50] 2)
       ;;   (concat
       ;;     (repeat 2 50)
       ;;     (repeat 2 100)
       ;;     (repeat 2 70)
       ;;     (repeat 2 85)))
       ;; )
       ;;
       ;; (repeat
       ;;   2
       ;; (mapv
       ;;   #(sins % [2 2.5 2.501 3] 2)
       ;;   (concat
       ;;     (repeat 2 50)
       ;;     (repeat 2 100)
       ;;     (repeat 2 70)
       ;;     (repeat 2 85)))
       ;; )


       ))

  )

(defn sin-machine-melody []
     (mapcat
       identity
   (concat
     []
   ;;     (repeat
   ;;       8
   ;;     (mapv
   ;;       #(sins % [65 35 39.9] 1)
   ;;       (concat
   ;;         (repeat 2 15)
   ;;         (repeat 4 20)
   ;;         (repeat 2 17)
   ;;         (repeat 4 18.5)))
   ;;     )

       (let [rots @asdf.touch-osc/rotaries
             sin-vals (concat (mapv (partial * 10) (take 6 rots))
                               (mapv (partial * 50) (take-last 2 rots)))]
         (repeat
           8
           (mapv
             #(sins % sin-vals 0.3)
             (concat
               (repeat 3 125)
               (repeat 2 133)
               (repeat 1 127)
               (repeat 2 128)
               ))))
       ;; (repeat
       ;;   1
       ;;   [[{:freq 0 :sustain 0.3}]])

       ;; (prn @asdf.touch-osc/rotaries)
       ;; [0.6857199 0.7063233 0.6279535 0.34755757 0.5718701 0.3511272 0.38973886 0.18721163]

;; INFO: (1.1670621484518051 7.679459452629089 9.601776003837585 8.960022926330566 5.6619322299957275 0.8458331972360611 6.601698696613312 14.407819509506226)
;; INFO: (2.28851780295372 3.5815733671188354 1.2760154902935028 8.960022926330566 5.6619322299957275 5.153743624687195 39.471760392189026 39.06703293323517)
;; INFO: (3.9576125144958496 7.98323392868042 0.5697963759303093 2.0423825085163116 6.41944944858551 7.031213045120239 41.74787998199463 11.993938684463501)

   )))

(defn ram [x]
  #_(openhat x))

(defn pampam []
  [50 50 35 35 25 60 60 40])

(defn pampam2 []
  [55 60 90 100])

(se
  (metronome 100)
  (ranger sin-machine sin-machine-drone 1000)

  (metronome 200)
  (ranger sin-machine sin-machine-melody 4000)

  (metronome 50)
  (ranger ram pampam 200)

  (metronome 120)
  (ranger ram pampam2 400)
  )
(stop)



(definst something-fuck-you [f 440 krf 500 hf 1000]
  (hpf (sin-osc (* f (sin-osc:kr krf))) hf))
(something-fuck-you)
(stop)
(do (ctl something-fuck-you :f 545 :krf 81 :hf 100) nil)

(defn sese-notes []
  [440 500 600 330]

  (map (comp midi->hz note) [:c3 :g4 :e4 :c2]))

(defn sese []
  (se
    (metronome 50)
    (repeat
      50
      (fn []
        (se
          (metronome 400)
          (ranger #(ctl something-fuck-you :f %)
                  sese-notes
                  1000
                  )
               ))
        ))
    )

(sese)
(stop)

;; obczaic synthy, budowac sobie biblioteke rzeczy takich jak `se`, `ranger`, etc. i instrumenty
;; more emacs

(definst something-fuck-you [f 440 krf 500 hf 1000]
  (hpf (sin-osc (* f (sin-osc:kr krf))) hf))

(stop)
