(ns asdf.tutorial)
(use 'overtone.live)

(demo (pan2 (sin-osc (mouse-x 440 880))))

(stop)

(odoc pan2)

(definst trancy-waves [freq 200]
  (let [freq (mouse-x 100 150)]
  (pan2 (* 0.2 (+ (sin-osc freq) (saw freq) (saw (+ freq 132)) (sin-osc (- freq 128))))))
  )
(trancy-waves)
(ctl trancy-waves :freq 100)
(stop)

(odoc lf-noise1)
(odoc linen)

(definst scratch-pendulum [pulse 1]
  (let [pul (sin-osc:kr (lin-lin:kr (sin-osc:kr pulse) -1 1 1 10))
        ;; kon (sin-osc:kr pul)
        kon (lf-noise1:kr (mouse-x 1 1000))
        lpk (lin-lin:kr kon -1 1 80 1500)
        src (bpf (+
                  (sin-osc 440)
                  (sin-osc 500)
                  (sin-osc 880)
                  (sin-osc 1254)) lpk)]
    [src src]))
(scratch-pendulum)
(ctl scratch-pendulum :pulse 145)
(stop)
(scratch-pendulum 2)
(scratch-pendulum 5)
(scratch-pendulum 10)
(scratch-pendulum 400)
(stop)

(definst roaming-sines
  [a 0.2 b 20]
  (let [freqs (take 5 (repeatedly #(ranged-rand 40 2000)))
        ampmod [a b]
        snd (splay (* 0.5 (sin-osc [40 100 500 600 300])))]
    (* (sin-osc ampmod) snd)))
(roaming-sines)
(ctl roaming-sines :a 50 :b 1400)
(stop)
(odoc splay)

;; (definst both []
;;   (out 0 (pan2 scratch-pendulum)))

(odoc mix)
