(ns asdf.looper)
(use 'overtone.live)

(def my-bus (audio-bus))

(def num-buffer-channels 1)

(def my-buf (buffer (* 44100 5) num-buffer-channels))

(defsynth in-syn [out-bus 0]
  (out out-bus (sound-in 0)))

(defsynth noisemaker [freq 220 out-bus 0 ]
  (out out-bus (lf-tri:ar freq)))

(defsynth recorder [in-bus my-bus buf my-buf]
  (let [signal (in:ar in-bus) ]
    (record-buf:ar [signal] buf :loop 0 :pre-level 0.5 :rec-level 0.5)))

(defsynth player [out-bus 0 buf 0]
  (out out-bus (play-buf:ar num-buffer-channels buf :loop 1)))

;; syntetic input
(noisemaker 120 my-bus)
(noisemaker 220 my-bus)
(kill noisemaker)

;; mic input
(in-syn my-bus)
(in-syn)
(kill in-syn)

;; recording
(recorder my-bus my-buf)
(kill recorder)

;; playing
(player my-buf)
(kill player)

;; hmm
(stop)



;; some other stuff

(def ba (buffer 2048))
(def bb (buffer 2048))

(demo 10
      (let [input  (sound-in) ; mic
            src    (white-noise) ; synth - try replacing this with other sound sources
            formed (pv-mul (fft ba input) (fft bb src))
            audio  (ifft formed)]
        (pan2 (* 0.5 audio))))
