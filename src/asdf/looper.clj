(ns asdf.looper)
(use 'overtone.live)

(def my-bus (audio-bus))

(def num-buffer-channels 1)

(def b (buffer (* 44100 10) num-buffer-channels))

(defsynth in-syn [out-bus 0]
  (out out-bus (sound-in 0)))

(defsynth noisemaker [freq 220 out-bus 0 ]
  (out out-bus (lf-tri:ar freq)))

(defsynth recorder []
  (let [signal (in:ar my-bus) ]
    (record-buf:ar [signal] b :loop 0)))

(defsynth player [buf 0]
  (out 0 (play-buf:ar num-buffer-channels buf :loop 1)))

(noisemaker 120 my-bus)
(kill noisemaker)
(in-syn my-bus)
(recorder)
(kill recorder)
(stop)
(player b)
(kill player)






(def a (buffer 2048))
(def b (buffer 2048))

(demo 10
      (let [input  (sound-in) ; mic
            src    (white-noise) ; synth - try replacing this with other sound sources
            formed (pv-mul (fft a input) (fft b src))
            audio  (ifft formed)]
        (pan2 (* 0.1 audio))))
