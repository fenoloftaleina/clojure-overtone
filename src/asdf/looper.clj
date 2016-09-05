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
    (record-buf:ar [signal] b)))

(defsynth player [buf 0]
  (out 0 (play-buf:ar num-buffer-channels buf)))

(noisemaker 120 my-bus)
(in-syn my-bus)
(recorder)
(stop)
(player b)
