;; I wanna make a looping machine.
;; I wanna have channels that loop.
;; I wanna set their looping point.
;; I wanna set their length according to the main channel probably.
;; I wanna pan them.
;; I wanna have a cool invisible shortcutful interface for this.
;; I wanna store stuff in files (channels and the whole thing).
;; I wanna record mic, midi, and the synthesized stuff.

(ns asdf.looper
  (:use
    [overtone.live]))

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

(defsynth player [out-bus my-bus buf my-buf]
  (out out-bus (play-buf:ar num-buffer-channels buf :start-pos 1000000.0 :loop true)))


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

;; {:name "PlayBuf",
;;        :args [{:name "num-channels"
;;                :mode :num-outs
;;                :doc "The number of channels that the buffer will
;;                      be. This must be a fixed integer. The architechture
;;                      of the SynthDef cannot change after it is
;;                      compiled. Warning: if you supply a bufnum of a
;;                      buffer that has a different numChannels then you
;;                      have specified to the play-buf, it will fail
;;                      silently." }
;;
;;               {:name "bufnum"
;;                :default 0
;;                :doc "The index of the buffer to use." }
;;
;;               {:name "rate"
;;                :default 1.0
;;                :doc "1.0 is the server's sample rate, 2.0 is one octave
;;                      up, 0.5 is one octave down -1.0 is backwards normal
;;                      rate ... etc. Interpolation is cubic. Note: if the
;;                      buffer's sample rate is different from the
;;                      server's, you will need to multiply the desired
;;                      playback rate by (file's rate / server's rate). The
;;                      UGen (buf-rate-scale bufnum) returns this factor."}
;;
;;               {:name "trigger"
;;                :default 1.0
;;                :doc "A trigger causes a jump to the startPos. A trigger
;;                      occurs when a signal changes from <= 0 to > 0." }
;;
;;               {:name "start-pos"
;;                :default 0.0
;;                :doc "Sample frame to start playback." }
;;
;;               {:name "loop"
;;                :default 0.0
;;                :doc "1 means true, 0 means false. This is modulateable."}
;;
;;               {:name "action"
;;                :default 0
;;                :doc "an integer representing an action to be executed
;;                      when the buffer is finished playing. This can be
;;                      used to free the enclosing synth. Action is only
;;                      evaluated if loop is 0"}]
;;        :doc "Plays back a sample resident in a buffer"}


;; some other stuff

(def ba (buffer 2048))
(def bb (buffer 2048))

(demo 10
      (let [input  (sound-in) ; mic
            src    (white-noise) ; synth - try replacing this with other sound sources
            formed (pv-mul (fft ba input) (fft bb src))
            audio  (ifft formed)]
        (pan2 (* 0.5 audio))))
