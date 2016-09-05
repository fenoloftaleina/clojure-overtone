(ns asdf.mic)
(use 'overtone.live)


;; Efects

(definst external
  []
  (sound-in 0))

(external)
(stop)

(inst-fx! external fx-distortion2)
(inst-fx! external fx-reverb)

(def lowpass (inst-fx! external fx-rlpf))

(ctl lowpass :cutoff 10000)
(ctl lowpass :cutoff 5000)
(ctl lowpass :cutoff 100)

(clear-fx external)


;; Vocoder

(def a (buffer 2048))
(def b (buffer 2048))

(demo 10
      (let [input  (sound-in) ; mic
            src    (white-noise) ; synth - try replacing this with other sound sources
            formed (pv-mul (fft a input) (fft b src))
            audio  (ifft formed)]
        (pan2 (* 0.1 audio))))
