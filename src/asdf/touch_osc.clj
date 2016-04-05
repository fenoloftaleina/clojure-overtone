(ns asdf.touch_osc)
(use 'overtone.live)

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

