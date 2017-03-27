(ns asdf.midi
  (:use [overtone.live])
  (:require [overtone.midi :as midi]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan close! thread alts! alts!! timeout]]))

(def keyboard (midi/midi-in))

(def notes-chan (chan))

(defn time-now [] (new java.util.Date))

(defn group-handled-input []
  (go
    (loop [events []]
      (let [t (timeout 50)
            [event port] (alts! [notes-chan t])
            timeout? (= port t)
            maybe-print-buffer
            (fn []
              (when-not (empty? events)
                (let [sound-letters
                      ["c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"]
                      midi-note->sound-letter
                      (fn [note] (get sound-letters (mod note 12)))
                      buffer
                      (mapv
                        (fn [{:keys [note]}]
                          (let [letter (midi-note->sound-letter note)
                                octave (dec (int (/ note 12)))]
                            (str letter octave)))
                        (sort-by :note events))]
                  (println (apply str (interpose " " buffer))))))]
        (if timeout?
          (do
            (maybe-print-buffer)
            (recur []))
          (recur (conj events event)))))))

(defn handle-input []
  (on-event [:midi :note-on] (fn [event] (>!! notes-chan event)) ::note-printer))

(defn wait-indefinitely []
  (<!! (chan)))

(defn display-keyboard-input []
  (group-handled-input)
  (handle-input)
  (wait-indefinitely))

(defn -main []
  (display-keyboard-input))
