(ns braeburn.demo
  (:require [braeburn.core :refer [start!]]
            [braeburn.image :refer [image]]))

(def game-handlers
  {:init (fn [] [128 128])
   :on-key (fn [[x y] key-evs]
             (cond
               (get key-evs "ArrowUp") [x (- y 3)]
               (get key-evs "ArrowDown") [x (+ y 3)]
               (get key-evs "ArrowLeft") [(- x 3) y]
               (get key-evs "ArrowRight") [(+ x 3) y]
               :else [x y]))
   :on-tick (fn [state _] state)
   :to-draw (fn [[x y]]
              (let [img (image "https://media.giphy.com/media/NMr9UUZSqQbhS/giphy.gif")]
                {:background (image "https://i.pinimg.com/originals/e1/ff/53/e1ff53238b5263d0e6a963363e3a4ff0.jpg")
                 :sprites [[img x y]]
                 :text [["THIS IS A TEST" 16 16]]}))})

(start! game-handlers)