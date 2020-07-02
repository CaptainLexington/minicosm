(ns minicosm.core
  (:require [minicosm.ddn :refer [render!]]
            [clojure.set :as set]))

(defn- make-callback
  [url key assets asset]
  (fn [_]
    (println (name key) url)
    (swap! assets assoc key asset)))

(defn- url-to-img [url assets]
  (let [img (js/Image.)]
    (set! (.-onload img) (make-callback url :loaded assets img))
    (set! (.-onerror img) (make-callback url :error assets img))
    (set! (.-src img) url)
    :loading))

(defn- url-to-audio [url assets]
  (let [audio (js/Audio. url)]
    (.addEventListener audio "canplaythrough" (make-callback url :loaded assets audio))
    (set! (.-onerror audio) (make-callback url :error assets audio))
    :loading))

(defn- dispatch-load [[type val] assets]
  (case type 
    :image (url-to-img val assets)
    :audio (url-to-audio val assets)))

(defn- draw-loading [ctx]
  (let [w (.. ctx -canvas -width)
        h (.. ctx -canvas -height)
        old-ta (.-textAlign ctx)]
    (.clearRect ctx 0 0 w h)
    (set! (.-textAlign ctx) "center")
    (.fillText ctx "Loading..." (/ w 2) (/ h 2))
    (set! (.-textAlign ctx) old-ta)))

(defn- load-assets [urls assets]
  (into {} (map (fn [[k v]] [k (dispatch-load v assets)]) urls)))


(defn- handle-audio [state assets audio-state to-play]
  (let [curr-state (deref audio-state)
        new-state (to-play state assets curr-state)
        pruned-state (update new-state :effects (fn [effects] (filter #(.ended %) effects)))
        {curr-music :music curr-effects :effects} curr-state
        {new-music :music new-effects :effects} new-state
        effects-to-stop (set/difference curr-effects new-effects)
        effects-to-start (set/difference new-effects curr-effects)
        music-to-stop (set/difference curr-music new-music)
        music-to-start(set/difference new-music curr-music)]
    (doseq [e effects-to-stop]
      (.pause e)
      (set! (.-currentTime e) 0))
    (doseq [e effects-to-start]
      (.play e))
    (doseq [m music-to-stop]
      (.pause m)
      (set! (.-currentTime m) 0))
    (doseq [m music-to-start]
      (set! (.-loop m) true)
      (.play m))
    (reset! audio-state pruned-state)))

(defn- game-loop! [t ctx key-evs state assets-fn assets audio-state {:keys [on-key on-tick to-play to-draw] :as handlers}]
  (let [new-state (-> state
                      (on-key @key-evs)
                      (on-tick t))
        asset-urls (assets-fn new-state)
        new-asset-keys (clojure.set/difference (set (keys asset-urls))
                                               (set (keys @assets)))
        new-assets (load-assets (select-keys asset-urls new-asset-keys) assets)]
    (swap! assets merge new-assets)
    (.clearRect ctx 0 0 (.. ctx -canvas -width) (.. ctx -canvas -height))
    (handle-audio state new-assets audio-state to-play)
    (render! ctx (to-draw new-state new-assets))
    (js/requestAnimationFrame (fn [t] (game-loop! t ctx key-evs new-state assets-fn assets audio-state handlers)))))

(defn start! game-loop)
