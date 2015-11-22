(ns sicp-escher.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;; Helper functions
(defn for-each [f coll]
  "for-each clojure style"
  (doseq [x coll]
    (f x)))

;; 2d vector class

(defn make-vec [x y]
  [x y])

(defn xcor-vec [v]
  (first v))

(defn ycor-vec [v]
  (second v))

(defn add-vec [v1 v2]
  (make-vec (+ (xcor-vec v1) (xcor-vec v2))
            (+ (ycor-vec v1) (ycor-vec v2))))

(defn sub-vec [v1 v2]
  (make-vec (- (xcor-vec v1) (xcor-vec v2))
            (- (ycor-vec v1) (ycor-vec v2))))

(defn scale-vec [s v]
  (make-vec (* s (xcor-vec v))
            (* s (ycor-vec v))))

;; Line segment class
(defn make-segment
  ([v1 v2] (vector v1 v2))
  ([x1 y1 x2 y2] (vector (make-vec x1 y1) (make-vec x2 y2))))

(defn start-segment [v]
  (first v))

(defn end-segment [v]
  (second v))


;; Frame class
(defn make-frame [origin edge1 edge2]
  (vector origin edge1 edge2))

(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (second frame))

(defn edge2-frame [frame]
  (last frame))

(defn frame-coord-map [frame]
  "Create a procedure that takes and unit vector and transforms maps it
  on to the coordinates of the frame."
  (fn [v]
    (add-vec
     (origin-frame frame)
     (add-vec (scale-vec (xcor-vec v) (edge1-frame frame))
              (scale-vec (ycor-vec v) (edge2-frame frame))))))

;; Drawing functions

(defn draw-line [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (q/line x1 y1 x2 y2)))

;; a painter is a procedure that takes a frame and
;; draws a particular image shifted and scaled to fit the
;; frame.
(defn segments->painter [segment-list]
  "A create a painter from a list of segments"
  (fn [frame]
    (for-each
     (fn [segment]
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


;; primitive segment painters
(def outline-painter
  (segments->painter [(make-segment (make-vec 0 0) (make-vec 0 1))
                      (make-segment (make-vec 0 1) (make-vec 1 1))
                      (make-segment (make-vec 1 1) (make-vec 1 0))
                      (make-segment (make-vec 1 0) (make-vec 0 0))]))

(def x-painter
  (segments->painter [(make-segment (make-vec 0 0) (make-vec 1 1))
                      (make-segment (make-vec 0 1) (make-vec 1 0))]))

(def diamond-painter
  (segments->painter [(make-segment (make-vec 0.5 0) (make-vec 0 0.5))
                      (make-segment (make-vec 0 0.5) (make-vec 0.5 1))
                      (make-segment (make-vec 0.5 1) (make-vec 1 0.5))
                      (make-segment (make-vec 1 0.5) (make-vec 0.5 0))]))


(def wave-painter
  (segments->painter [;; legs
                      (make-segment (make-vec 0.25 0) (make-vec 0.375 0.375))
                      (make-segment (make-vec 0.375 0) (make-vec 0.5 0.375))
                      (make-segment (make-vec 0.75 0) (make-vec 0.625 0.375))
                      (make-segment (make-vec 0.625 0) (make-vec 0.5 0.375))
                      ;; body
                      (make-segment (make-vec 0.375 0.375) (make-vec 0.4 0.6875))
                      (make-segment (make-vec 0.625 0.375) (make-vec 0.6 0.6875))
                      ;; right arm
                      (make-segment (make-vec 0.6 0.6875) (make-vec 0.75 0.5))
                      (make-segment (make-vec 0.6 0.8125) (make-vec 0.75 0.625))
                      (make-segment (make-vec 0.75 0.625) (make-vec 1 1))
                      (make-segment (make-vec 0.75 0.5) (make-vec 1 0.875))
                      ;; left arm
                      (make-segment (make-vec 0.4 0.6875) (make-vec 0 0.25))
                      (make-segment (make-vec 0.4 0.8125) (make-vec 0 0.375))
                      ;; shoulders
                      (make-segment (make-vec 0.4 0.8125) (make-vec 0.45 0.8125))
                      (make-segment (make-vec 0.6 0.8125) (make-vec 0.55 0.8125))
                      ;; neck
                      (make-segment (make-vec 0.45 0.8125) (make-vec 0.45 0.85))
                      (make-segment (make-vec 0.55 0.8125) (make-vec 0.55 0.85))
                      ;; head
                      (make-segment (make-vec 0.55 0.85) (make-vec 0.6 0.925))
                      (make-segment (make-vec 0.45 0.85) (make-vec 0.4 0.925))
                      (make-segment (make-vec 0.6 0.925) (make-vec 0.55 1))
                      (make-segment (make-vec 0.4 0.925) (make-vec 0.45 1))]))


(def heart-painter
  (segments->painter [(make-segment 0.5 0.0 0.0 0.75)
                      (make-segment 0.0 0.75 0.25 1.0)
                      (make-segment 0.25 1.0 0.5 0.75)
                      (make-segment 0.5 0.75 0.75 1.0)
                      (make-segment 0.75 1.0 1.0 0.75)
                      (make-segment 1.0 0.75 0.5 0.0)]))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame
                  new-origin
                  (sub-vec (m corner1) new-origin)
                  (sub-vec (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vec 0.0 1.0)
                     (make-vec 1.0 1.0)
                     (make-vec 0.0 0.0)))

(defn flip-horizontal [painter]
  (transform-painter painter
                     (make-vec 1.0 0.0)
                     (make-vec 0.0 0.0)
                     (make-vec 1.0 1.0)))


(defn rotate-90 [painter]
  (transform-painter painter
                     (make-vec 0.0 1.0)
                     (make-vec 0.0 0.0)
                     (make-vec 1.0 1.0)))


(defn beside [painter1 painter2]
  (let [split-point (make-vec 0.5 0.0)
        paint-left (transform-painter painter1
                                      (make-vec 0.0 0.0)
                                      split-point
                                      (make-vec 0.0 1.0))
        paint-right (transform-painter painter2
                                       split-point
                                       (make-vec 1.0 0.0)
                                       (make-vec 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn below [painter1 painter2]
  (let [split-point (make-vec 0.0 0.5)
        paint-top (transform-painter painter2
                                     split-point
                                     (make-vec 1.0 0.5)
                                     (make-vec 0.0 1.0))
        paint-bottom (transform-painter painter1
                                        (make-vec 0.0 0.0)
                                        (make-vec 1.0 0.0)
                                        split-point)]
    (fn [frame]
      (paint-top frame)
      (paint-bottom frame))))

(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))

(defn right-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller)))))

(defn corner-split [painter n]
  (if (= n 0)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (dec n))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horizontal quarter) quarter)]
    (below (flip-vert half) half)))


;; --------------
(def wave2 (beside wave-painter (flip-vert wave-painter)))
(def wave4 (below wave2 wave2))
(def wave4-sym (below (flip-vert wave2) wave2))
(def wave4-sym2 (beside
                 (below (flip-vert wave-painter)
                        wave-painter)
                 (flip-horizontal
                  (below (flip-vert wave-painter)
                         wave-painter))))


(defn tile-4 [painter]
  "create a painter that divides frame into 4 equal sized
  quadrants and paints the same pattern in each."
  (let [vsplit (below painter painter)]
    (beside vsplit vsplit)))

(def wave4-tile (tile-4 wave4-sym2))


;; Drawing function where the magic happens!
(defn my-draw [w h]
  (let [frame (make-frame
               (make-vec 0 h)
               (sub-vec (make-vec w h) (make-vec 0 h))
               (sub-vec (make-vec 0 0) (make-vec 0 h)))]

    ((square-limit (below wave-painter heart-painter) 4) frame)))


;; Quil graphics

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)

  (my-draw 400 400))


(q/defsketch sicp-escher
  :host "sicp-escher"
  :size [400 400]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
