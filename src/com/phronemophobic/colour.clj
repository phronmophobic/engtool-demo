(ns com.phronemophobic.colour
  (:import (java.util Random)))

(defn hsl->rgb
  [[h s l]]
  (let [c    (* s (- 1 (abs (- (* 2 l) 1))))
        h'   (/ h 60)
        x    (* c (- 1 (abs (- (mod h' 2) 1))))
        m    (- l (/ c 2))
        rgb1 (cond
               (<= 0 h' 1) [c x 0]
               (<= 1 h' 2) [x c 0]
               (<= 2 h' 3) [0 c x]
               (<= 3 h' 4) [0 x c]
               (<= 4 h' 5) [x 0 c]
               (<= 5 h' 6) [c 0 x])]
    (mapv + [m m m] rgb1)))

(defn random-monochrome
  [seed]
  (let [r  (Random. seed)
        h  (.nextInt r 360)
        s1 (.nextDouble r)
        s2 (.nextDouble r)
        l1 (+ 0.25 (* 0.25 (.nextDouble r)))
        l2 (+ 0.75 (* 0.25 (.nextDouble r)))]
    {:fg   (hsl->rgb [h s1 l1])
     :bg   (hsl->rgb [h s2 l2])
     :seed seed}))

(defn random-complementary
  [seed]
  (let [r  (Random. seed)
        h1 (.nextInt r 360)
        h2 (mod (+ h1 180) 360)
        s1 (.nextDouble r)
        s2 (.nextDouble r)
        l1 (+ 0.25 (* 0.25 (.nextDouble r)))
        l2 (+ 0.75 (* 0.25 (.nextDouble r)))]
    {:fg   (hsl->rgb [h1 s1 l1])
     :bg   (hsl->rgb [h2 s2 l2])
     :seed seed}))

(defn random-tertiary
  [seed]
  (let [r  (Random. seed)
        h1 (.nextInt r 360)
        sn (let [choice (.nextInt r 2)]
             (case choice
               0 -
               1 +))
        h2 (mod (sn h1 22) 360)
        s1 (.nextDouble r)
        s2 (.nextDouble r)
        l1 (+ 0.25 (* 0.25 (.nextDouble r)))
        l2 (+ 0.75 (* 0.25 (.nextDouble r)))]
    {:fg   (hsl->rgb [h1 s1 l1])
     :bg   (hsl->rgb [h2 s2 l2])
     :seed seed}))

(defn random-cols
  [seed]
  (let [r      (Random. seed)
        choice (.nextInt r 3)
        method (case choice
                 0 random-monochrome
                 1 random-complementary
                 2 random-tertiary)]
    (method seed)))

(defn invert
  [{:keys [fg bg] :as pallette}]
  (merge pallette {:fg bg :bg fg}))
