(ns com.phronemophobic.uitool
  (:require [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [clojure.core.async :as async]
            [membrane.component :as component
             :refer [defui defeffect]]
            [com.phronemophobic.colour :as col]
            [clojure.pprint :as pp]))



(def fontsize 11)
(def font (ui/font "Berkeley Mono" fontsize))
(def description-font  (ui/font "Berkeley Mono" (+ 0 fontsize)))
(def title-font (assoc (ui/font "Berkeley Mono" (* 4 fontsize)) :weight :bold))
(let [[w h] (ui/bounds (ui/label "#" font))]
  (def glyph-width w)
  (def glyph-height h))

(def input-height (Math/round (* 1.6 fontsize)))

(defui mybutton
  [{:keys [hover? text on-click]}]
  (let [gray [0 0 0 0.16]]
    (ui/on
     :mouse-down on-click
     (ui/translate
      0 0 #_#_1 1
      (let [height input-height
            lbl    (ui/label text font)
            width  (+ (* 2 glyph-width) (first (ui/bounds lbl)))]
        [(ui/with-style :membrane.ui/style-fill
           (ui/with-color gray
             (ui/rectangle width height)))
         (ui/center lbl [width glyph-height])
         (ui/with-style :membrane.ui/style-stroke
           (ui/rectangle width height))])))))

(defui counter
  [{:keys [num min max]
    :or   {num 0}}]
  (let [w   100
        lbl (ui/label num font)
        bm  (mybutton {:text "-"
                       :on-click
                       (fn [_]
                         [[::counter-dec $num min]])})
        bp  (mybutton {:text "+"
                       :on-click
                       (fn [_]
                         [[::counter-inc $num max]])})
        el  [(ui/with-color [0 0 0 0]
              (ui/rectangle w 1))
             (ui/center lbl [w glyph-height])
             bm
             (ui/translate (- w (ui/width bp)) 0 bp)]]
    [(ui/translate
      0 0 #_#_1 1
      (ui/with-style :membrane.ui/style-stroke
        (apply ui/rectangle (ui/bounds el))))
     el]))

(defeffect ::counter-dec [$num min]
  (if min
    (dispatch! :update $num #(max min (dec %)))
    (dispatch! :update $num dec)))

(defeffect ::counter-inc [$num max]
  (if max
    (dispatch! :update $num #(min max (inc %)))
    (dispatch! :update $num inc)))

(defn ->$keyword [kw]
  (keyword (str "$" (name kw))))

(defui number-slider
  [{:keys [num max-width min max integer? mdown?]
    :or   {max-width 100}}]
  (let [ratio (/ (- num min)
                 (- max min))
        width (* max-width (double ratio))
        gray  [0 0 0 0.16]]
    (ui/on
     :mouse-down
     (fn [[x y]]
       [[:set $mdown? true]
        [::update-slider $num min max max-width integer? x]])
     :mouse-up
     (fn [[x y]]
       [[:set $mdown? false]
        [::update-slider $num min max max-width integer? x]])
     :mouse-move
     (fn [[x y]]
       (when mdown?
         [[::update-slider $num min max max-width integer? x]]))
     (ui/translate
      0 0 #_#_1 1
      (let [height input-height
            lbl    (ui/label
                    (if integer?
                      num
                      (format "%.2f" (double num)))
                    font)]
        [(ui/with-style :membrane.ui/style-fill
           (ui/with-color gray
             (ui/rectangle width height)))
         (ui/center lbl [max-width glyph-height])
         (ui/with-style :membrane.ui/style-stroke
           (ui/rectangle max-width height))])))))

(defeffect ::update-slider [$num min max max-width integer? x]
  (let [ratio (/ x max-width)
        num (+ min (* ratio (- max min)))
        num (if integer?
              (int num)
              (double num))]
    (dispatch! :set $num
               (clojure.core/max
                min
                (clojure.core/min num
                                  max)))))

(defui text
  [{:keys [text font coerce text-string starting-value]}]
  (ui/on
   :key-event
   (fn [_ _ _ _]
     [[::coerce-text coerce text-string starting-value $text]])
   (let [el (basic/textarea
             {:text    (or text-string (str starting-value))
              :border? false
              :font    font})]
     [(ui/translate 2 0 el)
      (ui/translate
       0 0 #_#_1 1
       (ui/with-style :membrane.ui/style-stroke
         (ui/rectangle (first (ui/bounds el)) input-height)))])))

(defeffect ::coerce-text
  [coerce text starting-value $coerced-text]
  (let [coercefn (case coerce
                   (:key :keyword)
                   keyword

                   :string
                   str

                   :number
                   read-string

                   :float
                   (fn [s] (-> s read-string float))

                   :double
                   (fn [s] (-> s read-string double))

                   (:integer :int)
                   (fn [s] (-> s read-string int))

                   identity)
        coerced-text (try
                       (coercefn text)
                       (catch Exception _e starting-value))]
    (dispatch! :set $coerced-text coerced-text)))

(defn- input-label-str
  [n k]
  (format (str "%" n "s") (name k)))

(defui generic-counter [{:keys [min max input-key input] :as m}]
  (apply
   ui/horizontal-layout
   (interpose
    (ui/spacer 10)
    [(ui/label (input-label-str (:label-padding m) input-key) font)
     (counter {:num input
               :min min
               :max max})])))

(defui generic-slider [{:keys [min max integer? input-key input] :as m}]
  (apply
   ui/horizontal-layout
   (interpose
    (ui/spacer 10)
    [(ui/label (input-label-str (:label-padding m) input-key) font)
     (number-slider {:num      input
                     :min      min
                     :max      max
                     :integer? integer?})])))

(defui generic-text [{:keys [coerce value input-key input] :as m}]
  (let [{:keys [label-padding]} m]
    (apply
     ui/horizontal-layout
     (interpose
      (ui/spacer 10)
      [(ui/label (input-label-str label-padding input-key) font)
       (text {:text           input
              :starting-value value
              :coerce         coerce
              :border?        true
              :font           font})]))))


(defn center
  [[w h] elem]
  (ui/center elem [w h]))


(def type->input-view-fn
  {:counter #'generic-counter
   :slider #'generic-slider
   :text #'generic-text})

(defn input-views [input-spec]
  (into {}
        (map (fn [[k input]]
               (let [input-view-fn (type->input-view-fn (:type input))
                     view (input-view-fn 
                           (assoc input :input-key k))]
                 [k view])))
        input-spec))

(defui inputs-viewer [{:keys [input-views input-values seed width height fncall view-width view-height viz]}]
  [ ;; background
   (ui/with-color (:bg (col/random-cols seed))
    (ui/rectangle (* 3 width) (* 3 height)))
   ;; 2 Column layout begins here.
   (ui/horizontal-layout
    
    ;; COL 1
    (center
     [(/ width 2) height]
     (apply
      ui/vertical-layout
      (concat
       
       ;; Description
       [(when (:description fncall)
          (ui/with-color
           (:fg (col/random-cols seed))
           (ui/label (:description fncall))))]
       
       ;; Applet Title
       [(ui/with-color
         (:fg (col/random-cols seed))
         (ui/label (str (:app-name fncall) "\n") title-font))]
       
       )))
    
    ;; COL 2
    (center
     [(/ width 2) height]
     (apply
      ui/vertical-layout
      (concat
       
       ;; VIZ
       (let [{:keys [computed-view
                     computed-view-input-values]} viz
             update-label (ui/label "updating..." font)]
         [(if (not= computed-view-input-values
                       input-values)
            update-label
            (ui/spacer (ui/width update-label) (ui/height update-label)))
          (ui/scissor-view
            [0 0] [view-width view-width]
            [(ui/no-events
              (ui/with-style :membrane.ui/style-stroke
                             (ui/with-color
                              (:fg (col/random-cols seed))
                              (ui/rectangle view-width view-width))))
             (ui/with-color (:fg (col/random-cols seed))
                            [computed-view])])])
       
       ;; INPUTS
       [[(ui/with-color [0 0 0 0]
                        (ui/rectangle view-width 1))
         (apply
          ui/vertical-layout
          (concat
           [(ui/with-color [0 0 0 0] (ui/label "-" font))]
           (into []
                 (comp (map (fn [[k view]]
                              
                              (let [input (get input-values k)
                                    input (or input (:value view))
                                    input-extra (get extra [::input k])]
                                (assoc view
                                       :extra input-extra
                                       :$extra $input-extra
                                       :context context
                                       :$context $context
                                       :input input
                                       :$input $input))))
                       (map (fn [view]
                              (ui/with-color (:fg (col/random-cols seed)) 
                                             view))))
                 input-views)))]]))))])

(defn show [spec]
  (let [input-spec (:input-spec spec)
        initial-state (assoc spec
                             :input-views (input-views input-spec)
                             :input-values (into {}
                                                 (map (fn [[k m]]
                                                        [k (:value m)]))
                                                 input-spec))
        atm (atom initial-state)
        handler (component/default-handler atm)
        app (component/make-app #'inputs-viewer 
                                atm
                                handler)
        
        update-viz-chan (async/chan (async/sliding-buffer 1))
        
        async-thread
        (async/thread
         (try
           (let [->view (-> initial-state
                            :fncall
                            :viz)]
             (loop []
               (let [input-values (async/<!! update-viz-chan)
                     computed-view (->view input-values)]
                 (swap! atm assoc :viz {:computed-view computed-view
                                        :computed-view-input-values input-values}))
               
               (recur)))
           (catch Throwable e
             (tap> e)
             (println "error. quitting..."))))]
    (add-watch atm ::update-viz
               (fn [k ref old new]
                 (let [old-inputs (-> old :viz :computed-view-input-values)
                       current-inputs (-> new :input-values)]
                   (when (not= old-inputs
                               current-inputs)
                     (async/put! update-viz-chan current-inputs)))))
    (skia/run app
              {:window-start-width (:width initial-state)
               :window-start-height (:height initial-state)
               :window-title (-> initial-state :fncall :app-name)})))

(comment
  
  (show
   {:input-spec {:gap   {:type     :slider
                         :min      0
                         :max      1.01
                         :integer? false
                         :value    0.3}
                 :deg-a {:type     :slider
                         :min      0
                         :max      360
                         :integer? false
                         :value    45}}
    
    :seed 1
    :width 800
    :height 800
    :view-width 200
    :view-height 200
    :fncall {:description "test description"
             :app-name "test app name"
             :viz (fn [{:keys [gap deg-a]}]
                    (Thread/sleep 2000)
                    (ui/vertical-layout
                     (ui/label (format "%.02f" (double gap)))
                     (ui/label (format "%.02f" (double deg-a)))))}})


  
  
  ,)

