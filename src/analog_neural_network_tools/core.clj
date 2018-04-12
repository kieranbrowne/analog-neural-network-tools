(ns analog-neural-network-tools.core
  (:require [quil.core :as q]
            [clojure.math.combinatorics :as combo]
            [quil.middleware :as m]))


(def cut [255 0 0])
(def etch [0 0 255])
(def raster-etch [0 0 0])

(def mm->300dpi (partial * 11.81))

(defn function-block [fn start end step]
  (q/with-stroke etch
    (q/begin-shape)
    (doseq [i (range start (+ end step) step)]
      (q/vertex
        (q/map-range i start end 0 (q/width))
        (q/map-range
          (fn i) -1 1 0 (q/height))))
    (q/end-shape)))

;; (Math/PI)

(defn puzzle-join
  [{:keys [from-d to-d rotation]}]
  (q/with-rotation [rotation]
    (q/with-stroke cut
      ;; (q/stroke-weight 1)
      (q/begin-shape)
      (q/vertex 0 (* -1/2 from-d))

      (q/bezier-vertex
       0  (q/map-range 0.1 0 1 (* -1/2 to-d) (* -1/2 from-d))

       20 (q/map-range 1.2 0 1 (* -1/2 to-d) (* -1/2 from-d))

       ;; middle point
       20 (q/map-range 0.5 0 1 (* -1/2 to-d) (* -1/2 from-d))
       )

      (q/bezier-vertex
       20 (q/map-range -0.2 0 1 (* -1/2 to-d) (* -1/2 from-d))
       0  (q/map-range 0.9 0 1 (* -1/2 to-d) (* -1/2 from-d))
       0  (* -1/2 to-d))

      (q/end-shape)
      )
    )
  )

(defn function-round
  [{:keys [fn start end points max min radius mode numbers? tick-size text-fn prefix] :or {mode :outer numbers? true tick-size (* 0.005 (q/height)) text-fn identity prefix ""}}]
  ;; (q/with-stroke cut
  ;;   (q/ellipse 0 0 radius radius))

  ;; (q/text-font (q/create-font "IM FELL Double Pica PRO" 32))
  (q/text-font (q/create-font "Times" 32))
  (q/text-align :center :center)
  (q/with-stroke etch
    (doseq [i points]
      (q/with-rotation
        [(q/map-range
          (fn i)
          min max 0 (* 2 (Math/PI)))]
        (q/with-fill etch
          (let [number (apply str
                              (take (if (< (text-fn i) 0) 4 3)
                                    (str (text-fn i))))
                x (- (* (count number)
                        (if (rational? i)
                          (* 0.0015 (q/height))
                          (* 0.002 (q/height))
                            )))
                y ((case mode :inner + :outer -) (* 1/2 radius)
                   (if (rational? i)
                     (* 0.036 (q/height))
                     (* 0.02 (q/height))
                     ))]

            (if (rational? i)

              (q/text-size (* 0.02 (q/height)))
              (q/text-size (* 0.012 (q/height))))
            (q/push-matrix)
            (q/translate 0 y)
            (when (= mode :inner)
              ;; (q/text-align :right)
              (q/rotate Math/PI)
              )
            (when numbers?
              (q/with-fill raster-etch
                (q/text-size (* 0.014 (q/height)))
                (q/text-align :center :baseline)
                (q/text prefix (* -0.008 (q/height)) (* 0.012 (q/height)))
                (if (rational? i)
                  (q/text-size (* 0.02 (q/height)))
                  (q/text-size (* 0.012 (q/height))))
                (q/text number (* 0.006 (q/height)) (* 0.012 (q/height)))
                ))
            ;; (q/text-align :left)
            (q/pop-matrix)
            ))

        (q/no-fill)
        (q/line 0 (/ radius 2)
                0 (- (/ radius 2)
                     ((case mode :inner - :outer +)
                      (if (rational? i) (* 0.014 (q/height)) tick-size))))
        ))))




(defn sigmoid [x]
  (Math/tanh x))

(defn log [x]
  (Math/log x))


(defn setup []
  (q/text-mode :shape)
  (q/no-fill)
  {:color 0
   :angle 0
   :method :dev})

(defn update [method]
  (constantly {:method method}))

;; (def mult-outer
;;    {:fn log :start 1 :end 10
;;     :points (range 1 10)
;;     :max 0 :min 2.302 :radius 700}
;;    {:fn log :start 1 :end 10
;;     :points (range 1 10 0.1)
;;     :max 0 :min 2.302 :radius 700 :numbers? false}
;;    {:fn log :start 1 :end 10
;;     :points (range 1 2 0.1)
;;     :max 0 :min 2.302 :radius 700}
;;    {:fn log :start 1 :end 10
;;     :points (range 2 3 0.2)
;;     :max 0 :min 2.302 :radius 700}
;;   )

(defn approx= [x y]
  (> 0.001
     (Math/abs (- x y))))

(defn mult-outer []
  ((juxt
    identity
    #(assoc % :points
            (remove (fn [x] (or (approx= x (Math/ceil x)) (approx= x (Math/floor x)))) (range 1 9.9 0.1)) :numbers? false)
    #(assoc % :points (remove (fn [x] (approx= x (Math/floor x))) (range 1.1 2.0 0.1)) :numbers? false)
    #(assoc % :points (remove (fn [x] (approx= x (Math/floor x))) (range 2.2 3.0 0.2)) :numbers? false))
   {:fn log :start 1 :end 10
    :points (range 1 10)
    :prefix "β"
    :max 0 :min 2.302 :radius (* 0.84 (q/height))}))

(= 1.0 (Math/floor 1.2))
(range -4 4 0.1)
(approx= 1 1.1)
(defn add-outer []
  ((juxt
    identity
    #(assoc % :points (remove (fn [x] (approx= x (Math/floor x))) (range -4 4 0.1)) :numbers? false)
    ;; #(assoc % :points (range -9 9 0.05) :numbers? false :tick-size 2)
    )
   {:fn identity :points (range -4 5 1) :max 5 :min -5 :prefix "δ"}))

(== 1 1.0)
(filter)


(defn connect-all [as bs max-dist]
  (doall
   (for [a as]
     (doall
      (for [b bs]
        (if (> max-dist (apply q/dist (into a b)))
          (apply q/line (into a b)))))
     ))
  )
(defn circular-coords [n r]
  (for [i (range n)]
    [(* (q/sin (* (/ (Math/PI) n) i 2)) r)
     (* (q/cos (* (/ (Math/PI) n) i 2)) r)]))

(defn pattern [n size]
  ;; (combo/combinations [[1 2] 2 3] 2)
  (let [points (circular-coords n size)
        l2 (circular-coords 16 (* 0.7 size))
        l3 (circular-coords 12 (* 0.45 size))
        l4 (circular-coords 7 (* 0.22 size))
        l5 [[0 0]]

        ]
    (q/no-fill)
    (apply q/stroke etch)
    (doall (map #(apply q/ellipse (into % [(* 1/4 size) (* 1/4 size)])) points))
    (doall (map #(apply q/ellipse (into % [(* 1/6 size) (* 1/6 size)])) l2))
    (doall (map #(apply q/ellipse (into % [(* 1/10 size) (* 1/10 size)])) l3))
    (doall (map #(apply q/ellipse (into % [(* 1/14 size) (* 1/14 size)])) l4))
    (connect-all points l2 (* 0.100 (q/height)))
    (connect-all l2 l3 (* 0.08 (q/height)))
    (connect-all l3 l4 (* 0.07 (q/height)))
    (connect-all l4 l5 (* 0.08 (q/height)))
    (q/ellipse 0 0 (* 1/18 size) (* 1/18 size))
    ;; (connect-all l4 l4)
    ;; (doall
    ;;  (map #(apply q/line (flatten %))
    ;;       (combo/combinations
    ;;        points 2)))
    ))


(defn star [n size in-size]
  (q/begin-shape)
  (doall
   (for [i (range (* 2 n))]
     (q/vertex
      (* (q/sin (* (/ (Math/PI) (* 2 n)) i 2)) (if (= 0 (mod i 2)) size in-size))
      (* (q/cos (* (/ (Math/PI) (* 2 n)) i 2)) (if (= 0 (mod i 2)) size in-size)))))
  (q/end-shape :close)
  )

(q/ellipse)
(second
 (first
  (combo/combinations
   (for [i (range 5)]
     [(* (q/sin (* (/ (Math/PI) 5) i 2)) 50)
      (* (q/cos (* (/ (Math/PI) 5) i 2)) 50)]
     )
   2)))


(defn indicator [r mode]
  ;; (q/ellipse 0 0 (* 0.46 (q/height)) (* 0.46 (q/height)))
  (q/no-stroke)
  (q/triangle
   0 (* -1/2 r (q/height))
   (*  0.004 (q/height)) ((case mode :inward - +) (* -1/2 r (q/height)) (* 0.01 (q/height)))
   (* -0.004 (q/height)) ((case mode :inward - +) (* -1/2 r (q/height)) (* 0.01 (q/height)))
  ))

(defn curved-word [word size offset spacing]
  (apply q/fill raster-etch)
  (doall
   (for [i (range (count word))]
     (q/with-rotation
       ;; [0]
       [(+ (q/radians (* (- (/ (+ -1.0 (count word)) 2) i) (/ 360 spacing)))
           offset)]
       ;; (q/no-fill)
       ;; (apply q/stroke etch)
       ;; (q/ellipse 0 145 30 30)
       (q/text-align :center :baseline)
       ;; (q/no-stroke)
       (q/text (nth (map str word) i) 0 size)
       ))))

(defn draw [state]
  (q/background 255)
  (q/no-fill)
  (q/with-translation [(* (q/width) 0.25) (/ (q/height) 2)]
    (when (= (:method state) :print) (q/scale 0.7))
    ;; (when (= (:method state) :dev) (q/scale 0.3))


    ; draw outer multiplier disc
    (q/stroke-weight (mm->300dpi 0.1))
    (q/with-stroke cut
      ;; edge of device
      (q/ellipse 0 0 (* 0.96 (q/height)) (* 0.96 (q/height)))
      (q/with-translation [(* 1/2 (q/width)) 0]
        (q/ellipse 0 0 (* 0.96 (q/height)) (* 0.96 (q/height)))

        (q/with-stroke etch
          (q/ellipse 0 0 (* 0.72 (q/height)) (* 0.72 (q/height)))
          (q/ellipse 0 0 (* 0.7 (q/height)) (* 0.7 (q/height)))
          (q/ellipse 0 0 (* 0.46 (q/height)) (* 0.46 (q/height)))
          (q/ellipse 0 0 (* 0.44 (q/height)) (* 0.44 (q/height))))
        )
      )
    ;; (q/ellipse 0 0 800 (q/height))
    (mapv function-round
          (map #(assoc % :mode :inner :prefix "α")
               (mult-outer)))
    ; draw inner multiplier disc
    (q/with-stroke cut
      (q/ellipse 0 0 (* 0.84 (q/height)) (* 0.84 (q/height))))
    (mapv function-round (mult-outer))

    (q/with-stroke cut
      (q/ellipse 0 0 (* 0.72 (q/height)) (* 0.72 (q/height))))

    ;; (puzzle-join
    ;;  {:from-d 700 :to-d 800 :rotation 1})
    ;; (puzzle-join
    ;;  {:from-d 100 :to-d 200 :rotation 0})

    ;; summer
    ;; outer marker
    (q/with-stroke cut
      (q/ellipse 0 0 (* 0.7 (q/height)) (* 0.7 (q/height))))
    ;; (q/with-fill etch
    ;;   (q/triangle 0 -296 -2 -300 2 -300)
    ;;   ;; (q/triangle 0 -204 -2 -200 2 -200)
    ;;   )

    (q/with-stroke cut
      (q/ellipse 0 0 (* 0.58 (q/height)) (* 0.58 (q/height))))

    ; draw outer add disc
    (mapv function-round
          (map #(assoc % :radius (* 0.7 (q/height)) :numbers? false)
               (add-outer)))
    (mapv function-round
          (map #(assoc % :radius (* 0.58 (q/height)) :mode :inner :prefix "γ")
               (add-outer)))
    ;; (q/rotate 4)

    (q/with-stroke cut
      (q/ellipse 0 0 (* 0.46 (q/height)) (* 0.46 (q/height)))
      )
    ;; ; draw inner add disc
    (mapv function-round
          (map #(assoc % :radius (* 0.58 (q/height)) :numbers? false) (add-outer)))
    (mapv function-round
          (map #(assoc % :radius (* 0.46 (q/height)) :mode :inner :fn - :text-fn (partial max 0))
               (add-outer))
          )
    (q/with-stroke cut
      ;; (q/ellipse 0 0 (* 0.44 (q/height)) (* 0.44 (q/height)))
      )
    ;; (q/blend
    ;;  (q/load-image "snake.png")
    ;;  0 0 2100 3431
    ;;  (* 0.32 (q/width))  (* 0.49 (q/height)) 300 400
    ;;  :multiply)
    ;; (q/ellipse 0 0 410 410)

    ; inner draw sum indicator
    ;; (q/ellipse 0 0 410 410)
    (q/with-fill raster-etch
      (indicator 0.7 :inward)
      (indicator 0.46 :outward)
      )

    ;; (pattern 6 30)
    (q/stroke-weight (mm->300dpi 0.1))
    ;; (pattern 2 170)
    ;; (pattern 3 120)
    (q/no-fill)
    (apply q/stroke etch)
    (pattern 12 (* 0.17 (q/height)))
    ;; (q/ellipse 0 0 60 60)
    ;; (q/ellipse 0 0 97 97)
    ;; (q/ellipse 0 0 190 190)
    ;; (q/ellipse 0 0 227 227)
    ;; (pattern 6 (* 0.032 (q/height)))
    ;; (star 6 120 46)
    ;; (star 3 (* 0.05 (q/height)) (* 0.014 (q/height)))
    ;; (star 4 (* 0.22 (q/height)) (* 0.13 (q/height)))
    ;; (star 4 (* 0.13 (q/height)) (* 0.23 (q/height)))
    ;; (star 6 (* 0.14 (q/height)) (* 0.14 (q/height)))
    ;; (star 6 (* 0.10 (q/height)) (* 0.06 (q/height)))
    ;; (star 6 120 180)
    ;; (star 6 (* 0.1 (q/height)) (* 0.1 (q/height)))
    ;; (q/ellipse 0 0 80 80)
    ;; (q/ellipse 0 0 240 240)
    ;; (q/ellipse 0 0 360 360)

    (q/ellipse 0 0 (* 0.44 (q/height)) (* 0.44 (q/height)))

    ;; (q/ellipse 0 0 262 262)
    ;; (star 3 180 22)
    ;; (star 3 180 122)

    ;; (pattern 4 170)
    ;; (pattern 5 170)
    ;; (pattern 6 121)
    ;; (pattern 6 61)
    ;; (q/text-size 40)
    (apply q/fill raster-etch)
    (q/text-font (q/create-font "Big Caslon" 16))
    ;; (let [size 169
    ;;       spacing 100]
    ;;   (curved-word "αααααα" size (q/radians 30) spacing)
    ;;   (curved-word "νοῦς" size (q/radians 90) spacing)
    ;;   (curved-word "νοῦς" size (q/radians 150) spacing)
    ;;   (curved-word "νοῦς" size (q/radians 210) spacing)
    ;;   (curved-word "νοῦς" size (q/radians 270) spacing)
    ;;   (curved-word "νοῦς" size (q/radians 330) spacing))
    ;; (curved-word "αααααα" 363 (q/radians 35) 6)
    ;; (curved-word "ββββββ" 320 (q/radians 20) 6)
    ;; (curved-word "γγγγγγγγγγ" 257 (q/radians 35) 10)
    ;; (curved-word "εεεεεεεεεε" 215 (q/radians 35) 10)

    ;; (q/ellipse 0 0 400 400)
    ; draw centre point
    ;; (q/with-stroke [0]
    ;;   (q/ellipse 0 0 2 2))
    ;; function-round
    ;; (function-round
    ;;  {:fn identity :start 1 :end 10 :points (range -1 1 0.1) :max -1 :min 1 :radius 800})
    )
  ;; (q/save "test.png")
  (when-not (= (:method state) :dev) (q/exit)))


(map str "sdfasdf")
(count "wer")

(+ 1 2)

(q/defsketch analog-neural-network-tools-dev
  :size [800 800]
  :renderer :java2d
  :setup setup
  :update (update :dev)
  :middleware [m/fun-mode]
  :draw draw)

(q/defsketch analog-neural-network-tools-print
  :size [595 842]
  :renderer :pdf
  :output-file "output.pdf"
  :setup setup
  :update (update :print)
  :middleware [m/fun-mode]
  :draw draw)

(mm->300dpi 0.1)

(q/defsketch analog-neural-network-tools-lazer-cut
  :size (map (comp int #(Math/ceil %) mm->300dpi) [1400 700])
  :renderer :pdf
  :output-file "output.pdf"
  :setup setup
  :update (update :lazer-cut)
  :middleware [m/fun-mode]
  :draw draw)


;; sizing / weight
(let [;; assuming 70cm diameter in brass
      area (* Math/PI (Math/pow 40 2)) ;; cm^2
      volume (* area 0.6) ;; cm^3
      brass-weight (* volume 8.4)
      ]
  (/ brass-weight 1000) ;; kg
  )
