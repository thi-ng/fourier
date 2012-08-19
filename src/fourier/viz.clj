(ns fourier.viz
  (:require
    [fourier.analysis :as ana]
    [piksel.core :as pix])
  (:import
    [java.awt Graphics2D Color]
    [java.awt.image BufferedImage]))

(defn resource-stream
  [name]
  (->
    (Thread/currentThread)
    (.getContextClassLoader)
    (.getResourceAsStream (str "fourier/" name))))

(def gradients
  (into {} 
    (map (fn[id]
           [(keyword id)
            ^BufferedImage (pix/load-image (resource-stream (str id ".png")))])
         ["db.rainbow" "db.rainbow2" "db.delta" "db.delta2"])))

(defn line
  [gfx x1 y1 x2 y2 col]
  (.setPaint gfx col)
  (.drawLine gfx x1 y1 x2 y2))

(defn ^:static draw-stereo-sample
  [gfx [l r] x yl yr h col-l col-r]
  (let [l (+ (* l h) yl)
        r (+ (* r h) yr)]
    (line x l x yl col-l)
    (line x r x yr col-r)))

(defn draw-waveform
  [img frame]
  (let [gfx (.createGraphics img)
        pairs (partition 2 frame)]
    (loop[pairs pairs x 0]
      (when-let [p (first pairs)]
        (draw-stereo-sample gfx p x)
        (recur (rest pairs) (inc x))))))

(defn draw-ruler
  [gfx width height step col]
  (doseq[x (range 0 width step)]
    (line gfx x 0 x height col)))

(defn ^BufferedImage draw-waveform
  [samples skip w h]
  (let [img (pix/make-image w h)
        gfx (.createGraphics img)
        sh (/ skip 2.0)
        tx (int (/ 44100 (/ skip 2)))
        yl (int (* h 0.25))
        yr (int (* h 0.75))
        h2 (int (/ h 2))
        h4 (int (/ h 4))
        gl (pix/linear-gradient :start [0 0] :end [0 h2] :fractions [0 0.5 1.0] :colors [[0 0 0 64] [255 0 0 255] [0 0 0 64]])
        gr (pix/linear-gradient :start [0 h2] :end [0 h] :fractions [0 0.5 1.0] :colors [[0 0 0 64] [0 0 255 255] [0 0 0 64]])
        bg (pix/linear-gradient :start [0 0] :end [0 h] :fractions [0 1.0] :colors [[0 0 0 0] [0 0 0 96]])]
    (doto gfx (.setPaint bg) (.fillRect 0 0 w h))
    (loop[samples samples x 0]
      (when (zero? (rem x tx)) (line gfx x 0 x h Color/BLACK))
      (if-let[f (take 2 samples)]
        (when (and (= (count f) 2) (< x w))
          (draw-stereo-sample gfx f x yl yr h4 gl gr)
          (recur (drop skip samples) (inc x)))))
    img))

(defn ^BufferedImage draw-power
  [spec width height f rate windowsize]
  (let [img (pix/make-image width height)
        gfx (.createGraphics img)
        secs (/ rate windowsize)]
    (loop [vol (map #(reduce + %) (take width spec)) x 0]
      (when-let [v (first vol)]
        (when (zero? (rem x 100)) (prn x))
        (line gfx x height x (f height v) Color/RED)
        (recur (rest vol) (inc x))))
    (draw-ruler gfx width height secs (Color. 1.0 1.0 1.0 0.5))
    img))

(defn ^BufferedImage draw-spectrum-mono
  [spec & {:keys[shader width rate windowsize]
           :or {rate 44100 windowsize 1024}}]
  (let [height (count (first spec))
        img (pix/make-image width height)
        gfx (.createGraphics img)
        pixels (.getRGB img 0 0 width height nil 0 width)
        secs (/ rate windowsize)]
    (loop[spec (take width spec) x 0]
      (when (zero? (rem x 100)) (prn x))
      (if-let [s (first spec)]
        (let [peak (reduce max s)]
          (dorun
            (map (fn[f y]
                   (let[[a r g b] (shader f)]
                     (pix/blend-pixel pixels x y width r g b a pix/blend-replace)))
                 s (range height)))
          (recur (rest spec) (inc x)))
        (do
          (.setRGB img 0 0 width height pixels 0 width)
          (draw-ruler gfx width height secs (Color. 1.0 1.0 1.0 0.5))
          img)))))

(defn ^BufferedImage draw-spectrum-stereo
  [spec-l spec-r amp-fn width]
  (let [height (count (first spec-l))
        img (pix/make-image width height)
        pixels (.getRGB img 0 0 width height nil 0 width)]
    (loop[spec-l (take width spec-l) spec-r (take width spec-r) x 0]
      (when (zero? (rem x 100)) (prn x))
      (let [sl (first spec-l) sr (first spec-r)]
        (if (and sl sr)
          (do
            (dorun
              (map (fn[l r y]
                     (let[l (amp-fn l)
                          r (amp-fn r)]
                       (pix/blend-pixel pixels x y width l 0.0 r 1.0 pix/blend-replace)))
                   sl sr (range height)))
            (recur (rest spec-l) (rest spec-r) (inc x)))
          (do
            (.setRGB img 0 0 width height pixels 0 width)
            img))))))

(defn clip [x mi mx] (max mi (min mx x)))

(defn shade-db
  [^BufferedImage img]
  (let[w (.getWidth img)
       lut (.getRGB img 0 0 w 1 nil 0 w)
       s (/ w -100.0)
       w (dec w)]
    (fn[x]
      (pix/unpack-argb
        (aget
          lut
          (clip (int (* s (if (Double/isInfinite x) -1000 x))) 0 w))))))

(defn shade-db-delta
  [^BufferedImage img]
  (let[w (.getWidth img)
       lut (.getRGB img 0 0 w 1 nil 0 w)
       w2 (/ w 2)
       w (dec w)
       s (/ w2 100.0)]
    (fn[x]
      (pix/unpack-argb
        (aget
          lut
          (clip (int (+ w2 (* s (if (or (Double/isInfinite x)
                                        (Double/isNaN x))
                                  0 x))))
                0 w))))))
        
(defn shade-linear
  [^BufferedImage img amp]
  (let[w (.getWidth img)
       lut (.getRGB img 0 0 w 1 nil 0 w)
       s (/ w amp)
       w (dec w)]
    (fn[x] (pix/unpack-argb (aget lut (- w (clip (int (* s x)) 0 w)))))))

(defn shade-linear-delta
  [^BufferedImage img amp]
  (let[w (.getWidth img)
       lut (.getRGB img 0 0 w 1 nil 0 w)
       w2 (/ w 2)
       s (/ w2 amp)
       w (dec w)]
    (fn[x] (pix/unpack-argb (aget lut (clip (int (+ w2 (* s x))) 0 w))))))
