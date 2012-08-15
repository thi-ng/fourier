(ns fourier.viz
  (:require
    [piksel.core :as pix])
  (:import
    [java.awt Graphics2D Color]
    [java.awt.image BufferedImage]))

(defn ^:static draw-stereo-sample
  [gfx [l r] x yl yr h col-l col-r]
  (let [l (+ (* l h) yl)
        r (+ (* r h) yr)]
    (.setPaint gfx col-l)
    (.drawLine gfx x l x yl)
    (.setPaint gfx col-r)
    (.drawLine gfx x r x yr)))

(defn draw-ruler
  [gfx x h]
  (.setPaint gfx Color/BLACK)
  (.drawLine gfx x 0 x h))

(defn draw-waveform
  [img frame]
  (let [gfx (.createGraphics img)
        pairs (partition 2 frame)]
    (loop[pairs pairs x 0]
      (when-let [p (first pairs)]
        (draw-stereo-sample gfx p x)
        (recur (rest pairs) (inc x))))))

(defn ^BufferedImage render-waveform
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
      (when (zero? (rem x tx)) (draw-ruler gfx x h))
      (if-let[f (take 2 samples)]
        (when (and (= (count f) 2) (< x w))
          (draw-stereo-sample gfx f x yl yr h4 gl gr)
          (recur (drop skip samples) (inc x)))))
    img))

(defn ^BufferedImage render-spectrum-mono
  [spec amp-fn width]
  (let [height (count (first spec))
        img (pix/make-image width height)
        pixels (.getRGB img 0 0 width height nil 0 width)]
    (loop[spec (take width spec) x 0]
      (when (zero? (rem x 100)) (prn x))
      (if-let [s (first spec)]
        (do
          (let [peak (reduce max s)]
            (doall
              (map (fn[f y]
                     (let[ff (amp-fn f)]
                       (if (= f peak)
                         (pix/blend-pixel pixels x y width 255 255 255 1.0 pix/blend-replace)
                         (pix/blend-pixel pixels x y width 0.0 ff 0.0 1.0 pix/blend-replace))))
                   s (range height)))
            (recur (rest spec) (inc x))))
        (do
          (.setRGB img 0 0 width height pixels 0 width)
          img)))))

(defn ^BufferedImage render-spectrum-stereo
  [spec-l spec-r amp-fn width]
  (let [height (count (first spec-l))
        img (pix/make-image width height)
        pixels (.getRGB img 0 0 width height nil 0 width)]
    (loop[spec-l (take width spec-l) spec-r (take width spec-r) x 0]
      (when (zero? (rem x 100)) (prn x))
      (let [sl (first spec-l) sr (first spec-r)]
        (if (and sl sr)
          (do
            (doall
              (map (fn[l r y]
                     (let[l (amp-fn l)
                          r (amp-fn r)]
                       (pix/blend-pixel pixels x y width l 0.0 r 1.0 pix/blend-replace)))
                   sl sr (range height)))
            (recur (rest spec-l) (rest spec-r) (inc x)))
          (do
            (.setRGB img 0 0 width height pixels 0 width)
            img))))))
