(ns fourier.core-test
  (:require
    [clojure.java.io :as io])
  (:use clojure.test
        fourier.core
        fourier.analysis
        fourier.viz
        piksel.core))

(defn render-spectrum
  [path outpath & {:keys [bins bw window-fn equalizer-fn sub-bands dropin width]
           :or {window-fn hanning
                bins 512
                equalizer-fn (fn[n] (repeat n 1.0))
                dropin 0
                sub-bands 1
                width 1024}}]
  (let [props (mp3-file-props (io/file path))
        rate (:mp3.frequency.hz props)
        chan (:mp3.channels props)
        _ (prn props)
        windowsize (* 2 bins)
        len (* chan width windowsize)
        _ (prn "reading samples" len)
        samples (-> path mp3-frames mp3-samples)
        samples (take len (take-nth 2 (drop (* dropin chan rate) samples)))
        _ (prn "finding peak...")
        peak 1.0;(reduce (fn[^double p ^double x] (max p (Math/abs x))) 0.0 samples)
        normf (/ 1.0 peak)
        _ (prn "peak" peak "normf" normf)
        samples (map #(* % normf) samples)
        octs (octaves :windowsize windowsize :rate rate :bandwidth bw)
        bands (octave-bands rate octs sub-bands)
        eq (equalizer-fn bins)
        spec (spectrum-seq
               :window-fn window-fn
               :windowsize windowsize
               :pipeline #(-> %
                            (amplify eq)
                            (spectrum-bands windowsize rate bands))
               :samples samples)
        db-spec (map-series linear->db spec)
        thresh-spec (map-series (partial threshold 18 0.0) (derivative db-spec))]
    (save-png (draw-spectrum-mono
                db-spec
                :shader (shade-db (:db.rainbow2 gradients))
                :width width
                :rate rate
                :windowsize windowsize)
              outpath)
    (save-png (draw-spectrum-mono
                thresh-spec
                :shader (shade-db-delta (:db.delta gradients))
                :width width
                :rate rate
                :windowsize windowsize)
              (str "d-" outpath))
    (save-png (draw-spectrum-mono
                spec
                :shader (shade-linear (:db.rainbow gradients) (/ 0.5 windowsize))
                :width width
                :rate rate
                :windowsize windowsize)
              (str "l-" outpath))
    (save-png (draw-spectrum-mono
                (map-series (partial threshold 0.0001 0.0) (derivative spec))
                :shader (shade-linear-delta (:db.delta gradients) (/ 0.5 windowsize))
                :width width
                :rate rate
                :windowsize windowsize)
              (str "ld-" outpath))
    (save-png (draw-power spec width 100 (fn[h x] (* -1 (linear->db x))) rate windowsize) (str "vol-" outpath)) 
    spec))
    
(defn test-spec []
  (render-spectrum
    ;"/Users/toxi/Music/iTunes/iTunes Media/Music/Heyoka/Gate Code/08 The Way of the Blue Towel.mp3"
    ;"/Users/toxi/Music/iTunes/iTunes Media/Music/Ludovico Einaudi/Unknown Album/Due Tramonti.mp3"
    ;"/Users/toxi/Music/iTunes/iTunes Media/Music/kieran/crystalmafia/Some girls wander by mistake.mp3"
    ;"/Users/toxi/Music/iTunes/iTunes Media/Music/kieran/beautiful hesitations/adorelaura.mp3"
    ;"/Users/toxi/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/marceldettmann_dawning.mp3"
    "/Users/toxi/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album (8_26_2007 1_49_44 PM)/17 Track 17.mp3"
    ;"../lcom/dev/scmdub2.mp3"
    ;"../lcom/dev/sines.mp3"
    ;"../lcom/dev/light.mp3"
    "spec-track17.png"
    :dropin 0
    :bins 512
    :sub-bands 12
    :equalizer-fn (equalize -0.04)
    ;:window-fn hamming
    :window-fn (gauss 0.4)
    :width 2584
    ))

;(def spec (test-spec))
