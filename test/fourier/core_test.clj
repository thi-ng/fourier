(ns fourier.core-test
  (:require
    [clojure.java.io :as io])
  (:use clojure.test
        fourier.core
        fourier.analysis
        fourier.viz
        piksel.core))

(defn dump-spectrum
  [path outpath & {:keys [bins bw window-fn eq sub-bands dropin width]
           :or {window-fn hanning
                bins 512
                eq normalize
                dropin 0
                sub-bands 1
                width 1024}}]
  (let [rate (:mp3.frequency.hz (mp3-file-props (io/file path)))
        windowsize (* 2 bins)
        bw (if (nil? bw) (bandwidth windowsize rate) bw)
        oct (octaves :window windowsize :rate rate)
        samples (sample-seq (mp3-frame-seq path))
        _ (prn dropin)
        spec (spectrum-seq window-fn windowsize (take-nth 2 (drop (* dropin 2 rate) samples)))
        equalized (map (fn[s] (pmap * s (eq bins))) spec)
        bands (pmap (fn[s] (spectrum-octaves windowsize rate bw oct sub-bands s)) equalized)]
    (save-png (render-spectrum-mono bands (linear-amp 16000) width) outpath)))
    
    
(dump-spectrum
  ;"/Users/toxi/Music/iTunes/iTunes Media/Music/Heyoka/Gate Code/08 The Way of the Blue Towel.mp3"
  "../lcom/dev/scmdub2.mp3"
  "spec.png"
  :dropin 60
  :bins 512
  :sub-bands 24
  :eq (equalize -0.02)
  :width 256
  :window-fn (gauss 0.4))
