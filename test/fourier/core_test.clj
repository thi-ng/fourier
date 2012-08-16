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
                equalizer-fn normalize
                dropin 0
                sub-bands 1
                width 1024}}]
  (let [samples (sample-seq (mp3-frame-seq path))
        rate (:mp3.frequency.hz (mp3-file-props (io/file path)))
        windowsize (* 2 bins)
        oct (octaves :window windowsize :rate rate :bandwidth bw)
        bands (octave-bands rate oct sub-bands)
        eq (equalizer-fn bins)
        spec (spectrum-seq
               :window-fn window-fn
               :windowsize windowsize
               :pipeline #(-> % (amplify eq) (spectrum-bands windowsize rate bands))
               :samples (take-nth 2 (drop (* dropin 2 rate) samples)))]
    (save-png (render-spectrum-mono spec (linear-amp 16000) width) outpath)))
    
    
(render-spectrum
  ;"/Users/toxi/Music/iTunes/iTunes Media/Music/Heyoka/Gate Code/08 The Way of the Blue Towel.mp3"
  "/Users/toxi/Music/iTunes/iTunes Media/Music/kieran/crystalmafia/Some girls wander by mistake.mp3"
  ;"../lcom/dev/scmdub2.mp3"
  ;"../lcom/dev/sines.mp3"
  "spec.png"
  :dropin 85
  :bins 512
  :sub-bands 12
  :equalizer-fn (equalize -0.025)
  ;:window-fn (gauss 0.4)
  :width 1024
  )
