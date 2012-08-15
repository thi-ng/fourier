(ns fourier.analysis
  (:import
    [edu.emory.mathcs.jtransforms.fft FloatFFT_1D]))

(def ^:const PI Math/PI)
(def ^:const TWO_PI (* 2 Math/PI))

(defn bandwidth
  "Computes the bandwidth of single bin for the given sample rate and window size."
  [windowsize rate] (double (/ rate windowsize)))

(def octaves
  "Computes the number of octaves for the given sample rate, window size and
  desired minimum bandwidth. :bandwidth defaults to result of bandwidth fn."
  (memoize
    (fn [& {windowsize :window minbw :bandwidth rate :rate
            :or {windowsize 1024 rate 44100}}]
      (let [minbw (if (nil? minbw) (bandwidth windowsize rate) minbw)]
        (loop [n rate octaves 1]
          (let [n (/ n 2.0)]
            (if (<= n minbw)
              octaves
              (recur n (inc octaves)))))))))

(def octave-bands
  (memoize
    (fn [rate octaves sub-bands]
      (let[nyquist (/ rate 2.0)]
        (apply
          concat
          (map (fn[o]
                 (let[invo (- octaves o)
                      low (if (zero? o) 0.0 (/ nyquist (Math/pow 2.0 invo)))
                      high (/ nyquist (Math/pow 2.0 (dec invo)))
                      bw (/ (- high low) sub-bands)]
                   (for[b (range sub-bands)]
                     {:low (+ low (* b bw)) :high (+ low (* (inc b) bw)) :bandwidth bw})))
               (range octaves)))))))

(defn freq->bin
  [freq windowsize rate bandwidth]
  (let [bw (/ bandwidth 2.0)]
    (cond (< freq bw) 0
          (> freq (- (/ rate 2.0) bw)) (dec (int (/ windowsize 2)))
          :default (int (Math/round (* windowsize (/ freq rate)))))))
        
(defn band-average
  [windowsize rate spectrum low high bandwidth]
  (let [l (freq->bin low windowsize rate bandwidth)
        h (freq->bin high windowsize rate bandwidth)
        bw (inc (- h l))]
    (/ (reduce + (take bw (drop l spectrum))) bw)))

(defn log-amp [amp logbase]
  (let[log (Math/log logbase)]
    (fn [x] (* (/ (Math/log (inc x)) log) amp))))

(defn linear-amp [amp]
  (fn [x] (* x amp)))

(defn equalize
  [amp]
  (memoize
    (fn[n]
      (let [inv (/ 1.0 n)]
        (map (fn[x] (* amp (Math/log (* inv (- n x))))) (range n))))))

(def normalize
  (memoize (fn[n] (repeat n (/ 1.0 n)))))
  
(defn fft->spectrum
  [fft]
  (loop[spec [] fft fft]
    (let[[r i] (take 2 fft)]
      (if (nil? r) spec
        (recur (conj spec (Math/sqrt (+ (* r r) (* i i))))
               (drop 2 fft))))))

(def rect (memoize (fn[windowsize] (repeat windowsize 1.0))))

(def hanning
  (memoize
    (fn[windowsize]
      (let [n (/ TWO_PI (dec windowsize))] 
        (map #(* 0.5 (- 1.0 (Math/cos (* % n)))) (range windowsize))))))

(def hamming
  (memoize
    (fn[windowsize]
      (let [n (/ TWO_PI (dec windowsize))] 
        (map #(- 0.54 (* 0.46 (Math/cos (* % n)))) (range windowsize))))))

(def lanczos
  (memoize
    (fn[windowsize]
      (let [n (/ 2.0 (dec windowsize))] 
        (map #(let[x (* PI (- (* n %) 1.0))] (/ (Math/sin x) x)) (range windowsize))))))

(defn gauss [theta]
  (memoize
    (fn[windowsize]
      (let [n (/ (dec windowsize) 2.0)
            o (/ 1.0 (* theta n))] 
        (map #(Math/exp (* -0.5 (Math/pow (* (- % n) o) 2))) (range windowsize))))))

(defn apply-window
  [window-fn windowsize samples]
  (pmap * samples (window-fn windowsize)))


(defn spectrum-seq
  ([window-fn windowsize samples] (spectrum-seq (FloatFFT_1D. windowsize) window-fn windowsize samples))
  ([fft window-fn windowsize samples]
    (lazy-seq
      (let [ws (take windowsize samples)
            n (count ws)]
        (if (pos? n)
          (let [buf (float-array
                      (apply-window window-fn windowsize
                        (if (< n windowsize)
                          (concat ws (repeat (- windowsize n) 0.0))
                          ws)))]
            (.realForward fft buf)
            (cons (fft->spectrum buf)
                  (spectrum-seq fft window-fn windowsize (drop windowsize samples)))))))))

(defn spectrum-octaves
  [windowsize rate bwidth octaves bands-per-octave spectrum]
  (map
    (fn[{:keys[low high]}]
      (band-average windowsize rate spectrum low high bwidth))
    (octave-bands rate octaves bands-per-octave)))

