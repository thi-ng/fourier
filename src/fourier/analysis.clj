(ns fourier.analysis
  (:import
    [edu.emory.mathcs.jtransforms.fft FloatFFT_1D]))

(def ^:const PI Math/PI)
(def ^:const TWO_PI (* 2 Math/PI))

(defn bandwidth
  "Computes the bandwidth of single bin for the given
  window size and sample rate."
  ^double [windowsize rate] (/ (double rate) windowsize))

(def octaves
  "Computes the number of octaves for the given :rate, :windowsize and
  desired minimum :bandwidth. The latter defaults to result of
  (bandwidth windowsize rate)."
  (memoize
    (fn [& {windowsize :windowsize minbw :bandwidth rate :rate
            :or {windowsize 1024 rate 44100}}]
      (let [minbw (if (nil? minbw) (bandwidth windowsize rate) minbw)]
        (loop [n rate octaves 1]
          (let [n (/ n 2.0)]
            (if (<= n minbw)
              octaves
              (recur n (inc octaves)))))))))

(def octave-bands
  "Computes & memoizes a lazy seq of frequency bands for the given sample rate,
  number of octaves and bands per octave. Each band is a map with 4 keys:
  :low lowest freq in band
  :high highest freq in band
  :center center freq of band
  :width band width"
  (memoize
    (fn [rate octaves sub-bands]
      (let[nyquist (/ rate 2.0)]
        (apply
          concat
          (map (fn[o]
                 (let[invo (- octaves o)
                      low (if (zero? o) 0.0 (/ nyquist (Math/pow 2.0 invo)))
                      high (/ nyquist (Math/pow 2.0 (dec invo)))
                      bw (/ (- high low) sub-bands)
                      bw2 (* 0.5 bw)]
                   (for[b (range sub-bands)]
                     (let[l (+ low (* b bw))]
                       {:low l :high (+ l bw) :center (+ l bw2) :width bw}))))
               (range octaves)))))))

(defn freq->band
  ([freq bands] (freq->band freq bands 0))
  ([freq bands id]
    (if-let[{:keys[low high]} (first bands)]
      (if (<= low freq high) id
        (recur freq (rest bands) (inc id))))))

(defn band->freq
  [id bands]
  (:center (nth bands id)))

(defn freq->bin
  [freq windowsize rate bandwidth]
  (let [bw (/ bandwidth 2.0)]
    (cond (< freq bw) 0
          (> freq (- (/ rate 2.0) bw)) (dec (int (/ windowsize 2)))
          :default (int (Math/round (* windowsize (/ (double freq) rate)))))))

(defn bin->freq
  [bin windowsize rate bandwidth]
  (cond (= bin 0) (* 0.25 bandwidth)
        (>= bin (/ windowsize 2)) (- (/ rate 2) (* 0.25 bandwidth))
        :default (* bin bandwidth)))

(defn band-average
  [spectrum windowsize rate low high bandwidth]
  (let [l (freq->bin low windowsize rate bandwidth)
        h (freq->bin high windowsize rate bandwidth)
        bins (inc (- h l))]
    (/ (apply + (take bins (drop l spectrum))) bins)))

(defn ^double linear->db
  ([^double x] (* 10.0 (Math/log10 x)))
  ([^double offset ^double x] (+ (* 10.0 (Math/log10 x)) offset)))

(defn ^double db->linear
  ([^double x] (Math/pow 10 (* 0.1 x)))
  ([^double offset ^double x] (Math/pow 10 (* 0.1 (- x offset)))))

(defn ^double linear-delta->db
  [^double x]
  (if (pos? x)
    (Math/abs (linear->db x))
    (if (neg? x)
      (linear->db (Math/abs x))
      0.0)))

(defn equalize
  [amp]
  (memoize
    (fn[n]
      (let [inv (/ 1.0 n)]
        (map (fn[x] (* amp (Math/log (* inv (- n x))))) (range n))))))

(def normalize
  (memoize (fn[n] (repeat n (/ 1.0 n)))))

(defn magnitude
  [fft windowsize]
  (loop[spec [] fft fft n (/ 1.0 windowsize)]
    (let[[r i] (take 2 fft)]
      (if (nil? r) spec
        (recur (conj spec (* n (+ (* r r) (* i i))))
               (drop 2 fft)
               n)))))

(def dirichlet
  (memoize
    (fn[windowsize] (repeat windowsize 1.0))))

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

(defn gauss [sigma]
  (memoize
    (fn[windowsize]
      (let [n (/ (dec windowsize) 2.0)
            o (/ 1.0 (* sigma n))]
        (map #(Math/exp (* -0.5 (Math/pow (* (- % n) o) 2.0))) (range windowsize))))))

(defn amplify
  [coll amp]
  (map * coll amp))

(defn map-series
  [f & colls]
  (apply map (fn[& slices] (apply map f slices)) colls))

(defn threshold
  [t minv x] (if (>= x t) x minv))

(defn- spectrum-seq*
  ([^FloatFFT_1D fft window windowsize pipeline samples]
    (lazy-seq
      (let [slice (take windowsize samples)
            n (count slice)]
        (if (pos? n)
          (let [buf (float-array windowsize (amplify slice window))
                _ (.realForward fft buf)
                spec (pipeline (magnitude buf windowsize))]
            (cons spec
                  (spectrum-seq* fft window windowsize pipeline (drop windowsize samples)))))))))

(defn spectrum-seq
  [& {:keys[window-fn windowsize pipeline samples]
    :or {window-fn dirichlet
         windowsize 1024
         pipeline identity}}]
  (spectrum-seq*
    (FloatFFT_1D. windowsize)
    (window-fn windowsize)
    windowsize pipeline samples))

(defn spectrum-bands
  [spectrum windowsize rate bands]
  (let[bw (bandwidth windowsize rate)]
    (map
      (fn[{:keys[low high]}] (band-average spectrum windowsize rate low high bw))
      bands)))

(defn derivative
  ([slices] (derivative (repeat Double/NEGATIVE_INFINITY) slices))
  ([prev more]
    (lazy-seq
      (let[curr (first more)]
        (if (and prev curr)
          (cons (pmap (fn[c p]
                        (if (Double/isInfinite c)
                          (if (Double/isInfinite p)
                            c
                            -100)
                          (if (Double/isInfinite p)
                            100
                            (- c p))))
                        curr prev)
                (derivative curr (rest more))))))))

(defn normalize-samples
  "Consumes entire seq and returns a lazy-seq of
  normalized values (-1.0 .. +1.0) of `samples`."
  [samples]
  (let [normf (/ 1.0 (reduce (fn[^double p ^double x] (max p (if (pos? x) x (- x)))) 0.0 samples))]
    (map #(* (double normf) (double %)) samples)))
