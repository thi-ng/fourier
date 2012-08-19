(ns fourier.core
  (:require
    [clojure.java.io :as io])
  (:import
    [javax.sound.sampled AudioFormat]
    [javazoom.jl.decoder Bitstream Decoder Header SampleBuffer]
    [javazoom.jl.player Player]
    [javazoom.spi.mpeg.sampled.file MpegAudioFileReader]))

(defn mp3-file-props
  [f]
  (let[props (-> (MpegAudioFileReader.) (.getAudioFileFormat f) (.properties))]
    (zipmap (map keyword (keys props)) (vals props))))

(defn play-mp3
  [path & opts]
  (let [stream (io/input-stream path)
        player (Player. stream)
        {:keys[len async] :or {len 0x7fffffff async true}} (apply hash-map opts)]
    (if async
      (.start (Thread. #(doto player (.play len) (.close))))
      (doto player (.play len) (.close)))))

(defn mp3-frames
  ([path]
    (let [stream (io/input-stream path)
          bits (Bitstream. stream)
          decoder (Decoder.)]
      (mp3-frames bits decoder)))
  ([bits decoder]
    (lazy-seq
      (if-let [header (.readFrame bits)]
        (let[^SampleBuffer buf (.decodeFrame decoder header bits)
             len (.getBufferLength buf)
             frame (take len (.getBuffer buf))]
          (.closeFrame bits)
          (cons frame (mp3-frames bits decoder)))
        (.close bits)))))

(defn mp3-samples
  ([frames] (mp3-samples (first frames) (rest frames)))
  ([curr frames]
    (lazy-seq
      (if-let [f (first curr)]
        (cons (/ f 32768.0) (mp3-samples (rest curr) frames))
        (when-let [n (first frames)]
          (mp3-samples n (rest frames)))))))
