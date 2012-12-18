(ns fourier.core
  (:require
    [clojure.java.io :as io])
  (:import
    [javax.sound.sampled AudioFormat]
    [javazoom.jl.decoder Bitstream Decoder Header SampleBuffer]
    [javazoom.jl.player Player]
    [javazoom.spi.mpeg.sampled.file MpegAudioFileReader]))

(def ^:const ^:double INV16BIT (/ 1.0 32768.0))

(defn mp3-file-props
  "Returns map of mp3 properties for the given file."
  [f]
  (let[props (-> (MpegAudioFileReader.) (.getAudioFileFormat f) (.properties))]
    (zipmap (map keyword (keys props)) (vals props))))

(defn play-mp3
  "Creates a player instance for the given mp3 file and starts playing it.
  Returns player. Options:

      :async - default, starts playback in separate thread, returns immediatedly
      :len - limit playback to given number of mp3 frames"
  [path & {:keys [len async] :or {len 0x7fffffff async true}}]
  (let [stream (io/input-stream path)
        player (Player. stream)
        play-fn #(doto player (.play len) (.close))]
    (if async
      (.start (Thread. play-fn))
      (play-fn))
    player))

(defn player-position
  "Returns current player position."
  [^Player player]
  (.getPosition player))

(defn player-done?
  "Returns true, if player has finished."
  [^Player player]
  (.isComplete player))

(defn mp3-frames
  "Returns lazy-seq of mp3 frames for the given mp3 file."
  ([path]
    (mp3-frames (Bitstream. (io/input-stream path)) (Decoder.)))
  ([^Bitstream bits ^Decoder decoder]
    (lazy-seq
      (if-let [header (.readFrame bits)]
        (let[buf (.decodeFrame decoder header bits)
             len (.getBufferLength ^SampleBuffer buf)
             frame (take len (.getBuffer ^SampleBuffer buf))]
          (.closeFrame bits)
          (cons frame (mp3-frames bits decoder)))
        (.close bits)))))

(defn mp3-samples
  "Returns lazy-seq of floating point samples from the given mp3-frames seq.
  Sequence will contain interleaved left-right values for stereo streams.
  Sample values are in the range -1.0 .. +1.0"
  ([frames] (mp3-samples (first frames) (rest frames)))
  ([curr frames]
    (lazy-seq
      (if-let [^double f (first curr)]
        (cons (* f INV16BIT) (mp3-samples (rest curr) frames))
        (when-let [n (first frames)]
          (mp3-samples n (rest frames)))))))
