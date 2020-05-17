(ns gd-edit.io.core)

(defn ba->boolean
  ([ba]
   (ba->boolean ba 0))
  ([ba offset]
   (if (zero? (bit-and 0xff (nth ba (+ offset 0))))
     false
     true)))

(defn ba->short
  ([ba]
   (ba->short ba 0))
  ([ba offset]
   (.shortValue
    (bit-or
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 0))) 0)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 1))) 8)))))

(defn ba->int
  ([ba]
   (ba->int ba 0))
  ([ba offset]
   (.intValue (bit-or
               (bit-shift-left (bit-and 0xff (nth ba (+ offset 0))) 0)
               (bit-shift-left (bit-and 0xff (nth ba (+ offset 1))) 8)
               (bit-shift-left (bit-and 0xff (nth ba (+ offset 2))) 16)
               (bit-shift-left (bit-and 0xff (nth ba (+ offset 3))) 24)))))

(defn ba->long
  ([ba]
   (ba->long ba 0))
  ([ba offset]
   (bit-or
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 0))) 0)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 1))) 8)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 2))) 16)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 3))) 24)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 4))) 32)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 5))) 40)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 6))) 48)
    (bit-shift-left (bit-and 0xff (nth ba (+ offset 7))) 56))))

(defn ba->float
  ([ba]
   (ba->float ba 0))
  ([ba offset]
   (Float/intBitsToFloat
    (bit-or
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 0))) 0)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 1))) 8)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 2))) 16)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 3))) 24)))))

(defn ba->double
  ([ba]
   (ba->double ba 0))
  ([ba offset]
   (Double/longBitsToDouble
    (bit-or
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 0))) 0)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 1))) 8)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 2))) 16)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 3))) 24)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 4))) 32)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 5))) 40)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 6))) 48)
     (bit-shift-left (bit-and 0xff (nth ba (+ offset 7))) 56)))))

(defprotocol DataReader
  (get-byte [this])
  (get-byte-array [this byte-arr offset read-count])
  (get-int16 [this])
  (get-int32 [this])
  (get-int64 [this])
  (get-float [this])
  (get-double [this])
  (get-boolean [this]))

(defprotocol Positionable
  (get-position [this])
  (set-position [this pos])
  (get-remaining [this]))

(defn has-remaining?
  [positionable]
  (not (zero? (get-remaining positionable))))

(defn get-bytes
  [reader count]
  (let [arr (byte-array count)]
    (get-byte-array reader arr 0 count)
    arr))

(extend-type java.nio.ByteBuffer
  DataReader
  (get-byte [this]
            (.get this))

  (get-byte-array [this byte-arr offset read-count]
    (.get this byte-arr offset read-count))

  (get-int16 [this]
             (.getShort this))
  (get-int32 [this]
             (.getInt this))
  (get-int64 [this]
             (.getLong this))
  (get-float [this]
             (.getFloat this))
  (get-double [this]
              (.getDouble this))
  (get-boolean [this]
               (.get this))

  Positionable
  (get-position [this]
                (.position this))
  (set-position [this pos]
                (.position this pos))
  (get-remaining [this]
                 (.remaining this)))

(deftype ReaderSlice
    [reader slice-min slice-max]

  DataReader
  (get-byte [this]
    (get-byte (.reader this)))

  (get-byte-array [this byte-arr offset read-count]
    (get-byte-array (.reader this) byte-arr offset read-count))

  (get-int16 [this]
    (get-int16 (.reader this)))

  (get-int32 [this]
    (get-int32 (.reader this)))

  (get-int64 [this]
    (get-int64 (.reader this)))

  (get-float [this]
    (get-float (.reader this)))

  (get-double [this]
    (get-double (.reader this)))

  (get-boolean [this]
    (get-boolean (.reader this)))

  Positionable
  (get-position [this]
    (- (get-position (.reader this))
       (.slice-min this)))

  (set-position [this pos]
    (set-position (.reader this) (Math/min (+ pos (.slice-min this))
                                           (.slice-max this))))

  (get-remaining [this]
    (- (.slice-max this) (get-position this))))

(defn reader-slice
  [reader slice-min slice-max]
  (let [slice (->ReaderSlice reader slice-min slice-max)]
    (set-position slice 0)
    slice))
