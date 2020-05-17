(ns gd-edit.io.arc-record-reader
  (:require [gd-edit.io.arc :as arc]
            [gd-edit.io.core :as io.core]
            [gd-edit.utils :as u]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.nio ByteBuffer]))

(defn target-file-part
  [file-parts target-pos]

  ;; (let [result (reduce (fn [remainder file-part]
  ;;                        (let [size (:decompressed-size file-part)]
  ;;                          (if (>= size remainder)
  ;;                            (reduced file-part)
  ;;                            (- remainder size))))
  ;;                      target-pos
  ;;                      file-parts)]
  ;;   (when (map? result)
  ;;     result))

  ;; While this is certainly faster...
  ;; It's unknown if all the file chunks always have the same decompressed size
  (nth file-parts
       (unchecked-divide-int target-pos (:decompressed-size (first file-parts)))))

(defn file-part-loaded?
  [reader file-part]
  (get-in  @(.state reader) [:part-content (:idx file-part)]))

(defn file-part-content
  [reader file-part]
  (get-in  @(.state reader) [:part-content (:idx file-part)]))

(defn load-file-part
  [reader file-part]
  (let [contents (arc/load-record-file-part (.arc-file-bb reader)
                                            (:arc-header (.arc-headers reader))
                                            (.record-header reader)
                                            (:idx file-part))]
    (swap! (.state reader) assoc-in [:part-content (:idx file-part)] contents)))

(defn load-file-part-by-idx
  [reader file-part-idx]
  (when-let [file-part (nth (.file-parts reader) file-part-idx)]
    (load-file-part reader file-part)))

(defn ensure-file-part-loaded
  [reader file-part]
  (when-not (file-part-loaded? reader file-part)
    (load-file-part reader file-part)))

(defn ensure-file-part-loaded-by-idx
  [reader file-part-idx]
  (when-let [file-part (nth (.file-parts reader) file-part-idx)]
    (ensure-file-part-loaded reader file-part)))

(defn ensure-data-range-loaded
  ([reader pos]
   (ensure-file-part-loaded reader
                            (target-file-part (.file-parts reader) pos)))

  ([reader pos-min pos-max]
   (let [max (target-file-part (.file-parts reader) pos-max)
         min (target-file-part (.file-parts reader) pos-min)]
     (doseq [i (range (:idx min) (inc (:idx max)))]
       (ensure-file-part-loaded-by-idx reader i)))))

(defn file-part-contains-pos?
  [file-part pos]

  (let [offset (:decompressed-offset file-part)
        length (:decompressed-size file-part)]
    (and
     (>= pos offset)
     (<= pos (+ offset length)))))

(defn get-byte-array-
  [this byte-arr ba-offset requested-read-count]
  (let [state @(.state this)
        final-read-cnt (loop [fp (nth (.file-parts this) (:current-file-part state))
                              pos (:position state)
                              src-cursor (- pos (:decompressed-offset fp))
                              remaining requested-read-count
                              dst-cursor ba-offset]

                         ;; Does this file-part contain all of the rest of the data we need?
                         (if (file-part-contains-pos? fp (+ pos remaining))
                           ;; if so, just copy the rest of the data and we're done
                           (do
                             (ensure-file-part-loaded this fp)
                             (System/arraycopy (file-part-content this fp) src-cursor
                                               byte-arr dst-cursor
                                               remaining)
                             (+ dst-cursor remaining))

                           ;; Otherwise, copy as much data from this file-part as we can
                           (let [read-cnt (- (:decompressed-size fp) src-cursor)
                                 _ (ensure-file-part-loaded this fp)
                                 _ (System/arraycopy (file-part-content this fp) src-cursor
                                                     byte-arr dst-cursor
                                                     read-cnt)]

                             ;; We didn't copy as much data as requested...
                             ;; Is there more data available to be copied?
                             (if-let [next-fp (nth (.file-parts this) (inc (:idx fp)) nil)]
                               ;; If so, setup the loop to copy more data from the next file-part
                               (recur next-fp
                                      (+ pos read-cnt)
                                      0
                                      (- remaining read-cnt)
                                      (+ dst-cursor read-cnt))

                               ;; Otherwise, report bytes copied
                               ;; This would be short of the actual bytes requested
                               (+ dst-cursor read-cnt)))))

        new-pos (+ (:position state) final-read-cnt)]

    (swap! (.state this) assoc
           :position new-pos
           :current-file-part (:idx (target-file-part (.file-parts this) new-pos)))
    final-read-cnt))

(defn set-position-
  [this pos]
  (when-let [file-part (target-file-part (.file-parts this) pos)]
    (swap! (.state this) assoc :position pos)
    (swap! (.state this) assoc :current-file-part (:idx file-part))))

(defn get-byte-
  [this]
  (let [state @(.state this)
        pos (:position state)
        cur-fp (nth (.file-parts this) (:current-file-part state))
        idx (- pos (:decompressed-offset cur-fp))
        _ (ensure-file-part-loaded this cur-fp)
        content (nth (file-part-content this cur-fp) idx)]

    (swap! (.state this) update :position inc)
    (swap! (.state this) assoc :current-file-part (:idx (target-file-part (.file-parts this) pos)))

    content))

(defn get-remaining-
  [this]
  (let [last-fp (last (.file-parts this))
        pos (io.core/get-position this)]
    ;; Find the total length of this file
    (- (+ (:decompressed-offset last-fp)
          (:decompressed-size last-fp))

       ;; Subtract current position
       pos)))

(deftype ArcRecordReader
    [arc-file-bb arc-headers record-header file-parts state]

  io.core/DataReader
  (get-byte [this]
    (get-byte- this))

  (get-byte-array [this byte-arr offset read-count]
    (get-byte-array- this byte-arr offset read-count))

  (get-int16 [this]
    (io.core/ba->short (io.core/get-bytes this 2)))

  (get-int32 [this]
    (io.core/ba->int (io.core/get-bytes this 4)))

  (get-int64 [this]
    (io.core/ba->long (io.core/get-bytes this 8)))

  (get-float [this]
    (io.core/ba->float (io.core/get-bytes this 4)))

  (get-double [this]
    (io.core/ba->double (io.core/get-bytes this 8)))

  (get-boolean [this]
    (let [result (io.core/get-byte this)]
      (if (zero? result)
        false
        true)))

  io.core/Positionable
  (get-position [this]
    (:position @state))
  (set-position [this pos]
    (set-position- this pos))
  (get-remaining [this]
    (get-remaining- this)))

(defn arc-record-reader
  [filepath recordname]
  (let [bb (u/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)
        ;; Grab top level headers for the arc file
        arc-headers (arc/load-arc-headers bb)]

    ;; Grab the header that represents the targeted file
    (when-let [record-header (some #(when(= recordname (:filename %))
                                      %)
                                   (:record-headers arc-headers))]

      ;; Load up the file-parts descriptors
      (let [file-parts (->>(arc/load-record-file-part-headers bb (:arc-header arc-headers) record-header)
                           u/indexed
                           (map (fn [[idx m]]
                                  (assoc m :idx idx)))
                           (reduce (fn [accum file-part]
                                     (-> accum
                                         (update :file-parts conj (assoc file-part :decompressed-offset (:offset accum)))
                                         (update :offset #(+ (:decompressed-size file-part) %))))
                                   {:offset 0 :file-parts []})
                           :file-parts
                           )

            ;; Return a ArcRecordReader
            reader (->ArcRecordReader bb arc-headers record-header file-parts (atom {}))]
        (io.core/set-position reader 0)
        reader))))

(defn hex
  [x]
  (str/upper-case (Integer/toHexString x)))

(defn byte-array->hex
  [byte-arr]
  (map hex byte-arr))


(comment

  (def r
    (arc-record-reader "/Volumes/Skyrim/SteamLibrary/steamapps/common/Grim Dawn/resources/Levels.arc" "world001.map"))

  (def s
    (io.core/reader-slice r 9205 (+ 9205 297973))
    )

  (io.core/get-byte r)

  (ensure-file-part-loaded-by-idx r 0)

  (ensure-data-range-loaded r 0)

  (ensure-data-range-loaded r 0 262145)

  (io.core/set-position r 0x394BD)

  (def b
    (io.core/get-bytes s 8))

  (u/hexify (seq b))

  )
