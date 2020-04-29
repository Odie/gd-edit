(ns gd-edit.io.stash
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as u]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.game-dirs :as dirs])
  (:import  [java.nio ByteBuffer ByteOrder]))

(def ^:dynamic *debug* false)

(defn read-int-no-update
  [bb context]
  (gdc/decrypt-int (.getInt bb) (:enc-state @context)))

(defn write-int-no-update
  [bb data context]
  (.putInt bb (gdc/encrypt-int data (:enc-state @context))))

(def InventorySack
  (s/struct-def
   :width           :int32
   :height          :int32
   :inventory-items (s/array gdc/InventoryItem)))

(declare read-block18 write-block18)

(def Block18
  (s/struct-def
   ;; :version   :int32
   ;; :unknown   :int32
   ;; :mod       (s/string :ascii)
   ;; :stash     (s/array
   ;;             (block-spec {0 InventorySack}))
   {:struct/read #'read-block18
    :struct/write #'write-block18}))

(defn read-block18
  [^ByteBuffer bb context]

  (let [version (gdc/read-int! bb context)
        unknown (read-int-no-update bb context)
        mod (gdc/read-string! bb context)
        expansion-status (gdc/read-byte! bb context)
        stash-count (gdc/read-int! bb context)
        stash (->>
               (for [idx (range stash-count)]

                 (gdc/read-block bb context {0 InventorySack}))
               (into []))]
    (u/collect-as-map version unknown mod expansion-status stash)))

(defn write-block18
  [^ByteBuffer bb block context]

  (gdc/write-int! bb (:version block) context)
  (write-int-no-update bb 0 context)
  (gdc/write-string! bb (:mod block) context)
  (gdc/write-byte! bb (:expansion-status block) context)
  (gdc/write-int! bb (count (:stash block)) context)
  (doseq [sack (:stash block)]
    (gdc/write-block bb sack context {0 InventorySack})))


(defn load-stash-file
  [filepath]

  (let [bb ^ByteBuffer (u/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) 1431655765)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (gdc/make-enc-context seed enc-table)

        magic-number (gdc/read-int! bb enc-context)]
    (when (not= magic-number 2)
      (throw (Throwable. "I don't understand this stash format!")))

    (merge
      (gdc/read-block bb enc-context {18 Block18})
      {:meta-stash-seed seed
       :meta-stash-loaded-from filepath})))

(defn write-stash-file
  [stash savepath]
  (let [bb (ByteBuffer/allocate (* 512 1024))
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (:meta-stash-seed stash)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (gdc/make-enc-context seed enc-table {:direction :write})]

    (.putInt bb (bit-xor seed 1431655765))  ;; enc key
    (gdc/write-int! bb 2 enc-context)       ;; magic number

    (gdc/write-block bb stash enc-context {18 Block18})
    ;; (gdc/write-int! bb 18 enc-context) ;; Block id
    ;; (write-int-no-update bb 0 enc-context)  ;; Block length

    ;; (s/write-struct Block18 bb stash enc-context) ;; content of the block

    ;; Write the checksum
    (.putInt bb (.intValue (bit-and 0x00000000ffffffff (:enc-state @enc-context))))

    ;; Dump everything to file
    (.flip bb)
    (gdc/write-to-file bb savepath)))


(comment
  (load-stash-file (dirs/get-transfer-stash))
  (load-stash-file "/Users/Odie/tmp/gd-stash.gst")

  (let [stash (load-stash-file "/Volumes/Untitled/Program Files (x86)/Steam/userdata/10000062/219990/remote/save/transfer.gst")]
    (write-stash-file stash "/Users/Odie/tmp/out.gst"))

  (load-stash-file "/Users/Odie/tmp/out.gst")

  )
