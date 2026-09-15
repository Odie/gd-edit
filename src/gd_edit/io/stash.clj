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

(defn struct-block
  [block-specs]
  (with-meta block-specs
    {:struct/type :block}))

(defmethod s/read-spec :block
  [spec bb _ context]
  (gdc/read-block bb context spec))

(defmethod s/write-spec :block
  [spec bb data context]
  (gdc/write-block bb data context spec))

(def TransferStashItem
  (into gdc/Item
        (s/struct-def
         :X :float
         :Y :float)))

(def TransferStashItemV2
  (into gdc/ItemV2
        (s/struct-def
         :X :float
         :Y :float)))

(def InventorySack
  (s/struct-def
   :width           :int32
   :height          :int32
   :inventory-items (s/array TransferStashItem)))

;; Fangs of Asterkarn (GD 1.3.x) added five trailing u32 fields to every stash
;; page whose block-18 version >= ITEM_V2_BLOCK_VERSION (11).
(def InventorySackV2
  (s/struct-def
   :width           :int32
   :height          :int32
   :inventory-items (s/array TransferStashItemV2)
   :page-unk1       :int32
   :page-unk2       :int32
   :page-unk3       :int32
   :page-unk4       :int32
   :page-unk5       :int32))

(defn read-block18
  [^ByteBuffer bb context]
  (let [version (gdc/read-int! bb context)
        unknown (read-int-no-update bb context)
        mod (gdc/read-string! bb context)
        expansion-status (gdc/read-byte! bb context)
        page-spec (if (>= version 11) InventorySackV2 InventorySack)
        stash (reduce (fn [accum _]
                        (conj accum (gdc/read-block bb context {0 page-spec})))
                      []
                      (range (gdc/read-int! bb context)))]
    {:version version
     :unknown unknown
     :mod mod
     :expansion-status expansion-status
     :stash stash}))

(defn write-block18
  [^ByteBuffer bb block context]
  (let [version (:version block)
        page-spec (if (>= version 11) InventorySackV2 InventorySack)]
    (gdc/write-int! bb version context)
    (write-int-no-update bb (:unknown block) context)
    (gdc/write-string! bb (:mod block) context)
    (gdc/write-byte! bb (:expansion-status block) context)
    (gdc/write-int! bb (count (:stash block)) context)
    (doseq [page (:stash block)]
      (gdc/write-block bb page context {0 page-spec}))))

(def Block18
  (s/struct-def
   {:struct/read read-block18
    :struct/write write-block18}))

;; The transfer stash file seem to have a
(defn make-enc-context
  [& rest]
  (let [context (apply gdc/make-enc-context rest)]
    (swap! context update-in [:rw-fns] assoc
           :int32- [:int32- 4 read-int-no-update write-int-no-update])
    context))

(defn load-stash-file
  [filepath]

  (let [bb ^ByteBuffer (u/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) 1431655765)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table)

        magic-number (gdc/read-int! bb enc-context)]
    (when (not= magic-number 2)
      (throw (Throwable. "I don't understand this stash format!")))

    (merge
      (gdc/read-block bb enc-context {18 Block18})
      {:meta-stash-seed seed
       :meta-stash-loaded-from filepath})))

(defn write-stash-file
  [stash savepath]
  (let [bb (ByteBuffer/allocate (* 10 1024 1024))
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (:meta-stash-seed stash)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table {:direction :write})]

    (.putInt bb (bit-xor seed 1431655765))  ;; enc key
    (gdc/write-int! bb 2 enc-context)       ;; magic number

    (gdc/write-block bb stash enc-context {18 Block18})

    ;; Dump everything to file
    (.flip bb)
    (gdc/write-to-file bb savepath)))


(comment
  (load-stash-file (dirs/get-transfer-stash))
  (with-bindings {#'gd-edit.io.gdc/*debug* true
                  #'gd-edit.structure/*debug* true}
    (load-stash-file "/Users/Odie/tmp/gd-stash.gst")
    )

  (load-stash-file "/Volumes/Untitled/Users/Odie/Documents/my games/Grim Dawn/save/transfer.gst")

  (let [stash (load-stash-file "/Volumes/Untitled/Users/Odie/Documents/my games/Grim Dawn/save/transfer.gst")]
    (write-stash-file stash "/Users/Odie/tmp/out.gst"))

  (load-stash-file "/Users/Odie/tmp/out.gst")

  )
