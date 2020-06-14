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

(def InventorySack
  (s/struct-def
   :width           :int32
   :height          :int32
   :inventory-items (s/array TransferStashItem)))

(def Block18
  (s/struct-def
   :version   :int32
   :unknown   :int32-
   :mod       (s/string :ascii)
   :expansion-status :byte
   :stash     (s/array
               (struct-block {0 InventorySack}))))

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
