(ns gd-edit.inventory
  "This module deals with character inventory.
  At the moment, it mostly deals with calculating inventory layout.

  The fit-new-item function, in particular is useful for attempting to add a new item
  into a sack.
  "
  (:require [gd-edit.globals :as globals]
            [gd-edit.arc-reader :as arc]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.utils :as u]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            ))

(defn pixel-dims->slot-dims
  [dims]

  (assoc dims
         :width (/ (:width dims) 32)
         :height (/ (:height dims) 32)))

(defn inv-slot-dims
  [num]

  (let [grid-info (if (= num 0)
                    (@globals/db-index "records/ui/character/characterinventory/inventory_grid0.dbr")
                    (@globals/db-index "records/ui/character/characterinventory/inventory_grid1.dbr"))]
    (-> {:width (grid-info "inventoryXSize")
         :height (grid-info "inventoryYSize")}
        (pixel-dims->slot-dims))))

(defn- strip-first-component
  [path]

  (when path
      (->> (u/filepath->components path)
           (drop 1)
           (str/join "/"))))

(defn- stride
  [inv-idx]

  (:width (inv-slot-dims inv-idx)))


(defn dlc-texture-file
  "Returns the dlc db file as a java.lang.File object or nil"
  []
  (when-let [dlc-dir (io/file (dirs/get-game-dir) "gdx1")]
    (let [dlc-db-file (io/file dlc-dir "resources" "Items.arc")]
      (when (.exists dlc-db-file)
        dlc-db-file))))

(defn- get-texture-files
  []
  (filterv some?
           [(dirs/get-resources-file "Items.arc")
            (dlc-texture-file)]))

(defn make-dims-lookup-fn
  "Given a texture file path, return a function that can be used to look up dimensions of
  textures in the file.

  Returns: (string) -> (texture dimension hashmap)"
  [filepath]

  (arc/make-load-tex-fn filepath
                        #(-> (arc/read-tex-header %)
                             (arc/texture-dimensions)
                             (pixel-dims->slot-dims))))

(def texture-dim-fns nil)

(defn texture-slot-dims-data-loaded?
  []

  (some? texture-dim-fns))

(defn bind-texture-slot-dims-fn
  "Binds the texture-slot-dims functions when called.
  This is done because:
  1. The function depends on the settings file having loaded
  2. Depends on the :game-dir field of the settings file

  This function makes it possible to delay binding the function into the module.
  It also makes it easy to rebind/recreate the function when the dependencies change.
  "
  []

  (def texture-dim-fns
    (->> (get-texture-files)
         (map make-dims-lookup-fn))))

(defn lazy-load-texture-slot-dims
  []
  (if-not (texture-slot-dims-data-loaded?)
    (bind-texture-slot-dims-fn)))

(defn texture-slot-dims
  [texture-name]

  (some #(% texture-name) texture-dim-fns))


;; The texture-slot-dims functions is bound/rebound whenever the :game-dir setting changes.
(remove-watch globals/settings ::texture-slot-dims)
(add-watch globals/settings ::texture-slot-dims
           (fn [key settings old-state new-state]
             (if (not= (old-state :game-dir) (new-state :game-dir))
               (bind-texture-slot-dims-fn))))

(defn- coord->slot-id
  [{:keys [X Y]} stride]

  (+ X (* Y stride)))

(defn- slot-id->coord
  [slot-id stride]

  {:X (mod slot-id stride) :Y (quot slot-id stride)})


(defn- rect->slot-ids
  [{:keys [X Y width height]} stride]

  (when (and (number? width) (number? height))
    ;; Given the width and height,
    ;; generate all possible x & y offsets
    (for [x-offset (range width)
          y-offset (range height)]

      ;; Combine the origin with the offset to get a coordinate for the
      ;; slot we're examining, then the coordinates into the slot-id
      (coord->slot-id
       {:X (+ X x-offset) :Y (+ Y y-offset)}
       stride))))

(defn- item->bitmap-name
  [item]

  (or
   (get item "bitmap")
   (get item "relicBitmap")
   (get item "artifactBitmap")
   (get item "artifactFormulaBitmapName")
   (get item "emptyBitmap")))

(defn items->dims
  [items]

  (lazy-load-texture-slot-dims)

  (->> items
       (map :basename)
       (map @globals/db-index)
       (map item->bitmap-name)
       (map strip-first-component)
       (map texture-slot-dims)))

(defn item->dims
  [item]

  (lazy-load-texture-slot-dims)

  (->> item
       (:basename)
       (@globals/db-index)
       (item->bitmap-name)
       (strip-first-component)
       (texture-slot-dims)))

(defn items->rects
  [items]

  (map merge
       (map #(select-keys % [:basename :X :Y]) items)
       (items->dims items)))

(defn occupied-slots
  [inv-idx inv-items]

  (let [;; Determine the total size/dimensions of the inventory
        inv-dims (inv-slot-dims inv-idx)

        ;; The given inventory items should have X, Y in slot coordinates already.
        ;; Determine the width and height in terms of slots
        item-rects (items->rects inv-items)]

    ;; Generate a list of slot ids that are occupied
    (->> item-rects
         (mapcat #(rect->slot-ids % (:width inv-dims))))))

(defn empty-slots
  "Return a list of empty slot ids.
  inv-idx: index of the inventory/sack in the character
           This is used to determine the total size of the inventory
  inv-items: list of items in the inventory "
  [inv-idx inv-items]

  (let [;; Generate a list of slot ids that are occupied
        occupied-slots (set (occupied-slots inv-idx inv-items))

        ;; Generate a list of number that represent all slots in the inventory/sack
        inv-dims (inv-slot-dims inv-idx)
        inv-slots (set (range (* (:width inv-dims) (:height inv-dims))))]

    (set/difference inv-slots occupied-slots)))

(defn fit-new-item
  "Given an inventory and an new item, return the coordinate in the sack
  where the given item can be placed"
  [inv-idx inv-items new-item]

  (let [new-item-dims (item->dims new-item)
        empty-slots (set (empty-slots inv-idx inv-items))
        stride (stride inv-idx)]


    ;; Look for some empty slot such that...
    (if-let [target-slot
             (some (fn [candidate-slot]
                     (let [candidate-rect (merge new-item (slot-id->coord candidate-slot stride) new-item-dims)
                           required-slots (set (rect->slot-ids candidate-rect stride))]

                       ;; All slots required when placing the item there are empty/available
                       (if (= (set/intersection required-slots empty-slots) required-slots)
                         candidate-slot)
                       ))
                   (sort empty-slots))]

      (slot-id->coord target-slot stride))))
