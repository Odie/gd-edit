(ns gd-edit.inventory
  "This module deals with character inventory.
  At the moment, it mostly deals with calculating inventory layout.

  The fit-new-item function, in particular is useful for attempting to add a new item
  into a sack."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.globals :as globals]
            [gd-edit.io.arc :as arc]
            [gd-edit.utils :as u]
            [gd-edit.db-utils :as dbu]))

(defn- character-inventory
  [n]
  (get-in @globals/character [:inventory-sacks n :inventory-items]))

(defn pixel-dims->slot-dims
  [dims]

  (assoc dims
         :width (/ (:width dims) 32)
         :height (/ (:height dims) 32)))

(defn- strip-first-component
  [path]

  (when path
    (->> (u/filepath->components path)
         (drop 1)
         (str/join "/"))))

(defn- strip-last-component
  [path]

  (when path
    (->> (u/filepath->components path)
         (drop-last)
         (str/join "/"))))

(defn inv-slot-dims
  "Return the expected dimensions of the inventory/stash/transfer stash"
  [path-to-items]

  (cond
    (= (first path-to-items) :inventory-sacks)
    (let
        [inv-info (if (= (second path-to-items) 0)
                    (dbu/record-by-name "records/ui/character/characterinventory/inventory_grid0.dbr")
                    (dbu/record-by-name "records/ui/character/characterinventory/inventory_grid1.dbr"))]
      (pixel-dims->slot-dims
       {:width (inv-info "inventoryXSize")
        :height (inv-info "inventoryYSize")}))
    (or
     (= (first path-to-items) :stashes)
     (= (first path-to-items) :transfer-stashes))
    (-> (->> path-to-items
             drop-last
             (get-in @globals/character))
        (select-keys [:width :height]))))


(defn- stride
  [inv-idx]

  (:width (inv-slot-dims inv-idx)))


(defn make-dims-lookup-fn
  "Given a texture file path, return a function that can be used to look up dimensions of
  textures in the file.

  Returns: (string) -> (texture dimension hashmap)"
  [filepath]

  (arc/make-load-tex-fn filepath
                        #(-> (arc/read-tex-header %)
                             (arc/texture-dimensions)
                             (pixel-dims->slot-dims))))

(def ^:redef texture-dim-fns nil)

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

  (alter-var-root #'texture-dim-fns
                  (constantly
                   (->> (dirs/get-file-and-overrides dirs/texture-file)
                        (map make-dims-lookup-fn)))))

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
           (fn [_ _ old-state new-state]
             (when (not= (old-state :game-dir) (new-state :game-dir))
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
       (map (dbu/db-recordname-index))
       (map item->bitmap-name)
       (map strip-first-component)
       (map texture-slot-dims)))

(defn item->dims
  [item]

  (lazy-load-texture-slot-dims)

  (->> item
       (:basename)
       ((dbu/db-recordname-index))
       (item->bitmap-name)
       (strip-first-component)
       (texture-slot-dims)))

(defn items->rects
  [items]

  (map merge
       (map #(select-keys % [:basename :X :Y]) items)
       (items->dims items)))

(defn occupied-slots
  "Return a list of occupied slot ids.
  inv-idx: index of the inventory/sack in the character.
           This is used to determine the dimensions and the items in the inventory/sack."
  [path-to-items]

  (let [inv-items (get-in @globals/character path-to-items)

        ;; Determine the total size/dimensions of the inventory
        inv-dims (inv-slot-dims path-to-items)

        ;; The given inventory items should have X, Y in slot coordinates already.
        ;; Determine the width and height in terms of slots
        item-rects (items->rects inv-items)]

    ;; Generate a list of slot ids that are occupied
    (->> item-rects
         (mapcat #(rect->slot-ids % (:width inv-dims)))
         (map u/maybe-int))))

(defn empty-slots
  "Return a list of empty slot ids for the given inventory/stash."
  [path-to-items]

  (let [;; Generate a list of slot ids that are occupied
        occupied-slots (set (occupied-slots path-to-items))

        ;; Generate a list of number that represent all slots in the inventory/sack
        inv-dims (inv-slot-dims path-to-items)
        inv-slots (set (range (* (:width inv-dims) (:height inv-dims))))]

    (set/difference inv-slots occupied-slots)))

(defn fit-new-item
  "Given an inventory and an new item, return the coordinate in the sack
  where the given item can be placed"
  [path-to-items new-item]

  (let [new-item-dims (item->dims new-item)
        empty-slots (set (empty-slots path-to-items))
        stride (stride path-to-items)]

    ;; Look for some empty slot such that...
    (if-let [target-slot
             (some (fn [candidate-slot]
                     (let [candidate-rect (merge new-item (slot-id->coord candidate-slot stride) new-item-dims)
                           required-slots (set (rect->slot-ids candidate-rect stride))]

                       ;; Are all slots required to place the item empty/available?
                       (when (= (set/intersection required-slots empty-slots) required-slots)
                         candidate-slot)))
                   (sort empty-slots))]

      (slot-id->coord target-slot stride))))

(comment
  (fit-new-item
   [:transfer-stashes 4 :inventory-items]
   {:var1 0, :stack-count 1, :basename "records/items/gearweapons/swords1h/d002_sword.dbr", :unknown 0, :relic-seed 0, :prefix-name "", :relic-completion-level 0, :seed 949214695, :augment-seed 0, :transmute-name "", :augment-name "", :modifier-name "", :relic-name "", :suffix-name "", :relic-bonus ""})

  (require 'repl)

  (repl/cmd "set tra/4/items 'crystallum'")

  (repl/cmd "write stash")

  )
