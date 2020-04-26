(ns gd-edit.item-summary
  (:require [gd-edit.db-utils :as dbu]
            [clojure.string :as str]
            [gd-edit.equation-eval :as eq]
            [gd-edit.level :as level]
            [com.rpl.specter :as s]
            [gd-edit.utils :as u]
            [clojure.edn :as edn]
            [gd-edit.globals :as globals]
            [taoensso.timbre :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [jansi-clj.core :refer [red green yellow bold black]]))


(defn maybe-int
  [x]
  (if-not (number? x)
    x
    (let [int-x (int x)]
      (if (== int-x x)
        int-x
        x))))

(defn sign
  [number]
  (when number
    (if (> number 0)
      "+")))

(defn number
  [number]
  (when number
    (yellow (str number))))

(defn signed-number
  [number]
  (when number
    (yellow
     (str (sign number) number))))

(defn signed-percentage
  [number]
  (when number
    (yellow
     (str (sign number) number "%"))))

(defn percentage
  [number]
  (when number
    (yellow
     (str number "%"))))

(defn record-class-subtype
  [base-record]
  (let [class (base-record "Class")]
    (str/lower-case (subs class (inc (str/last-index-of class "_"))))))

(defn record-class-display-name
  [class]

  (get {"ArmorJewelry_Amulet" "Amulet"
        "ArmorJewelry_Medal" "Medal"
        "ArmorJewelry_Ring" "Ring"
        "ArmorProtective_Chest" "Chest Armor"
        "ArmorProtective_Feet" "Boots"
        "ArmorProtective_Hands" "Gloves"
        "ArmorProtective_Head" "Helm"
        "ArmorProtective_Legs" "Pants"
        "ArmorProtective_Shoulders" "Shoulders"
        "ArmorProtective_Waist" "Belts"
        "WeaponArmor_Offhand" "Offhand"
        "WeaponArmor_Shield" "Shield"
        "WeaponHunting_Ranged1h" "One-Handed Ranged"
        "WeaponHunting_Ranged2h" "Two-Handed Ranged"
        "WeaponMelee_Axe" "One-Handed Axe"
        "WeaponMelee_Axe2h" "Two-Handed Axe"
        "WeaponMelee_Dagger" "One-Handed Daggar"
        "WeaponMelee_Mace" "One-Handed Mace"
        "WeaponMelee_Mace2h" "Two-Handed Mace"
        "WeaponMelee_Scepter" "One-Handed Scepter"
        "WeaponMelee_Sword" "One-Handed Sword"
        "WeaponMelee_Sword2h" "Two-Handed Sword"}
       class
       "Unknown"))

(defn record-cost
  [record]
  (letfn [(cost [record cost-record]
            (let [subtype (record-class-subtype record)
                  requirements (->> cost-record
                                    (filter #(str/starts-with? (key %) subtype))
                                    (map (fn [[k equation]]
                                           [(str/lower-case (subs k
                                                                  (count subtype)
                                                                  (- (count k) (count "Equation"))))
                                            (Math/round (eq/evaluate equation {"itemLevel" (record "itemLevel")
                                                                               ;; "totalAttCount" 0
                                                                               "totalAttCount" (level/attribute-points-total-at-level (record "levelRequirement"))
                                                                               "itemPrefixCost" 0
                                                                               "itemSuffixCost" 0
                                                                               }))]))
                                    (into {}))]

              [(when-let [strength-req (requirements "strength")]
                 (format "Required Physique: %s" (number strength-req)))
               (when-let [dexterity-req (requirements "dexterity")]
                 (format "Required Cunning: %s" (number dexterity-req)))
               (when-let [spirit-req (requirements "intelligence")]
                 (format "Required Spirit: %s" (number spirit-req)))]))]

    (->> (let [cost-record (or (dbu/record-by-name (record "itemCostName"))
                               (dbu/record-by-name "records/game/itemcostformulas.dbr"))]
           (cost record cost-record))
         (remove nil?))))

(defn augment-skills
  "Given a record, extract the skills it augments"
  [record]
  (->> record
       (filter #(str/starts-with? (key %) "augmentSkill"))
       (group-by (comp last key))
       vals
       (map #(into {} %))
       (s/transform [s/ALL s/MAP-KEYS] (fn [k]
                                         (if (str/includes? k "Name")
                                            :name
                                            :level)))
       (s/transform [s/ALL :name] (fn [record-path]
                                    (dbu/skill-name-from-record (dbu/record-by-name record-path))))))

(defn augment-masteries
  "Given a record, extract the skills it augments"
  [record]
  (->> record
       (filter #(str/starts-with? (key %) "augmentMastery"))
       (group-by (comp last key))
       vals
       (map #(into {} %))
       (s/transform [s/ALL s/MAP-KEYS] (fn [k]
                                         (if (str/includes? k "Name")
                                           :name
                                           :level)))
       (s/transform [s/ALL :name] (fn [record-path]
                                    (dbu/skill-name-from-record (dbu/record-by-name record-path))))))

(defn skill-modifiers
  [record]
  (->> record
       (filter #(or (str/starts-with? (key %) "modifiedSkillName")
                    (str/starts-with? (key %) "modifierSkillName")))
       (group-by #(last (key %)))
       vals
       (map #(into {} %))
       (s/transform [s/ALL s/MAP-KEYS] (fn [k]
                                         (if (str/includes? k "modified")
                                           :name
                                           :modifier)))
       (s/transform [s/ALL :name] #(dbu/skill-name-from-record (dbu/record-by-name %)))
       (s/transform [s/ALL :modifier] #(dbu/record-by-name %))))

(defn maybe-choose-by-skill-level
  "A record field's value may be an array. In that case, a specific value needs to be chosen."
  [record v]
  (if (vector? v)
    (or
     (when-let [skill-level (record "itemSkillLevel")]
       (nth v skill-level))
     (throw (ex-info "Cannot select a value from an array" (u/collect-as-map record v))))
    v))

(defn maybe-resolve-v
  [record v]
  (maybe-choose-by-skill-level record v))

(defn collect-val-range-
  [record val-name]
  (->> (vals (select-keys record [(str val-name "Min") (str val-name "Max")]))
       (map #(maybe-resolve-v record %))))

(defn collect-val-range
  [record val-name]
  (let [v (->> (collect-val-range- record val-name)
             (map maybe-int))]
    (if (and (= (count v) 2)
             (= (first v) (second v)))
      (first v)
      v)))

(defn range-str
  [range]
  (str/join "-" range))

(def effect-types
  [{:name "Physical", :type :immediate, :record-ref "Physical"}
   {:name "Internal Trauma", :type :dot, :record-ref "SlowPhysical"}
   {:name "Fire", :type :immediate, :record-ref "Fire"}
   {:name "Burn" :type :dot, :record-ref "SlowFire"}
   {:name "Cold", :type :immediate, :record-ref "Cold"}
   {:name "Frostburn" :type :dot, :record-ref "SlowCold"}
   {:name "Lightning", :type :immediate, :record-ref "Lightning"}
   {:name "Electrocute" :type :dot, :record-ref "SlowLightning"}
   {:name "Acid", :type :immediate, :record-ref "Acid"}
   {:name "Poison" :type :dot, :record-ref "Poison"}
   {:name "Vitality", :type :immediate, :record-ref "Life"}
   {:name "Vitality Decay" :type :dot, :record-ref "SlowLife"}
   {:name "Pierce", :type :immediate, :record-ref "Pierce"}
   {:name "Bleeding" :type :dot, :record-ref "Bleeding"}
   {:name "Aether", :type :immediate, :record-ref "Aether"}
   {:name "Chaos", :type :immediate, :record-ref "Chaos"}
   {:name "Life Leech", :type :immediate, :record-ref "SlowLifeLeach"}
   {:name "Mana Leech", :type :immediate, :record-ref "SlowLifeLeach"}])

(defn split-camelcase
  [s]
  (->> (clojure.string/split s #"(?=[A-Z])")
       (map clojure.string/lower-case)))

(defn camelcase->keywords
  [s]
  (->> (split-camelcase s)
       (map keyword)))

(defn keywords->camelcase
  [s]
  (let [strs (map #(name %) s)]
    (apply str
           (first strs)
           (map #(str/capitalize %) (rest strs)))))

(def effect-components
  (sort-by #(count (:components %)) >
           [{:name "Poison & Acid", :components #{:defensive :poison}}
            {:name "Physical", :components #{:physical}}
            {:name "Internal Trauma", :components #{:slow :physical}}
            {:name "Fire", :components #{:fire}}
            {:name "Burn" , :components #{:slow :fire}}
            {:name "Cold", , :components #{:cold}}
            {:name "Frostburn" , :components #{:slow :cold}}
            {:name "Lightning", :components #{:lightning}}
            {:name "Electrocute" , :components #{:slow :lightning}}
            {:name "Acid", :components #{:poison}}
            {:name "Poison" :components #{:slow :poison}}
            {:name "Vitality", :components #{:life}}
            {:name "Vitality Decay" :components #{:slow :life}}
            {:name "Pierce", :components #{:pierce}}
            {:name "Bleeding" :components #{:bleeding}}
            {:name "Aether", :components #{:aether}}
            {:name "Chaos", :components #{:chaos}}
            {:name "Life Leech", :components #{:life :leech}}
            {:name "Life Leech", :components #{:life :leach}}
            {:name "Mana Leech", :components #{:mana :leach}}
            {:name "Block", :components #{:block}}
            {:name "Elemental", :components #{:elemental}}
            ]))

(defn effect-by-components
  [components]
  (some #(when (every? (into #{} components) (:components %))
           %)
        effect-components))

(defn effect-name
  [components]
  (:name (effect-by-components components)))

(defn looks-like-effect-keyname
  [keyname]
  (let [components (camelcase->keywords keyname)]
    (when (or (#{"skillCooldownTime"
                 "petBonusName"
                 "weaponDamagePct"
                 "petLimit"
                 "petBurstSpawn"
                 "augmentAllLevel"
                 "waveDistance"
                 "spawnObjectsTimeToLive"
                 "lifeMonitorPercent"
                 "damageAbsorptionPercent"
                 }
               keyname)
              (contains? #{:character :defensive :offensive :skill :retaliation :projectile :spark :conversion} (first components))
              (effect-by-components components))
      true)))

(defn contains?+
  [coll x]

  (cond
    (set? coll)
    (coll x)

    (map? coll)
    (coll x)

    :else
    (some #(= x %) coll)))

(defn val->string-
  [v & opts]
  (let [signed? (contains?+ opts :signed)
        percentage? (contains?+ opts :percentage)
        negative? (contains?+ opts :negative)]
    (cond->> v
      negative?
      (-)
      (and signed? percentage?)
      (signed-percentage)
      (and (not signed?) percentage?)
      (percentage)
      (and signed? (not percentage?))
      (signed-number)
      :else
      (number))))

(defn val-or-range->string
  [v & opts]
  ;; (println v)
  (if (seq? v)
    (if (> (count v) 1)
      (apply format "%s-%s"
             (map #(apply val->string- % opts) v))
      (apply val->string- (first v) opts))
    (apply val->string- v opts)))

(defn lookup-and-resolve-
  [record fieldname]
  (maybe-resolve-v record (record fieldname)))

(defn lookup-and-resolve
  [record fieldname]
  (maybe-int (lookup-and-resolve- record fieldname)))

(defn effect-name
  [record [k v] components effect]

  (cond
    (= k "defensivePoisonDuration")
    "Poison"
    :else
    (:name effect)))

(defn generic-effect-kv->string-
  [record [k v :as kv]]

  ;; (println "generic-effect:" k)
  (when-not (or (str/ends-with? k "Max")
                (str/ends-with? k "Chance"))
    (let [components (camelcase->keywords k)]
      ;; Does this actually look like an effect?
      (if-let [effect (effect-by-components components)]

        ;; Sometimes, `v` should really be a range of values
        (let [v (cond (= (last components) :min)
                      (collect-val-range record (u/subs+ k 0 -3))

                      ;; Sometimes, `v` will be an array.
                      ;; The actual value to use will be determined by the item skill level
                      :else
                      (maybe-resolve-v record v))

              [message val-display-type] (cond
                                           ;; Things marked as "offensive" describe damage
                                           (= (first components) :offensive)
                                           ["%s %s Damage"]

                                           (and (= (first components) :defensive)
                                                (= (take-last 2 components) [:max :resist]))
                                           ["%s Maximum %s Resistance" [:signed :percentage]]

                                           (and (= (first components) :defensive)
                                                (= (last components) :duration))
                                           ["%s Reduction in %s Duration"  [:signed :percentage]]

                                           ;; Things marked as "defensive" describe resistance
                                           (= (first components) :defensive)
                                           ["%s %s Resistance" [:percentage]]

                                           (= (first components) :retaliation)
                                           ["%s %s Retaliation"]

                                           :else
                                           (log/debug "Unrecognized effect" (u/collect-as-map k v)))]
          (when message
            (let [key-base (cond-> components
                             (= (last components) :min)
                             drop-last)

                  chance (->> (str (keywords->camelcase key-base) "Chance")
                              (lookup-and-resolve record)
                              percentage)

                  val-display-type (if (contains?+ components :modifier)
                                     [:signed :percentage]
                                     val-display-type)
                  params (list (apply val-or-range->string v val-display-type) (effect-name record kv components effect))
                  [message params] (if chance
                                     [(str "%s Chance of " message) (conj params chance)]
                                     [message params])
                  ]

              ;; (pprint  message)
              ;; (pprint  params)
              (apply format message params))))))))

(defn slow-effect-kv->string-
  [record [k v :as kv]]

  ;; (println "slow effect:" k)
  (let [components (camelcase->keywords k)
        effect (effect-by-components components)]
    (cond
      (= [:duration :min] (take-last 2 components))
      (let [key-base (keywords->camelcase (drop-last 2 components))
            duration v ;(maybe-resolve-v record v)
            total-dmg (->> key-base
                           (collect-val-range record)
                           (map #(* % duration)))]

        (when-not (empty? total-dmg)
          (if (contains?+ components :retaliation)
            (if-let [chance (lookup-and-resolve record (str key-base "Chance"))]
              (format "%s Chance of %s %s Retaliation Damage over %s seconds"
                      (percentage chance)
                      (range-str total-dmg)
                      (:name effect)
                      duration)
              (format "%s %s Retaliation Damage over %s seconds"
                      (range-str total-dmg)
                      (:name effect)
                      duration))
            (format "%s %s Damage over %s seconds"
                    (range-str total-dmg)
                    (:name effect)
                    duration))))

      (= [:duration :modifier] (take-last 2 components))
      (format "%s %s Duration" (signed-percentage v) (:name effect))

      (str/ends-with? k "Modifier")
      (generic-effect-kv->string- record kv))))

(defn organize-map
  [m]
  (->> (sort-by key m)
       (into (sorted-map))))

(def effect-string-map
  {"augmentAllLevel" ["%s to all Skills" [:signed]]
   "characterAttackSpeedModifier" ["%s Attack Speed" [:signed :percentage]]
   "characterConstitutionModifier" ["%s Constitution" [:percentage]]
   "characterDefensiveAbility" ["%s Defense Ability" [:signed]]
   "characterDefensiveAbilityModifier" ["%s Defensive Ability" [:signed :percentage]]
   "characterDeflectProjectile" ["%s Chance to Avoid Projectiles" [:percentage]]
   "characterDodgePercent" ["%s Chance to Avoid Melee Attacks" [:percentage]]
   "characterDexterity" ["%s Cunning" [:signed]]
   "characterEnergyAbsorptionPercent" ["%s Energy Absorbed from Enemy Spells" [:signed :percentage]]
   "characterHuntingDexterityReqReduction" ["%s Cunning Requirement for Ranged Weapons" [:percentage :negative]]
   "characterIncreasedExperience" ["%s Experience Gained" [:signed :percentage]]
   "characterIntelligence" ["%s Spirit" [:signed]]
   "characterLife" ["%s Health" [:signed]]
   "characterLifeModifier" ["%s Health" [:signed :percentage]]
   "characterLifeRegen" ["%s Health Regenerated per second" [:signed]]
   "characterLifeRegenModifier" ["Increases Health Regeneration by %s" [:percentage]]
   "characterMana" ["%s Energy"]
   "characterManaRegen" ["%s Energy Regenerated per second" [:signed]]
   "characterManaRegenModifier" ["Increases Energy Regeneration by %s" [:percentage]]
   "characterManaLimitReserve" ["%s Energy Reserved"]
   "characterOffensiveAbility" ["%s Offensive Ability" [:signed]]
   "characterSpellCastSpeedModifier" ["%s Casting Speed" [:signed :percentage]]
   "characterStrength" ["%s Physique" [:signed]]
   "characterStrengthModifier" ["%s Physique" [:signed :percentage]]
   "characterTotalSpeedModifier" ["%s Total Speed" [:signed :percentage]]
   "damageAbsorptionPercent" ["%s Damage Absorption" [:percentage]]
   "defensiveBleedingDuration" ["%s Reduction in Bleeding Duration" [:percentage]]
   "defensiveBlockAmountModifier" ["%s Shield Damage Blocked" [:signed :percentage]]
   "defensiveElementalResistance" ["%s Elemental Resistance" [:percentage]]
   "defensiveFreeze" ["%s Reduced Freeze Duration" [:percentage]]
   "defensivePetrify" ["%s Reduced Petrify Duration" [:percentage]]
   "defensiveProtection" ["%s Armor"]
   "defensiveProtectionModifier" ["Increases Armor by %s" [:percentage]]
   "defensiveSleep" ["%s Sleep Resistance" [:percentage]]
   "defensiveStun" ["%s Reduced Stun Duration" [:percentage]]
   "defensiveTotalSpeedResistance" ["%s Slow Resistance" [:percentage]]
   "defensiveTrap" ["%s Reduced Entrapment Duration" [:percentage]]
   "lifeMonitorPercent" ["Activates when Health drops below %s" [:percentage]]
   "offensiveCritDamageModifier" ["%s Crit Damage" [:signed :percentage]]
   "offensiveFearMin" ["Terrify target for %s Seconds"]
   "offensiveLifeLeechMin" ["%s of Attack Damage converted to Health" [:percentage]]
   "offensivePierceRatioMin" ["%s Armor Piercing" [:percentage]]
   "offensiveStunMin" ["Stun target for %s Second"]
   "offensiveTotalDamageModifier" ["%s to All Damage" [:signed :percentage]]
   "petBurstSpawn" ["%s Summon" [:signed]]
   "petLimit" ["%s Summon Limit"]
   "projectileExplosionRadius" ["%s Meter Radius"]
   "projectileLaunchNumber" ["%s Projectile(s)"]
   "projectilePiercingChance" ["%s Chance to passthrough Enemies" [:percentage]]
   "retaliationDamagePct" ["%s of Retaliation Damage added to Attack" [:percentage]]
   "retaliationTotalDamageModifier" ["%s to All Retaliation Damage" [:signed :percentage]]
   "skillActiveDuration" ["%s Second Duration"]
   "skillCooldownReduction" ["%s Skill Cooldown Reduction" [:signed :percentage]]
   "skillCooldownTime" ["%s Second Skill Recharge"]
   "skillActiveManaCost" ["%s Active Energy Cost per Second" ]
   "skillChargeDuration" ["%s Second Charge Level Duration"]
   "skillManaCost" ["%s Energy Cost"]
   "skillManaCostReduction" ["%s Skill Energy Cost" [:signed :percentage :negative]]
   "skillTargetRadius" ["%s Meter Target Area"]
   "spawnObjectsTimeToLive" ["Lives for %s seconds"]
   "waveDistance" ["%s Meter Range"]
   "weaponDamagePct" ["%s Weapon Damage" [:percentage]]
   })

(def effect-ignore-fields
  #{"skillChargeAura"
    "skillChargeMultipliers"
    })

(defn effect-kv->string
  [record [k v :as kv]]

  ;; (println "effect-kv->string:" k)
  (let [v (lookup-and-resolve record k)
        kv [k v]]
    (when-not (effect-ignore-fields k)
      ;; Many effects only depends on a single own `kv` pair.
      ;; If a `k` can be located in the effect-string-map, we can easily turn this
      ;; `kv` pair into a string
      (if-let [msg (effect-string-map k)]
        (format (first msg) (apply val-or-range->string v (second msg)))

        ;; Some effects require more than 1 `kv` pair to specify
        ;; We can handle this with a bit of custom code
        (cond
          (= k "offensiveElementalResistanceReductionAbsoluteMin")
          (format "%s Reduced target's Elemental Resistances for %s Seconds"
                  v (lookup-and-resolve record "offensiveElementalResistanceReductionAbsoluteDurationMin"))
          (= k "offensiveElementalResistanceReductionAbsoluteDurationMin")
          nil


          (str/starts-with? k "conversionInType")
          (let [idx (subs k (count "conversionInType"))]
            (format "%s %s Damage converted to %s Damage"
                    (percentage (lookup-and-resolve record "conversionPercentage"))
                    (record (str "conversionInType" idx))
                    (record (str "conversionOutType" idx))))
          (str/starts-with? k "conversionOutType") nil
          (str/starts-with? k "conversionPercentage") nil



          (= k "offensiveFumbleMin")
          (format "%s Chance for target to Fumble attacks for %s Seconds"
                  (percentage v)
                  (lookup-and-resolve record "offensiveFumbleDurationMin"))
          (= k "offensiveFumbleDurationMin") nil

          (= k "offensiveProjectileFumbleMin")
          (format "%s Chance of Impaired Aim to target for %s Seconds"
                  (percentage v)
                  (lookup-and-resolve record "offensiveProjectileFumbleDurationMin"))
          (= k "offensiveProjectileFumbleDurationMin") nil


          (= k "offensiveSlowOffensiveAbilityMin")
          (format "%s Reduced target's Offensive Ability for %s seconds"
                  v
                  (lookup-and-resolve record "offensiveSlowOffensiveAbilityDurationMin"))
          (= k "offensiveSlowOffensiveAbilityDurationMin") nil


          (= k "sparkChance")
          (if (== v 100)
            (format "Affects up to %s targets"
                    (lookup-and-resolve record "sparkMaxNumber"))
            (format "%s Chance of affecting up to %s targets"
                    (percentage v)
                    (lookup-and-resolve record "sparkMaxNumber")))
          (= k "sparkMaxNumber") nil


          (= k "offensivePercentCurrentLifeMin")
          (format "%s Reduction to Enemy's Health"
                  (->> (collect-val-range record "offensivePercentCurrentLife")
                       range-str
                       percentage))
          (= k "offensivePercentCurrentLifeMax") nil

          (= k "retaliationSlowAttackSpeedDurationMin")
          (format "%s Reduced Attack Speed Retaliation for %s seconds"
                  (percentage (lookup-and-resolve record "retaliationSlowAttackSpeedMin"))
                  v)
          (= k "retaliationSlowAttackSpeedMin") nil

          (= k "skillChargeLevel")
          (format "%s Charge Levels: %s"
                  v
                  (->> (lookup-and-resolve record "skillChargeMultipliers")
                       (map #(percentage %))
                       (str/join ", ")))
          (= k "skillChargeMultipliers") nil


          (= k "offensiveTotalDamageReductionPercentDurationMin")
          (format "%s Reduced target's Damage for %s Seconds"
                  (percentage (lookup-and-resolve record "offensiveTotalDamageReductionPercentMin"))
                  v)
          (= k"offensiveTotalDamageReductionPercentMin") nil

          (= k "offensiveSlowTotalSpeedDurationMin")
          (format "%s Slow target for %s seconds"
                  (percentage (lookup-and-resolve record "offensiveSlowTotalSpeedMin"))
                  v)
          (= k "offensiveSlowTotalSpeedMin") nil

          (= k "offensiveFreezeMin")
          (format "%s Chance to Freeze target for %s Second"
                  (percentage (lookup-and-resolve record "offensiveFreezeChance"))
                  v)
          (= k "offensiveSlowColdMin") nil


          (= k "offensiveTotalResistanceReductionAbsoluteDurationMin")
          (format "%s Reduced target's Resistances for %s Seconds"
                  (percentage (lookup-and-resolve record "offensiveTotalResistanceReductionAbsoluteMin"))
                  v)
          (= k "offensiveTotalResistanceReductionAbsoluteMin") nil

          (= k "projectileFragmentsLaunchNumberMin")
          (format "%s Fragments"
                  (->> (collect-val-range record "projectileFragmentsLaunchNumber")
                       range-str))
          (= k "projectileFragmentsLaunchNumberMax") nil

          (= k "offensiveSlowRunSpeedDurationMin")
          (format "%s Slower target Movement for %s Seconds"
                  (percentage (lookup-and-resolve record "offensiveSlowRunSpeedMin"))
                  v)

          (= k "offensiveSlowRunSpeedMin") nil

          (= k "offensiveTrapMin")
          (format "%s Chance to Immobilize target for %s Seconds"
                  (percentage (lookup-and-resolve record "offensiveTrapChance"))
                  (range-str (collect-val-range record "offensiveTrap")))

          (= k "offensiveTrapChance") nil

          ;;----------------------------------------------------------
          ;; There are other large number of effects that deals with
          ;; describing various kinds of damage.
          ;; We'll try to handle them generically here
          ;;
          (str/includes? k "Slow")
          (slow-effect-kv->string- record kv)

          :else
          (generic-effect-kv->string- record kv))))))

(defn sub-record->string
  [record]

  (cond
    (#{"skillLifeBonus" "skillLifePercent"} (key (first record)))
    (let [fields (select-keys record ["skillLifeBonus" "skillLifePercent"])]
      (if (= 2 (count fields))
        (format "%s %s Health Restored"
                (percentage (lookup-and-resolve record "skillLifePercent"))
                (signed-number (lookup-and-resolve record "skillLifeBonus")))
        (do
          (when-let [v (lookup-and-resolve record "skillLifePercent")]
            (format "%s Health Restored"
                    (percentage v)))
          (when-let [v (lookup-and-resolve record "skillLifeBonus")]
            (format "%s Health Restored"
                    v)))))))

(declare skill-mods-summary effect-summary)

(defn skill-mods-summary
  [record recursion-blocks]

  (flatten
   (for [{:keys [name modifier] :as entry} (skill-modifiers record)]
     (->> (effect-summary modifier (conj recursion-blocks :skill-mods))
          (map (fn [desc]
                 (format "%s to %s" desc name)))))))

(defn effect-display-order
  [key-name]
  (cond
    (str/starts-with? key-name "characterStrength") 100
    (str/starts-with? key-name "characterLife") 200
    (str/starts-with? key-name "characterManaRegen") 300
    (str/starts-with? key-name "character") 400
    (str/starts-with? key-name "offensive") 500
    (str/starts-with? key-name "defensive") 600
    :else Integer/MAX_VALUE))

(defn indent
  [s]
  (str "  " s))

(defn wrap-if-single
  [v]
  (cond
    (nil? v) nil
    (not (sequential? v)) [v]
    :else v))

(defn split-subrecords
  [record]
  (-> (->> record
           (group-by #(cond
                        (str/starts-with? (key %) "skillLife") "skillLife"))
           (s/transform [s/MAP-VALS] #(into {} %)))
      (set/rename-keys {nil :main})))

(defn effect-summary
  ([record]
   (effect-summary record #{}))

  ([record recursion-blocks]
   ;; (println "Summarizing: " (:recordname record))

   ;; Some records require a calculated `itemSkillLevel` to full resolve some of its values
   ;; In these cases, the `v` in the record will be a vector.
   ;; `itemSkillLevel` can be used as the index to retrieve the actual value of `v`
   (let [record (cond-> record
                  (and (not (record "itemSkillLevel"))
                       (record "itemSkillLevelEq")
                       (record "itemLevel"))
                  (assoc "itemSkillLevel" (dec (int (eq/evaluate (record "itemSkillLevelEq")
                                                                 {"itemLevel" (record "itemLevel")})))))

         sub-records (split-subrecords record)
         record (sub-records :main)
         sub-records (-> sub-records
                         (dissoc :main)
                         vals)
         ]

     (->> (concat

           ;; Racial bonuses
           (flatten
            (for [race-tag (wrap-if-single (record "racialBonusRace"))]
              [(when-let [absolute-dmg (lookup-and-resolve record "racialBonusAbsoluteDamage")]
                 (format "%s Damage to %s" (signed-number absolute-dmg ) (dbu/race-name race-tag)))
               (when-let [percent-def (lookup-and-resolve record "racialBonusPercentDefense")]
                 (format "%s Less Damage from %s" (percentage percent-def ) (dbu/race-name race-tag)))
               (when-let [dmg-percent (record "racialBonusPercentDamage")]
                 (format "%s Damage to %s" (signed-percentage dmg-percent) (dbu/race-name race-tag)))]))

           ;; General effects
           (for [kv (->> record
                         (filter #(and (string? (key %))
                                       (looks-like-effect-keyname (key %))))
                         (sort-by key #(< (effect-display-order %) (effect-display-order %2))))]
             (effect-kv->string record kv))

           (for [sub-record sub-records]
             (sub-record->string (cond-> sub-record
                                   (record "itemSkillLevel")
                                   (assoc "itemSkillLevel" (record "itemSkillLevel")))))

           ;; Additional skills
           (for [skill (augment-skills record)]
             (format "%s to %s" (signed-number (:level skill)) (:name skill)))

           ;; Skill modifications
           (when-not (recursion-blocks :skill-mods)
             (skill-mods-summary record recursion-blocks))


           ;; Pet skills
           (when-not (recursion-blocks :pet-skill)
             (when-let [pet-skill (dbu/record-by-name (record "petSkillName"))]
               (effect-summary pet-skill (conj recursion-blocks :pet-skill))))

           ;; Mastery augments
           (for [mastery (augment-masteries record)]
             (format "%s to all skills in %s" (signed-number (:level mastery)) (:name mastery)))

           ;; Additioanl sections
           ;; These have their own titles and are indented for readability
           (when-not (recursion-blocks :pets)
             (when-let [pet-bonus-record (dbu/record-by-name (record "petBonusName"))]
               (concat
                ["" "Bonus to All Pets"]
                (map indent
                     (effect-summary pet-bonus-record (conj recursion-blocks :pets))))))

           (when-not (recursion-blocks :item-skill)
             (when-let [item-skill (dbu/record-by-name (record "itemSkillName"))]
               (let [item-skill (cond-> item-skill
                                  (record "itemSkillLevel")
                                  (assoc "itemSkillLevel" (record "itemSkillLevel")))]
                 (concat
                  ["" "Granted Skills"
                   (indent (dbu/skill-name-from-record item-skill))]

                  ;; Include skill description
                  (map indent
                       (u/wrap-line 78 (dbu/skill-description-from-record item-skill)))

                  ;; Add the actual effect summary
                  [""]
                  (map indent
                       (effect-summary item-skill (conj recursion-blocks :item-skill)))))))

           (when-not (recursion-blocks :buff-skill)
             (when-let [buff-skill (dbu/record-by-name (record "buffSkillName"))]
               (let [buff-skill (cond-> buff-skill
                                  (record "itemSkillLevel")
                                  (assoc "itemSkillLevel" (record "itemSkillLevel")))]

                 (effect-summary buff-skill (conj recursion-blocks :buff-skill))))))

          (remove nil?)))))

(comment
  (effect-summary
   (dbu/record-by-name
    "records/items/gearhead/b016a_head.dbr"))

  (effect-summary
   (dbu/record-by-name "records/items/gearhead/b004_head.dbr"))

  (effect-summary
   (dbu/record-by-name "records/items/gearhead/b002_head.dbr"))

  (effect-summary
   (dbu/record-by-name "records/items/gearhead/b006_head.dbr"))


  (effect-summary
   (dbu/record-by-name
    "records/items/gearhead/b205a_head.dbr"))

  (dbu/record-by-name
   "records/skills/itemskillsgdx1/skillmodifiers/monsterinfrequents/head_b004_sphereofprotection.dbr"
   ))


(defn record-field
  [record field-name]
  (let [v (record field-name)]
    (if-let [referenced-record (dbu/record-by-name v)]
      referenced-record
      v)))

(defn record-primary-attributes
  [record]

  [(when-let [defensive-protection (record "defensiveProtection")]
     (effect-kv->string record ["defensiveProtection" defensive-protection]))
   (when-let [physical-dmg (record "offensivePhysicalMin")]
     (effect-kv->string record ["offensivePhysicalMin" physical-dmg]))
   (when-let [pierce-ratio (record "offensivePierceRatioMin")]
     (effect-kv->string record ["offensivePierceRatioMin" pierce-ratio]))
   (->> record
        (filter #(str/starts-with? (key %) "offensiveBase"))
        (map #(effect-kv->string record %)))])

(defn item-summary
  [item]

  (let [base-record (dbu/record-by-name (:basename item))
        subtype (record-class-subtype base-record)]

    (->>
     [;; Name of item
      (yellow (dbu/item-name item))
      (when-let [item-text (base-record "itemText")]
        (u/wrap-line 80 item-text))

      ;; Item classification
      (let [classifications (conj (into [] (vals (select-keys base-record ["itemClassification" "armorClassification"])))
                                  (record-class-display-name (base-record "Class")))]
        (str/join " " classifications))

      (record-primary-attributes base-record)

      ;; Item effects
      ""
      (effect-summary (->> (dissoc base-record
                                   "defensiveProtection"
                                   "offensivePhysicalMin"
                                   "offensivePierceRatioMin")
                           (filter (fn [[k v]]
                                     (not (str/starts-with? k "offensiveBase"))))
                           (into {})))


      ;; Requirements section
      ""
      (when-let [level-req (base-record "levelRequirement")]
        (format "Required Player Level: %s" (number level-req)))

      (record-cost base-record)

      (format "Item Level: %s" (number (base-record "itemLevel")))]
     flatten
     (remove nil?))))

(defn interesting-fields
  [record]
  (dissoc record "physicsFriction" "armorFemaleMesh" "baseTexture" "outlineThickness" "actorRadius" "dropSound3D"
          "scale" "physicsMass" "maxTransparency" "dropSoundWater" "templateName" "mesh" "dropSound" "castsShadows"
          "attributeScalePercent" "bitmap" "armorMaleMesh" "actorHeight" "glowTexture" "bumpTexture"))

(comment
  (do
    (require 'repl
             [gd-edit.commands.item :as item])

    (repl/init)
    (repl/load-character "Odie"))

  (->> (seq @globals/db)
       (reduce (fn [interesting-keynames record]
                 (into interesting-keynames
                       (filter
                        (fn [keyname]
                          (or
                           ;; (str/includes? keyname "offensive")
                           ;; (str/includes? keyname "defensive")
                           ;; (str/includes? keyname "character")
                           ;; (str/includes? keyname "character")
                           (str/includes? keyname "AmountModifier")
                           ;; (str/includes? keyname "Slow")
                           ))
                        (keys record))))
               #{}))

  ;; Manual testing rig
  (def test-target (atom {}))

  (let [item-to-test "Titan Pauldrons"
        level-limit 71]
    (when-let [x (item/construct-item item-to-test @globals/db @globals/db-index level-limit)]
      (reset! test-target (dbu/record-by-name (:basename x)))))

  (effect-summary @test-target)


  (defn get-test-item
    [item-name level-limit]
    (when-let [x (item/construct-item item-name @globals/db @globals/db-index level-limit)]
      (reset! test-target x)))

  (get-test-item "Chausses of Barbaros" 84)


  (sort-by (comp name key)
           (-> @test-target
               (record-field :basename)
               ;; (record-field "modifierSkillName3")
               ;; (record-field "itemSetName")
               ;; (record-field "petSkillName")
               ;; (record-field "petBonusName")
               ;; (record-field "itemSkillName") ;; granted skill
               ;; (record-field "buffSkillName") ;; granted skill redirect
               (record-field "itemSkillAutoController")
               (interesting-fields)
               ))

  (item-summary @test-target)


  (->> @globals/db
       (map #(% "Class"))
       distinct
       (remove nil?)
       (filter #(str/includes? % "_"))
       (filter #(or (u/ci-match % "weapon")
                    (u/ci-match % "armor")))
       sort
       )

  )
