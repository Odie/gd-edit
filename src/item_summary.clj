(ns item-summary
  (:require [gd-edit.db-utils :as dbu]
            [clojure.string :as str]
            [gd-edit.equation-eval :as eq]
            [com.rpl.specter :as s]
            [gd-edit.utils :as u]
            [clojure.edn :as edn]
            [gd-edit.commands.item :as item]
            [gd-edit.globals :as globals]
            [gd-edit.commands.level :as level]

            [taoensso.timbre :as log]))

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
  (if (> number 0)
    "+"))

(defn signed-number
  [number]
  (str (sign number) number))

(defn signed-percentage
  [number]
  (str (sign number) number "%"))

(defn percentage
  [number]
  (str number "%"))

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
                                                                               }))]))
                                    (into {}))]

              [(when-let [strength-req (requirements "strength")]
                 (format "Required Physique: %d" strength-req))
               (when-let [dexterity-req (requirements "dexterity")]
                 (format "Required Cunning: %d" dexterity-req))
               (when-let [spirit-req (requirements "intelligence")]
                 (format "Required Spirit: %d" spirit-req))]))]

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

(defn collect-val-range
  [record val-name]
  (let [v (->> (vals (select-keys record [(str val-name "Min") (str val-name "Max")]))
               (map #(maybe-resolve-v record %)))]
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
           [{:name "Physical", :components #{:physical}}
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
            {:name "Block", :components #{:block}}]))

(def damage-types
  #{:physical
    :fire
    :cold
    :lightning
    :acid
    :pierce
    :bleeding
    :aether
    :chaos})

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
                 "conversionInType"
                 "conversionOutType"
                 "conversionPercentage"
                 "petLimit"
                 "petBurstSpawn"
                 "augmentAllLevel"
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
        percentage? (contains?+ opts :percentage)]
    (cond->> (maybe-int v)
      (and signed? percentage?)
      (signed-percentage)
      (and (not signed?) percentage?)
      (percentage)
      (and signed? (not percentage?))
      (signed-number)
      :else
      (str))))

(defn val-or-range->string
  [v & opts]
  ;; (println v)
  (if (seq? v)
    (if (> (count v) 1)
      (apply format "%s-%s"
             (map #(apply val->string- % opts) v))
      (apply val->string- (first v) opts))
    (apply val->string- v opts)))

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

(defn generic-effect-kv->string-
  [record [k v]]

  (when (not (str/ends-with? k "Max"))
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

                                           ;; Things marked as "defensive" describe resistance
                                           (= (first components) :defensive)
                                           ["%s %s Resistance" [:percentage]]

                                           (= (first components) :retaliation)
                                           ["%s %s Retaliation"]

                                           :else
                                           (log/debug "Unrecognized effect" (u/collect-as-map k v)))]
          (when message
            (format message
                    (cond (some #{:modifier} components)
                          (val-or-range->string v :signed :percentage)
                          :else
                          (apply val-or-range->string v val-display-type))
                    (:name effect))))))))


(defn lookup-and-resolve
  [record fieldname]
  (maybe-resolve-v record (record fieldname)))

(defn slow-effect-kv->string-
  [record [k v :as kv]]

  (let [components (camelcase->keywords k)
        effect (effect-by-components components)]
    (cond
      (= [:duration :min] (take-last 2 components))
      (let [key-base (keywords->camelcase (drop-last 2 components))
            duration (maybe-resolve-v record v)
            total-dmg (->> components
                           (drop-last 2)
                           keywords->camelcase
                           (collect-val-range record)
                           (map #(maybe-resolve-v record %))
                           (map #(* % duration)))]

        (if (contains?+ components :retaliation)
          (if-let [chance (record (str key-base "Chance"))]
            (format "%s Chance of %s %s Retaliation Damage over %s seconds"
                    (percentage (maybe-int chance)) (range-str (map int total-dmg)) (:name effect) (int duration))
            (format "%s %s Retaliation Damage over %s seconds"
                    (range-str (map int total-dmg)) (:name effect) (int duration)))))

      (= [:duration :modifier] (take-last 2 components))
      (format "%s %s Duration" (signed-percentage (maybe-int (maybe-resolve-v record v))) (:name effect))

      (str/ends-with? k "Modifier")
      (generic-effect-kv->string- record kv))))


(defn effect-kv->string
  [record [k v :as kv]]

  ;; (println k)

  (let [v (cond-> v
            (and (vector? v)
                 (record "itemSkillLevel"))
            (nth  (record "itemSkillLevel"))

            :then
            maybe-int)]
    (cond

      (= k "defensiveProtection")
      (format "%s Armor" v)

      (= k "defensiveBlockAmountModifier")
      (format "%s Shield Damage Blocked" (signed-percentage v))

      (= k "skillCooldownTime")
      (format "%s Second Skill Recharge" (signed-number v))

      (= k "characterAttackSpeedModifier")
      (format "%s Attack Speed" (signed-percentage v))

      (= k "defensiveProtectionModifier")
      (format "Increases Armor by %s" (percentage v))

      (= k "offensiveCritDamageModifier")
      (format "%s Crit Damage" (signed-percentage v))

      (= k "skillManaCostReduction")
      (format "%s Skill Energy Cost" (signed-percentage (- v)))

      (= k "characterDefensiveAbility")
      (format "%s Defense Ability" (signed-number v))

      (= k "characterOffensiveAbility")
      (format "%s Offensive Ability" (signed-number v))

      (= k "skillActiveDuration")
      (format "%s Second Duration" v)

      (= k "characterLifeRegen")
      (format "%s Health Regenerated per second" (signed-number v))
      (= k "characterLifeRegenModifier")
      (format "Increases Health Regeneration by %s" (percentage v))
      (= k "characterManaRegen")
      (format "%s Energy Regenerated per second" (signed-number v))

      (= k "characterEnergyAbsorptionPercent")
      (format "%s Energy Absorbed from Enemy Spells" (signed-percentage v))

      (= k "characterTotalSpeedModifier")
      (format "%s Total Speed" (signed-percentage v))

      (= k "offensiveElementalResistanceReductionAbsoluteMin")
      (format "%s Reduced target's Elemental Resistances for %s Seconds"
              v (maybe-int (lookup-and-resolve record "offensiveElementalResistanceReductionAbsoluteDurationMin")))
      (= k "offensiveElementalResistanceReductionAbsoluteDurationMin")
      nil

      (= k "characterLife")
      (format "%s Health" (signed-number v))
      (= k "characterLifeModifier")
      (format "%s Health" (signed-percentage v))

      (= k "characterIntelligence")
      (format "%s Spirit" (signed-number v))

      (= k "offensiveElementalMin")
      (format "%s Elemental Damage" v)

      (= k "defensiveElementalResistance")
      (format "%s Elemental Resistance" (percentage v))

      (= k "weaponDamagePct")
      (format "%s Weapon Damage" (percentage v))

      (= k "skillTargetRadius")
      (format "%s Meter Radius" v)

      (= k "skillCooldownReduction")
      (format "%s Skill Cooldown Reduction" (signed-percentage v))

      (= k "offensiveLifeLeechMin")
      (format "%s of Attack Damage converted to Health" (percentage v))

      (= k "offensiveElementalModifier")
      (format "%s Elemental Damage" (signed-percentage v))

      (= k "projectileExplosionRadius")
      (format "%s Meter Radius" v)

      (= k "defensiveTotalSpeedResistance")
      (format "%s Slow Resistance" (percentage v))

      (str/starts-with? k "conversionInType")
      (let [idx (subs k (count "conversionInType"))]
        (format "%s %s Damage converted to %s Damage"
                (percentage (maybe-int (lookup-and-resolve record "conversionPercentage")))
                (record (str "conversionInType" idx))
                (record (str "conversionOutType" idx))))
      (str/starts-with? k "conversionOutType") nil
      (str/starts-with? k "conversionPercentage") nil

      (= k "defensiveStun")
      (format "%s Reduced Stun Duration" (percentage v))
      (= k "defensiveFreeze")
      (format "%s Reduced Freeze Duration" (percentage v))

      (= k "petLimit")
      (format "%s Summon Limit" v)


      (= k "offensiveFumbleMin")
      (format "%s Chance for target to Fumble attacks for %s Seconds"
              (percentage v)
              (maybe-int (lookup-and-resolve record "offensiveFumbleDurationMin")))
      (= k "offensiveFumbleDurationMin") nil

      (= k "offensiveProjectileFumbleMin")
      (format "%s Chance of Impaired Aim to target for %s Seconds"
              (percentage v)
              (maybe-int (lookup-and-resolve record "offensiveProjectileFumbleDurationMin")))
      (= k "offensiveProjectileFumbleDurationMin") nil

      (= k "offensiveTotalDamageModifier")
      (format "%s to All Damage" (signed-percentage v))

      (= k "defensivePoison")
      (format "%s Poison & Acid Resistance" (percentage v))

      (= k "retaliationTotalDamageModifier")
      (format "%s to All Retaliation Damage" (percentage v))

      (= k "characterDeflectProjectile")
      (format "%s Chance to Avoid Projectiles" (percentage v))

      (= k "petBurstSpawn")
      (format "%s Summon" (signed-number v))

      (= k "skillManaCost")
      (format "%s Energy Cost" v)

      (= k "characterSpellCastSpeedModifier")
      (format "%s Casting Speed" (signed-percentage v))

      (= k "characterIncreasedExperience")
      (format "%s Experience Gained" (signed-percentage v))

      (= k "characterStrength")
      (format "%s Physique" (signed-number v))

      (= k "characterDexterity")
      (format "%s Cunning" (signed-number v))

      (= k "offensiveSlowOffensiveAbilityMin")
      (format "%s Reduced target's Offensive Ability for %s seconds"
              v
              (maybe-int (lookup-and-resolve record "offensiveSlowOffensiveAbilityDurationMin")))
      (= k "offensiveSlowOffensiveAbilityDurationMin") nil

      (= k "projectileLaunchNumber")
      (format "%s Projectile(s)" v)

      (= k "projectilePiercingChance")
      (format "%s Chance to passthrough Enemies" (percentage v))

      (= k "augmentAllLevel")
      (format "%s to all Skills" (signed-number v))

      (= k "offensivePierceRatioMin")
      (format "%s Armor Piercing" (percentage v))

      (= k "characterConstitutionModifier")
      (format "%s Constitution" (percentage v))

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
                   (map maybe-int)
                   (range-str)
                   (percentage)))
      (= k "offensivePercentCurrentLifeMax") nil

      (= k "defensivePetrify")
      (format "%s Reduced Petrify Duration" (percentage v))

      (= k "characterStrengthModifier")
      (format "%s Physique" (signed-percentage v))

      :else
      (cond
        (str/includes? k "Slow")
        (slow-effect-kv->string- record kv)
        :else
        (generic-effect-kv->string- record kv)))))

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
                                                                 {"itemLevel" (record "itemLevel")})))))]
     (->> (concat

           ;; Racial bonuses
           (flatten
            (for [race-tag (wrap-if-single (record "racialBonusRace"))]
              [(when-let [absolute-dmg (lookup-and-resolve record "racialBonusAbsoluteDamage")]
                 (format "%s Damage to %s" (signed-number (maybe-int absolute-dmg) ) (dbu/race-name race-tag)))
               (when-let [percent-def (lookup-and-resolve record "racialBonusPercentDefense")]
                 (format "%s Less Damage from %s" (percentage (maybe-int percent-def) ) (dbu/race-name race-tag)))
               (when-let [dmg-percent (record "racialBonusPercentDamage")]
                 (format "%s Damage to %s" (signed-percentage (maybe-int dmg-percent)) (dbu/race-name race-tag)))]))

           ;; General effects
           (for [kv (->> record
                         (filter #(and (string? (key %))
                                       (looks-like-effect-keyname (key %))))
                         (sort-by key #(< (effect-display-order %) (effect-display-order %2))))]
             (effect-kv->string record kv))

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
      (dbu/item-name item)
      (when-let [item-text (base-record "itemText")]
        (u/wrap-line 80 item-text))

      ;; Item classification
      (let [classifications (conj (into [] (vals (select-keys base-record ["itemClassification" "armorClassification"])))
                                  (record-class-display-name (base-record "Class")))]
        (str/join " " classifications))

      ;; (when-let [defensive-protection (base-record "defensiveProtection")]
      ;;   (effect-kv->string base-record ["defensiveProtection" defensive-protection]))
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
        (format "Required Player Level: %d" level-req))

      (record-cost base-record)

      (format "Item Level: %d" (base-record "itemLevel"))]
     flatten
     (remove nil?))))

(comment
  (require 'repl)

  (repl/init)

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

  (repl/cmd "set inv/1/items/0 \"Putrid Necklace\" 8")

  (repl/cmd "set inv/1/items/0 \"Sister's Amulet of Lifegiving\" 10")

  (repl/cmd "set inv/1/items/0 \"Death-Watcher Pendant\" 30")

  (repl/cmd "set inv/1/items/0 \"Putrid Necklace\" 32")

  (repl/cmd "set inv/1/items/0 \"Ellena's Necklace\" 35")

  (repl/cmd "set inv/1/items/0 \"Kaisan's Burning Eye\" 40")

  (repl/cmd "set inv/1/items/0 \"Honed Longsword\" 15")

  (repl/cmd "set inv/1/items/0 \"Boneblade\" 20")

  (repl/cmd "set inv/1/items/0 \"Manticore Longsword\" 30")

  (repl/cmd "set inv/1/items/0 \"Bonescythe\" 20")

  (repl/cmd "set inv/1/items/0 \"Spectral Battle Axe\" 32")

  (repl/cmd "set inv/1/items/0 \"Empowered Immaterial Edge\" 75")

  (repl/cmd "set inv/1/items/0 \"Stormcaller's Spellblade\" 22")

  (repl/cmd "set inv/1/items/0 \"Voidblade\" 22")

  (repl/cmd "set inv/1/items/0 \"Plagueblood Carver\" 25")

  (repl/cmd "set inv/1/items/0 \"Jaravuuk's Bite\" 45")

  (repl/cmd "set inv/1/items/0 \"Empowered Twilight's Veil\" 50")

  (repl/cmd "set inv/1/items/0 \"Putrid Necklace\" 8")

  (repl/cmd "set inv/1/items/0 \"Titan Paldrons\" 8")


  (item-summary
   (repl/get-at-path @globals/character "inventory-sacks/1/inventory-items/0"))

  (level/attribute-points-total-at-level 1)
  (level/attribute-points-total-at-level 99)

  (record-cost
   (dbu/record-by-name
    "records/items/gearaccessories/necklaces/b002a_necklace.dbr"))

  (do
    (repl/load-character "Odie")

    (repl/cmd "set inv/1/items/0 \"Milton's Casque\" 15")

    (= (with-out-str
         (print-item-summary
          (repl/get-at-path @globals/character "inventory-sacks/1/inventory-items/0"))

         )

       (u/strip-leading-indent
        "Milton's Casque
        \"A notch on the helm for every Taken he's killed.\"
        Rare Heavy Helm
        78.0 Armor

        +10.0% Aetherial
        +10.0% Aether Corruption
        +12.0% Aether Resistance
        +10.0% Shield Damage Blocked
        +2 to Overguard
        +2 to Blitz
        -0.3 Second Skill Recharge to Blitz

        Required Player Level: 12
        Required Physique:  127
        Item Level: 12
        ")))

  (do
    (repl/load-character "Odie")
    (repl/cmd "set inv/1/items/0 \"Ascended Casque\" 20")
    (= (with-out-str
         (print-item-summary
          (repl/get-at-path @globals/character "inventory-sacks/1/inventory-items/0")))

       (dbu/strip-leading-indent
        "Ascended Casque
        Rare Heavy Helm
        126.0 Armor

        2-5 Aether Damage
        +8.0% Aether Damage
        +16.0% Aether Resistance
        +3 to Maiven's Sphere of Protection
        +3 to Spectral Binding
        +8.0% Attack Speed to Maiven's Sphere of Protection
        Increases Armor by +12.0% to Maiven's Sphere of Protection

        Required Player Level: 18
        Required Physique:  202
        Item Level: 20
        ")))

  (do
    (repl/load-character "Odie")
    (repl/cmd "set inv/1/items \"Incendiary Casque\" 20")
    (= (with-out-str
         (print-item-summary
          (repl/get-at-path @globals/character "inventory-sacks/1/inventory-items/23")))

       (dbu/strip-leading-indent
        "Incendiary Casque
        Rare Heavy Helm
        126.0 Armor

        2-5 Fire Damage
        +8.0% Fire Damage
        +8.0% Burn Damage
        +16.0% Chaos Resistance
        +2 to Flames of Ignaffar
        +2 to Fire Strike
        +15.0% Crit Damage to Flames of Ignaffar
        -6.0% Skill Energy Cost to Flames of Ignaffar

        Required Player Level: 18
        Required Physique:  202
        Item Level: 20
        ")))

  (do
    (repl/load-character "Odie")
    (repl/cmd "set inv/1/items \"Murderer's Cowl\" 20")
    (= (with-out-str
         (print-item-summary
          (repl/get-at-path @globals/character "inventory-sacks/1/inventory-items/23")))

       (dbu/strip-leading-indent
        "Murderer's Cowl
        Rare Light Helm
        100.0 Armor

        4 Bleeding Damage over 3 seconds
        +10.0% Bleeding Damage
        +15.0 Defensive Ability
        +2 to Blade Trap
        +2 to Ring of Steel
        +2.0 Second Duration to Blade Trap
        +90 Bleeding Damage over 3 seconds to Ring of Steel

        Required Player Level: 18
        Required Physique:  125
        Item Level: 20
        ")))


  (do

    (repl/load-character "Odie")

    (repl/cmd "set inv/1/items/0 \"Milton's Casque\" 20")

    (= (with-out-str
         (print-item-summary
          (repl/get-at-path @globals/character "inventory-sacks/1/inventory-items/0")))

       (strip-leading-indent
        "Murderer's Cowl
        Rare Light Helm
        100.0 Armor

        4 Bleeding Damage over 3 seconds
        +10.0% Bleeding Damage
        +15.0 Defensive Ability
        +2 to Blade Trap
        +2 to Ring of Steel
        +2.0 Second Duration to Blade Trap
        +90 Bleeding Damage over 3 seconds to Ring of Steel

        Required Player Level: 18
        Required Physique:  125
        Item Level: 20
        ")))

  (defn interesting-fields
    [record]
    (dissoc record "physicsFriction" "armorFemaleMesh" "baseTexture" "outlineThickness" "actorRadius" "dropSound3D"
            "scale" "physicsMass" "maxTransparency" "dropSoundWater" "templateName" "mesh" "dropSound" "castsShadows"
            "attributeScalePercent" "bitmap" "armorMaleMesh" "actorHeight" "glowTexture" "bumpTexture"))

  (def test-target (atom {}))

  (let [item-to-test "Titan Pauldrons"
        level-limit 71]
    (when-let [x (item/construct-item item-to-test @globals/db @globals/db-index level-limit)]
      (reset! test-target (dbu/record-by-name (:basename x)))))


  (let [item-to-test "Titan Pauldrons"
        level-limit 71]
    (when-let [x (item/construct-item item-to-test @globals/db @globals/db-index level-limit)]
      (reset! test-target x)))

  (sort-by (comp name key)
           (-> @test-target
               (record-field :basename)
               (record-field "itemSkillName")
               (interesting-fields)
               ))

  (item-summary @test-target)

  (interesting-fields @test-target)


  (->> @test-target
       (filter #(u/ci-match (key %) "skill"))
       (sort-by key))

  (effect-summary @test-target)

  (augment-masteries @test-target)

  (effect-summary
   (dbu/record-by-name "records/skills/itemskillsgdx1/granted/noxiousfumes_01.dbr")
   )

  (-> (record-field @test-target "modifierSkillName1")
      ;; (record-field "petSkillName")
      )

  (record-field @test-target "itemSkillName")

  (record-field @test-target "modifiedSkillName1")

  (-> (record-field @test-target "augmentSkillName1")
      (record-field "petSkillName")
      (record-field "buffSkillName")
      )

  (augment-skills @test-target)

  (record-field @test-target "petBonusName")


  (dbu/record-by-name
   (@test-target "itemSkillName"))

  (effect-summary
   (dbu/record-by-name
    "records/items/gearhead/b208a_head.dbr"))

  (effect-summary
   (dbu/record-by-name "records/items/gearhead/b202a_head.dbr"))

  (effect-summary
   (dbu/record-by-name "records/items/gearhead/b201a_head.dbr"))

  (dbu/record-by-name

   "records/skills/itemskillsgdx2/skillmodifiers/mi/head_b201_grenado.dbr")

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
