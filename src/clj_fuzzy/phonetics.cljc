;; -------------------------------------------------------------------
;; clj-fuzzy Phonetics API
;; -------------------------------------------------------------------
;;
;;
;;   Author: PLIQUE Guillaume (Yomguithereal)
;;   Version: 0.1
;;
(ns clj-fuzzy.phonetics
  (:require clj-fuzzy.metaphone
            clj-fuzzy.double-metaphone
            clj-fuzzy.soundex
            clj-fuzzy.nysiis
            clj-fuzzy.caverphone
            clj-fuzzy.match-rating
            clj-fuzzy.cologne))

(def ^:export metaphone clj-fuzzy.metaphone/process)
(def ^:export double-metaphone clj-fuzzy.double-metaphone/process)
(def ^:export soundex clj-fuzzy.soundex/process)
(def ^:export nysiis clj-fuzzy.nysiis/process)
(def ^:export caverphone clj-fuzzy.caverphone/process)
(def ^:export mra-codex clj-fuzzy.match-rating/mra-codex)
(def ^:export cologne clj-fuzzy.cologne/process)
