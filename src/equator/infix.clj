(ns equator.infix
  (:require [clojure.walk :as walk]))

(defn infix= [[op a b]]
  `(~a ~op ~b))

(defn infix+ [form]
  (interpose '+ (drop 1 form)))

(defn infix- [form]
  (interpose '- (drop 1 form)))

(defn infix* [form]
  (interpose '* (drop 1 form)))

(defn infix-division [form]
  (interpose '/ (drop 1 form)))

(defn infix-power [[_ x e]]
  `(~x "^" ~e))

(defn infix-form [form]
  (if (sequential? form)
    (case (first form)
      = (infix= form)
      + (infix+ form)
      - (infix- form)
      * (infix* form)
      / (infix-division form)
      pow (infix-power form))
    form))

(defn infix-tree-form [form]
  (walk/postwalk infix-form form))
