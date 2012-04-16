(ns equator.core
  (:require [clojure.walk :as walk]))

(def equation
  '(= (+ (* 2 (pow x 2)) x 1) 4))

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

(defn naive-simplify [form]
  (let [simple (try (eval form) (catch Exception e))]
    (if simple simple form)))

(defn simplify [form]
  (walk/postwalk naive-simplify form))

(defn infix-tree-form [form]
  (walk/postwalk infix-form form))

(defn pow [x e] (Math/pow x e))

(defn balanced-action [[eq a b] op x]
  `(~eq (~op ~a ~x) (~op ~b ~x)))

(defn add [equation x]
  (balanced-action equation '+ x))

(defn subtract [equation x]
  (balanced-action equation '- x))

(defn multiply [equation x]
  (balanced-action equation '* x))

(defn divide [equation x]
  (balanced-action equation '/ x))
