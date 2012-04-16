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

(defn infix-tree-form [form]
  (walk/postwalk infix-form form))

(defn third [x]
  (nth x 2))

(defn eval-if-possible [form]
  (let [simple (try (eval form) (catch Exception e))]
    (if simple simple form)))

(defn calc-partly [form]
  (let [op (first form)
        non-numbers (filter (complement number?) (rest form))
        numbers (filter number? form)
        result (eval-if-possible `(~op ~@numbers))]
    (if result
      (if (empty? non-numbers)
        result
        (concat [op] non-numbers [result]))
      form)))

(defn simplify [form]
  (if (= (first form) '=)
    (list '= (simplify (second form)) (simplify (third form)))
    (walk/postwalk #(if (sequential? %) (calc-partly %) %) form)))

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
