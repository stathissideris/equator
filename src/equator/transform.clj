(ns equator.transform
  (:use equator.core)
  (:require [clojure.walk :as walk]
            [equator.form :as form]))

(defn eval-when-possible [form]
  (try (eval form) (catch Exception e)))

(defn calc-partly [form]
  (let [op (first form)
        non-numbers (filter (complement number?) (rest form))
        numbers (filter number? form)
        result (eval-when-possible `(~op ~@numbers))]
    (if result
      (if (empty? non-numbers)
        result
        (concat [op] non-numbers [result]))
      form)))

;;todo would be nice to not move stuff when merging
(defn merge-associative [form]
  (let [parent-operator (first form)]
    (if (form/associative-operator? parent-operator)
      (let [mergeable-forms
            (filter #(form/operator= % parent-operator) (form/operands form))
            non-mergeable-forms
            (filter #(not (form/operator= % parent-operator)) (form/operands form))]
        (concat
         [parent-operator]
         non-mergeable-forms
         (mapcat form/operands mergeable-forms)))
      form)))

(defn simplify [form]
  (if (= (first form) '=)
    (list '= (simplify (second form)) (simplify (third form)))
    (walk/postwalk
     #(if (sequential? %)
        (calc-partly (merge-associative %))
        %) form)))

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
