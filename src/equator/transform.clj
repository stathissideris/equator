(ns equator.transform
  (:use equator.core)
  (:require [clojure.walk :as walk]
            [equator.form :as form]))

(defn eval-when-possible [form]
  (try (eval form) (catch Exception e)))

(defn calc-numeric [form]
  (let [op (first form)
        non-numbers (filter (complement number?) (rest form))
        numbers (filter number? form)
        result (eval-when-possible `(~op ~@numbers))]
    (if (and result (not (empty? numbers)))
      (if (empty? non-numbers)
        result
        (if (form/multiplication? form)
          (concat [op] [result] non-numbers)
          (concat [op] non-numbers [result])))
      form)))

(defn add-term-factors
  "When passed a list of polynomial terms with the same power, it
  returns a polynomial term whose factor is the addition of all the
  factors of the passed terms. If passed one term, it returns it
  untouched."
  [terms variable]
  (if (= 1 (count terms))
    (first terms)
    (list
     '*
     (apply list '+ (map #(form/term-factor % variable) terms))
     (form/term-without-factor (first terms) variable))))

(defn undistribute-poly-terms
  "Converts
  2x^2 + 3x + x^2 + 5x
  to
  (2 + 1)x^2 + (3 + 5)x

  Sorts by descending power."
  [form variable]
  (apply list
         (form/operator form)
         (reverse
          (sort-by #(form/term-power % variable)
           (map #(add-term-factors % variable)
                (vals (group-by #(form/term-power % variable) (rest form))))))))

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

(defn simplify [form variable]
  (cond (not (sequential? form))
          form
        (form/equation? form)
          (list (form/operator form)
                (simplify (second form) variable)
                (simplify (third form) variable))
        :else
          (walk/postwalk
           #(if (sequential? %) (calc-numeric %) %)
           (undistribute-poly-terms form variable))))

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
