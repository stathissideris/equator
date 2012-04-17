(ns equator.latex
  (:refer-clojure :exclude [numerator denominator])
  (:use equator.core
        equator.form)
  (:require [clojure.walk :as walk]))

(defn add-space [coll]
  (interpose " " coll))

(defn latex-equation [form]
  (apply str (add-space ((juxt second operator third) form))))

(defn latex-addition [form]
  (apply str (interpose " + " (drop 1 form))))

(defn latex-power [[_ x e]]
  (if (= 1 (count (str e)))
    (str x "^" e)
    (str x "^{" e "}")))

(defn latex-subscript [[_ x s]]
  (if (= 1 (count (str s)))
    (str x "_" s)
    (str x "_{" s "}")))

(def dispatch-map
  {equation? latex-equation
   addition? latex-addition
   power? latex-power
   subscript? latex-subscript})

(defmacro dispatch []
  `(cond ~@(mapcat (fn [[k v]] `((~k ~'form) (~v ~'form))) dispatch-map)))

(defn form-to-latex [form]
  (if (sequential? form)
    (dispatch)
    form))

(defn to-latex [form]
  (walk/postwalk form-to-latex form))