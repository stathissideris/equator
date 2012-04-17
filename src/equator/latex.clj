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

(defn latex-multiplication [form]
  )

(defn latex-fraction [form]
  (str "\\frac{" (numerator form) "}{" (denominator form) "}"))

(defn latex-power [[_ x e]]
  (if (= 1 (count (str e)))
    (str x "^" e)
    (str x "^{" e "}")))

(defn latex-subscript [[_ x s]]
  (if (= 1 (count (str s)))
    (str x "_" s)
    (str x "_{" s "}")))

(def dispatch-map
  [ratio? latex-fraction
   #(not (sequential? %)) identity
   equation? latex-equation
   addition? latex-addition
   multiplication? latex-multiplication
   division? latex-fraction
   fraction? latex-fraction
   power? latex-power
   subscript? latex-subscript])

(defmacro dispatch []
  `(cond ~@(mapcat (fn [[k v]] `((~k ~'form) (~v ~'form)))
                   (partition 2 dispatch-map))
         :else form))

(defn form-to-latex [form]
  (dispatch)
  #_(let [result (dispatch)]
      (prn form)
      result))

(defn to-latex [form]
  (walk/postwalk form-to-latex form))
