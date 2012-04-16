(ns equator.form
  (:refer-clojure :exclude [numerator denominator])
  (:use equator.core))

(defn operator= [form op]
  (and (sequential? form) (= (first form) op)))

(defn operands [form]
  (when (sequential? form) (rest form)))

(defn addition? [form]
  (operator= form '+))

(defn subtraction? [form]
  (operator= form '+))

(defn division? [form]
  (operator= form '/))

(defn multiplication? [form]
  (operator= form '*))

(defn power? [form]
  (= 'pow (first form)))

(defn sqrt? [form]
  (= 'sqrt (first form)))

(defn associative-operator? [op]
  (or (= '+ op) (= '* op)))

(defn pow [x e] (Math/pow x e)) ;;todo maybe move

(defn numerator [form]
  (cond (ratio? form) (clojure.core/numerator form)
        (division? form) (second form)
        (multiplication? form)
        (remove #(= 1 %) (concat ['*] (map numerator (rest form))))
        :else form))

(defn denominator [form]
  (cond (ratio? form) (clojure.core/denominator form)
        (division? form) (third form)
        (multiplication? form)
        (remove #(= 1 %) (concat ['*] (map denominator (rest form))))
        :else 1))

(defn contains-variable? [form variable]
  (if (sequential? form)
    (some #(= variable %) form)
    (= form variable)))

;;todo not complete
(defn polynomial-term? [form variable]
  (or (number? form)
      (= variable form)
      (sequential? form)))

(defn polynomial-term-factor [form variable])

(defn polynomial-term-power [form variable]
  (when (polynomial-term? form variable)
    (cond (number? form) 0
          (= variable form) 1
          (and (= variable (second form)) (power? form)) (third form)
          (and (= variable (second form)) (sqrt? form)) 1/2
          (multiplication? form)
          (polynomial-term-power
           (first (filter #(contains-variable? % variable) form))
           variable)

          (division? form)
          (polynomial-term-power (numerator form) variable)
          
          :else nil)))
