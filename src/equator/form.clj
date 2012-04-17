(ns equator.form
  (:refer-clojure :exclude [numerator denominator])
  (:use equator.core))

;;power:       (pow x exp)
;;square root: (sqrt x)
;;fraction:    (frac numerator denominator)
;;subscript    (sub x s)
;;not equals:  not=

(defn op= [form op]
  (and (sequential? form) (= (first form) op)))

(defn operator [form]
  (first form))

(defn operands [form]
  (when (sequential? form) (rest form)))

(defn equation? [form]
  (and (sequential? form)
       (or (op= form '=)
           (op= form 'not=))))

(defn addition? [form]
  (op= form '+))

(defn subtraction? [form]
  (op= form '+))

(defn division? [form]
  (op= form '/))

(defn fraction? [form]
  (op= form 'frac))

(defn multiplication? [form]
  (op= form '*))

(defn power? [form]
  (op= form 'pow))

(defn sqrt? [form]
  (op= form 'sqrt))

(defn subscript? [form]
  (op= form 'sub))

(defn associative-operator? [op]
  (or (= '+ op) (= '* op)))

(defn pow [x e] (Math/pow x e)) ;;todo maybe move

(defn numerator [form]
  (cond (ratio? form) (clojure.core/numerator form)
        (division? form) (second form)
        (fraction? form) (second form)
        (multiplication? form)
        (remove #(= 1 %) (concat ['*] (map numerator (rest form))))
        :else form))

(defn denominator [form]
  (cond (ratio? form) (clojure.core/denominator form)
        (division? form) (third form)
        (fraction? form) (third form)
        (multiplication? form)
        (remove #(= 1 %) (concat ['*] (map denominator (rest form))))
        :else 1))

(defn contains-variable? [form variable]
  (if (sequential? form)
    (some #(= variable %) form)
    (= form variable)))

;;todo not complete
(defn poly-term? [form variable]
  (or (number? form)
      (= variable form)
      (sequential? form)))

(defn term-factor [form variable]
  (when (poly-term? form variable)
    (cond (number? form) form
          (= variable form) 1
          (multiplication? form)
            (first (filter #(not (contains-variable? % variable)) (rest form)))
          (or (division? form) (fraction? form))
            (list '/
                  (term-factor (numerator form) variable)
                  (denominator form))
          :else 1)))

(defn term-power [form variable]
  (when (poly-term? form variable)
    (cond (number? form) 0
          (= variable form) 1
          (and (power? form) (= variable (second form))) (third form)
          (and (sqrt? form) (= variable (second form))) 1/2
          (multiplication? form)
            (term-power
             (first (filter #(contains-variable? % variable) form))
             variable)
          (or (division? form) (fraction? form))
            (term-power (numerator form) variable)
          :else nil)))

(defn term-without-factor [form variable]
  (when (poly-term? form variable)
    (cond (or (number? form) (= variable form)) 1
          (or (power? form) (sqrt? form)) form
          (multiplication? form)
            (first (filter #(contains-variable? % variable) form))
          (division? form) (term-without-factor (numerator form) variable)
          :else nil)))
