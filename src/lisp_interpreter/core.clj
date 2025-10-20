(ns lisp-interpreter.core
  (:gen-class))

(def env-base
  {'+ +
   '- -
   '* *
   '/ /
   '= =
   'not not
   '< <
   '> >
   '<= <=
   '>= >=})

(declare -eval eval-if)

(defn is-definition? [expr]
  (= expr 'define))

(defn eval-definition [expr])

(defn self-evaluation? [expr]
  (or
   (number? expr)
   (string? expr)))

(defn application? [expr]
  (and (seq? expr) (symbol? (first expr))))

(defn operator [expr]
  (first expr))

(defn operands [expr]
  (rest expr))

(defn if? [expr]
  (= (operator expr) 'if))

(defn eval-if [expr env]
  (if (-eval (nth expr 1) env)
    (-eval (nth expr 2) env)
    (-eval (nth expr 3) env)))

(defn variable? [expr]
  (symbol? expr))




(defn -eval [expr, env]
  (println expr)
  (cond
    (self-evaluation? expr) expr
    (variable? expr) (get env expr)
    ;(is-definition? expr) (eval-definition expr)
    (if? expr) (eval-if expr env)
    (application? expr) (apply (-eval (operator expr) env) (map #(-eval % env) (operands expr)))))


(defn read-loop []
  (let [l (read-line)]
    (-eval (read-string l) env-base)))

(defn -main
  [& args]
  (read-loop))






