(ns lisp-interpreter.core
  (:gen-class))

(def primitives
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

(def env-base
  (atom (conj primitives {})))

(declare -eval eval-if)


;(defn compound-)

(defn is-definition? [expr]
  (->
   expr
   first
   (= 'define)))

(defn eval-definition [expr, env]
  (let [definition (second expr)]
    (cond
      (symbol? definition) (swap! env assoc definition (last expr))
      :else
      (let [name (first definition)
            params (rest definition)
            body (last expr)]
        (swap! env assoc name (list 'procedure params body))))))

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

(defn is-primitive? [expr]
  (->
   primitives
   vals
   set
   (contains? expr)))

(defn -apply [func params]
  (cond
    (is-primitive? func) (func params)))

(defn -eval [expr, env]
  (println (deref env))
  (cond
    (self-evaluation? expr) expr
    (symbol? expr) (get (deref env) expr)
    (is-definition? expr) (eval-definition expr env)
    (if? expr) (eval-if expr env)
    (application? expr) (apply (-eval (operator expr) env) (map #(-eval % env) (operands expr)))))


(defn read-loop []
  (let [l (read-line)]
    (println (-eval (read-string l) env-base))))

(defn -main
  [& args]
  (read-loop))






