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
   '>= >=
   'list list})

(def env-base
  (atom (conj primitives {})))

(declare -eval eval-if)

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
            body (drop 2 expr)]
        (swap! env assoc name (list 'procedure params body))))))

(defn self-evaluation? [expr]
  (or
   (number? expr)
   (string? expr)
   (boolean? expr)))

(defn application? [expr] (seq? expr))

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

(defn is-procedure? [func]
  (-> func
      first
      (= 'procedure)))

(defn quoted? [expr]
  (= 'quote (first expr)))

(defn create-env [func params current-env]
  (let [symbols (second func)
        bindings (zipmap symbols params)]
    (atom (into bindings {:last-env current-env}))))

(defn -apply [func params env]
  (cond
    (is-primitive? func) (apply func params)
    (is-procedure? func) (let [new-env (create-env func params env)]
                           (-> #(-eval % new-env)
                               (map (last func))
                               last))))

(defn get-from-env [expr env]
  (let [env-deref (deref env)
        val (get env-deref expr)]
    (if (= val nil)
      (get-from-env expr (:last-env env-deref))
      val)))

(defn -eval [expr, env]
  (cond
    (self-evaluation? expr) expr
    (quoted? expr) (second expr)
    (symbol? expr) (get-from-env expr env)
    (is-definition? expr) (eval-definition expr env)
    (if? expr) (eval-if expr env)
    (application? expr) (-apply (-eval (operator expr) env) (map #(-eval % env) (operands expr)) env)))


(defn read-loop []
  (let [l (read-line)]
    (println (-eval (read-string l) env-base))))

(defn -main
  [& args]
  (read-loop))



;implementar car cdr cons display newline and lambda listas, quotes


