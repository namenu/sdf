(ns sdf.ch2)

(defn compose [f g]
  (let [the-composition (fn [& args]
                          (f (apply g args)))]
    the-composition))

(defn parallel-combine [h f g]
  (let [the-combination (fn [& args]
                          (h (apply f args) (apply g args)))]
    the-combination))

(defn procedure-arity [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(def arity-table (atom {}))

(defn restrict-arity [proc nargs]
  (swap! arity-table assoc proc nargs)
  proc)

(defn get-arity [proc]
  (or (get @arity-table proc)
      (procedure-arity proc)))

(defn spread-combine [h f g]
  (let [n (get-arity f)
        m (get-arity g)
        t (+ n m)
        the-combination (fn [& args]
                          (assert (= (count args) t))
                          (h (apply f (take n args))
                             (apply g (drop n args))))]
    (restrict-arity the-combination t)))

(spread-combine list
                (fn [x y] (list 'foo x y))
                (fn [u v w] (list 'bar u v w)))

;; 2.1
(defn compose [f g]
  ;; check f and g are compatiable
  (assert (= (get-arity f) 1))
  (let [the-composition (fn [& args]
                          ;; check that it is given the gorrect number of arguments
                          (assert (= (count args) (get-arity g)))
                          (f (apply g args)))]
    ;; advertise the arity correctly for get-arity
    (restrict-arity the-composition (get-arity g))))

;; test
(let [f (compose (fn [x] (list 'foo x))
                 (fn [x y] (list 'bar x)))]
  (f 'z))

(defn parallel-combine [h f g]
  (assert (= (get-arity h) 2))
  (assert (= (get-arity f) (get-arity g)))
  (let [the-combination (fn [& args]
                          (assert (= (count args) (get-arity f)))
                          (h (apply f args) (apply g args)))]
    (restrict-arity the-combination (get-arity f))))

((parallel-combine (restrict-arity list 2)
                   (fn [x y z] (list 'foo x y z))
                   (fn [u v w] (list 'bar u v w)))
 'a 'b 'c)