(ns fourclojure.core)

(def one true)

(def two 4)

(def three "HELLO WORLD")

(def four (list :a :b :c))

(def five '(1 2 3 4))

(def six [:a :b :c])

(def seven [1 2 3 4])

(defn twenty-one [coll n]
  (loop [left coll i 0]
    (if (= i n)
      (first left)
      (recur (rest left) (inc i)))))

(defn fifty-four [size coll]
  (loop [ret [] rest coll]
    (if (>= (count rest) size)
      (recur
        (conj ret (take size rest))
        (drop size rest))
      ret)))

(defn sixty
  ([f coll])
  ([f init coll]))

(defn sixty-nine [f & maps]
  (reduce
    (fn [memo map]
      (reduce
        (fn [new-memo key]
          (let [old (get memo key)
                new (get map key)]
            (if old
              (assoc new-memo key (f old new))
              (assoc new-memo key new))))
        memo
        (keys map)))
    {}
    maps))

