(ns fourclojure.core)

(defn fifty-four [size coll])

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

