(ns fourclojure.core)

(def one true)

(def two 4)

(def three "HELLO WORLD")

(def four (list :a :b :c))

(def five '(1 2 3 4))

(def six [:a :b :c])

(def seven [1 2 3 4])

(defn twenty [coll]
  (nth coll (- (count coll) 2)))

(defn forty [x coll]
  (drop-last
    (reduce
      (fn [new y] (apply conj new [y x])) [] coll)))

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
  ([f coll]
    (lazy-seq
      (if (seq coll)
        (sixty f (first coll) (rest coll))
        [(f)])))
  ([f init coll]
    (cons init
      (lazy-seq
        (when (seq coll)
          (sixty f (f init (first coll)) (rest coll)))))))

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

(defn seventy [sentence]
  (let [words (-> sentence
                (clojure.string/replace #"[\.!]" "")
                (clojure.string/split #"\s"))]
    (sort-by clojure.string/lower-case words)))

(defn eighty [n]
  (let [divisors (filter #(zero? (mod n %)) (range 1 n))
        sum (apply + divisors)]
    (= sum n)))

(defn eighty-five [s]
  (apply clojure.set/union #{s} (map #(eighty-five (disj s %)) s)))

