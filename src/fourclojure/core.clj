(ns fourclojure.core)

(def one true)

(def two 4)

(def three "HELLO WORLD")

(def four (list :a :b :c))

(def five '(1 2 3 4))

(def six [:a :b :c])

(def seven [1 2 3 4])

(def eight #{:a :b :c :d})

(def nine 2)

(def ten 20)

(def eleven [:b 2])

(def twelve 3)

(def thirteen [20 30 40])

(def fourteen 8)

(def fifteen (partial * 2))

(defn sixteen [first-name]
  (str "Hello, " first-name "!"))

(def seventeen '(6 7 8))

(def eighteen '(6 7))

(defn nineteen [coll]
  (first (reverse coll)))

(defn twenty [coll]
  (second (reverse coll)))

; get nth elt
(defn twenty-one [coll n]
  (loop [coll coll
         acc 0]
    (if (= acc n)
      (first coll)
      (recur (rest coll) (inc acc)))))

; count
(def twenty-two (partial reduce (fn [a b] (inc a)) 0))

; reverse a seq
(def twenty-three (partial reduce conj '()))

; sum of sequence
(def twenty-four (partial reduce +))

; odd numbers
(def twenty-five (partial filter odd?))

; fibonacci
(defn twenty-six [n]
  (loop [n n
         acc [1 1]]
    (if (= (count acc) n)
      acc
      (recur n (conj acc (+ (last acc) (second (reverse acc))))))))

; palindrome
(defn twenty-seven [sequ]
  (= (apply str sequ) (apply str (reverse sequ))))

; flatten array
(defn flattenize [elt]
  (cond (and (sequential? elt)
             (>= (count (filter sequential? elt)) 1)) (mapcat flattenize elt)
        (sequential? elt) elt
        :else [elt]))

; flatten array
(defn twenty-eight [coll]
  (mapcat flattenize coll))

; pull caps from string
(defn twenty-nine [phrase]
  (apply str (filter #(and (= (clojure.string/upper-case %) %)
                           (not (= (clojure.string/upper-case %)
                                   (clojure.string/lower-case %))))
                     (map str phrase))))

; remove consecutive duplicates
(def thirty dedupe)

; combine consecutive dupes into sub-list
(defn shovel-consecs [acc elt]
  (if (= (last (last acc)) elt)
    (conj (vec (drop-last acc)) (conj (last acc) elt))
    (conj acc [elt])))

; combine consecutive dupes into sub-list
(defn thirty-one [coll]
  (reduce shovel-consecs [] coll))

; duplicate elements
(defn thirty-two [coll]
  (reduce #(conj %1 %2 %2) [] coll))

; duplicate elements n times
(defn thirty-three [coll n]
  (reduce #(apply (partial conj %1) (repeat n %2)) [] coll))

; list of integers w/in given range
(defn thirty-four [start end]
  (loop [acc [start]]
    (if (= (- end start) (count acc))
      acc
      (recur (conj acc (inc (last acc)))))))

(def thirty-five 7)

(def thirty-six [7 3 1])

(def thirty-seven "ABC")

; max number
(defn thirty-eight [& numbers]
  (-> numbers
      (sort)
      (last)))

; flat-zip two collections
(defn thirty-nine [coll1 coll2]
  (mapcat vector coll1 coll2))

; interpose given separator into collection
(defn forty [sep coll]
  (->> coll
       (reduce #(conj %1 %2 sep) [])
       (drop-last)))

; drop every nth item from seq
(defn forty-one [coll n]
  (keep-indexed #(if (or (zero? %1)
                         (->> n
                              (rem (inc %1))
                              (zero?)
                              (not)))
                   %2) coll))

; factorial
(defn forty-two [n]
  (->> n
       (inc)
       (range 1)
       (reverse)
       (reduce *)))

; reverse interleave
(defn forty-three [sequ n]
  (loop [coll (partition n sequ)
        acc []]
    (if (empty? (flatten coll))
      acc
      (recur (map rest coll) (->> coll
                                  (map first)
                                  (conj acc))))))

; rotate sequence
(defn rotate [coll]
  (conj (-> coll
            (rest)
            (vec)) (first coll)))

; rotate a sequence
(defn reverse-rotate [coll]
  (conj (drop-last coll) (last coll)))

; rotate a sequence
(defn forty-four [n coll]
  (let [times (if (pos? n) n (- n))
        rotator (if (pos? n) rotate reverse-rotate)]
    (loop [acc coll
           times times]
      (if (zero? times)
        acc
        (recur (rotator acc) (dec times))))))

(def forty-five '(1 4 7 10 13))

(defn forty-six [f]
  (fn [a b] (f b a)))

(def forty-seven 4)

(def forty-eight 6)

; split sequence at n
(defn forty-nine [n coll]
  (reduce (fn [acc elt]
             (if (= (nth coll n) elt)
              (conj acc [elt])
              (conj (-> acc
                        (drop-last)
                        (vec)) (conj (last acc) elt))))
          [[]] coll))

; split set by types
(defn fifty [coll]
  (vals (group-by type coll)))

; partition

(defn fifty-four [n coll]
  (->> coll
       (reduce (fn [acc elt]
                 (if (= (-> acc
                            (last)
                            (count)) n)
                   (conj acc [elt])
                   (conj (-> acc
                             (drop-last)
                             (vec)) (conj (last acc) elt)))) [[]])
       (filter #(= (count %) n))))

; intermediate reduce (w/ lazy-seq)
(defn sixty
  ([f acc sequ]
   (let [filled? (complement empty?)]
     (cons acc (lazy-seq
                 (if (filled? sequ)
                   (sixty f (f acc (first sequ)) (rest sequ)))))))
  ([f sequ] (sixty f (first sequ) (rest sequ))))

; merge-with
(defn merge-matching-with [f accumulator elt]
  (defn merge-accu [acc b]
    (if (contains? acc b)
              (assoc acc b (f (acc b) (elt b)))
              (merge elt acc)))
  (->> elt
       (keys)
       (reduce merge-accu accumulator)))

; split sentence and sort by words (case-insensitive)
(defn seventy [phrase]
  (->> (clojure.string/split
             (->> phrase
                 (drop-last)
                 (apply str)) #" ")
       (sort-by clojure.string/lower-case)))

(def seventy-one last)

(def seventy-two (partial reduce +))

; perfect squares from comma separated integers
(defn is-sqrt? [n]
  (let [n (read-string n)]
    (some #(= (* % %) n) (range n))))

; perfect squares from comma separated integers
(defn seventy-four [comma-sep-ints]
  (->> (clojure.string/split comma-sep-ints #",")
       (filter is-sqrt?)
       (interpose ",")
       (apply str)))

; Euler's totient
(defn divisors [n]
  (->> n
       (range 1)
       (filter #(zero? (mod n %)))
       (into #{})))

; Euler's totient
(defn greatest-common-divisor [a b]
  (->> a
       (divisors)
       (clojure.set/intersection (divisors b))
       (last)))

; Euler's totient
(defn seventy-five [n]
  (if (= 1 n)
    1
    (->> n
         (range 1)
         (map (partial greatest-common-divisor n))
         (filter (partial = 1))
         (count)
         (dec))))

(def seventy-six [1 3 5 7 9 11])

; extract all anagrams in vector
(defn seventy-seven [coll]
  (letfn [(to-unicode [n] (reduce #(+ (int %1) (int %2)) n))]
    (->> coll
         (group-by to-unicode)
         (vals)
         (remove #(= 1 (count %)))
         (map set)
         (into #{}))))

; trampoline
(defn seventy-eight [f & args]
  (if-not (fn? f)
    f
    (seventy-eight (apply f args))))

; returns true if number is perfect
(defn eighty [n]
  (->> n
       (divisors)
       (reduce +)
       (= n)))

; intersection of 2 sets
(defn eighty-one [a b]
  (->> a
       (filter #(some (partial = %) b))
       (into #{})))

; create map from keywords in vector
(defn separate-keywords [acc b]
  (let [last-key (last (keys acc))
        last-val (last (last acc))]
    (if (keyword? b)
      (assoc acc b [])
      (->> b
           (conj last-val)
           (assoc acc last-key)))))

; create map from keywords in vector
(defn one-hundred-five [coll]
  (reduce separate-keywords {} coll))
