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
    (if (#{n} acc)
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
    (if (#{n} (count acc))
      acc
      (recur n (conj acc (+ (last acc) (second (reverse acc))))))))

; palindrome
(defn twenty-seven [sequ]
  (let [stringify (partial apply str)]
    (= ((comp stringify reverse) sequ) (stringify sequ))))

; flatten array
(defn flattenize [elt]
  (cond (and (sequential? elt)
             (#{1} ((comp count (partial filter sequential?)) elt))) (mapcat flattenize elt)
        (sequential? elt) elt
        :else [elt]))

; flatten array
(def twenty-eight (partial mapcat flattenize))

; pull caps from string
(defn twenty-nine [phrase]
  (apply str (filter #(and (#{%} (clojure.string/upper-case %))
                           (not= (clojure.string/upper-case %)
                                 (clojure.string/lower-case %)))
                     (map str phrase))))

; remove consecutive duplicates
(def thirty dedupe)

; combine consecutive dupes into sub-list
(defn shovel-consecs [acc elt]
  (if (#{elt} ((comp last last) acc))
    (conj ((comp vec drop-last) acc) (conj (last acc) elt))
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

; ; max number
(defn thirty-eight [& numbers]
  ((comp last sort) numbers))


; flat-zip two collections
(def thirty-nine (partial mapcat vector))

; ; interpose given separator into collection
(defn forty [sep coll]
  ((comp drop-last (partial reduce #(conj %1 %2 sep) [])) coll))


; drop every nth item from seq
(defn forty-one [coll n]
  (keep-indexed #(if (or (zero? %1)
                         (->> n
                              (rem (inc %1))
                              ((complement zero?))))
                   %2) coll))

; factorial
(def forty-two (comp (partial reduce *) reverse (partial range 1) inc))

; reverse interleave
(defn forty-three [sequ n]
  (loop [coll (partition n sequ)
        acc []]
    (if ((comp empty? flatten) coll)
      acc
      (recur (map rest coll) (->> coll
                                  (map first)
                                  (conj acc))))))

; rotate sequence
(defn rotate [coll]
  (conj ((comp vec rest) coll) (first coll)))

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
             (if (#{elt} (nth coll n))
              (conj acc [elt])
              (conj ((comp vec drop-last) acc) (conj (last acc) elt))))
          [[]] coll))

; split set by types
(def fifty (comp vals (partial group-by type)))

; partition
(defn fifty-four [n coll]
  (->> coll
       (reduce (fn [acc elt]
                 (if ((comp #{n} count last) acc)
                     (conj acc [elt])
                     (conj ((comp vec drop-last) acc) (conj (last acc) elt)))) [[]])
       (filter (comp #{n} count))))

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
             ((comp (partial apply str) drop-last) phrase) #" ")
       (sort-by clojure.string/lower-case)))

(def seventy-one last)

(def seventy-two (partial reduce +))

; perfect squares from comma separated integers
(defn is-sqrt? [n]
  (let [n (read-string n)
        sq (fn [n] (* n n))]
    (some (comp #{n} sq) (range n))))

; perfect squares from comma separated integers
(def seventy-four
  (comp
    (partial apply str)
    (partial interpose ",")
    (partial filter is-sqrt?)
    #(clojure.string/split % #",")))

; Euler's totient
(defn divisors [n]
  (->> n
       (range 1)
       (filter (comp zero? (partial mod n)))
       (into #{})))

; Euler's totient
(defn greatest-common-divisor [a b]
  ((comp
     last
     (partial clojure.set/intersection (divisors b))
     divisors)
   a))


; Euler's totient
(defn seventy-five [n]
  (if (= 1 n)
    1
    ((comp
       dec
       count
       (partial filter #{1})
       (partial map (partial greatest-common-divisor n))
       (partial range 1))
     n)))

(def seventy-six [1 3 5 7 9 11])

; extract all anagrams in vector
(def seventy-seven
  (letfn [(to-unicode [n] (reduce #(+ (int %1) (int %2)) n))]
    (comp
      (partial into #{})
      (partial map set)
      (partial remove (comp #{1} count))
      vals
      (partial group-by to-unicode))))

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
