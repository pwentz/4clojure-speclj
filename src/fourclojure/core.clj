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

(defn fifteen [x]
  (* 2 x))

(defn sixteen [name]
  (format "Hello, %s!" name))

(def seventeen [6 7 8])

(def eighteen [6 7])

(defn nineteen [coll]
  (if-let [the-rest (next coll)]
    (recur the-rest)
    (first coll)))

(defn twenty [coll]
  (nth coll (- (count coll) 2)))

(defn twenty-one [coll n]
  (loop [left coll i 0]
    (if (= i n)
      (first left)
      (recur (rest left) (inc i)))))

(defn twenty-two [x]
  (loop [c 1 y x]
    (if-let [n (next y)]
      (recur (inc c) n)
      c)))

(defn twenty-three [coll]
  (reduce
    (fn [reversed x]
      (cons x reversed))
    []
    coll))

(defn twenty-four [coll]
  (apply + coll))

(defn twenty-five [coll]
  (filter odd? coll))

(defn twenty-six [n]
  (letfn [(fib [x y]
            (cons x
              (lazy-seq
                (fib y (+ x y)))))]
    (take n
      (fib 1 1))))

(defn twenty-seven [coll]
  (= (seq coll) (reverse coll)))

(defn twenty-eight [coll]
  (reduce
    (fn [flat s]
      (if (coll? s)
        (concat flat (twenty-eight s))
        (concat flat [s])))
    []
    coll))

(defn twenty-nine [s]
  (clojure.string/replace s #"[^A-Z]" ""))

(defn forty [x coll]
  (drop-last
    (reduce
      (fn [new y] (apply conj new [y x])) [] coll)))

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

(def seventy-one last)

(defn seventy-two [xs] (apply + xs))

(defn seventy-three [board]
  (let [rows board
        cols [(map first board) (map second board) (map last board)]
        diag1 (map-indexed #(nth %2 %1) board)
        diag2 (map-indexed #(nth %2 %1) (reverse board))
        lines (concat [diag1 diag2] cols rows)]
    (->> lines
         (filter #(= (count (set %)) 1))
         (filter #(not= #{:e} (set %)))
         (first)
         (first))))

(defn seventy-four [string]
  (let [nums (->> (clojure.string/split string #",")
                  (map #(Integer. %)))
        square? (fn [x] (some #(= x (* % %)) (range 1 (inc x))))]
    (->> (filter square? nums)
         (clojure.string/join ","))))

(defn seventy-five [x]
  (letfn [(gcd [y]
    (reduce
      (fn [gc n]
        (if (and (= (mod x n) 0)
                 (= (mod y n) 0))
          n
          gc))
      1
      (range 1 x)))]
    (->> (range 1 (inc x))
         (filter #(= (gcd %) 1))
         count)))

(def seventy-six [1 3 5 7 9 11])

(defn seventy-seven [words]
  (letfn [(get-memo [word] (sort (clojure.string/split word #"")))]
    (->> (reduce
           (fn [anagrams word]
             (let [memo (get-memo word)]
               (if (anagrams memo)
                 (update-in anagrams [memo] #(conj % word))
                 (assoc anagrams memo #{word}))))
          {}
          words)
         vals
         (filter #(< 1 (count %)))
         set)))

(defn seventy-eight
  ([fn & args]
    (let [ret (apply fn args)]
      (if (fn? ret)
        (seventy-eight ret)
        ret)))
  ([fn]
    (let [ret (fn)]
      (if (fn? ret)
        (recur ret)
        ret))))

(defn seventy-nine
  ([triangle]
    (seventy-nine triangle 0))
  ([triangle score]
    (let [root (+ score (first (first triangle)))
          left (map drop-last (rest triangle))
          right (map #(drop 1 %) (rest triangle))]
      (if (next triangle)
        (min (seventy-nine left root) (seventy-nine right root))
        root))))

(defn eighty [n]
  (let [divisors (filter #(zero? (mod n %)) (range 1 n))
        sum (apply + divisors)]
    (= sum n)))

(defn eighty-five [s]
  (apply clojure.set/union #{s} (map #(eighty-five (disj s %)) s)))

