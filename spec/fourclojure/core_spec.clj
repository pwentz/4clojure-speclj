(ns fourclojure.core-spec
  (:require [speclj.core :refer :all]
            [fourclojure.core :refer :all]))

(defn build-redefs [redefs fn-name]
  (concat redefs [fn-name
                  `(fn [& args#]
                    (-fail (format "%s is not allowed." ~(str fn-name))))]))

(defmacro with-restrictions [fns & body]
  (let [redefs (reduce build-redefs [] fns)]
    `(with-redefs ~redefs
       ~@body)))

(describe "#1"
  (it "This is a clojure form. Enter a value which will make the form evaluate
       to true. Don't over think it! If you are confused, see the getting
       started page. Hint: true is equal to true."

    (should= true one)))

(describe "#2"
  (it "If you are not familiar with polish notation, simple arithmetic might
       seem confusing.Note: Enter only enough to fill in the blank (in this case,
       a single number) - do not retype the whole problem."

    (should= (- 10 (* 2 3)) two)))

(describe "#3"
  (it "Clojure strings are Java strings. This means that you can use any of the
       Java string methods on Clojure strings."

    (should= (.toUpperCase "hello world") three)))

(describe "#4"
  (it "Lists can be constructed with either a function or a quoted form."

    (should= '(:a :b :c) four)))

(describe "#5"
  (it "When operating on a list, the conj function will return a new list
       with one or more items 'added' to the front."

    (should= (conj '(2 3 4) 1) five)
    (should= (conj '(3 4) 2 1) five)))

(describe "#6"
  (it "Vectors can be constructed several ways. You can compare them with lists."

    (should (= (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c) six))))

(describe "#7"
  (it "When operating on a Vector, the conj function will return a new vector with
       one or more items 'added' to the end."

    (should= (conj [1 2 3] 4) seven)
    (should= (conj [1 2] 3 4) seven)))

(describe "#20"
  (it "Write a function which returns the second to last element from a sequence."

    (should= 4 (twenty (list 1 2 3 4 5)))
    (should= "b" (twenty ["a" "b" "c"]))
    (should= [1 2] (twenty [[1 2] [3 4]]))))

(describe "#21"
  (it "Write a function which returns the Nth element from a sequence."

    (with-restrictions [nth]

      (should= 6 (twenty-one '(4 5 6 7) 2))
      (should= :a (twenty-one [:a :b :c] 0))
      (should= 2 (twenty-one [1 2 3 4] 1))
      (should= [5 6] (twenty-one '([1 2] [3 4] [5 6]) 2)))))

(describe "#40"
  (it "Write a function which separates the items of a sequence by an arbitrary value."

    (with-restrictions [interpose]

      (should= [1 0 2 0 3] (forty 0 [1 2 3]))
      (should= "one, two, three" (apply str (forty ", " ["one" "two" "three"])))
      (should= [:a :z :b :z :c :z :d] (forty :z [:a :b :c :d])))))

(describe "#54"
  (it "Write a function which returns a sequence of lists of x items each. Lists
       of less than x items should not be returned."

    (with-restrictions [partition partition-all]

      (should=
        '((0 1 2) (3 4 5) (6 7 8))
        (fifty-four 3 (range 9)))

      (should=
        '((0 1) (2 3) (4 5) (6 7))
        (fifty-four 2 (range 8)))

      (should=
        '((0 1 2) (3 4 5))
        (fifty-four 3 (range 8))))))

(describe "#60"
  (it "Write a function which behaves like reduce, but returns each intermediate value
       of the reduction. Your function must accept either two or three arguments, and
       the return sequence must be lazy."

    (with-restrictions [reductions]

      (should=
        [0 1 3 6 10]
        (take 5 (sixty + (range))))

      (should=
        [[1] [1 2] [1 2 3] [1 2 3 4]]
        (sixty conj [1] [2 3 4]))

      (should=
        (reduce * 2 [3 4 5])
        120
        (last (sixty * 2 [3 4 5]))))))

(describe "#69"
  (it "Write a function which takes a function f and a variable number of maps.
       Your function should return a map that consists of the rest of the maps
       conj-ed onto the first. If a key occurs in more than one map, the mapping(s)
       from the latter (left-to-right) should be combined with the mapping in the
       result by calling (f val-in-result val-in-latter)"

    (with-restrictions [merge-with]

      (should=
        {:a 4, :b 6, :c 20}
        (sixty-nine * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}))

      (should=
        {1 7, 2 10, 3 15}
        (sixty-nine - {1 10, 2 20} {1 3, 2 10, 3 15}))

      (should=
        {:a [3 4 5], :b [6 7], :c [8 9]}
        (sixty-nine concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})))))

(describe "#70"
  (it "Write a function that splits a sentence up into a sorted list of words.
       Capitalization should not affect sort order and punctuation should be ignored."

    (should= ["a" "day" "Have" "nice"] (seventy "Have a nice day."))
    (should= ["a" "Clojure" "fun" "is" "language"] (seventy "Clojure is a fun language!"))
    (should= ["fall" "follies" "foolish" "Fools" "for"] (seventy "Fools fall for foolish follies."))))

(describe "#80"
  (it "A number is 'perfect' if the sum of its divisors equal the number itself. 6 is a
       perfect number because 1+2+3=6. Write a function which returns true for perfect numbers
       and false otherwise."

    (should= true (eighty 6))
    (should= false (eighty 7))
    (should= true (eighty 496))
    (should= false (eighty 500))
    (should= true (eighty 8128))))

(describe "#85"
  (it "Write a function which generates the power set of a given set. The power set of a
       set x is the set of all subsets of x, including the empty set and x itself."

    (should= #{#{1 :a} #{:a} #{} #{1}} (eighty-five #{1 :a}))
    (should= #{#{}} (eighty-five #{}))
    (should= #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}} (eighty-five #{1 2 3}))
    ;(should= 1024 (count (eighty-five (into #{} (range 10)))))
))

