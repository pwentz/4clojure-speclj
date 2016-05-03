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

(describe "#8"
  (it "Sets are collections of unique values."

    (should= (set '(:a :a :b :c :c :c :c :d :d)) eight)
    (should= (clojure.set/union #{:a :b :c} #{:b :c :d}) eight)))

(describe "#9"
  (it "When operating on a set, the conj function returns a new set
       with one or more keys 'added'."

    (should= #{1 2 3 4} (conj #{1 4 3} nine))))

(describe "#10"
  (it "Maps store key-value pairs. Both maps and keywords can be used
       as lookup functions. Commas can be used to make maps more readable,
       but they are not required."

    (should= ((hash-map :a 10, :b 20, :c 30) :b) ten)
    (should= (:b {:a 10, :b 20, :c 30}) ten)))

(describe "#11"
  (it "When operating on a map, the conj function returns a new map with
       one or more key-value pairs 'added'."

    (should= {:a 1, :b 2, :c 3} (conj {:a 1} eleven [:c 3]))))

(describe "#12"
  (it "All Clojure collections support sequencing. You can operate on
       sequences with functions like first, second, and last."

    (should= (first '(3 2 1)) twelve)
    (should= (second [2 3 4]) twelve)
    (should= (last (list 1 2 3)) twelve)))

(describe "#13"
  (it "The rest function will return all the items of a sequence except the first."

    (should= (rest [10 20 30 40]) thirteen)))

(describe "#14"
  (it "Clojure has many different ways to create functions."

    (should= ((fn add-five [x] (+ x 5)) 3) fourteen)
    (should= ((fn [x] (+ x 5)) 3) fourteen)
    (should= (#(+ % 5) 3) fourteen)
    (should= ((partial + 5) 3) fourteen)))

(describe "#15"
  (it "Write a function which doubles a number."

    (should= 4 (fifteen 2))
    (should= 6 (fifteen 3))
    (should= 22 (fifteen 11))
    (should= 14 (fifteen 7))))

(describe "#16"
  (it "Write a function which returns a personalized greeting."

    (should= "Hello, Dave!" (sixteen "Dave"))
    (should= "Hello, Jenn!" (sixteen "Jenn"))
    (should= "Hello, Rhea!" (sixteen "Rhea"))))

(describe "#17"
  (it "The map function takes two arguments: a function (f) and a sequence (s).
       Map returns a new sequence consisting of the result of applying f to each
       item of s. Do not confuse the map function with the map data structure."

    (should= (map #(+ % 5) '(1 2 3)) seventeen)))

(describe "#18"
  (it "The filter function takes two arguments: a predicate function (f) and
       a sequence (s). Filter returns a new sequence consisting of all the items
       of s for which (f item) returns true."

    (should= (filter #(> % 5) '(3 4 5 6 7)) eighteen)))

(describe "#19"
  (it "Write a function which returns the last element in a sequence."

    (with-restrictions [last]

      (should= 5 (nineteen [1 2 3 4 5]))
      (should= 3 (nineteen '(5 4 3)))
      (should= "d" (nineteen ["b" "c" "d"])))))


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

(describe "#22"
  (it "Write a function which returns the total number of elements in a sequence."

    (with-restrictions [count]

      (should= 5 (twenty-two '(1 2 3 3 1)))
      (should= 11 (twenty-two "Hello World"))
      (should= 3 (twenty-two [[1 2] [3 4] [5 6]]))
      (should= 1 (twenty-two '(13)))
      (should= 3 (twenty-two '(:a :b :c))))))

(describe "#23"
  (it "Write a function which reverses a sequence."

    (with-restrictions [reverse rseq]

      (should= [5 4 3 2 1] (twenty-three [1 2 3 4 5]))
      (should= '(7 5 2) (twenty-three (sorted-set 5 7 2 7)))
      (should= [[5 6][3 4][1 2]] (twenty-three [[1 2][3 4][5 6]])))))

(describe "#24"
  (it "Write a function which returns the sum of a sequence of numbers."

    (should= 6 (twenty-four [1 2 3]))
    (should= 8 (twenty-four (list 0 -2 5 5)))
    (should= 7 (twenty-four #{4 2 1}))
    (should= -1 (twenty-four '(0 0 -1)))
    (should= 14 (twenty-four '(1 10 3)))))

(describe "#25"
  (it "Write a function which returns only the odd numbers from a sequence."

    (should= '(1 3 5) (twenty-five #{1 2 3 4 5}))
    (should= '(1) (twenty-five [4 2 1 6]))
    (should= '(1 1 1 3) (twenty-five [1 1 1 3]))))

(describe "#26"
  (it "Write a function which returns the first X fibonacci numbers."

    (should= '(1 1 2) (twenty-six 3))
    (should= '(1 1 2 3 5 8) (twenty-six 6))
    (should= '(1 1 2 3 5 8 13 21) (twenty-six 8))))

(describe "#27"
  (it "Write a function which returns true if the given sequence is a palindrome.
       Hint: 'racecar' does not equal '(r a c e c a r)"

    (should= false (twenty-seven '(1 2 3 4 5)))
    (should= true (twenty-seven "racecar"))
    (should= true (twenty-seven [:foo :bar :foo]))
    (should= true (twenty-seven '(1 1 3 3 1 1)))
    (should= false (twenty-seven '(:a :b :c)))))

(describe "#28"
  (it "Write a function which flattens a sequence."

    (with-restrictions [flatten]

      (should= '(1 2 3 4 5 6) (twenty-eight '((1 2) 3 [4 [5 6]])))
      (should= '("a" "b" "c") (twenty-eight ["a" ["b"] "c"]))
      (should= '(:a) (twenty-eight '((((:a)))))))))

(describe "#29"
  (it "Write a function which takes a string and returns a new string containing
       only the capital letters."

    (should= "HLOWRD" (twenty-nine "HeLlO, WoRlD!"))
    (should (empty? (twenty-nine "nothing")))
    (should= "AZ" (twenty-nine "$#A(*&987Zf"))))

(describe "#30"
  (it "Write a function which removes consecutive duplicates from a sequence."

    (should= "Leroy" (apply str (thirty "Leeeeeerrroyyy")))
    (should= '(1 2 3 2 3) (thirty [1 1 2 3 3 2 2 3]))
    (should= '([1 2] [3 4] [1 2]) (thirty [[1 2] [1 2] [3 4] [1 2]]))))

(describe "#31"
  (it "Write a function which packs consecutive duplicates into sub-lists."

    (should= '((1 1) (2) (1 1 1) (3 3)) (thirty-one [1 1 2 1 1 1 3 3]))
    (should= '((:a :a) (:b :b) (:c)) (thirty-one [:a :a :b :b :c]))
    (should= '(([1 2] [1 2]) ([3 4])) (thirty-one [[1 2] [1 2] [3 4]]))))

(describe "#32"
  (it "Write a function which duplicates each element of a sequence."

    (should= '(1 1 2 2 3 3) (thirty-two [1 2 3]))
    (should= '(:a :a :a :a :b :b :b :b) (thirty-two [:a :a :b :b]))
    (should= '([1 2] [1 2] [3 4] [3 4]) (thirty-two [[1 2] [3 4]]))
    (should= '([1 2] [1 2] [3 4] [3 4]) (thirty-two [[1 2] [3 4]]))))

(describe "#33"
  (it "Write a function which replicates each element of a sequence a variable number of times."

    (should= '(1 1 2 2 3 3) (thirty-three [1 2 3] 2))
    (should= '(:a :a :a :a :b :b :b :b) (thirty-three [:a :b] 4))
    (should= '(4 5 6) (thirty-three [4 5 6] 1))
    (should= '([1 2] [1 2] [3 4] [3 4]) (thirty-three [[1 2] [3 4]] 2))
    (should= [44 44 33 33] (thirty-three [44 33] 2))))

(describe "#34"
  (it "Write a function which creates a list of all integers in a given range."

    (with-restrictions [range]

      (should= '(1 2 3) (thirty-four 1 4))
      (should= '(-2 -1 0 1) (thirty-four -2 2))
      (should= '(5 6 7) (thirty-four 5 8)))))

(describe "#35"
  (it "Clojure lets you give local names to values using the special let-form."

    (should= (let [x 5] (+ 2 x)) thirty-five)
    (should= (let [x 3 y 10] (- y x)) thirty-five)
    (should= (let [x 21] (let [y 3] (/ x y))) thirty-five)))

(describe "#36"
  (it "Can you bind x, y, and z so that these are all true?"

    (let [x (thirty-six 0)
          y (thirty-six 1)
          z (thirty-six 2)]

      (should= 10 (+ x y))
      (should= 4 (+ y z))
      (should= 1 z))))

(describe "#37"
  (it "Regex patterns are supported with a special reader macro."

    (should= (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")) thirty-seven)))

(describe "#38"
  (it "Write a function which takes a variable number of parameters and returns the maximum value."

    (with-restrictions [max max-key]

      (should= 8 (thirty-eight 1 8 3 4))
      (should= 30 (thirty-eight 30 20))
      (should= 67 (thirty-eight 45 67 11)))))

(describe "#39"
  (it "Write a function which takes two sequences and returns the first item from each, then
       the second item from each, then the third, etc."

    (with-restrictions [interleave]

      (should= '(1 :a 2 :b 3 :c) (thirty-nine [1 2 3] [:a :b :c]))
      (should= '(1 3 2 4) (thirty-nine [1 2] [3 4 5 6]))
      (should= [1 5] (thirty-nine [1 2 3 4] [5]))
      (should= [30 25 20 15] (thirty-nine [30 20] [25 15])))))

(describe "#40"
  (it "Write a function which separates the items of a sequence by an arbitrary value."

    (with-restrictions [interpose]

      (should= [1 0 2 0 3] (forty 0 [1 2 3]))
      (should= "one, two, three" (apply str (forty ", " ["one" "two" "three"])))
      (should= [:a :z :b :z :c :z :d] (forty :z [:a :b :c :d])))))

(describe "#41"
  (it "Write a function which drops every Nth item from a sequence."

    (should= [1 2 4 5 7 8] (forty-one [1 2 3 4 5 6 7 8] 3))
    (should= [:a :c :e] (forty-one [:a :b :c :d :e :f] 2))
    (should= [1 2 3 5 6] (forty-one [1 2 3 4 5 6] 4))))

(describe "#42"
  (it "Write a function which calculates factorials."

    (should= 1 (forty-two 1))
    (should= 6 (forty-two 3))
    (should= 120 (forty-two 5))
    (should= 40320 (forty-two 8))))

(describe "#43"
  (it "Write a function which reverses the interleave process into x number of subsequences."

    (should= '((1 3 5) (2 4 6))
              (forty-three [1 2 3 4 5 6] 2))

    (should= '((0 3 6) (1 4 7) (2 5 8))
              (forty-three (range 9) 3))

    (should= '((0 5) (1 6) (2 7) (3 8) (4 9))
              (forty-three (range 10) 5))))

(describe "#44"
  (it "Write a function which can rotate a sequence in either direction."

    (should= '(3 4 5 1 2) (forty-four 2 [1 2 3 4 5]))
    (should= '(4 5 1 2 3) (forty-four -2 [1 2 3 4 5]))
    (should= '(2 3 4 5 1) (forty-four 6 [1 2 3 4 5]))
    (should= '(:b :c :a) (forty-four 1 '(:a :b :c)))
    (should= '(:c :a :b) (forty-four -4 '(:a :b :c)))))

(describe "#45"
  (it "The iterate function can be used to produce an infinite lazy sequence."

    (should= (take 5 (iterate #(+ 3 %) 1)) forty-five)))

(describe "#46"
  (it "Write a higher-order function which flips the order of the arguments of an input function."

    (should= 3 ((forty-six nth) 2 [1 2 3 4 5]))
    (should= true ((forty-six >) 7 8))
    (should= 4 ((forty-six quot) 2 8))
    (should= [1 2 3] ((forty-six take) [1 2 3 4 5] 3))))

(describe "#47"
  (it "The contains? function checks if a KEY is present in a given collection.
       This often leads beginner clojurians to use it incorrectly with numerically
       indexed collections like vectors and lists."

    (should (contains? #{4 5 6} forty-seven))
    (should (contains? [1 1 1 1 1] forty-seven))
    (should (contains? {4 :a 2 :b} forty-seven))))

(describe "#48"
  (it "The some function takes a predicate function and a collection. It returns the
       first logical true value of (predicate x) where x is an item in the collection."

    (should= (some #{2 7 6} [5 6 7 8]) forty-eight)
    (should= (some #(when (even? %) %) [5 6 7 8]) forty-eight)))

(describe "#49"
  (it "Write a function which will split a sequence into two parts."

    (with-restrictions [split-at]

      (should= [[1 2 3] [4 5 6]] (forty-nine 3 [1 2 3 4 5 6]))
      (should= [[:a] [:b :c :d]] (forty-nine 1 [:a :b :c :d]))
      (should= [[[1 2] [3 4]] [[5 6]]] (forty-nine 2 [[1 2] [3 4] [5 6]])))))

(describe "#50"
  (it "Write a function which takes a sequence consisting of items with
       different types and splits them up into a set of homogeneous sub-sequences.
       The internal order of each sub-sequence should be maintained, but the sub-sequences
       themselves can be returned in any order (this is why 'set' is used in the test cases)."

    (should= #{[1 2 3] [:a :b :c]} (set (fifty [1 :a 2 :b 3 :c])))
    (should= #{[:a :b] ["foo" "bar"]} (set (fifty [:a "foo"  "bar" :b])))
    (should= #{[[1 2] [3 4]] [:a :b] [5 6]} (set (fifty [[1 2] :a [3 4] 5 6 :b])))))

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

(describe "#71"
  (it "The -> macro threads an expression x through a variable number of forms.
       First, x is inserted as the second item in the first form, making a list of it if
       it is not a list already. Then the first form is inserted as the second item in the
       second form, making a list of that form if necessary. This process continues for all
       the forms. Using -> can sometimes make your code more readable."

    (should=
      5
      (seventy-one (sort (rest (reverse [2 5 4 1 3 6]))))
      (-> [2 5 4 1 3 6]
          (reverse)
          (rest)
          (sort)
          (seventy-one)))))

(describe "#72"
  (it "The ->> macro threads an expression x through a variable number of forms. First,
       x is inserted as the last item in the first form, making a list of it if it is not
       a list already. Then the first form is inserted as the last item in the second form,
       making a list of that form if necessary. This process continues for all the forms.
       Using ->> can sometimes make your code more readable."

    (should=
      11
      (seventy-two (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
      (->> [2 5 4 1 3 6]
           (drop 2)
           (take 3)
           (map inc)
           (seventy-two)))))

(describe "#73"
  (it "A tic-tac-toe board is represented by a two dimensional vector. X is represented
       by :x, O is represented by :o, and empty is represented by :e. A player wins by
       placing three Xs or three Os in a horizontal, vertical, or diagonal row. Write
       a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if
       O has won, and nil if neither player has won."

    (should=
      nil
      (seventy-three [[:e :e :e]
                      [:e :e :e]
                      [:e :e :e]]))
    (should=
      :x
      (seventy-three [[:x :e :o]
                      [:x :e :e]
                      [:x :e :o]]))
    (should=
      :o
      (seventy-three [[:e :x :e]
                      [:o :o :o]
                      [:x :e :x]]))
    (should=
      nil
      (seventy-three [[:x :e :o]
                      [:x :x :e]
                      [:o :x :o]]))
    (should=
      :x
      (seventy-three [[:x :e :e]
                      [:o :x :e]
                      [:o :e :x]]))
    (should=
      :o
      (seventy-three [[:x :e :o]
                      [:x :o :e]
                      [:o :e :x]]))
    (should=
      nil
      (seventy-three [[:x :o :x]
                      [:x :o :x]
                      [:o :x :o]]))))

(describe "#74"
  (it "Given a string of comma separated integers, write a function which returns a new
       comma separated string that only contains the numbers which are perfect squares."

    (should= "4,9" (seventy-four "4,5,6,7,8,9"))
    (should= "16,25,36" (seventy-four "15,16,25,36,37"))))

(describe "#75"
  (it "Two numbers are coprime if their greatest common divisor equals 1.
       Euler's totient function f(x) is defined as the number of positive integers
       less than x which are coprime to x. The special case f(1) equals 1. Write a function which
       calculates Euler's totient function."

    (should= 1 (seventy-five 1))
    (should= (count '(1 3 7 9)) 4 (seventy-five 10))
    (should= 16 (seventy-five 40))
    (should= 60 (seventy-five 99))))

(describe "#76"
  (it "The trampoline function takes a function f and a variable number of parameters.
       Trampoline calls f with any parameters that were supplied. If f returns a function,
       trampoline calls that function with no arguments. This is repeated, until the return
       value is not a function, and then trampoline returns that non-function value. This is
       useful for implementing mutually recursive algorithms in a way that won't consume the stack."

    (should=
     (letfn
       [(foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                     x
                     #(foo x (+ 2 y))))]
       (trampoline foo [] 1))
      seventy-six)))

(describe "#77"
  (it "Write a function which finds all the anagrams in a vector of words. A word x is an
       anagram of word y if all the letters in x can be rearranged in a different order to
       form y. Your function should return a set of sets, where each sub-set is a group of
       words which are anagrams of each other. Each sub-set should have at least two words.
       Words without any anagrams should not be included in the result."

    (should=
      #{#{"meat" "team" "mate"}}
      (seventy-seven ["meat" "mat" "team" "mate" "eat"]))
    (should=
      #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}
      (seventy-seven ["veer" "lake" "item" "kale" "mite" "ever"]))))

(describe "#78"
  (it "Reimplement the function described in 76"

    (with-restrictions [trampoline]

      (should= 82
        (letfn [(triple [x] #(sub-two (* 3 x)))
                (sub-two [x] #(stop? (- x 2)))
                (stop? [x] (if (> x 50) x #(triple x)))]
          (seventy-eight triple 2)))

      (should= [true false true false true false]
        (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
          (map (partial seventy-eight my-even?) (range 6)))))))

(describe "#79"
  (it "Write a function which calculates the sum of the minimal path through a triangle.
       The triangle is represented as a collection of vectors. The path should start at
       the top of the triangle and move to an adjacent number on the next row until the
       bottom of the triangle is reached."

    (should= 7
      (seventy-nine '([1]
                     [2 4]
                    [5 1 4]
                   [2 3 4 5]))) ; 1->2->1->3
    (should= 20
      (seventy-nine '([3]
                     [2 4]
                    [1 9 3]
                   [9 9 2 4]
                  [4 6 6 7 8]
                 [5 7 3 5 1 4]))))) ; 3->4->3->2->7->1

(describe "#80"
  (it "A number is 'perfect' if the sum of its divisors equal the number itself. 6 is a
       perfect number because 1+2+3=6. Write a function which returns true for perfect numbers
       and false otherwise."

    (should= true (eighty 6))
    (should= false (eighty 7))
    (should= true (eighty 496))
    (should= false (eighty 500))
    (should= true (eighty 8128))))

(describe "#81"
  (it "Write a function which returns the intersection of two sets.
       The intersection is the sub-set of items that each set has in common."

    (with-restrictions [clojure.set/intersection]

      (should= #{2 3} (eighty-one #{0 1 2 3} #{2 3 4 5}))
      (should= #{} (eighty-one #{0 1 2} #{3 4 5}))
      (should= #{:a :c :d} (eighty-one #{:a :b :c :d} #{:c :e :a :f :d})))))

(describe "#82"
  (it "A word chain consists of a set of words ordered so that each word differs by
       only one letter from the words directly before and after it. The one letter
       difference can be either an insertion, a deletion, or a substitution. Here is an example word chain:
       cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
       Write a function which takes a sequence of words, and returns true if they can be
       arranged into one continous word chain, and false if they cannot."

    (should (eighty-two? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
    (should-not (eighty-two? #{"cot" "hot" "bat" "fat"}))
    (should-not (eighty-two? #{"to" "top" "stop" "tops" "toss"}))
    (should (eighty-two? #{"spout" "do" "pot" "pout" "spot" "dot"}))
    (should (eighty-two? #{"share" "hares" "shares" "hare" "are"}))
    (should-not (eighty-two? #{"share" "hares" "hare" "are"}))))


(describe "#85"
  (it "Write a function which generates the power set of a given set. The power set of a
       set x is the set of all subsets of x, including the empty set and x itself."

    (should= #{#{1 :a} #{:a} #{} #{1}} (eighty-five #{1 :a}))
    (should= #{#{}} (eighty-five #{}))
    (should= #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}} (eighty-five #{1 2 3}))
    #_(should= 1024 (count (eighty-five (into #{} (range 10)))))))

(describe "#86"
  (it "Happy numbers are positive integers that follow a particular formula:
       take each individual digit, square it, and then sum the squares
       to get a new number. Repeat with the new number and eventually,
       you might get to a number whose squared sum is 1. This is
       a happy number. An unhappy number (or sad number) is one that loops
       endlessly. Write a function that determines if a number is happy or not."
    (should= true (eighty-six 7))
    (should= true (eighty-six 986543210))
    (should= false (eighty-six 2))
    (should= false (eighty-six 3))))

(describe "#105"
  (it "Given an input sequence of keywords and numbers, create a map such that
      each key in the map is a keyword, and the value is a sequence of all the
      numbers (if any) between it and the next keyword in the sequence."

    (should= {} (one-hundred-five []))
    (should= {:a [1]} (one-hundred-five [:a 1]))
    (should= {:a [1], :b [2]} (one-hundred-five [:a 1, :b 2]))
    (should= {:a [1 2 3], :b [], :c [4]} (one-hundred-five [:a 1 2 3 :b :c 4]))))
