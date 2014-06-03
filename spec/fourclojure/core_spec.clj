(ns fourclojure.core-spec
  (:require [speclj.core :refer :all]
            [fourclojure.core :refer :all]))

(defn throw-restricted [restriction]
  (fn [& args]
    (-fail (format "%s is not allowed." "Function"))))

(defmacro with-restrictions [fns & body]
  (let [redefs (reduce #(concat %1 [%2 (throw-restricted %2)]) [] fns)]
    `(with-redefs ~redefs
       ~@body)))

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
        (fifty-four 3 (range 8)))
    )
  )
)

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
        (sixty-nine concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}))
    )
  )
)

