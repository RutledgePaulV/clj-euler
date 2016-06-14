(ns clj-euler.core
  (:require [clojure.string :as str]))

(defn slurp-resource [file]
  (slurp (.getFile (clojure.java.io/resource file))))

(defn load-digits [file]
  (filter (partial not= -1)
          (map #(Character/getNumericValue ^Character %) (slurp-resource file))))

(defn get-lines [file] (str/split-lines (slurp-resource file)))
(defn load-ledger [file] (map #(BigInteger. ^String %) (get-lines file)))

(defn load-matrix [file]
  (map (fn [x] (map #(Integer/parseInt %) (str/split x #" "))) (get-lines file)))

(def ! (memoize #(apply * (range 1N (inc %)))))

(defn binomial-coefficient [n k]
  (/ (! n) (* (! (- n k)) (! k))))

(defn catalan-nums []
  (map #(/ (! (* 2N %)) (* (! (inc %)) (! %))) (range)))

(defn fibonacci-nums []
  ((fn recursive-fibo [a b]
     (cons a (lazy-seq (recursive-fibo b (+ a b))))) 0N 1N))

(defn cartesian-product [colls]
  (if (empty? colls) '(())
                     (for [x (first colls)
                           more (cartesian-product (rest colls))]
                       (cons x more))))

(defn cartesian-square [vals] (cartesian-product [vals vals]))

(defn gcd [a b] (if (= 0 b) a (recur b (mod a b))))
(defn lcm [a, b] (/ (* a b) (gcd a b)))
(defn pow [n x] (reduce * (repeat n x)))
(defn pow-sum [power vals] (reduce + (map #(pow power %) vals)))
(defn sum-pow [power vals] (#(pow power %) (reduce + vals)))

(defn triplet? [a b c] (= (+ (pow a 2) (pow b 2)) c))

(defn palindrome? [s]
  (let [as-string (str s)]
    (= as-string (clojure.string/join (reverse as-string)))))

(defn select-palindrome [op]
  (fn [vals] (apply op (filter palindrome? vals))))

(defn sliding [op]
  (fn [size vals] (map #(reduce op %) (partition size 1 vals))))

(defn flatten [op]
  (fn [vals] (map (partial reduce op) vals)))

(defn n-digits
  ([min] (n-digits min min))
  ([min max] (range (pow (- min 1) 10) (pow max 10))))

(defn multiples-of [min max & rest]
  (distinct (apply concat (map #(range min max %) rest))))

(defn valid-coords? [max-x max-y]
  (fn [coords] (not-any? #(or (neg? (first %))
                   (neg? (second %))
                   (> (first %) max-x)
                   (> (second %) max-y)) coords)))

(defn extract-coords [coords matrix]
  (map #(nth (nth matrix (second %)) (first %)) coords))

(defn orthogonal-jaunts [x y size max-x max-y]
  (let [west (map #(vector % y) (range x (- x size) -1))
        east (map #(vector % y) (range x (+ x size)))
        north (map #(vector x %) (range y (- y size) -1))
        south (map #(vector x %) (range y (+ y size)))]
    (filter (valid-coords? max-x max-y) [west east north south])))

(defn diagonal-jaunts [x y size max-x max-y]
  (let [northwest (map #(vector (first %) (second %)) (map vector (range x (- x size) -1) (range y (- y size) -1)))
        northeast (map #(vector (first %) (second %)) (map vector (range x (+ x size)) (range y (- y size) -1)))
        southwest (map #(vector (first %) (second %)) (map vector (range x (- x size) -1) (range y (+ y size))))
        southeast (map #(vector (first %) (second %)) (map vector (range x (+ x size)) (range y (+ y size))))]
    (filter (valid-coords? max-x max-y) [northwest northeast southwest southeast])))

(defn octopus-jaunts [x y size max-x max-y]
  (concat (orthogonal-jaunts x y size max-x max-y)
          (diagonal-jaunts x y size max-x max-y)))

(defn sliding-octopus [op]
  (fn[size matrix] ((flatten op) (map #(extract-coords % matrix)
   (set (map set (mapcat identity (for [[x row] (map-indexed vector matrix) [y _] (map-indexed vector row)]
    (octopus-jaunts x y size (- (count (first matrix)) 1) (- (count matrix) 1))))))))))

(def smallest-palindrome (select-palindrome min))
(def largest-palindrome (select-palindrome max))
(def sliding-product (sliding *))
(def sliding-sum (sliding +))
(def flatten-product (flatten *))
(def flatten-sum (flatten +))
(def octopus-product (sliding-octopus *))
(def octopus-sum (sliding-octopus +))

(defmulti explode (fn [val] (class val)))
(defmethod explode Number [val] (map #(Character/getNumericValue ^Character %) (str val)))
(defmethod explode String [val] (apply vector val))

(defmulti implode (fn [val] (if (empty? val) nil (class (first val)))))
(defmethod implode Number [pieces] (BigInteger. (str/join pieces)))
(defmethod implode Character [pieces] (str/join pieces))
(defmethod implode nil [_] nil)

(println "Problem 1:" (reduce + (multiples-of 0 1000 3 5)))
(println "Problem 2:" (reduce + (filter even? (take-while #(> 4e6 %) (fibonacci-nums)))))
(println "Problem 4:" (largest-palindrome (flatten-product (cartesian-square (n-digits 3)))))
(println "Problem 5:" (reduce lcm (range 1 21)))
(println "Problem 6:" (- (sum-pow 2 (range 1 101)) (pow-sum 2 (range 1 101))))
(println "Problem 8:" (apply max (sliding-product 13 (load-digits "8.txt"))))
(println "Problem 11:" (apply max (octopus-product 4 (load-matrix "11.txt"))))
(println "Problem 13:" (implode (take 10 (explode (reduce + (load-ledger "13.txt"))))))
(println "Problem 15:" (binomial-coefficient 40 20))
(println "Problem 16:" (reduce + (explode (pow 1000N 2N))))
(println "Problem 20:" (reduce + (explode (! 100))))