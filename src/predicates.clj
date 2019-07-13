(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [x] (< x n)))

(defn equal-to [n]
  (fn [x] (== x n)))


(defn set->predicate [a-set]
  (fn [x] (if (= x nil)
            "nil" 
            (a-set x))))


(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))


(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))


(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (or (every? whitespace? string) (nil? string) (empty? string)))


(defn has-award? [book award]
  (if (nil? (:awards book))
    false
    (let [trues (map (fn [a] (= a award)) (:awards book))]
      (> (count trues) 0))))

(defn HAS-ALL-THE-AWARDS? [book awards]  
  (let [book-awards (:awards book)
        trues (filter true? (map (fn [a] (contains? book-awards a)) awards))]
    (= (count trues) (count awards))))


(defn my-some [pred a-seq]
  (let [trues (filter pred a-seq)]
    (> (count trues) 0)))


(defn my-every? [pred a-seq]
  (let [trues (filter pred a-seq)]
    (= (count a-seq) (count trues))))

(defn prime? [n]
  (let [pred (fn [val] (= (mod n val) 0))]
    (not (some pred (range 2 n)))))

;^^
