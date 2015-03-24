(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [b accum e]
                 (cond 
                   (= exp 0) 1
                   (= e exp) accum
                   :else (recur b (* b accum) (inc e))))]
    (helper base 1 0)))

(defn last-element [a-seq]
  (loop [k (count a-seq) my-seq a-seq]
    (if (<= k 1) (first my-seq)
      (recur (dec k) (rest my-seq)))))

(defn seq= [seq1 seq2]
  (if (not (= (count seq1) (count seq2))) false
    (let [size (count seq1)]
      (loop [index 0 a-seq1 seq1 a-seq2 seq2]
        (cond
         (= 0 (count a-seq1)) true
         (not (= (first a-seq1) (first a-seq2))) false
         (= (dec size) index) true
         :else (recur (inc index) (rest a-seq1) (rest a-seq2)))))))

(defn find-first-index [pred a-seq]
  (loop [index 0 w-seq a-seq cnt (count a-seq)]
    (cond
     (= index cnt) nil
     (pred (first w-seq)) index
     :else (recur (inc index) (rest w-seq) cnt))))

(defn avg [a-seq]
  (if (= 0 (count a-seq)) nil
      (loop [sum 0 index 0 my-seq a-seq]
        (if (= (count a-seq) index) (/ sum index)
          (recur (+ sum (first my-seq)) (inc index) (rest my-seq))))))
 (defn toggle [a-set elem]
       (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
     )
(defn parity [a-seq]
  (loop [my-set #{} my-seq a-seq]
    (if (empty? my-seq) my-set
      (recur (toggle my-set (first my-seq)) (rest my-seq)))))

(defn fast-fibo [n]
  (loop [k 3 init [1 1]]
    (cond
      (<= n 0) 0
      (<= n 2) 1
      (> k n) (init 1)
      :else (recur (inc k) [(init 1) (apply + init)]))))

(defn cut-at-repetition [a-seq]
  (loop [so-far #{} up-to-seq [] rest-seq a-seq]
    (if (or (empty? rest-seq) (contains? so-far (first rest-seq))) up-to-seq
      (recur (conj so-far (first rest-seq)) (conj up-to-seq (first rest-seq)) (rest rest-seq)))))


    

