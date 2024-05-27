(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc)base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [el, seq]
                 (if (empty? seq)
                   el
                   (recur (first seq) (rest seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (not (= (count seq1) (count seq2))) false
                   (and (empty? seq1) (empty? seq2)) true
                   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
                   :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         cnt 0
         seq a-seq]
    (if (= (count seq) 0)
      (/ sum cnt)
      (recur (+ sum (first seq)) (inc cnt) (rest seq)))))

(defn parity [a-seq]
  (loop [res (set '())
         seq a-seq]
    (cond
      (= (count seq) 0) res
      (contains? res (first seq)) (recur (disj res (first seq)) (rest seq))
      :else (recur (conj res (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [i 2
         fibn 1
         fibn1 0]
    (cond
      (= n 0) 0
      (= n 1) 1
      (= n i) (+ fibn fibn1)
      :else (recur (inc i) (+ fibn fibn1) fibn))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         seq a-seq]
    ;; (println seq)
    (cond
      (= (count seq) 0) res
      (some #(= (first seq) %) res) res
      :else (recur (conj res (first seq)) (rest seq)))))

