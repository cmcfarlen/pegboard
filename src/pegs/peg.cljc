(ns pegs.peg)

;;                    0
;;                  /   \
;;                1 ---- 2
;;              /  \    /  \
;;            3----- 4 ---- 5
;;          /  \   /  \   /   \
;;         6---- 7 ---- 8 ---- 9
;;       /  \  /  \   /  \   /   \
;;     10 -- 11 -- 12 --- 13 --- 14


;; possible moves from each spot
(def moves
  {0 #{[1 3] [2 5]}
   1 #{[3 6] [4 8]}
   2 #{[4 7] [5 9]}
   3 #{[1 0] [4 5] [7 12] [6 10]}
   4 #{[7 11] [8 13]}
   5 #{[8 12] [9 14] [4 3] [2 0]}
   6 #{[3 1] [7 8]}
   7 #{[4 2] [8 9]}
   8 #{[4 1] [7 6]}
   9 #{[5 2] [8 7]}
   10 #{[6 3] [11 12]}
   11 #{[7 4] [12 13]}
   12 #{[11 10] [7 3] [8 5] [13 14]}
   13 #{[12 11] [8 4]}
   14 #{[13 12] [9 5]}})


(defn occupied?
  [bd i]
  (= 1 (bd i)))

(def vacant? (complement occupied?))

(defn find-move
  "Find the valid move from from to to.  Returns nil if the move is not valid"
  [from to]
  (first (filter #(= to (second %)) (moves from))))

(defn valid-move?
  [bd from to]
  (let [[over _] (find-move from to)]
    (when (and (occupied? bd from)
             over
             (occupied? bd over)
             (vacant? bd to))
      [from over to])))

(defn possible-moves
  "Return all of the possible moves of the given board"
  [bd]
  (reduce-kv (fn [mvs k v]
               (if (occupied? bd k)
                 (concat mvs
                         (filter (fn [[from over to]] (and (occupied? bd from)
                                                           (occupied? bd over)
                                                           (vacant? bd to)))
                                 (map (fn [[o t]] [k o t]) v)))
                 mvs)) '() moves))

(defn game-over?
  "Return true if there are no more valid moves"
  [bd]
  (empty? (possible-moves bd)))

(def not-game-over? (complement game-over?))


(defn move
  "Perform the move. This function does NOT check to see if the move is valid.  Do that before calling"
  [bd [from over to]]
  (-> bd
      (assoc from 0 over 0 to 1)))

(defn move-seq
  "return a sequence of tuples of boards and accululated moves of all possible moves starting
  from the given board."
  [bd]
  (let [root [bd []]
        branches? (comp not-game-over? first)
        children (fn [[bd moves]]
                   (map (fn [mv]
                          [(move bd mv) (conj moves mv)]) (possible-moves bd)))]
    (tree-seq branches? children root)))

(defn find-solution
  "Return a seq of lists of moves to solve the given board"
  [bd]
  (map second (filter #(= 1 (apply + (first %)))
                        (move-seq bd))))

(comment

 (time (count (move-seq [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1])))

 (time (first (find-solution [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1])))

 (filter #(= 1 (apply + %)) (tree-seq
                              not-game-over?
                              (fn [bd] (map (partial move bd) (possible-moves bd)))
                              [1 1 0 1 1 1 0 0 0 0 0 0 0 0 0]))

 (let [bd [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1]]
   (map (partial move bd) (possible-moves bd)))

 (possible-moves [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1])

 (valid-move? [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1] 13 4)
 (valid-move? [1 1 1 1 0 1 1 1 1 1 1 1 1 1 1] 13 5)

 )

