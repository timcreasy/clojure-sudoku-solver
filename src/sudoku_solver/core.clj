(ns sudoku-solver.core
  (:gen-class)
  (:require [clojure.set :as set]))

(def sudoku-board [[5 3 0 0 7 0 0 0 0]
                   [6 0 0 1 9 5 0 0 0]
                   [0 9 8 0 0 0 0 6 0]
                   [8 0 0 0 6 0 0 0 3]
                   [4 0 0 8 0 3 0 0 1]
                   [7 0 0 0 2 0 0 0 6]
                   [0 6 0 0 0 0 2 8 0]
                   [0 0 0 4 1 9 0 0 5]
                   [0 0 0 0 8 0 0 7 9]])

(def empty-board [[0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 0 0]])


(defn value-at [board coords]
  (get-in board coords))

(defn has-value? [board coords]
  (not (zero? (value-at board
                        coords))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row]
              (get row col))
            board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-coords [[row col]]
  (let [block-row (* 3 (quot row 3))
        block-col (* 3 (quot col 3))]
    (map #(map +
              [block-row block-col]
              %)
         (coord-pairs [0 1 2]))))

(defn block-values [board coords]
  (set
    (map #(value-at board %)
         (block-coords coords))))

(defn valid-values-for [board coords]
  (let [allowed-vals (apply hash-set (range 1 10))
        current-vals (set/union (row-values board coords)
                                (col-values board coords)
                                (block-values board coords))]
    (set/difference allowed-vals current-vals)))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map set board))

(defn cols [board]
  (let [sorted-cols (partition (count board) (apply interleave board))]
    (map set sorted-cols)))

(defn blocks [board]
  (map #(block-values board %)
       (coord-pairs [0 3 6])))

(defn valid-rows? [board]
  (if (= 9 (count
             (filter #(= (count %) 9)
                     (rows board))))
    true
    false))

(defn valid-cols? [board]
  (if (= 9 (count
             (filter #(= (count %) 9)
                     (cols board))))
    true
    false))

(defn valid-blocks? [board]
  (if (= 9 (count
             (filter #(= (count %) 9)
                     (cols board))))
    true
    false))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-val]
  (assoc-in board coord new-val))

(defn find-empty-point [board]
  (first (remove (partial has-value? board) (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-location (find-empty-point board)]
     (for [value (valid-values-for board empty-location)
           solution (solve-helper (set-value-at board empty-location value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))