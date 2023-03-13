(ns dactyl-keyboard.lib.collections)

(defn filter-by-index [coll idxs]
  (keep-indexed #(when ((set idxs) %1) %2)
                coll))

(defn remove-by-index [coll idxs]
  (keep-indexed #(when-not ((set idxs) %1) %2)
                coll))