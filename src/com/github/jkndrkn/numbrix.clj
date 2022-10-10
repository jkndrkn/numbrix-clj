(ns primer.numbrix
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require
   [clojure.core.logic.fd :as fd]
   [clojure.pprint :refer [cl-format]]))

(defn bind [var hint]
  (if-not (zero? hint)
    (== var hint)
    succeed))

(defn bind-all [vars hints]
  (and* (map bind vars hints)))

(defn coordinate-valid? [n coordinate]
  (and (>= coordinate 0)
       (< coordinate n)))

(defn get-neighbor [n rows x y]
  (when (and (coordinate-valid? n x)
             (coordinate-valid? n y))
    (nth (nth rows y) x)))

(defn get-neighborhood [n rows x y]
  (let [home (get-neighbor n rows x y)
        north (get-neighbor n rows x (dec y))
        east (get-neighbor n rows (inc x) y)
        south (get-neighbor n rows x (inc y))
        west (get-neighbor n rows (dec x) y)]
    (into [] (remove nil? [home north east south west]))))

(defn check-neighborhood
  "`neighborhood` is a vector consisting of the `home` (the head) and `neighbors` (the rest).
   `home` has an integer value V and can have a predecessor V - 1 or successor V + 1
   A neighborhood is valid when any of these conditions are true:

   * Both a predecessor and successor exist.
   * The home has the initial value 1 and it has a successor.
   * The home has the final value (`size`) and has a predecessor."
  [neighborhood size]
  (let [home (first neighborhood)
        neighbors (rest neighborhood)
        home-is-initial (== home 1)
        home-is-final (== home size)
        predecessor-neighbor-exists (map #(fd/- home % 1) neighbors)
        successor-neighbor-exists (map #(fd/- % home 1) neighbors)]
    (conda
     [(and* [(or* successor-neighbor-exists) (or* predecessor-neighbor-exists)])]
     [(and* [home-is-initial (or* successor-neighbor-exists)])]
     [(and* [home-is-final (or* predecessor-neighbor-exists)])])))

(defn get-rows [elements n]
  (->> elements (partition n) (map vec) (into [])))

(defn numbrix-fd [hints size n]
  (let [vars (repeatedly size lvar)
        rows (get-rows vars n)
        neighborhoods (for [y (range 0 n)
                            x (range 0 n)]
                        (get-neighborhood n rows x y))]
    (run 1 [q]
         (== q vars)
         (everyg #(fd/in % (fd/interval 1 size)) vars)
         (bind-all vars hints)
         (fd/distinct vars)
         (everyg #(check-neighborhood % size) neighborhoods))))

(defn solve [hints]
  (let [size (count hints)
        n (int (Math/sqrt size))
        solution (first (numbrix-fd hints size n))]
    (if (not-empty solution)
      (let [rows (get-rows solution n)]
        (cl-format *out* "~{~{~4d~}~%~}" rows))
      :not-solvable)))

(def hints-3x3
  [1 0 0
   0 0 0
   0 6 0])

(def hints-4x4
  [1 0 0 0
   0 0 6 0
   0 12 0 0
   0 0 0 0])

(def hints-4x4-unsolvable
  [1 1 0 0
   0 0 0 0
   0 0 0 0
   0 0 0 0])

(def hints-5x5
  [3 0 0 0 15
   0 0 0 0 0
   0 0 0 0 0
   0 9 0 0 0
   0 0 0 0 0])

;; Very long execution time. Why?
;; Poorly constrained problem?
(def hints-6x6
  [0 0 0 0 0 0
   0 0 6 0 0 0
   0 0 0 0 0 0
   0 0 0 0 0 0
   0 12 0 0 0 20
   0 0 0 0 0 0])