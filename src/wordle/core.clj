(ns wordle.core
  (:gen-class)
  (:require [clojure.string :as strn]
            [clojure.set :refer [difference]]))

(def words (strn/split-lines
            (slurp
             "words.txt")))

;; now we have a list of all possible words (12972 long)
;; start with a word that has the most common letters and nothing repeated, like
;; aside.

(def letters (into #{} (map char (range 97 (+ 97 26)))))

;; function from [letter pos state] to new group of classes
;; for every class in group, have its pos, and go through prev vector by reduction.
;; possibilities: letter state is \0 then remove from class
;; letter state is \1 and pos is same, remove from class otherwise don't
;; letter state is \2 and pos is same, make class the letter otherwise do nothing
(defn update-class [ch-class class-pos letter letter-pos state]
  (cond (= \0 state) (difference ch-class #{letter})
        (= \1 state) (if (= class-pos letter-pos)
                       (difference ch-class #{letter})
                       ch-class)
        (= \2 state) (if (= class-pos letter-pos) #{letter}
                         ch-class)
        :else ((println "invalid state") ch-class)))

(defn update-classes-with-word-and-result [class-vector prev-word result]
  (map (fn [ch-class class-pos]
         (reduce (fn [prev [letter pos state]]
                   (update-class prev class-pos letter pos state))
                 ch-class
                 (map (fn [letter pos state] [letter pos state])
                      prev-word
                      (range (count prev-word))
                      result)))
       class-vector (range (count class-vector))))

(defn -main []
  (loop [classes (into [] (repeat 5 letters))
         try 1
         word-list words]
    (cond (= try 1) (do (println "align") (recur classes (inc try) word-list))
          (= try 7) nil
          :else
          (let [tried (first word-list)
                result (read-line)
                new-classes (update-classes-with-word-and-result classes tried result)
                new-regex-classes (map #(str \[ (strn/join %) \]) new-classes)
                pattern (re-pattern (strn/join new-regex-classes))
                new-word-list (filter #(re-find pattern %) word-list)
                dist (first (filter (partial apply distinct?) new-word-list))
                new-tried (or dist (first new-word-list))]
            (if (= result "22222")
              (println "success!")
              (do
                (println new-tried)
                (recur new-classes (inc try) new-word-list)))))))