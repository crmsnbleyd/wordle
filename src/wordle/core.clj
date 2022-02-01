(ns wordle.core
  (:gen-class)
  (:require [clojure.string :as strn]
            [clojure.set :refer [difference]]))

(def words (strn/split-lines
            (slurp
             "resources/words.txt")))

;; now we have a list of all possible words (12972 long)
;; start with a word that has the most common letters and nothing repeated, like aside.

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
         word-list words
         to-print "align"]
    
    (cond (= try 1) (recur classes (inc try) word-list to-print)
          (= try 7) nil
          :else
          (let [eff to-print
                tried (do (println eff) (print "do you want to enter a different word? y/n ") (flush)
                          (if (= (read-line) "y") (read-line) eff))
                result (do (println "enter encoded result") (read-line))
                new-classes (update-classes-with-word-and-result classes tried result)
                required-letters (filter (complement nil?)
                                                  (map (fn [letter state]
                                                         (if (= \1 state) letter nil))
                                                       tried result))
                required-classes (map #(re-pattern (str ".*" % ".*")) required-letters)
                new-regex-classes (map #(str \[ (strn/join %) \]) new-classes)
                pattern (re-pattern (strn/join new-regex-classes))
                filter-func (fn [word] (every? #(re-find % word) (conj required-classes pattern)))
                new-word-list (filter filter-func word-list)
                dist (first (filter (partial apply distinct?) new-word-list))
                new-tried (or dist (first new-word-list))]
            (cond 
              (= result "22222") (println "success!")
              (= nil new-tried) ((println "no words left that match :(")
                                 (System/exit 0))
              :else (recur new-classes (inc try) new-word-list new-tried))))))
