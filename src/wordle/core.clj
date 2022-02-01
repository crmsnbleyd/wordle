(ns wordle.core
  (:gen-class)
  (:require [clojure.string :as strn]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.set :refer [difference]]))

(def words "12972 member list of all possible words"
  (strn/split-lines
   (slurp
    "resources/words.txt")))

(defn init-classes
  "initial state that every letter is possible for all 5 letters of word"
  []
  (let [letters
        (into #{} (map char (range 97 (+ 97 26))))]
    (into [] (repeat 5 letters))))

;; for every class in group, have its pos, and go through prev vector by reduction.

(defn update-class
  "uses class, its position (0-4), a letter, its position and state to modify class accordingly
  possibilities: letter state is \0 then remove from class
  letter state is \1 and pos is same, remove from class otherwise don't
  letter state is \2 and pos is same, make class the letter otherwise do nothing"
  [ch-class class-pos letter letter-pos state]
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

(defn yellow-letters
  "uses word and encoded result from wordle to make a list of letters that are required"
  [word result]
  (filter (complement nil?)
          (map (fn [letter state]
                 (if (= \1 state) letter nil))
               word result)))

(defn validate-word
  "Checks if  given word matches every pattern in patterns"
  [patterns word] (every? #(re-find % word) patterns))

(defn make-patterns-vector
  "uses required letters and generated character classes (eg [a-z])
  to make a vector of patterns for validation of words"
  [required-letters classes]
  (conj
   (map #(re-pattern (str ".*" % ".*")) required-letters)
   (re-pattern (strn/join
                (map #(str \[ (strn/join %) \])
                     classes)))))

(defn get-first-distinct-maybe
  "gets first word of word list with distinct letters
  or gets the first word"
  [word-list]
  (or
   (first (filter (partial apply distinct?) word-list))
   (first word-list)))

(defn solve-wordle
  "main function used when no -h flag"
  []
  (loop [classes (init-classes)
         try 2
         word-list words
         to-print (do
                    (print "enter starting word, ex. aside: ")
                    (flush)
                    (strn/trim (read-line)))]

    (let [eff to-print
          tried (do (printf "the chosen word is: %s\n" eff)
                    (print "do you want to enter a different word?(y/n): ")
                    (flush)
                    (if (= (strn/trim (read-line)) "y")
                      (read-line)
                      eff))
          result (do (print "enter encoded result from wordle: ") (flush) (read-line))
          new-classes (update-classes-with-word-and-result classes tried result)
          required-letters (yellow-letters tried result)
          pattern-vector (make-patterns-vector required-letters new-classes)
          new-word-list (filter
                         (partial validate-word pattern-vector)
                         word-list)
          new-tried (get-first-distinct-maybe new-word-list)]

      (cond
        (= result "22222") (println "success!")
        (= nil new-tried) ((println "no words left that match :(")
                           (System/exit 0))
        :else (recur new-classes (inc try) new-word-list new-tried)))))

(def help-message
  "This program is a wordle solver.
Enter only 5 letter words.
When the program asks for encoded result,
use the color-coded output from wordle and
convert it to a number. 
Grey letters are represented as 0, 
yellow as 1 and green as 2. 
For example, Grey Grey Yellow Yellow Green is represented as 000112.

The program will repeat until you get a 
winning state (represented as 22222) 
or it runs out of words to use (i.e there are no words 
that correspond to the previous inputs)")

(defn -main [& args]
  (if (get-in (parse-opts args [["-h" "--help"]]) [:options :help])
    (println help-message)
    (solve-wordle)))
