(ns t3.core)

;; game board

(def game-board
  "Create gameboard"
  (let [squares (vec (take 9 (iterate inc 0)))
        tup (fn [n] [(keyword (str "s" n)) n])]
    (into {} (map tup squares))))


;; state
(def game-state
  "Game state holder"
  (atom {:board game-board
         :current_player "x"
         :history []}))


(defn switch-player []
  (swap! game-state assoc :current_player (next-player)))


(defn next-player
  []
  (if (= (:current_player @game-state) "x")
    "o"
    "x"))


(defn place
  "Place X or O in the gameboard"
  [board k sym]
  (assoc board k sym))


(defn check-winner
  "Check winner from the gameboard"
  [gb]
  (or
   ;; horizontals
   (= (:s0 gb) (:s1 gb) (:s2 gb))
   (= (:s3 gb) (:s4 gb) (:s5 gb))
   (= (:s6 gb) (:s7 gb) (:s8 gb))
   ;; verticals
   (= (:s0 gb) (:s3 gb) (:s6 gb))
   (= (:s1 gb) (:s4 gb) (:s7 gb))
   (= (:s2 gb) (:s5 gb) (:s8 gb))
   ;; diagonals
   (= (:s0 gb) (:s4 gb) (:s8 gb))
   (= (:s2 gb) (:s4 gb) (:s6 gb))))


(defn draw-board
  [gb]
  (print "- - - - - - - - - - - -")
  (print "\n")
  (print "|      |       |      |")
  (print "\n")
  (print (str "|  " (:s0 gb) "   |   " (:s1 gb) "   |   " (:s2 gb) "  |"))
  ;;(print "|  s0  |  s1   |  s2  |")
  (print "\n")
  (print "|      |       |      |")
  (print "\n")
  (print "- - - - - - - - - - - -")
  (print "\n")
  (print "|      |       |      |")
  (print "\n")
  (print (str "|  " (:s3 gb) "   |   " (:s4 gb) "   |  " (:s5 gb) "   |"))
  ;;(print "|  s3  |  s4   |  s5  |")
  (print "\n")
  (print "|      |       |      |")
  (print "\n")
  (print "- - - - - - - - - - - -")
  (print "\n")
  (print "|      |       |      |")
  (print "\n")
  (print (str "|  " (:s6 gb) "   |   " (:s7 gb) "   |  " (:s8 gb) "   |"))
  ;;(print "|  s6  |  s7   |  s8  |")
  (print "\n")
  (print "|      |       |      |")
  (print "\n")
  (print "- - - - - - - - - - - -"))



(defn is-valid
  [s]
  (let [b (:board @game-state)
        k (keyword (str "s" s))]
    (and (contains? b k)
         (int? (k b)))))

(defn ask-input
  [text]
  (println text)
  (def x (read-line))
  (if (is-valid x)
    x
    (let []
      (println "Invalid input: ")
      (ask-input text))))

(defn game-loop
  []
  (while (not (check-winner (:board @game-state)))
    (println "Tic Tac Toe")

    )
