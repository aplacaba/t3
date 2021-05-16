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

(defn reset-state
  []
  (swap! game-state (fn [_] {:board game-board
                            :current_player "x"
                            :history []})))

(defn next-player
  []
  (if (= (:current_player @game-state) "x")
    "o"
    "x"))

(defn switch-player []
  (swap! game-state assoc :current_player (next-player)))


(defn place
  "Place X or O in the gameboard"
  [board k sym]
  (assoc board k sym))


(defn check-winner
  "Check winner from the gameboard"
  [gb]
  (and
   ;; check if gameboard contains enough pieces
   (>= (count
        (filter (fn [s] (not (int? s)))
                (vals gb)))
       5)
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
    (= (:s2 gb) (:s4 gb) (:s6 gb)))))


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
  (print "- - - - - - - - - - - -")
  (println))



(defn move-validator
  [s]
  (let [b (:board @game-state)
        k (keyword (str "s" s))]
    (and (contains? b k)
         (int? (k b)))))

(defn yn-validator
  [s]
  (or (= "y" s)
      (= "n" s)))

(defn ask-input
  [text validator]
  (println text)
  (def x (read-line))
  (if (validator x)
    x
    (let []
      (println "Invalid input: ")
      (ask-input text validator))))

(defn game-loop
  []
  (while (not (check-winner (:board @game-state)))
    (println "Tic Tac Toe")
    (draw-board (:board @game-state))
    (println "Current Player: " (:current_player @game-state))
    (def move (ask-input "Enter square number: " move-validator))
    (swap! game-state assoc :board (place (:board @game-state)
                                          (keyword (str "s" move))
                                          (:current_player @game-state)))
    (if (not (check-winner (:board @game-state)))
      (switch-player)))
  (draw-board (:board @game-state))
  (println (:current_player @game-state) " wins")
  (def play-again? (ask-input "Play again? (y/n)" yn-validator))
  (if (= "y" play-again?)
    (fn []
      (reset-state)
      (game-loop))
    (println "Thanks for playing.")))
