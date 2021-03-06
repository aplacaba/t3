(ns t3.core)

;; game board

(def game-board
  "Create gameboard"
  (let [squares (vec (take 9 (iterate inc 0)))
        tup (fn [n] [(keyword (str "s" n)) n])]
    (into {} (map tup squares))))

(def default-state
  {:board game-board
   :current_player "x"
   :status :stopped
   :history []
   :scores {:x 0
            :o 0
            :draw 0}})


(def game-state (atom default-state))

;; state manipulators
(defn reset-state
  []
  (reset! game-state default-state))

(defn reset-board
  []
  (swap! game-state assoc :board game-board))

(defn reset-scores
  []
  (swap! game-state assoc :scores {:x 0 :o 0 :draw 0}))

;; game logic
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
  (let [pc-count (count (filter (fn [s] (not (int? s))) (vals gb)))]
    (cond
      (and
       ;; check if gameboard contains enough pieces
       (>= pc-count 5)
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
      :win

      (>= pc-count 9)
      :draw

      :else :noresult)))


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

;; input validators
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
      (println "Invalid input\n")
      (ask-input text validator))))
;;
(defn game-is-started? []
  (= :started (:status @game-state)))

(defn game-setup
  []
  (swap! game-state assoc
         :board game-board
         :status :started
         :current_player "x"))

(defn get-move
  []
  (ask-input "Enter move (square no.) " move-validator))

(defn play-again?
  []
  (ask-input "Play again? (y/n)" yn-validator))

(defn game-ended
  []
  (swap! game-state assoc :status :ended))

(defn game-summary
  []
  (println "Exit"))

(defn update-score
  [r player]
  (if (= r :noresult)
    0
    (let [target-key (if (= r :draw) :draw (keyword player))
          old-score (:scores @game-state)
          new-score {target-key 1}]
      (swap! game-state assoc :scores (merge-with + old-score
                                                    new-score))
      1)))

(defn scoreboard
  [scores]
  (let [x (:x scores)
        o (:o scores)
        d (:draw scores)]
    (println "Scores\n")
    (println "X:     " x)
    (println "O:     " o)
    (println "Draws: " d)))

(defn game-loop
  []
  (println "TicTacToe \n\n")
  (game-setup)
  (while (game-is-started?)
    (draw-board (:board @game-state))
    (println "Current Turn: " (:current_player @game-state))
    (println)
    (let [m (get-move)]
      (swap! game-state assoc :board (place (:board @game-state)
                                            (keyword (str "s" m))
                                            (:current_player @game-state))))

    (draw-board (:board @game-state))
    (println)

    (let [result (check-winner (:board @game-state))]
      (if (pos? (update-score result (:current_player @game-state)))
        (let []
          (game-ended)
          (println (scoreboard (:scores @game-state)))
          (if (= "y" (play-again?))
            (game-loop)
            (println "Thanks for playing!")))
        (switch-player)))))

(defn main
  []
  (game-loop)
  (game-summary))
