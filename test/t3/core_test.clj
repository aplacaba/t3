(ns t3.core-test
  (:require [clojure.test :refer :all]
            [t3.core :refer :all]))


(deftest check-winner-test
  (testing "draw"
    (let [b {:s0 "x" :s1 "o" :s2 "x"
             :s3 "o" :s4 "x" :s5 "o"
             :s6 "o" :s7 "x" :s8 "o"}]
      (is (= :draw (check-winner b)))))

  (testing "win"
    (let [b (merge game-board {:s0 "x" :s1 "x" :s2 "x"
                               :s3 "o" :s4 "o"})]
      (is (= :win (check-winner b)))))

  (testing "noresult"
    (is (= :noresult (check-winner game-board)))))
