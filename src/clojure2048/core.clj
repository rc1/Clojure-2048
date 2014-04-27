(ns clojure2048.core)

;; # Configuration
(def config
  {:new-values [1 2]
   :size 4})

;; # The Board
(def next-tile-id 
  (let [counter (ref 0)]
    (fn [] (dosync (alter counter inc)))))

(defn make-tile
  "Creates a tile with a :value and a unqiue :id. Values are [1 2 3 4] not [2 4 8 16]
   like the original game. The x*x this is left for the renderer"
  [] (hash-map :value 0 :id (next-tile-id)))

(defn make-empty-board
  "Creates a new board where all values are 0 and tiles have unique id for rendering"
  []
  (vec (for [_ (range 0 16)] (make-tile))))

(def current-board 
  (atom (make-empty-board)))

(defn reset-current-board [] 
  "Sets the current board no have new tiles with the value of 0"
  (reset! current-board (make-empty-board)))

;; # Adding Values
(defn indices-with-zero-value
  "Return the indices where tiles on the board have a zero value"
  [board]
  (reduce
    (fn [acc [idx tile]]
      (if (zero? (tile :value)) 
        (conj acc idx)
        acc))
    []
    (map-indexed vector board)))

(defn add-random-value
 "Attempts to add a new value to the board."
 [board]
 (let [zero-indices (indices-with-zero-value board)]
   (if (empty? zero-indices)
     board
     (assoc-in board [(rand-nth zero-indices) :value] (rand-nth (config :new-values))))))

;; # Moving Mergeing Cells
;; ## Highest level merge steps abstraction, passes
(defn create-passes []
  "Returns a pattern like [0 1 2 0 1 0] for a size of 4"
  (vec (flatten (reverse (for [x (range 1 (config :size))] (range 0 x))))))

;; ## Lowest level merge steps abstraction. Strategies for making each step based on the direction for the move/merge
(def steps-reduce-fns 
  { :up (fn [steps pass]
          (let [size (config :size)
                needle (* pass size)
                needle+size (+ size needle)]
            (apply conj steps (map vector (range needle needle+size) (range needle+size (+ needle+size size))))))
   :down (fn [steps pass]
           (let [size (config :size)
                 needle (* size (- size pass 1))
                 needle+size (+ size needle)
                 needle-size (- needle size)]
             (apply conj steps (map vector (range needle needle+size) (range needle-size needle)))))
   :left (fn [steps pass]
           (let [size (config :size)
                 needle pass
                 totalsize (* size size)]
             (apply conj steps (for [i (range needle (+ totalsize needle) size)] [i (inc i)]))))
   :right (fn [steps pass]
            (let [size (config :size)
                  needle (- (dec size) pass)
                  totalsize (* size size)]
              (apply conj steps (for [i (range needle (+ totalsize needle) size)] [i (dec i)]))))})

;; ## Step Creation
(defn create-steps 
  ;; Returns the a vector of steps. Each step is a vector contains 2 indices of board tiles to be merged
  [direction]
  (reduce (steps-reduce-fns  direction) [] (create-passes)))

;; ## Moving And Merging
(defn preform-steps
  [board steps]
  (reduce
    (fn [board indices] 
      (let
        [idx1 (first indices)
         idx2 (last indices)
         tile1 (get board idx1)
         tile2 (get board idx2)
         v1 (:value tile1)
         v2 (:value tile2)
         values-zero? (and (zero? v1) (zero? v2))
         values-same? (= v1 v2)
         should-move? (zero? v1)]
        (cond
          values-zero? board
          values-same? (-> board 
                         (assoc idx1 (assoc tile1 :value (inc v1))) 
                         (assoc idx2 (make-tile)))
          should-move? (-> board
                         (assoc idx1 tile2)
                         (assoc idx2 (make-tile)))
          :else board)))
    board steps))

;; # Game Info
(defn calc-score
  "Gets the score"
  []
  (reduce #(+ %1 (:value %2)) 0 @current-board)) 
  
;; # Game Sequencing
(defn do-turn 
  "Makes a move based on the direction and returns the board."
  [direction]
  (swap! 
    current-board
    (fn [board]
      (->
        board  
        (preform-steps (create-steps direction))
        (add-random-value)))))
 
;; # Debug / Repl

(defn print-board-values
  "Draws the board pretty in REPL"
  [board]
  (print (flatten (interpose "\n" (partition 4 (map :value board))))))

(defn do-turn-dev [direction] 
  "Makes a turn  based on the direction. The prints the values to the repl" 
  (do-turn direction)
  (print-board-values @current-board))


;; Repl triggers to play game
(reset-current-board)
(do-turn-dev :up)
(do-turn-dev :down)
(do-turn-dev :left)
(do-turn-dev :right)
(calc-score)




