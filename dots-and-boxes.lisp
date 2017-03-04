;;;; dab.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:dab)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 0) (safety 3) (size 0) (debug 3)))

(defstruct point
  "A two dimensional point object."
  (x-loc 0 :type fixnum)
  (y-loc 0 :type fixnum))


(defstruct graph 
  "Graph data structure implemented using adjacency lists."
  (points (make-array 0 :element-type 'point) :type (VECTOR point *))
  (edges (make-array 0 :element-type 'list)  :type (VECTOR list *)))

(defun create-dab-graph (size)
  "Create a graph with (* size size) points arranged in a square."
  (let ((arr-size (* (+ size 1) (+ size 1))))
    (make-graph
     :points (make-array arr-size
                         :initial-contents
                         (loop for i below arr-size
                            collect (make-point
                                     :x-loc (mod i (1+ size))
                                     :y-loc (/ (- i (mod i (1+ size))) (1+ size))))
                         :element-type 'point)
     :edges (make-array arr-size :initial-element nil :element-type 'list))))

(defun add-edge (graph v0 v1)
  "Add an edge between vertices v0 and v1 to the graph."
  (with-slots (edges) graph
    (pushnew v1 (aref edges v0))
    (pushnew v0 (aref edges v1))))

(defun has-edge-p (graph v0 v1)
  "Return non-nil if an edge is in the graph, or nil if not."
  (with-slots (edges) graph
    (find v1 (aref edges v0))))

(defun game-size+1 (graph)
  "Compute the game size + 1 (the number of vertices across)."
  (isqrt (1+ (length (graph-edges graph)))))

(defun square-vertices-at (graph i j)
  "Return the vertices for the square at position i j."
  (verts-for-square graph (+ i (* j (game-size+1 graph)))))

(defun verts-for-square (graph v1)
  "The vertices that make up the square who's upper left corner is vertex v."
  (let* ((v2 (1+ v1))
         (v3 (+ (game-size+1 graph) v1))
         (v4 (1+ v3)))
    (values v1 v2 v3 v4)))

(defun count-complete-squares (graph)
  "Count the number of 'complete' squares in the graph."
  (let ((s-count 0)
        (size (1- (game-size+1 graph))))
    (dotimes (j size)
      (dotimes (i size)
        (multiple-value-bind (v1 v2 v3 v4) (square-vertices-at graph i j)
          (when (and (has-edge-p graph v1 v2)
                     (has-edge-p graph v1 v3)
                     (has-edge-p graph v2 v4)
                     (has-edge-p graph v3 v4))
            (incf s-count)))))
    s-count))

(defun get-possibilities (graph vertex)
  "Return the list of possible moves available for the specified vertex."
  (let* ((possibilities nil)
         (size+1 (game-size+1 graph))
         (j (mod vertex size+1))
         (i (/ (- vertex j) size+1)))

    (when (and (< i (1- size+1)) (not (has-edge-p graph vertex (+ size+1 vertex))))
      (push (+ size+1 vertex) possibilities))

    (when (and (< j (1- size+1)) (not (has-edge-p graph vertex (1+ vertex))))
      (push (1+ vertex) possibilities))

    (when (and (> j 0) (not (has-edge-p graph vertex (1- vertex))))
      (push (1- vertex) possibilities))

    (when (and (> i 0) (not (has-edge-p graph vertex (- vertex size+1))))
      (push (- vertex size+1) possibilities))

    possibilities))

(defun describe-graph (graph &optional (stream t))
  "Write an easy to read description of graph to the specified stream."
  (with-slots (edges) graph
    (dotimes (i (length edges))
      (dolist (gt (aref edges i))
        (when (< i gt) (format stream "~a - ~a~%" i gt))))))


(defun get-human-edge (graph)
  "Read the player's edge value from the keyboard."
  (declare (ignorable graph))
  (loop
     do
       (format t "Enter your first vertex: ")
       (let* ((v1 (read))
              (others (get-possibilities graph v1)))
         (if others
             (progn
               (format t "Enter second vertex (~{~a~^ ~}): " others)
               (let ((v2 (read)))
                 (when (and (find v2 others) (not (has-edge-p graph v1 v2)))
                   (return-from get-human-edge (values v1 v2)))
                 (format t "The edge ~a ~a is not allowed, please try again!~%" v1 v2)))
             (format t "That edge has no available edges! Please pick another!~%")))))

(defun get-random-computer-edge (graph)
  "Figure out a computer move."
  (declare (ignorable graph))
  (loop for vert = (random (length (graph-edges graph))) then (random (length (graph-edges graph)))
       for possibilities = (get-possibilities graph vert)
     while (not possibilities)
       finally (return-from get-random-computer-edge (values vert (nth (random (length possibilities)) possibilities)))))



(defstruct player
  "A player object containing a score, a function for getting an edge, a name, and a function to determine the next player."
  (score 0 :type fixnum)
  (edge-function #'get-human-edge)
  (name "human" :type string)
  (next-player #'cdr))

(defstruct dab
  "A structure representing a Dots and Boxes game."
  (game-size 2 :type fixnum)
  (num-squares 0 :type fixnum)
  (players (cons (make-player :edge-function #'get-human-edge :name "Human" :next-player #'cdr)
                 (make-player :edge-function #'get-random-computer-edge :name "Computer" :next-player #'car)))
  (graph (create-dab-graph 2) :type graph))


(defun create-two-player-dab (size p1-name p2-name)
  "Construct a human vs human Dots and Boxes game."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :players (cons (make-player :edge-function #'get-human-edge :name p1-name :next-player #'cdr)
                           (make-player :edge-function #'get-human-edge :name p2-name :next-player #'car))))

(defun create-computer-human-dab (size p-name)
  "Create a human vs computer Dots and Boxes game."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :players (cons (make-player :edge-function #'get-human-edge :name p-name :next-player #'cdr)
                           (make-player :edge-function #'get-random-computer-edge :name "Computer" :next-player #'car))))

(defun create-computer-computer-dab (size p1-name p2-name)
  "Create a computer vs computer Dots and Boxes game."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :players (cons (make-player :edge-function #'get-random-computer-edge :name p1-name :next-player #'cdr)
                           (make-player :edge-function #'get-random-computer-edge :name p2-name :next-player #'car))))




(defun show-square-graph (graph)
  "Display a square graph."
  (let ((size+1 (game-size+1 graph)))
    (dotimes (j size+1)
      (dotimes (i size+1)
        (multiple-value-bind (v1 v2 v3 v4) (square-vertices-at graph i j)
          (declare (ignorable v3 v4))
          (if (has-edge-p graph v1 v2)
              (format t "~2d---" v1)
              (format t "~2d   " v1))))
      (terpri)
      (dotimes (i size+1)
        (multiple-value-bind (v1 v2 v3 v4) (square-vertices-at graph i j)
          (declare (ignorable v3 v4))
          (if (has-edge-p graph v1 v3)
              (format t " |   ")
              (format t "     "))))
      (terpri))))


(defun play-dab-game (dab)
  "Play a game of Dots and Boxes against the computer."
  (with-slots (game-size num-squares players graph) dab
    (let ((cur-player (car players)))
      (loop until (= num-squares (* game-size game-size))
         do
           (with-slots (name score edge-function next-player) cur-player
             (format t "~a: ~a |  ~a: ~a~%"
                     (player-name (car players))
                     (player-score (car players))
                     (player-name (cdr players))
                     (player-score (cdr players)))
             (format t "================================~%")
             (show-square-graph graph)
             (format t "================================~%~%")
             (format t "~%It's ~a's turn!~%" name)
             (multiple-value-bind (v1 v2) (funcall edge-function graph)
               (add-edge graph v1 v2)
               (let ((new-squares (count-complete-squares graph)))
                 (if (> new-squares num-squares)
                     (incf score (- new-squares num-squares))
                     (setf cur-player (funcall next-player players)))
                 (setf num-squares new-squares))))))
        (terpri)
        (format t "~a: ~a |  ~a: ~a~%"
                (player-name (car players))
                (player-score (car players))
                (player-name (cdr players))
                (player-score (cdr players)))
        (format t "================================~%")
        (show-square-graph graph)
        (format t "================================~%")
        (let ((car-score (player-score (car players)))
              (cdr-score (player-score (cdr players))))
          (format t "~%Game over!~%The score was ~a to ~a~%" car-score cdr-score)

          (if (= car-score cdr-score)
              (format t "It was a tie! Try again!~%")
              (let ((winner (if (> car-score cdr-score)
                                (car players)
                                (cdr players))))
                (format t "The winner is ~a~%~%" (player-name winner)))))))


;; And now the GUI...

(define-widget main-window (QMainWindow)
  ())

(define-override (main-window close-event) (ev)
  (q+:accept ev))

(define-menu (main-window Game)
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Dots and Boxes.")))

(define-widget dab-drawer (QWidget)
  ((dab-game :initform (create-dab 4)))
  (:documentation "Dots and boxes ."))

(define-override (dab-drawer paint-event paint) (ev)
  "Handle paint events."

  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter dab-drawer))
       (pen (q+:make-qpen )))

    ;; Clear the background
    (q+:fill-rect painter (q+:rect dab-drawer) (q+:qt.black))
    (q+:set-color pen (q+:make-qcolor 0 205 0))
    (q+:set-pen painter pen)
    (let* ((height (q+:height dab-drawer))
           (width (q+:width dab-drawer)))
      (q+:draw-arc painter
                   (round (- (/ width 2) 10)) (round (- (/ height 2) 10))
                   20 20
                   0 (* 16 360))
      (q+:draw-line painter 0 0 width height))))

(define-subwidget (main-window dab-widget) (make-instance 'dab-drawer)
  "The dab-drawer itself.")

(define-initializer (main-window setup)
  "Set the window title and set the dab-widget to be the central widget."
  (setf (q+:window-title main-window) "Dots And Boxes")
  (setf (q+:central-widget main-window) dab-widget))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))
