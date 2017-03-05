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

(defun game-dot-size (graph)
  "Compute the game size + 1 (the number of vertices across)."
  (isqrt (1+ (length (graph-edges graph)))))

(defun verts-for-square (graph v1)
  "The vertices that make up the square who's upper left corner is vertex v."
  (let* ((v2 (1+ v1))
         (v3 (+ (game-dot-size graph) v1))
         (v4 (1+ v3)))
    (values v1 v2 v3 v4)))


(defun find-complete-squares (graph)
  "Count the number of 'complete' squares in the graph."
  (let ((s-count 0)
        (dot-size (game-dot-size graph))
        (box-size (1- (game-dot-size graph)))
        (squares nil))
    (dotimes (j box-size)
      (dotimes (i box-size)
        (multiple-value-bind (v1 v2 v3 v4) (verts-for-square graph (+ i (* j dot-size)))
          (when (and (has-edge-p graph v1 v2)
                     (has-edge-p graph v1 v3)
                     (has-edge-p graph v2 v4)
                     (has-edge-p graph v3 v4))
            (push v1 squares)))))
    squares))

(defun get-possibilities (graph vertex)
  "Return a list of the possible moves available for the specified vertex."
  (let* ((possibilities nil)
         (dot-size (game-dot-size graph))
         (j (mod vertex dot-size))
         (i (/ (- vertex j) dot-size)))

    ;; Order is important here and done this way to make sure the possibilities list is sorted
    (when (and (< i (1- dot-size)) (not (has-edge-p graph vertex (+ dot-size vertex))))
      (push (+ dot-size vertex) possibilities))

    (when (and (< j (1- dot-size)) (not (has-edge-p graph vertex (1+ vertex))))
      (push (1+ vertex) possibilities))

    (when (and (> j 0) (not (has-edge-p graph vertex (1- vertex))))
      (push (1- vertex) possibilities))

    (when (and (> i 0) (not (has-edge-p graph vertex (- vertex dot-size))))
      (push (- vertex dot-size) possibilities))

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
  (tag #\a)
  (next-player #'cdr))

(defstruct dab
  "A structure representing a Dots and Boxes game."
  (game-size 2 :type fixnum)
  (squares nil :type list)
  (owners nil :type list)
  (players (cons (make-player :edge-function #'get-human-edge :name "Human" :next-player #'cdr :tag #\A)
                 (make-player :edge-function #'get-random-computer-edge :name "Computer" :next-player #'car :tag #\B)))
  (graph (create-dab-graph 2) :type graph))


(defun create-two-player-dab (size p1-name p2-name)
  "Construct a human vs human Dots and Boxes game."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :players (cons (make-player :edge-function #'get-human-edge :name p1-name :next-player #'cdr :tag #\A)
                           (make-player :edge-function #'get-human-edge :name p2-name :next-player #'car :tag #\B))))

(defun create-computer-human-dab (size p-name)
  "Create a human vs computer Dots and Boxes game."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :players (cons (make-player :edge-function #'get-human-edge :name p-name :next-player #'cdr :tag #\A)
                           (make-player :edge-function #'get-random-computer-edge :name "Computer" :next-player #'car :tag #\B))))

(defun create-computer-computer-dab (size p1-name p2-name)
  "Create a computer vs computer Dots and Boxes game."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :players (cons (make-player :edge-function #'get-random-computer-edge :name p1-name :next-player #'cdr :tag #\A)
                           (make-player :edge-function #'get-random-computer-edge :name p2-name :next-player #'car :tag #\B))))




(defun show-square-graph (graph owners)
  "Display a square graph."
  (let ((dot-size (game-dot-size graph)))
    (dotimes (j dot-size)
      (dotimes (i dot-size)
        (multiple-value-bind (v1 v2 v3 v4) (verts-for-square graph (+ i (* j dot-size)))
          (declare (ignorable v3 v4))
          (if (has-edge-p graph v1 v2)
              (format t "~2d---" v1)
              (format t "~2d   " v1))))
      (terpri)
      (dotimes (i dot-size)
        (multiple-value-bind (v1 v2 v3 v4) (verts-for-square graph (+ i (* j dot-size)))
          (declare (ignorable v3 v4))
          (if (has-edge-p graph v1 v3)
              (let* ((vert (+ i (* j dot-size)))
                     (owner-pair (assoc vert owners))
                     (owner (if owner-pair (cdr owner-pair) #\space)))
                (format t " | ~a " owner))
              (format t "     "))))
      (terpri))))


(defun play-dab-game (dab &optional (verbosity 2))
  "Play a game of Dots and Boxes against the computer."
  (with-slots (game-size squares players graph owners) dab
    (let ((cur-player (car players))
          (old-len 0))
      (loop until (= (length squares) (* game-size game-size))
         do
           (with-slots (name score edge-function next-player tag) cur-player
             (when (> verbosity 1)
               (format t "~a: ~a |  ~a: ~a~%"
                       (player-name (car players))
                       (player-score (car players))
                       (player-name (cdr players))
                       (player-score (cdr players)))
               (format t "================================~%")
               (show-square-graph graph owners)
               (format t "================================~%~%")
               (format t "~%It's ~a's turn!~%" name))
             (multiple-value-bind (v1 v2) (funcall edge-function graph)
               (add-edge graph v1 v2)
               (let* ((new-squares (find-complete-squares graph))
                      (new-len (length new-squares)))
                 (if (> new-len old-len)
                     (progn 
                       (dolist (square (set-difference new-squares squares))
                         (push (cons square tag) owners))
                       (incf score (- new-len old-len)))
                     (setf cur-player (funcall next-player players)))
                 (setf squares new-squares)
                 (setf old-len new-len)))))
      (let ((car-score (player-score (car players)))
            (cdr-score (player-score (cdr players))))
        (when (> verbosity 0)
          (terpri)
          (format t "~a: ~a |  ~a: ~a~%"
                  (player-name (car players))
                  (player-score (car players))
                  (player-name (cdr players))
                  (player-score (cdr players)))
          (format t "================================~%")
          (show-square-graph graph owners)
          (format t "================================~%")
          (format t "~%Game over!~%The score was ~a to ~a~%" car-score cdr-score)

          (if (= car-score cdr-score)
              (format t "It was a tie! Try again!~%")
              (let ((winner (if (> car-score cdr-score)
                                (car players)
                                (cdr players))))
                (format t "The winner is ~a~%~%" (player-name winner)))))
        (cons car-score cdr-score)))))


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
