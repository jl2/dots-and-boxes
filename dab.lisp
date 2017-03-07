;;;; dab.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:dab)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 0) (safety 3) (size 0) (debug 3)))

(defclass click-streamer ()
  ((freq  :initform 440.0d0 :initarg :freq)
   (phase :initform 0.0d0))
  (:documentation "Streamer object for generating sounds."))

(defun sound-function (phase)
  "Generate a sound wave."
  (loop for val from 1 to 2 summing (+ (sin (/ phase val)) (sin (* phase val)))))

(defmethod mixalot:streamer-mix-into ((streamer click-streamer) mixer buffer offset length time)
  "Streamer callback that plays a sound into the mixer."
  (declare (ignore time))
  (with-slots (n freq phase playing) streamer
    (loop for index upfrom offset
       repeat length
       with dp = (* 2.0 pi freq 1/44100)
       as sample = (round (* 1500 (sound-function phase)))
       do
         (mixalot:stereo-mixf (aref buffer index) (mixalot:mono->stereo sample))
         (incf phase dp)))
  (mixalot:mixer-remove-streamer mixer streamer))

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

(defun remove-edge (graph v0 v1)
  "Add an edge between vertices v0 and v1 to the graph."
  (with-slots (edges) graph
    (setf (aref edges v0) (delete v1 (aref edges v0)))
    (setf (aref edges v1) (delete v0 (aref edges v1)))))

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

(defun get-random-computer-edge (graph)
  "Pick a random edge."
  (declare (ignorable graph))
  (loop for vert = (random (length (graph-edges graph))) then (random (length (graph-edges graph)))
     for possibilities = (get-possibilities graph vert)
     while (not possibilities)
     finally (return-from get-random-computer-edge (values vert (nth (random (length possibilities)) possibilities)))))

(defun get-greedy-computer-edge (graph)
  "Use a greedy algorithm to pick the best edge available."
  (declare (ignorable graph))
  (let* ((best-so-far (cons 0 nil))
         (squares (find-complete-squares graph))
         (len-squares (length squares)))
    (loop for vert below (length (graph-edges graph))
       for current-options = (get-possibilities graph vert) then (get-possibilities graph vert)
       do
         (dolist (v2 current-options)
           (add-edge graph vert v2)
           (let ((diff (- (length (find-complete-squares graph)) len-squares)))
             (if (> diff (car best-so-far))
                 (setf best-so-far (cons diff (cons vert v2)))))
           (remove-edge graph vert v2)))
    (if (cdr best-so-far)
        (values (cadr best-so-far) (cddr best-so-far))
        (get-random-computer-edge graph))))

(defstruct player
  "A player object containing a score, a function for getting an edge, a name, and a function to determine the next player."
  (score 0 :type fixnum)
  (color :green)
  (edge-function nil))

(defstruct dab
  "A structure representing a Dots and Boxes game."
  (game-size 2 :type fixnum)
  (squares nil :type list)
  (owners nil :type list)
  (human-player (make-player :color :green :score 0))
  (computer-player (make-player :color :red :score 0 :edge-function #'get-greedy-computer-edge))
  (graph (create-dab-graph 2) :type graph) 
  (current-player :human))

(defun game-over-p (dab)
  "Check if the game is over yet."
  (with-slots (squares game-size) dab
    (= (length squares) (* game-size game-size))))

(defun create-gui-dab (size)
  "Create a computer vs human  Dots and Boxes game for a GUI (edge functions stubbed out)."
  (make-dab :game-size size
            :graph (create-dab-graph size)
            :human-player (make-player :score 0 :color (q+:qt.green) :edge-function nil)
            :computer-player (make-player :score 0 :color (q+:qt.red) :edge-function #'get-greedy-computer-edge)))


;; And now the GUI...

(define-widget main-window (QMainWindow)
  ((next-game-size :initform 4 :type fixnum)
   (mixer    :initform (mixalot:create-mixer)))
  (:documentation "A Window containing a dots and boxes widget."))

(define-widget dab-game-widget (QWidget)
  ((dab-game :initform (create-gui-dab 4))
   (two-closest :initform nil)
   (mixer :initform nil)
   (click-streamer :initform (make-instance 'click-streamer :freq 1200))
   (box-streamer :initform (make-instance 'click-streamer :freq 1000))
   (win-streamer :initform (make-instance 'click-streamer :freq 1400))
   (lose-streamer :initform (make-instance 'click-streamer :freq 400)))
  (:documentation "A widget that playes a plays a game of Dots and Boxes."))

(define-initializer (dab-game-widget setup)
  "Turn on mouse tracking."
  (setf (q+:mouse-tracking dab-game-widget) t))

(defun handle-new-edge (dab v1 v2 mixer box-sound click-sound)
  "Add a new edge to the game graph, add any new squares to the list of them, increment score, and check for game over."
  (with-slots (graph squares owners current-player human-player computer-player) dab
    (add-edge graph v1 v2)
    (let* ((new-squares (find-complete-squares graph))
           (new-len (length new-squares))
           (old-len (length squares))
           (color (player-color (if (eq current-player :human) human-player computer-player))))
      (if (> new-len old-len)
          (progn
            (mixalot:mixer-add-streamer mixer box-sound)
            (dolist (square (set-difference new-squares squares))
              (push (cons square color) owners))
            (incf (player-score (if (eq current-player :human) human-player computer-player)) (- new-len old-len)))
          (progn
            (mixalot:mixer-add-streamer mixer click-sound)
            (setf current-player (if (eq :computer current-player) :human :computer))))
      (setf squares new-squares)
      (if (game-over-p dab)
          :game-over
          current-player))))

(define-slot (dab-game-widget new-game) ((size int))
  "Start a new game."
  (declare (connected dab-game-widget (new-game int)))
  (setf dab-game (create-gui-dab size))
  (setf two-closest nil)
  (q+:repaint dab-game-widget))

(define-slot (dab-game-widget computer-turn) ()
  "Place an edge for the computer."
  (declare (connected dab-game-widget (computer-turn)))
  (with-slots (computer-player graph squares) dab-game
    (multiple-value-bind (v1 v2) (funcall (player-edge-function computer-player) graph)
      (let ((result (handle-new-edge dab-game v1 v2 mixer box-streamer click-streamer)))
        (q+:repaint dab-game-widget)
        (cond ((eq result :game-over) (signal! dab-game-widget (game-over)))
              ((eq result :computer) (signal! dab-game-widget (computer-turn))))))))

(define-slot (dab-game-widget game-over) ()
  "Handle the end of the game."
  (declare (connected dab-game-widget (game-over)))
  (with-slots (computer-player human-player) dab-game
    (let* ((player-score (player-score human-player))
           (computer-score (player-score computer-player))
           (tie (= player-score computer-score))
           (human-won (> player-score computer-score))
           (message (if tie
                        "It was a tie! Try again!"
                        (if human-won
                            (format nil "You won, ~a to ~a!" player-score computer-score)
                            (format nil "You've lost! ~a to ~a!" player-score computer-score))))
           (sound (if tie
                      win-streamer
                      (if human-won
                          win-streamer
                          lose-streamer))))
      (mixalot:mixer-add-streamer mixer sound)
      (q+:qmessagebox-information dab-game-widget "Game Over!" message))))

(define-override (dab-game-widget paint-event paint) (ev)
  "Handle paint events."

  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter dab-game-widget))
       (green-pen (q+:make-qpen (q+:make-qcolor 0 205 0)))
       (red-pen (q+:make-qpen (q+:make-qcolor 205 0 0))))

    (with-slots (graph squares owners) dab-game

      ;; Clear the background
      (q+:fill-rect painter (q+:rect dab-game-widget) (q+:qt.black))
      (q+:set-pen painter green-pen)
      (let* ((height (q+:height dab-game-widget))
             (width (q+:width dab-game-widget))
             (smallest (min height width))
             (adjusted-size (+ 2 (dab-game-size dab-game)))
             (step-size (floor (/ smallest adjusted-size)))
             (points (graph-points graph)))

        ;; Draw filled squares
        (flet ((draw-square (square &optional temp)
                 (let ((pt (aref (graph-points graph) square)))
                   (q+:fill-rect painter 
                                 (+ step-size (* step-size (point-x-loc pt) ))
                                 (+ step-size (* step-size (point-y-loc pt) ))
                                 step-size
                                 step-size
                                 (if temp
                                     (q+:qt.blue)
                                     (cdr (assoc square owners)))))))

          ;; Finished squares
          (dolist (square squares)
            (draw-square square))

          ;; Potentially finished by player's next move
          (when two-closest
            (let ((v0 (cdar two-closest))
                  (v1 (cdadr two-closest)))
              (when (not (has-edge-p graph v0 v1 ))
                (add-edge graph v0 v1)
                (let ((new-squares (set-difference (find-complete-squares graph) squares)))
                  (dolist (square new-squares)
                    (draw-square square t)))
                (remove-edge graph v0 v1)))))

        ;; Draw edges
        (loop for i from 0
           for edges across (graph-edges graph)
           do
             (dolist (vert edges)
               (when (< i vert)
                 (let ((pt1 (aref points i))
                       (pt2 (aref points vert)))
                   (q+:draw-line painter
                                 (+ step-size (* step-size (point-x-loc pt1)))
                                 (+ step-size (* step-size (point-y-loc pt1)))
                                 (+ step-size (* step-size (point-x-loc pt2)))
                                 (+ step-size (* step-size (point-y-loc pt2))))))))

        ;; Draw player's potential next move
        (when two-closest
          (let ((pt1 (aref points (cdar two-closest)))
                (pt2 (aref points (cdadr two-closest))))
            (q+:set-pen painter red-pen)
            (q+:draw-line painter
                          (+ step-size (* step-size (point-x-loc pt1)))
                          (+ step-size (* step-size (point-y-loc pt1)))
                          (+ step-size (* step-size (point-x-loc pt2)))
                          (+ step-size (* step-size (point-y-loc pt2))))))

        ;; Draw points
        (q+:set-pen painter green-pen)
        (loop for pt across points
           do
             (let ((pt-x (- (+ step-size (* step-size (point-x-loc pt) )) 10))
                   (pt-y (- (+ step-size (* step-size (point-y-loc pt) )) 10)))
               (q+:draw-arc painter
                            pt-x
                            pt-y
                            20 20
                            0 (* 16 360))))))))

(defun distance-squared (x1 y1 x2 y2)
  "Squared distance between two points."
  (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))

(defun find-two-closest (x y step-size graph)
  "Return the two closest points to point x,y in graph."
  (sort (loop 
           for idx from 0
           for pt across (graph-points graph)
           collecting (cons (distance-squared x y 
                                              (+ step-size (* step-size (point-x-loc pt)))
                                              (+ step-size (* step-size (point-y-loc pt))))
                            idx))
        #'<
        :key #'car))


(define-override (dab-game-widget mouse-release-event mouse-release) (ev)
  "Handle a mouse click by possibly adding a new edge."
  (with-slots (game-size squares players graph owners current-player) dab-game
    (when (and (not (game-over-p dab-game)) (eq current-player :human))
      (let* ((height (q+:height dab-game-widget))
             (width (q+:width dab-game-widget))
             (smallest (min height width))
             (adjusted-size (+ 2 (dab-game-size dab-game)))
             (step-size (floor (/ smallest adjusted-size)))

             (x-loc (q+:x ev))
             (y-loc (q+:y ev))
             (closest (find-two-closest x-loc y-loc step-size graph))
             (v1 (cdar closest))
             (v2 (cdadr closest)))
        (when (not (has-edge-p graph v1 v2))
          (setf two-closest nil)
          (let ((result (handle-new-edge dab-game v1 v2 mixer click-streamer box-streamer)))
            (cond ((eq result :game-over) (signal! dab-game-widget (game-over)))
                  ((eq result :computer) (signal! dab-game-widget (computer-turn)))))
          (q+:repaint dab-game-widget))))))

(define-override (dab-game-widget mouse-move-event mouse-move) (ev)
  "Find the closest edge to the user's mouse and highlight it if it's not already in the graph."
  (let* ((height (q+:height dab-game-widget))
         (width (q+:width dab-game-widget))
         (smallest (min height width))
         (adjusted-size (+ 2 (dab-game-size dab-game)))
         (step-size (floor (/ smallest adjusted-size)))
         (graph (dab-graph dab-game))
         (x-loc (q+:x ev))
         (y-loc (q+:y ev))
         (closest (find-two-closest x-loc y-loc step-size graph))
         (v1 (cdar closest))
         (v2 (cddr closest)))

    (if (not (has-edge-p graph v1 v2))
        (setf two-closest closest)
        (setf two-closest nil)))
  (q+:repaint dab-game-widget))

(define-subwidget (main-window dab-widget) (make-instance 'dab-game-widget)
  "The dab-game-widget itself.")

(define-override (main-window close-event) (ev)
  "Handle close events."
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))

(define-menu (main-window Game)
  (:item ("New Game" (ctrl n))
         (signal! dab-widget (new-game int) next-game-size)
         (q+:repaint main-window))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

;; This is lame, I need to learn how to use check boxes in menus...
(define-menu (main-window Size)
  (:item ("Two" (ctrl 2))
         (setf next-game-size 2))
  (:item ("Three" (ctrl 3))
         (setf next-game-size 3))
  (:item ("Four" (ctrl 4))
         (setf next-game-size 4))
  (:item ("Five" (ctrl 5))
         (setf next-game-size 5))
  (:item ("Six" (ctrl 6))
         (setf next-game-size 6))
  (:item ("Seven" (ctrl 7))
         (setf next-game-size 7))
  (:item ("Eight" (ctrl 8))
         (setf next-game-size 8)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information main-window "About" "Dots and Boxes.")))

(define-slot (main-window game-size) ((size int))
  "Handle a change in game size."
  (setf next-game-size size))

(define-initializer (main-window setup)
  "Set the window title and set the dab-widget to be the central widget."
  (setf (q+:window-title main-window) "Dots And Boxes")
  (setf (q+:mouse-tracking main-window) t)
  (setf (slot-value dab-widget 'mixer) mixer)
  (setf (q+:central-widget main-window) dab-widget))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
