;;;; dab.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:dab)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))

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

(defun count-complete-squares (graph)
  "Count the number of 'complete' squares in the graph."
  (with-slots (edges) graph
    (let ((s-count 0)
          (size (1- (isqrt (1+ (length edges))))))
      (dotimes (j size)
        (dotimes (i size)
          (let* ((v1 (+ i (* j size)))
                 (v2 (1+ v1))
                 (v3 (+ 1 size v1))
                 (v4 (1+ v3))
                 (v1-v2 (find v2 (aref edges v1)))
                 (v1-v3 (find v3 (aref edges v1)))
                 (v2-v4 (find v4 (aref edges v2)))
                 (v3-v4 (find v4 (aref edges v3))))
            (when (and v1-v2 v1-v3 v2-v4 v3-v4)
              (incf s-count)))))
      s-count)))

(defun describe-graph (graph &optional (stream t))
  "Write an easy to read description of graph to the specified stream."
  (with-slots (edges) graph
    (dotimes (i (length edges))
      (dolist (gt (aref edges i))
        (when (< i gt) (format stream "~a - ~a~%" i gt))))))

(defstruct dab
  "A structure representing a Dots and Boxes game."
  (game-size 2 :type fixnum)
  (graph (create-dab-graph 2) :type graph))

(defun create-dab (size)
  "Construct a Dots and Boxes game object."
  (make-dab :game-size size
                       :graph (create-dab-graph size)))



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
