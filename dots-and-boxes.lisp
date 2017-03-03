;;;; dots-and-boxes.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:dots-and-boxes)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))

(defstruct point
  (x-loc 0 :type fixnum)
  (y-loc 0 :type fixnum))


(defstruct graph 
  (points (make-array 0 :element-type 'point) :type (VECTOR point *))
  (edges (make-array 0 :element-type 'list)  :type (VECTOR list *)))

(defun create-dab-graph (size)
  (let ((arr-size (* (+ size 1) (+ size 1))))
    (make-graph :points (make-array arr-size :initial-contents (loop for i below arr-size
                                                                  collect (make-point
                                                                           :x-loc (mod i (1+ size))
                                                                           :y-loc (/ (- i (mod i (1+ size))) (1+ size))))
                                    :element-type 'point)
                :edges (make-array arr-size :initial-element nil :element-type 'list))))

(defun add-edge (graph v0 v1)
  (with-slots (edges) graph
    (push v1 (aref edges v0))
    (push v0 (aref edges v1))))

(defun count-complete-squares (graph)
  0)

(defstruct dots-and-boxes
  (game-size 2 :type fixnum)
  (graph (create-dab-graph 2) :type graph))

(defun create-dots-and-boxes (size)
  (make-dots-and-boxes :game-size size
                       :graph (create-dab-graph size)))

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
  ((dab-game :initform (create-dots-and-boxes 4)))
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
