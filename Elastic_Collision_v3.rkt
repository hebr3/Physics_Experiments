#lang racket
(require 2htdp/universe)
(require 2htdp/image)

; http://www.physicsclassroom.com/mmedia/momentum/trece.cfm

; Constants
(define WIDTH 300)
(define HEIGHT 200)
(define DT 0.1)

; Structures

; object -> (vector Number Number Number Number Number Symbol)
; object -> (vector mass pos-x pos-y vel-x vel-y color)

; Helper functions
(define (object-mass obj)
  (vector-ref obj 0))
(define (object-pos-x obj)
  (vector-ref obj 1))
(define (object-pos-y obj)
  (vector-ref obj 2))
(define (object-vel-x obj)
  (vector-ref obj 3))
(define (object-vel-y obj)
  (vector-ref obj 4))
(define (object-color obj)
  (vector-ref obj 5))

; Image
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; object -> Image
(define (draw-object obj)
  (circle (object-mass obj) 'outline (object-color obj)))

; object Image -> Image
(define (place-object obj img)
  (let ([x (object-pos-x obj)]
        [y (object-pos-y obj)])
    (place-image (draw-object obj) x y img)))

; (Listof object) Image -> Image
(define (draw-listof-objects LoO)
  (foldl place-object BACKGROUND LoO))

; object -> object
(define (update-object-pos obj)
  (let ([m (object-mass obj)]
        [x (object-pos-x obj)]
        [y (object-pos-y obj)]
        [dx (object-vel-x obj)]
        [dy (object-vel-y obj)]
        [c (object-color obj)])
   (vector m (+ x (* DT dx)) (+ y (* DT dy)) dx dy c)))  

; object -> object
(define (change-color obj)
  (let ([m (object-mass obj)]
        [x (object-pos-x obj)]
        [y (object-pos-y obj)]
        [dx (object-vel-x obj)]
        [dy (object-vel-y obj)]
        [c (object-color obj)])
    (vector m x y dx dy 'red)))

; (List object object) -> Boolean
(define (collision? LoO)
  (let* ([o1 (car LoO)][o2 (cadr LoO)]
         [m1 (object-mass o1)][x1 (object-pos-x o1)][y1 (object-pos-y o1)]
         [m2 (object-mass o2)][x2 (object-pos-x o2)][y2 (object-pos-y o2)]
         [dx (abs (- x1 x2))][dy (abs (- y1 y2))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))])
    (< dist (+ m1 m2))))

; object -> object
(define (update-listof-objects LoO)
  (for/list ([o LoO])
    (if (collision? LoO)
        (change-color (update-object-pos o))
        (update-object-pos o))))



; objects
(define Truck
  (vector 30 50 100 20 0 'blue))

(define Car
  (vector 10 150 100 0 0 'blue))

(define world
  (list Truck Car))

; big-bang
(big-bang world
  (on-tick update-listof-objects)
  (to-draw draw-listof-objects))
;  (stop-when collision?))
