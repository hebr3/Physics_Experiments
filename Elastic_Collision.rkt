#lang racket
(require 2htdp/universe)
(require 2htdp/image)

; http://www.physicsclassroom.com/mmedia/momentum/trece.cfm

; Constants
(define WIDTH 300)
(define HEIGHT 200)
(define DT 0.1)

; Structures
; (Vec Integer Integer)
(struct Vec [x y] #:transparent)

; (Object Integer Vec Vec)
(struct Object [mass pos vel] #:transparent)

; Helper functions
(define (Object-pos-x obj)
  (Vec-x (Object-pos obj)))
(define (Object-pos-y obj)
  (Vec-y (Object-pos obj)))
(define (Object-vel-x obj)
  (Vec-x (Object-vel obj)))
(define (Object-vel-y obj)
  (Vec-y (Object-vel obj)))

; Image
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; Object -> Image
(define (draw-Object obj)
  (circle (Object-mass obj) 'outline 'red))

; Object Image -> Image
(define (place-Object obj img)
  (let ([x (Object-pos-x obj)]
        [y (Object-pos-y obj)])
    (place-image (draw-Object obj) x y img)))

; (Listof Object) Image -> Image
(define (draw-listof-Objects LoO)
  (foldl place-Object BACKGROUND LoO))

; Object -> Object
(define (update-Object-pos obj)
  (let ([m (Object-mass obj)]
        [x (Object-pos-x obj)]
        [y (Object-pos-y obj)]
        [dx (Object-vel-x obj)]
        [dy (Object-vel-y obj)])
   (Object m (Vec (+ x (* DT dx)) (+ y (* DT dy))) (Vec dx dy))))  

; Object -> Object
(define (update-listof-Objects LoO)
  (for/list ([o LoO])
    (update-Object-pos o)))

; Object Object -> Boolean
(define (collision? LoO)
  (let* ([o1 (car LoO)]
         [o2 (cadr LoO)]
         [m1 (Object-mass o1)]
         [x1 (Object-pos-x o1)]
         [y1 (Object-pos-y o1)]
         [m2 (Object-mass o2)]
         [x2 (Object-pos-x o2)]
         [y2 (Object-pos-y o2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))])
    (< dist (+ m1 m2))))

; Objects
(define Truck
  (Object 30 (Vec 50 100) (Vec 20 0)))

(define Car
  (Object 10 (Vec 150 100) (Vec 0 0)))

(define world
  (list Truck Car))

; big-bang
(big-bang world
  (on-tick update-listof-Objects)
  (to-draw draw-listof-Objects)
  (stop-when collision?))
