#lang racket
(require 2htdp/universe)
(require 2htdp/image)

; Constants
(define WIDTH 600)
(define HEIGHT 600)
(define DT 0.5)
(define G 1)

; Image
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; Structure
; point -> (vector Integer Integer Integer Integer Integer))
; point -> (vector Mass pos-x pos-y vel-x vel-y))

; Helper functions
(define (point-mass pt)
  (vector-ref pt 0))
(define (point-pos-x pt)
  (vector-ref pt 1))
(define (point-pos-y pt)
  (vector-ref pt 2))
(define (point-vel-x pt)
  (vector-ref pt 3))
(define (point-vel-y pt)
  (vector-ref pt 4))

; Vector Image -> Image
(define (draw-point pt img)
  (let ([m (point-mass pt)][x (point-pos-x pt)][y (point-pos-y pt)])
    (place-image (circle m 'outline 'red) x y img)))

; (Listof point) Image -> Image
(define (draw-vectorof-points lop)
  (for/fold ([bkg BACKGROUND])
            ([pt lop])
    (draw-point pt bkg)))

; Vector Vector -> Vector
(define (vector-addition v1 v2)
  (let ([x1 (vector-ref v1 0)]
        [x2 (vector-ref v2 0)]
        [y1 (vector-ref v1 1)]
        [y2 (vector-ref v2 1)])
    (vector (+ x1 x2) (+ y1 y2))))

; point point -> (vector Number Number)
; F ~> (/ (* m1 m2) d^2)
(define (calculate-force-gravity pt1 pt2)
  (let* ([m1 (point-mass pt1)]
         [x1 (point-pos-x pt1)]
         [y1 (point-pos-y pt1)]
         [m2 (point-mass pt2)]
         [x2 (point-pos-x pt2)]
         [y2 (point-pos-y pt2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))])
    (if (zero? dist)
        (vector 0 0)
        (let ([normalized-x (/ (- x2 x1) dist)]
              [normalized-y (/ (- y2 y1) dist)]
              [F (/ (* m1 m2) (sqr dist))])
          (vector (* F normalized-x)
                  (* F normalized-y))))))

; Point (Vectorof Point) -> Vector
(define (calculate-force-from-all pt VoP)
  (let ([VoF (vector-map (λ (x) (calculate-force-gravity pt x))
                         VoP)])
    (for/fold ([v (vector 0 0)])
              ([F VoF])
      (vector-addition v F))))

; Point Vector -> Point
(define (update-point-vel pt vec)
  (let ([m (point-mass pt)]
        [x (point-pos-x pt)]
        [y (point-pos-y pt)]
        [dx (point-vel-x pt)]
        [dy (point-vel-y pt)]
        [vx (vector-ref vec 0)]
        [vy (vector-ref vec 1)])
    (vector m x y (+ dx vx) (+ dy vy))))

; point -> point
(define (update-point pt)
  (let ([m (point-mass pt)]
        [x (point-pos-x pt)]
        [y (point-pos-y pt)]
        [dx (point-vel-x pt)]
        [dy (point-vel-y pt)])
    (vector m (+ x dx) (+ y dy) dx dy)))

; (Vectorof point) point -> (Vectorof point)
(define (vector-remove vec pt)
  (vector-filter-not (λ (x) (equal? x pt)) vec))

; (Vectorof point) -> (Vectorof point)
(define (update-all-points VoP)
  (for/vector ([i (vector-length VoP)])
    (let* ([p (vector-ref VoP i)]
           [pts (vector-remove VoP p)])
      (update-point (update-point-vel p (calculate-force-from-all p pts))))))

; point point -> point
(define (collision? pt1 pt2)
  (let* ([x1 (point-pos-x pt1)]
         [y1 (point-pos-y pt1)]
         [x2 (point-pos-x pt2)]
         [y2 (point-pos-y pt2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))]
         [m1 (point-mass pt1)]
         [m2 (point-mass pt2)])
    (> (log (if (> m1 m2) m1 m2)) dist)))

; point point -> point
(define (swallow pt1 pt2)
  (let* ([m1 (point-mass pt1)]
         [x1 (point-pos-x pt1)]
         [y1 (point-pos-y pt1)]
         [vx1 (point-vel-x pt1)]
         [vy1 (point-vel-y pt1)]
         [m2 (point-mass pt2)]
         [x2 (point-pos-x pt2)]
         [y2 (point-pos-y pt2)]
         [vx2 (point-vel-x pt2)]
         [vy2 (point-vel-y pt2)])
    (vector (+ m1 m2)
            (* 0.5 (+ x1 x2))
            (* 0.5 (+ y1 y2))
            (* 0.5 (+ vx1 vx2))
            (* 0.5 (+ vy1 vy2)))))

; (Listof point) -> (Listof point)
(define (update-collisions lop)
  (define (aux p pts)
    (foldl (λ (x y) (if (collision? x y) (swallow y x) y)) p pts))
  (cond
    [(empty? lop) '()]
    [else
     (cons (aux (car lop) (cdr lop)) (update-collisions (cdr lop)))]))

; (Listof point) -> (Listof point)
(define (update-positions lop)
  (for/list ([p lop])
    (update-point p)))

; point
(define world
  (vector-sort (for/vector ([i 25])
          (vector (+ 2 (* 3 (random)))
                  (random WIDTH)
                  (random HEIGHT)
                  0 0))
        (λ (x y) (> (point-mass x) (point-mass y)))))

; big-bang
(big-bang world
    (on-tick update-all-points)
    (to-draw draw-vectorof-points))
