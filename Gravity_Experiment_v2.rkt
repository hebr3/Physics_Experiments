#lang racket
(require 2htdp/universe)
(require 2htdp/image)

; Structure
(struct Point [m x y vel-x vel-y acc-x acc-y] #:transparent)

; Constants
(define WIDTH 300)
(define HEIGHT 300)
(define DT 0.8)
(define G 0.75)

; Image
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; Point Image -> Image
(define (draw-point pt img)
  (let ([m (Point-m pt)][x (Point-x pt)][y (Point-y pt)])
    (place-image (circle (log m) 'outline 'red) x y img)))

; (Listof Point) Image -> Image
(define (draw-listof-points lop)
;  (cond
;    [(empty? lop) BACKGROUND]
;    [else
;     (draw-listof-points (cdr lop) (draw-point (car lop) BACKGROUND))]))
  (foldl draw-point BACKGROUND lop))

; Point Point -> Point
(define (update-acceleration-gravity pt1 pt2)
  (let* ([x1 (Point-x pt1)]
         [y1 (Point-y pt1)]
         [x2 (Point-x pt2)]
         [y2 (Point-y pt2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))]
         [normalized-x (/ (- x2 x1) dist)]
         [normalized-y (/ (- y2 y1) dist)]
         [m1 (Point-m pt1)]
         [m2 (Point-m pt2)]
         [F (* G m1 m2 (/ (sqr dist)))]
         [delta-acc (/ F m1)]
         [vel-x1 (Point-vel-x pt1)]
         [vel-y1 (Point-vel-y pt1)]
         [acc-x1 (Point-acc-x pt1)]
         [acc-y1 (Point-acc-y pt1)]
         )
    (Point m1 x1 y1 vel-x1 vel-y1
           (+ acc-x1 (* normalized-x delta-acc))
           (+ acc-y1 (* normalized-y delta-acc)))))

; Point -> Point
(define (update-point pt)
  (let ([m (Point-m pt)]
        [x (Point-x pt)]
        [y (Point-y pt)]
        [vel-x (* DT (Point-vel-x pt))]
        [vel-y (* DT (Point-vel-y pt))]
        [acc-x (* DT (Point-acc-x pt))]
        [acc-y (* DT (Point-acc-y pt))])
    (Point m (+ x vel-x) (+ y vel-y)
           (+ vel-x acc-x) (+ vel-y acc-y)
           acc-x acc-y)))

; Point Point -> Point
(define (collision? pt1 pt2)
  (let* ([x1 (Point-x pt1)]
         [y1 (Point-y pt1)]
         [x2 (Point-x pt2)]
         [y2 (Point-y pt2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))]
         [dist (sqrt (+ (sqr dx) (sqr dy)))]
         [m1 (Point-m pt1)]
         [m2 (Point-m pt2)])
    (> (log (if (> m1 m2) m1 m2)) dist)))

; Point Point -> Point
(define (swallow pt1 pt2)
  (let* ([m1 (Point-m pt1)]
         [x1 (Point-x pt1)]
         [y1 (Point-y pt1)]
         [vx1 (Point-vel-x pt1)]
         [vy1 (Point-vel-y pt1)]
         [ax1 (Point-acc-x pt1)]
         [ay1 (Point-acc-y pt1)]
         [m2 (Point-m pt2)]
         [x2 (Point-x pt2)]
         [y2 (Point-y pt2)]
         [vx2 (Point-vel-x pt2)]
         [vy2 (Point-vel-y pt2)]
         [ax2 (Point-acc-x pt2)]
         [ay2 (Point-acc-y pt2)])
    (Point (+ m1 m2)
           (* 0.5 (+ x1 x2))
           (* 0.5 (+ y1 y2))
           (* 0.5 (+ vx1 vx2))
           (* 0.5 (+ vy1 vy2))
           (* 0.5 (+ ax1 ax2))
           (* 0.5 (+ ay1 ay2)))))

; (Listof Point) -> (Listof Point)
(define (update-collisions lop)
  (define (aux p pts)
    (foldl (λ (x y) (if (collision? x y) (swallow y x) y)) p pts))
  (cond
    [(empty? lop) '()]
    [else
     (cons (aux (car lop) (cdr lop)) (update-collisions (cdr lop)))]))

; (Listof Point) -> (Listof Point)
(define (update-positions lop)
  (remove-duplicates
   (update-collisions
    (for/list ([p lop])
      (let ([lop2 (remove p lop)])
        (update-point (foldr (λ (x y) (update-acceleration-gravity y x)) p lop2)))))
   collision?))

; Point
(define world
  (sort (for/list ([i 200])
          (Point (random 2 10)
                 (random WIDTH)
                 (random HEIGHT)
                 0 0 0 0))
   (λ (x y) (> (Point-m x) (Point-m y)))))

; big-bang
(big-bang world
    (on-tick update-positions)
    (to-draw draw-listof-points))
