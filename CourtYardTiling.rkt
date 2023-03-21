#lang racket

(require racket/gui)

(define frame-width 700)
(define frame-height 700)

(define n 5) ;;the value of n which defines the size of the board
(define empty-tile-pos '(2 2)) ;;the position of the empty

(define block-size (floor (/ frame-width (expt 2 n))))

(define frame (new frame%
                   [label "My Courtyard"]
                   [width frame-width]
                   [height frame-height]))

(define (ltiles-pos-rec empty-tile-pos center width)
  (if (<= width 2)
      (cond [(equal? empty-tile-pos (list (- (car center) block-size) (- (list-ref center 1) block-size)))
             (list (list center 4))]
            [(equal? empty-tile-pos (list (car center) (- (list-ref center 1) block-size)))
             (list (list (list (- (car center) block-size) (list-ref center 1)) 1))]
            [(equal? empty-tile-pos (list (- (car center) block-size) (list-ref center 1)))
             (list (list (list (car center) (- (list-ref center 1) block-size)) 3))]
            [(equal? empty-tile-pos center)
             (list (list (list (- (car center) block-size) (- (list-ref center 1) block-size)) 2))])
      
      (cond [(and (< (car empty-tile-pos) (car center)) (< (list-ref empty-tile-pos 1) (list-ref center 1)))
             (append (list (list center 4))
                     (ltiles-pos-rec empty-tile-pos (list (- (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec (list (car center) (- (list-ref center 1) block-size)) (list (+ (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec (list (- (car center) block-size) (list-ref center 1)) (list (- (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec center (list (+ (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2)))]
            [(and (>= (car empty-tile-pos) (car center)) (< (list-ref empty-tile-pos 1) (list-ref center 1)))
             (append (list (list (list (- (car center) block-size) (list-ref center 1)) 1))
                     (ltiles-pos-rec (list (- (car center) block-size) (- (list-ref center 1) block-size)) (list (- (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec empty-tile-pos (list (+ (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec (list (- (car center) block-size) (list-ref center 1)) (list (- (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec center (list (+ (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2)))]
            [(and (< (car empty-tile-pos) (car center)) (>= (list-ref empty-tile-pos 1) (list-ref center 1)))
             (append (list (list (list (car center) (- (list-ref center 1) block-size)) 3))
                     (ltiles-pos-rec (list (- (car center) block-size) (- (list-ref center 1) block-size)) (list (- (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec (list (car center) (- (list-ref center 1) block-size)) (list (+ (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec empty-tile-pos (list (- (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec center (list (+ (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2)))]
            [(and (>= (car empty-tile-pos) (car center)) (>= (list-ref empty-tile-pos 1) (list-ref center 1)))
             (append (list (list (list (- (car center) block-size) (- (list-ref center 1) block-size)) 2))
                     (ltiles-pos-rec (list (- (car center) block-size) (- (list-ref center 1) block-size)) (list (- (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec (list (car center) (- (list-ref center 1) block-size)) (list (+ (car center) (* block-size (/ width 4))) (- (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec (list (- (car center) block-size) (list-ref center 1)) (list (- (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2))
                     (ltiles-pos-rec empty-tile-pos (list (+ (car center) (* block-size (/ width 4))) (+ (list-ref center 1) (* block-size (/ width 4)))) (/ width 2)))]))
  )

(define (draw-canvas dc ltiles-pos)
  (if (<= (length ltiles-pos) 1)
      (draw-lshaped-tile dc (caaar ltiles-pos) (list-ref (caar ltiles-pos) 1) block-size (make-object color% (random 256) (random 256) (random 256)) (cadar ltiles-pos))
      (begin
        (draw-lshaped-tile dc (caaar ltiles-pos) (list-ref (caar ltiles-pos) 1) block-size (make-object color% (random 256) (random 256) (random 256)) (cadar ltiles-pos))
        (draw-canvas dc (cdr ltiles-pos)))))

(define (draw-lshaped-tile dc x y size color rotation)
  (send dc set-pen color 1 'transparent)
  (send dc set-brush color 'solid)
  (cond
   [(= rotation 1)
    (send dc draw-rectangle
      x (- y size)
      size (* size 2))
    (send dc draw-rectangle
      x y
      (* size 2) size)]
   [(= rotation 2)
    (send dc draw-rectangle
      x y
      size (* size 2))
    (send dc draw-rectangle
      x y
      (* size 2) size)]
   [(= rotation 3)
    (send dc draw-rectangle
      x y
      size (* size 2))
    (send dc draw-rectangle
      (- x size) y
      (* size 2) size)]
   [(= rotation 4)
    (send dc draw-rectangle
      x (- y size)
      size (* size 2))
    (send dc draw-rectangle
      (- x size) y
      (* size 2) size)])
 )

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (draw-canvas dc (ltiles-pos-rec (list (* (car empty-tile-pos) block-size) (* (list-ref empty-tile-pos 1) block-size)) (list (* (/ (expt 2 n) 2) block-size) (* (/ (expt 2 n) 2) block-size)) (expt 2 n)))
                )])

(send frame show #t)
