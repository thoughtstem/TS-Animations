#lang racket

(provide scene1 brains-on-earth)

(require meta-engine 2htdp/image "../assets.rkt")

(define-component size-stream stream?)

;Earth is home to 7.5 billion 3-pound computers.

(define (earth-brain angle)
  (entity
    (relative-size 0.5) 
    (relative-rotation angle)
    (relative-position (posn 0 0))

    (children
      (brain-entity
        (relative-size 1)
        (relative-rotation 0)
        (relative-position (posn 0 -170))))))

(define (brain-circle n)
  (define inc (/ (* 2 pi) n)) 

  (parent
    (children
      (map
        (lambda (i)
          (earth-brain (* inc i))) 
        (range n)))))

(define (pop-in-after delay e)
  (entity 
    (size-stream (stream-append 
                   (const-stream 0 delay)
                   (grow-pop 10 5))
                 (stream-flow (get-size-stream) 1))
    (relative-size 0 (stream-first (get-size-stream)))
    (relative-position (posn 0 0))
    (relative-rotation 0)

    (children e)))

(define (rotate-entity r e)
  (add-or-replace-components e (relative-rotation r)))

(define (pop-in-brains n delay extra)
  (map 
    (lambda (i)
      (pop-in-after (+ extra (* i delay))
                    (rotate-entity 
                      (* (/ (* pi 2) n) i)
                      (brain-circle 1)))) 
    (range n)))


(define (brains-on-earth . cs)
  (add-or-replace-components
    (parent
      (children
        (pop-in-brains 8 5 100)

        (parent
          (relative-size 1.1)
          (children
            (pop-in-brains 7 5 125)))

        (parent
          (relative-size 1.2)
          (children
            (pop-in-brains 5 5 150)))

        (parent
          (relative-size 1.3)
          (children
            (pop-in-brains 3 5 175)))

        (earth
          (relative-size 1)
          (relative-rotation 0
                             (+ (get-rotation) 0.01))
          (relative-position (posn 0 0)))))
    cs
    ))

(define (scene1)
  (enter-north-east
    #:exit 300
    (brains-on-earth)))

(module+ test
  (anim
    (scene1)
    (bg)))






