#lang racket

(provide scene2 brain-computer-pair)

(require meta-engine 2htdp/image "../assets.rkt")

(define (brain1 . cs)
  (add-or-replace-components
    (brain-entity 
      (relative-size 1) 
      (relative-rotation 0)
      (relative-position (posn 0 0))

      #:left-eye (left-eye 
                   (counter 0 (^ add1))
                   (sprite normal-eye-sprite
                           (blink-timeline #:delay 20)))

      #:right-eye (right-eye 
                    (counter 0 (^ add1))
                    (sprite normal-eye-sprite
                            (blink-timeline #:delay 20))))
    cs))

(define (computer1 #:sprite (s (next-computer-body-sprite)) . cs)
  (add-or-replace-components
    (computer-entity
      #:body (body (sprite s))
      (relative-size 1) 
      (relative-rotation 0)
      (relative-position (posn 0 0)))
    cs))

(define (zip-to #:delay (del 0) dest e)
  (parent
    (normal-counter)
    (destination dest)
    (relative-position 
      (posn 0 0)
      (after del 
             (move-to-destination 10)))
    (children e)))

(define (brain-computer-pair (p1 (posn -50 0)) 
                             (p2 (posn 50 0))
                             #:size (s 1))
  (parent
    (relative-position (posn 0 0))
    (children 
      (brain1 
        (relative-size s)
        (relative-position p1)) 
      (computer1
        (relative-size s)
        (relative-position p2)))))


(define (network . cs)
  (add-or-replace-components
    (parent
      (children
        (grow-pop-enter 
          (brain-computer-pair 
            (posn 50 -100) 
            (posn 50 -200)))

        (pause-until 50
                     (grow-pop-enter 
                       (brain-computer-pair 
                         (posn -50 -200) 
                         (posn -150 -200))))

        (pause-until 100
                     (grow-pop-enter 
                       (brain-computer-pair 
                         (posn 50 100) 
                         (posn 50 200))))

        (pause-until 150
                     (grow-pop-enter 
                       (brain-computer-pair 
                         (posn -50 200) 
                         (posn -150 200))))

        (pause-until 200
                     (grow-pop-enter 
                       (brain-computer-pair 
                         (posn  250 -100) 
                         (posn  150 -100))))))
    cs))

(define (first-network)
  (parent
    (normal-counter)
    (relative-position (posn 0 0))

    ;Start large, get small
    (relative-size 1 
                   (after 500
                          (to 0.5 #:by 0.1)))

    (children

      ;Show one brain.  Hold...
      (zip-to #:delay 50
              (posn -50 0)
              (grow-pop-enter (brain1)))

      ;Show one computer  Hold...
      (pause-until 100
                   (zip-to #:delay 50
                           (posn 50 0)
                           (grow-pop-enter 
                             (computer1 #:sprite computer-body-sprite-green))))

      ;As we shrink, pop in rest of computer/brain network
      (pause-until 300 (network)))))


(define (second-network p)
  (parent
    (normal-counter)
    (relative-position p)
    (relative-size 0.5)
    (children 
      (grow-pop-enter (brain1 (relative-position (posn -50 0))))
      (grow-pop-enter (computer1 (relative-position (posn 50 0))))
      (network))))

(define (pulsing-icon-callout p icon)
  (parent
    (relative-position p)
    (children
      (grow-pop-enter
        (parent
          (normal-counter)
          (relative-size 1 
                         (+ 1 (/ (sin (/ (get-counter) 10)) 30)))
          (children
            icon
            (callout)))))))

(define (school)
  (pulsing-icon-callout 
    (posn 150 125)
    (school-icon
      (relative-size 0.5)
      (relative-position (posn 200 200)))))

(define (factory)
  (pulsing-icon-callout 
    (posn -150 -125)
    (factory-icon
      (relative-size 0.5)
      (relative-position (posn -200 200)))))

(define (government)
  (pulsing-icon-callout 
    (posn 150 -125)
    (government-icon
      (relative-size 0.5)
      (relative-position (posn 200 -200)))))

(define (school/government/factory)
  (parent
    (children
      (school)
      (pause-until 100 (government))
      (pause-until 200 (factory)))))

(define (scene2)
  (define begin-exiting 1200)

  (parent
    (normal-counter)
    (relative-position (off-screen) (center-posn))
    (relative-rotation 0 (after begin-exiting (+ (get-local-rotation) 0.01)))
    (children
      (pause-until begin-exiting
                   (parent
                     (relative-rotation 0 
                                        (+ (get-local-rotation) 0.01))
                     (relative-position (posn 0 0))
                     (children
                       (grow-pop-enter
                         (earth
                           (transparency 0.5))))))
      (parent
        (relative-size 1 (after begin-exiting (to 0.5 #:by 0.01)))
        (normal-counter)
        (children

          ;Show first network -- hold longer on each character
          (first-network)

          ;Show second network (different location)
          (pause-until 500
                       (second-network (posn 300 300)))

          ;A couple beats later, show second network (different location)
          (pause-until 600
                       (second-network (posn -300 -300)))

          ;Call-outs on portions of the overall network: school, factory, government
          (pause-until 800
                       (school/government/factory)) 

          )))))

(module+ test
  (anim 
    (scene2)
    (bg)))



