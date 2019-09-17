#lang racket

(provide scene2)

(require meta-engine 2htdp/image "../assets.rkt")

;TODO: 
;  Get paredit plugin for vim...
;  Get icons, make callouts
;     https://thenounproject.com/term/school/2858756/
;     https://thenounproject.com/term/factory/2081608/
;     https://thenounproject.com/search/?q=government&i=40293

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

(define (scene2)
 (list
  (parent
   (normal-counter)
   (position (posn 0 0) (center-posn))

   (relative-size 1 
    (after 500
     (to 0.5 #:by 0.1)))

   (children

    (zip-to #:delay 50
     (posn -50 0)
     (grow-pop-enter (brain1)))

    (pause-until 100
     (zip-to #:delay 50
      (posn 50 0)
      (grow-pop-enter 
       (computer1 #:sprite computer-body-sprite-green))))

    (pause-until 300 (network))))

   (pause-until 500
     (parent
      (normal-counter)
      (position (posn 0 0) (posn-add  
                              (posn 150 150)
                              (center-posn)))
      (relative-size 0.5)
      (children 
        (grow-pop-enter (brain1 (relative-position (posn -50 0))))
        (grow-pop-enter (computer1 (relative-position (posn 50 0))))
        (network))))

   (pause-until 600
     (parent
      (normal-counter)
      (position (posn 0 0) (posn-add  
                              (posn -150 -150)
                              (center-posn)))
      (relative-size 0.5)
      (children 
         (grow-pop-enter (brain1 (relative-position (posn -50 0))))
         (grow-pop-enter (computer1 (relative-position (posn 50 0))))
         (network))))))

(module+ test
  (anim 
    (scene2)
    (bg)))
