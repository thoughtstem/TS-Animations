#lang racket

(provide scene4)

(require meta-engine 2htdp/image 
         "../assets.rkt" 
         "./scene1.rkt"
         "./scene2.rkt"
         )

(define (thinker)
  (brain-entity
    (relative-position (posn -50 0)
                       (to (posn -250 250)
                           #:by 10))
    #:mouth 
    (mouth (sprite normal-eye-sprite)
           (relative-rotation 0)) 
    #:left-eye 
    (left-eye (sprite wide-eye-sprite
                      (blink-timeline 
                        #:normal wide-eye-sprite
                        #:delay 20))
              (normal-counter)) 
    #:right-eye 
    (right-eye (sprite wide-eye-sprite
                       (blink-timeline 
                         #:normal wide-eye-sprite
                         #:delay 20))
               (normal-counter))))

(define (thoughts)
  (parent
    (children
      (pause-until 150 
                   (parent
                     (relative-position (off-screen)
                                        (posn 0 0))
                     (children
                       (brain-computer-pair)
                       (pause-until 200
                                    (grow-pop-enter 
                                      (brains-on-earth
                                        (normal-counter)
                                        (relative-position
                                          (posn 0 -100)
                                          (after 300
                                                 (to (posn -500 -100)
                                                     #:by 10)))
                                        (death #f (after 500 (despawn)))
                                        )

                                      )))))
      (grow-pop-enter
        (thought-bubble))
      )))

(define (scene4)
  (parent
    (relative-position (off-screen) (center-posn))
    (children
      (parent
        (children
          (thinker)
          (pause-until 100 (thoughts))
          
          )))))

(module+ test
  (anim 
    (scene4) 
    (bg) 
    ))






