#lang racket

(provide scene3)

(require meta-engine 2htdp/image "../assets.rkt")

(define (scene3)
  ;Put an unhappy brain by an unhappy computer...

  (parent
    (relative-position (off-screen) (center-posn))
    (children
      (grow-pop-enter
        (parent
          (children
            (computer-entity
              (relative-position (posn 50 0))
              #:left-eye 
              (left-eye ) 
              #:right-eye 
              (right-eye ))

            (brain-entity
              (relative-position (posn -50 0))
              #:mouth 
              (mouth (sprite normal-eye-sprite)
                     (relative-rotation 0)
                     ) 
              #:left-eye 
              (left-eye (sprite wide-eye-sprite
                                (blink-timeline 
                                  #:normal wide-eye-sprite
                                  #:delay 20)
                                )
                        (normal-counter)
                        (relative-position (posn -10 0) 
                                           (after 50 
                                                  (to (posn -5 0) 
                                                      #:by (posn 0.5 0))))) 
              #:right-eye 
              (right-eye (sprite wide-eye-sprite
                                 (blink-timeline 
                                   #:normal wide-eye-sprite
                                   #:delay 20))
                         (normal-counter)
                         (relative-position (posn 10 0) 
                                            (after 50 
                                                   (to (posn 15 0) 
                                                       #:by (posn 0.5 0))))) 
              )))))))

(module+ test
  (anim 
    (scene3) 
    (bg-chalkboard) 
    ))






