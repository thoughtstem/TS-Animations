#lang racket

(provide brain-entity
         left-eye
         right-eye
         left-leg
         right-leg
         left-arm
         right-arm
         mouth
         body
         
         closed-eye-sprite
         normal-eye-sprite
         wide-eye-sprite
         
         random-computer-body-sprite
         next-computer-body-sprite
         computer-body-sprite-green
         computer-body-sprite-red
         computer-body-sprite-blue
         computer-body-sprite-purple

         computer-entity)

(require meta-engine 2htdp/image
         (only-in racket/draw color%)
         (prefix-in p: pict))

(define brain-color
  (make-color 255 193 196))

(define brain-outline-color
  (make-color 255 96 139))

(define computer-outline-color
  "black")

(define brain-body-sprite 
  (register-sprite (bitmap "./images/brain-body.png")))

(define (computer-body-sprite color)
 (register-sprite 
    (p:pict->bitmap 
      (p:filled-rounded-rectangle 70 50
        #:color color 
        #:border-color "black"
        #:border-width 4))))

(define computer-body-sprite-green
 (computer-body-sprite
  (make-object color% 0 255 0 0.5)))

(define computer-body-sprite-red
 (computer-body-sprite
  (make-object color% 255 0 0 0.5)))

(define computer-body-sprite-blue
 (computer-body-sprite
  (make-object color% 0 0 255 0.5)))

(define computer-body-sprite-purple
 (computer-body-sprite
  (make-object color% 138 43 226 0.5)))

(define (brain-eye-sprite (angle -90)) 
  (register-sprite
    (scale 0.5
           (add-curve (circle 1 'solid 'transparent)
                      0 10 angle 1/2
                      20 10 (- angle) 1/2
                      (make-pen "black" 4 "solid" "round" "round")))))

(define normal-eye-sprite (brain-eye-sprite))
(define closed-eye-sprite (brain-eye-sprite 0))

(define wide-eye-sprite (register-sprite
                          (circle 5 'solid 'black)))

(define brain-limb-sprite
  (register-sprite 
    (overlay
      (line 0 15
            (pen brain-outline-color 
                 3 "solid" "round" "bevel"))
      (square 30 'solid 'transparent))))


(define computer-limb-sprite
  (register-sprite 
    (overlay
      (line 0 15
            (pen computer-outline-color 
                 3 "solid" "round" "bevel"))
      (square 30 'solid 'transparent))))

(define (left-eye . cs)
  (add-or-replace-components
    (entity
      (relative-size 1)
      (relative-rotation 0)
      (relative-position (posn -10 0))
      (sprite normal-eye-sprite))
    cs))

(define (right-eye . cs)
  (add-or-replace-components
    (entity
      (relative-size 1)
      (relative-rotation 0)
      (relative-position (posn 10 0))
      (sprite (brain-eye-sprite)))
    cs))   

(define (right-leg #:sprite (s brain-limb-sprite) . cs)
  (add-or-replace-components
    (entity
      (counter 0 (^ add1))
      (relative-size 1)
      (relative-rotation 0 (sin (/ (get-counter) 10)))
      (relative-position (posn 10 25))
      (children
        (entity
          (relative-size 1)
          (relative-rotation 0)
          (relative-position (posn 0 10))
          (sprite s))))
    cs))   

(define (left-leg #:sprite (s brain-limb-sprite) . cs)
  (add-or-replace-components
    (entity
      (counter 0 (^ add1))
      (relative-size 1)
      (relative-rotation 0 (sin (/ (get-counter) 10)))
      (relative-position (posn -10 25))
      (children
        (entity
          (relative-size 1)
          (relative-rotation 0)
          (relative-position (posn 0 10))
          (sprite s))))
    cs))   

(define (left-arm #:sprite (s brain-limb-sprite) . cs)
  (add-or-replace-components
    (entity
      (counter 0 (^ add1))

      (relative-size 1)
      (relative-rotation 0 (+ (sin (/ (get-counter) 10))
                              (/ pi 2)))
      (relative-position (posn -20 10))

      (children
        (entity
          (relative-size 1)
          (relative-rotation 0)
          (relative-position (posn 0 10))
          (sprite s)

          (children
            (entity
              (counter 0 (^ add1))
              (relative-size 1)
              (relative-rotation 0 (sin (/ (get-counter) 10)))
              (relative-position (posn 0 10))
              (children
                (entity
                  (relative-size 1)
                  (relative-rotation 0)
                  (relative-position (posn 0 7))
                  (sprite s))))))))
    cs))

(define (right-arm #:sprite (s brain-limb-sprite) . cs)
  (add-or-replace-components
    (entity
      (counter 0 (^ add1))

      (relative-size 1)
      (relative-rotation 0 (- (sin (/ (get-counter) 10))
                              (/ pi 2)))
      (relative-position (posn 20 10))

      (children
        (entity
          (relative-size 1)
          (relative-rotation 0)
          (relative-position (posn 0 10))
          (sprite s)

          (children
            (entity
              (counter 0 (^ add1))
              (relative-size 1)
              (relative-rotation 0 (sin (/ (get-counter) 10)))
              (relative-position (posn 0 10))
              (children
                (entity
                  (relative-size 1)
                  (relative-rotation 0)
                  (relative-position (posn 0 7))
                  (sprite s))))))))
    cs))

(define (mouth . cs)
  (add-or-replace-components
    (entity
      (relative-size 1)
      (relative-rotation (* pi 1))
      (relative-position (posn 0 10))
      (sprite normal-eye-sprite))
    cs))


(define (body . cs)
  (add-or-replace-components
    (entity
      (relative-size 1)
      (relative-rotation 0)
      (relative-position (posn 0 0))
      (sprite brain-body-sprite))
    cs))

(define (brain-entity #:left-eye  (the-left-eye  (left-eye))
                      #:right-eye (the-right-eye (right-eye))
                      #:left-leg  (the-left-leg  (left-leg))
                      #:right-leg (the-right-leg (right-leg))
                      #:mouth     (the-mouth     (mouth))
                      #:body      (the-body      (body))
                      #:left-arm  (the-left-arm  (left-arm))
                      #:right-arm (the-right-arm (right-arm))
                      . cs)

  (add-or-replace-components
    (parent 
      (children
        the-left-eye  
        the-right-eye  
        the-mouth
        the-body
        the-right-leg
        the-left-leg
        the-left-arm
        the-right-arm))
    cs))

(define (computer-entity #:left-eye  (the-left-eye  (left-eye))
                      #:right-eye (the-right-eye (right-eye))
                      #:left-leg  (the-left-leg  
                                    (left-leg #:sprite computer-limb-sprite))
                      #:right-leg (the-right-leg  
                                    (right-leg #:sprite computer-limb-sprite))
                      #:mouth     (the-mouth     (mouth))
                      #:body      (the-body      (body (sprite computer-body-sprite-green)))
                      #:left-arm  (the-left-arm  
                                    (left-arm #:sprite computer-limb-sprite))
                      #:right-arm (the-right-arm 
                                    (right-arm #:sprite computer-limb-sprite))
                      . cs)

  (add-or-replace-components
    (parent 
      (children
        the-left-eye  
        the-right-eye  
        the-mouth
        the-body
        the-right-leg
        the-left-leg
        the-left-arm
        the-right-arm))
    cs))

(define computer-body-sprites
  (list 
    computer-body-sprite-red
    computer-body-sprite-green
    computer-body-sprite-blue
    computer-body-sprite-purple))

(define (random-computer-body-sprite)
  (list-ref computer-body-sprites 
            (random (length computer-body-sprites))))


(define curr-brain-sprite-id -1)
(define (next-computer-body-sprite)
  (set! curr-brain-sprite-id 
    (remainder (add1 curr-brain-sprite-id)
               (length computer-body-sprites)))
  (list-ref computer-body-sprites curr-brain-sprite-id))





