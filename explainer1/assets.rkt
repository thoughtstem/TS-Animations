#lang racket

(provide bg earth anim
         get-time
         enter-north-east
         time-manager
         grow-pop-enter
         blink-timeline
         after

         halt-after
         pause-until
         tick-between
         off-screen

         callout
         school-icon
         government-icon
         factory-icon

         (all-from-out "brain.rkt"))

(require meta-engine 2htdp/image
         "brain.rkt")

(define paper-bg
  (register-sprite (bitmap "./images/paper.jpg")))

(define earth-sprite 
  (register-sprite (bitmap "./images/earth.png")))


(define (bg)
  (entity
    (position (posn 200 200)
              (center-posn))
    (size 2)
    (sprite paper-bg)))


(define (earth . cs)
  (add-or-replace-components
    (parent 
      ;(position (posn 200 200))
      (sprite earth-sprite))
    cs))

(define (time-manager)
  (entity 
    (name 'time-manager)
    (counter 0 
             (begin
               (set! global-time (get-counter))
               (add1 (get-counter))))))

(define global-time 0)
(define (get-time)
  global-time)

(define-component counter number?)


(require mode-lambda/backend/gl)
(require mode-lambda/shot)

(define frame 0)
(define (screenshots-in-dir! shot-dir)
  (λ (i w h bs)
    
    (define p (build-path shot-dir (format "~a.png" 
                                     (~a frame #:width 5 #:pad-string "0" #:align 'right))))
    (define bm (argb-bytes->bitmap w h bs))
    (set! frame (add1 frame))
    (save-bitmap! bm p)))

(define-syntax-rule (screenshots expr)
  (parameterize ([gl-screenshot! (screenshots-in-dir! "./gifs/")])
      expr))

(define (anim #:render? (render? #f) . es)
 (define starting-state 
  (tick (game 
         (time-manager) 
         es)))

 (no-contracts!
  (if render?
   (screenshots 
    (play! #:width 600 #:height 600
     starting-state))
   (play! #:width 600 #:height 600
    starting-state))))

(define (enter-north-east 
          #:enter (enter 0)
          #:exit  (exit +inf.0)
          . es)

  (define final-adj (posn 0 500))

  (define final
   (posn-add
    (center-posn) final-adj))

  (entity
    (destination (posn 500 -100)
                 (timeline
                   #:time (get-time)
                   #:normal (posn 500 -100) 
                   (change-to (center-posn)
                              #:at enter #:for (- exit enter))
                   (change-to final 
                              #:at (+ enter exit) #:for +inf.0)))

    (position (posn 500 -100)
              (move-to-destination 10))

    (rotation 0)

    (size 1)

    (death #f (if (<= (posn-dist (get-position) 
                                 final) 
                       1)
                  (despawn)
                   #f))

    (children
      es)))

(define-syntax-rule (after n v)
  (if (>= (get-counter) n)
      v
      (get-value (CURRENT-COMPONENT))))

(define-component size-stream stream?)

(define (grow-pop-enter #:enter (delay 0) e . cs)
 (add-or-replace-components
  (parent
   (children
    (add-or-replace-components e
     (list
      (size-stream (stream-append 
                    (const-stream 0 delay)
                    (grow-pop 10 5))
       (stream-flow (get-size-stream) 1))
      (relative-size 0 (stream-first (get-size-stream)))))))
  cs))



(define (blink-timeline #:delay (delay 0))
  (timeline  
    #:time (get-counter) 
    #:normal normal-eye-sprite 
    (change-to closed-eye-sprite
               #:at (+ delay 10) #:for 5) 
    (change-to closed-eye-sprite
               #:at (+ delay 30) #:for 5) 
    (change-to closed-eye-sprite
               #:at (+ delay 100) #:for 5) 
    (change-to closed-eye-sprite
               #:at (+ delay 120) #:for 5)))


;When this abstraction is working correctly,
;  move to extensions/
(define/contract (pause-until del e)
 (-> (and/c (negate zero?) positive?) 
     (or/c entity? (listof entity?))
     entity?)

 (define child-game
  (tick
   (game (parent-data-entity
          (position (posn 0 0))
          (rotation 0)
          (size 1))
    e)))

  (parent
    (normal-counter) 
    (also-render (game)
      (cond 
        [(< (get-counter) del) (game)]
        [(= (get-counter) del) child-game]
        [else  (tick (propagate-to-child-parent-data (get-also-render)))]))))

;TODO: Fix to be like above
(define (halt-after after e)
  (entity
    (normal-counter) 
    (also-render
      (game e)
      (cond 
        [(< (get-counter) after) (tick (get-also-render))]
        [(= (get-counter) after) (game)]
        [else  (get-also-render)]))))
      

(define (tick-between del after e)
  (halt-after after (pause-until del e)))

(define (off-screen)
  (posn -100000 -100000))

(define basic-line
 (register-sprite 
  (overlay
   (line 150 0
    (pen "black" 5 "solid" "round" "bevel"))
   (rectangle 160 10 'solid 'transparent))))

(define (callout . cs)
  (define (top)
    (parent
      (relative-position (posn 0 -100))
      (sprite basic-line)))
  (define (right)
    (parent
      (local-rotation (/ pi 2))
      (relative-position (posn 100 0))
      (sprite basic-line)))
  (define (bottom)
    (parent
      (relative-position (posn 0 100))
      (sprite basic-line)))
  (define (left)
    (parent
      (relative-position (posn -100 0))
      (local-rotation (/ pi 2))
      (sprite basic-line)))
 

  (add-or-replace-components
    (parent
      (children 
        (top)
        (right)
        (bottom)
        (left)))
    cs))


(define school-sprite
  (register-sprite (scale 0.1 (bitmap "./images/school.png"))))

(define (school-icon . cs)
  (add-or-replace-components
    (parent
      (sprite school-sprite))
    cs))


(define government-sprite
  (register-sprite (scale 0.1 (bitmap "./images/government.png"))))

(define (government-icon . cs)
  (add-or-replace-components
    (parent
      (sprite government-sprite))
    cs))


(define factory-sprite
  (register-sprite (scale 0.1 (bitmap "./images/factory.png"))))

(define (factory-icon . cs)
  (add-or-replace-components
    (parent
      (sprite factory-sprite))
    cs))

