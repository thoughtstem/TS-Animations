#lang racket

(require meta-engine 2htdp/image "./assets.rkt")

(define (blink-timeline)
  (timeline  
    #:time (get-counter) 
    #:normal normal-eye-sprite 
    (change-to closed-eye-sprite
               #:at 210 #:for 5) 
    (change-to closed-eye-sprite
               #:at 230 #:for 5) 
    (change-to closed-eye-sprite
               #:at 300 #:for 5) 
    (change-to closed-eye-sprite
               #:at 320 #:for 5)))



(define scene1
  (entity
    (destination (posn 500 -100)
                 (timeline
                   #:time (get-time)
                   #:normal (posn 500 -100) 
                   (change-to (posn 200 200)
                              #:at 100 #:for 100) 
                   (change-to (posn 200 500)
                              #:at 200 #:for +inf.0)))

    (position (posn 500 -100)
              (move-to-destination 10))

    (rotation 0
              (+ (get-rotation) 0.1))

    (size 1)

    (children
      (earth
        (relative-size 1)
        (relative-rotation 0)
        (relative-position (posn 0 0))
        
        ))))


(define-component size-stream stream?)

(define scene2
  (entity
    (position (posn 200 200))
    (rotation 0)
    (size 1)

    (children
      (brain-entity
        (relative-position (posn 0 -100))
        (relative-rotation 0)
        ;(relative-size 1)
        (size-stream (stream-append 
                       (const-stream 0 10)
                       (grow-pop 10 5))
                     (stream-flow
                       (get-size-stream)
                       1))

        (size 0 (stream-first (get-size-stream))))

      (brain-entity
        (relative-position (posn 0 0))
        (relative-rotation 0)
        ;(relative-size 1)

        (size-stream (grow-pop 10 5)
                     (stream-flow
                       (get-size-stream)
                       1))

        (size 0 (stream-first (get-size-stream)))

        #:left-eye (left-eye 
                     (counter 0 (^ add1))
                     (sprite normal-eye-sprite
                             (blink-timeline)))

        #:right-eye (right-eye 
                      (counter 0 (^ add1))
                      (sprite normal-eye-sprite
                              (blink-timeline))))
      
      )))


(anim
  scene1 
  scene2
  (bg))


;Earth is home to 7.5 billion 3-pound computers.
;Software globally connects 3.2 billion of them.
;Software runs companies, factories, research labs, governments, and our daily lives.
;Software is the connective tissue of the human ra--

;[MUSIC STOP]

;BUT -- Most who watch this video can't code.

;[MUSIC START]

;More and more are beginning to realize...
;  I need to learn to code...
;  My children should learn to code...
;  We should all learn to---

;[MUSIC STOP]

;BUT -- Fluency in coding takes time... and effort...

;[MUSIC START]

;Luckily, at ThoughtSTEM, we've trained over 10,000 coding students and hundreds of coding teachers.
;Founded by scientists, we study how brains acquire coding fluency.

;Together, we can write a better world, one line of --

;[MUSIC STOP]

;BUT -- ONLY together can we write a better world, one line of--

;[MUSIC START]

;Okay, okay.  That's exactly what I was saying.
;Let's just say it together.

;<a beat>

;[MUSIC STOP]

;Okay

;[MUSIC START]

;Many voices:
;  We can write a better world,
;  One line of code at a time.

