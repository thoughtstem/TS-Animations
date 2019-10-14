#lang racket

(require meta-engine)


(define (avatar)
  (entity
    (health 100)
    (children
      (body)
      (healthbar)
      (weapon))))

(define (body)
  (entity
    (width ())
    (sprite 
      (register-sprite (circle 30 'solid 'red)))))

(define (healthbar)
  (entity
    (sprite 
      (register-sprite (rectangle 100 5 'solid 'red)))))

(define (weapon)
  (entity
    (sprite 
      (register-sprite (sword)))))
