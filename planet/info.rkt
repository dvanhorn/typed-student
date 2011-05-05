#lang setup/infotab
(define name "Typed Student.")
(define categories '(metaprogramming))
(define required-core-version "4.1.0")
(define repositories (list "4.x"))
(define primary-file 
  '("advanced.rkt" "world.rkt"))
(define blurb
  (list '(div "Typed Student: typed languages and teachpacks for HtDP.")))
(define release-notes 
  (list
   '(div "Refined some uses of Number into Integer.")
   '(div "Initial, incomplete, release.")))