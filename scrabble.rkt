#lang racket
(require rackunit)
(provide subbag?)
(provide best-word)
(provide word-rank)
(provide valid-wordList)
(provide score-letter)

; inputs: two lists S and B
; output: #t if B contains all of the elements in S in a quantity at least as large but order doesn't matter
(define (subbag? S B)
  (cond
    [(empty? S) #t]
    [(empty? B) #f]
    [(member (first B) S) (subbag? (remove (first B) S) (rest B))]
    [else (subbag? S (rest B))]))


;; scrabble-tile-bag  
;;   letter tile scores and counts from the game of Scrabble
;;   the counts aren't needed they're obtained from
;;   http://en.wikipedia.org/wiki/Image:Scrabble_tiles_en.jpg
;;
(define scrabble-tile-bag
  '((#\a 1 9) (#\b 3 2) (#\c 3 2) (#\d 2 4) (#\e 1 12)
   (#\f 4 2) (#\g 2 3) (#\h 4 2) (#\i 1 9) (#\j 8 1)
   (#\k 5 1) (#\l 1 4) (#\m 3 2) (#\n 1 6) (#\o 1 8)
   (#\p 3 2) (#\q 10 1)(#\r 1 6) (#\s 1 4) (#\t 1 6)
   (#\u 1 4) (#\v 4 2) (#\w 4 2) (#\x 8 1) (#\y 4 2)
   (#\z 10 1) (#\_ 0 2)) ) 
;; end define scrabble-tile-bag
;; The underscore will be used to represent a blank tile, which is a wild-card


; input: a letter l
; output: the scrabble score of that letter
(define (score-letter l)
  (first (rest(assoc l scrabble-tile-bag))))


; input: a word w
; output: the scrabble score of that word
(define (score-word w)
  (if (empty? (string->list w))
      0
      (+ (score-letter (first (string->list w)))
         (score-word (list->string(rest (string->list w)))))))


; input: rack a string and WL a list of strings
; output: a list of all of the words in WL which can be made completely from the characters of rack
(define (valid-wordList rack WL)
  (cond
    [(empty? WL) WL]
    [(subbag? (string->list (first WL)) (string->list rack)) (cons (first WL) (valid-wordList rack (rest WL)))]
    [else (valid-wordList rack (rest WL))]))


; input: a list of strings WL
; output: a list of the Scrabble scores of all of the words in WL
(define (word-rank WL)
  (if (empty? WL)
      WL
      (append (list (list (first WL) (score-word (first WL)))) (word-rank (rest WL)))))


; inputs: a string rack and a list of strings WL
; output: a two element-list where the first element is the best word and the second element is the score of that word
(define (best-word rack WL)
  (let*
      ([validWL (valid-wordList rack WL)]
       [rankedL (word-rank validWL)]
       [sortedL (sort rankedL > #:key cadr)]) ; sorts rankedL with words with largest scrabble scores at the front of the list
    
    (if (empty? sortedL)
        '("" 0)
        (first sortedL))))


;---------------------------tests---------------------------
; provided tests subbag
(check-equal? (subbag? '()      '(s p a m s))   true)
(check-equal? (subbag? '(s s)   '(s p a m s))   true)
(check-equal? (subbag? '(s m)   '(s p a m s))   true)
(check-equal? (subbag? '(a p)   '(s p a m s))   true)
(check-equal? (subbag? '(a m a) '(s p a m s))   false)
(check-equal? (subbag? '(a s)   '(s a))         true)

; provided test score-letter
(check-equal? (score-letter '#\w) 4)

; provided test score-word
(check-equal? (score-word "zzz")  30)
(check-equal? (score-word "fortytwo") 17)
(check-equal? (score-word "twelve")  12)
  
; provided tests best-word
(check-equal? (best-word "academy" '("ace" "ade" "cad" "cay" "day")) 
 '("cay" 8))
(check-equal? (best-word "appler"  '("peal" "peel" "ape" "paper")) 
 '("paper" 9))
(check-equal? (best-word "paler"   '("peal" "peel" "ape" "paper"))
 '("peal" 6))
(check-equal? (best-word "kwyjibo" '("ace" "ade" "cad" "cay" "day"))
 '("" 0))
(check-equal? (second (best-word "bcademy" '("ace" "ade" "cad" "cay" "bay"))) 8)

; additional tests subbag
(check-true (subbag? '() '()))
(check-false (subbag? '(b) '()))

; additional tests valid-wordList
(check-equal? (valid-wordList "" '()) '())
(check-equal? (valid-wordList "abc" '()) '())
(check-equal? (valid-wordList "" '("acb" "cat")) '())

; additional tests word-rank
(check-equal? (word-rank '()) '())
(check-equal? (word-rank '("a")) '(("a" 1)))

; additional tests best-word
(check-equal? (best-word "" '()) '("" 0))
(check-equal? (best-word "abcd" '()) '("" 0))