#lang racket

(provide (all-defined-out))

(define max-winners   6)
(define cancelled-str "CANCELLED")

(define (select* key* ht) (for/list ([key key*]) (select key ht)))

(define (select key ht) (hash-ref ht key (λ () (error "missing field ~a from result ~a" key ht))))

#|
parse-show goes block by block, using heuristic tests to classify blocks and dispatch them to handlers. Handlers return zero or more result tuples and a (possibly updated) state. The state contains the date, event name, and prize data.
 |#

(define (parse-show str)
  (let*-values ([(block*)     (listing->block* str)]
                [(init-state) (parse-show-header (car block*))]
                [(result* _)  (for/fold ([result* '()] [state init-state])
                                        ([block (cdr block*)])
                                (let-values ([(res* state^) (parse-block block state)])
                                  (values (append res* result*) state^)))])
    result*))

(define (print-unrecognized str) (eprintf "Unrecognized: ~s\n" str))

(define (listing->block* str) (regexp-split #rx"\n *\n" (string-trim str)))

(define (parse-show-header str)
  (let*-values ([(line*) (string-split str "\n")]
                [(event date unrecognized*)
                 (for/fold ([event #f] [date #f] [unrecognized* '()])
                           ([line line*])
                   (define (! rx) (regexp-match rx line))
                   (cond [(! #px"^.*(?i:show name).\\s*(.*)$")
                          => (match-lambda [`(,_ ,show-name) (values show-name date unrecognized*)])]
                         [(! #px"^.*(?i:show date).\\s*(.*)$")
                          => (match-lambda [`(,_ ,show-date) (values event show-date unrecognized*)])]
                         [(or (! #px"^.*(?i:show host).\\s*(.*)$")
                              (! #px"^.*(?i:association/season).\\s*(.*)$")
                              (! #px"^.*(?i:notes?).\\s*(.*)$"))
                          (values event date unrecognized*)]
                         [else (values event date (cons line unrecognized*))]))])
    (cond [(not event) (error "Could not find show name in header: " str)]
          [(not date)  (error "Could not find show date in header: " str)]
          [else        (map print-unrecognized unrecognized*)
                       (hash 'event event 'date date)])))

(define (parse-block block state)
  (cond [(prize-block? block)     (parse-prize-block block state)]
        [(cancelled-block? block) (values '() state)]
        [(results-block? block)   (parse-results-block block state)]
        [else (begin (print-unrecognized block) (values '() state))]))

(define (prize-block? str)     (string-contains? str "$"))
(define (cancelled-block? str) (string-contains? str cancelled-str))
(define (results-block? str)   (regexp-match? rx:entry-count str))

(define (parse-prize-block str state)
  (let* ([normalized      (string-normalize-spaces str)]
         [prize-data-str* (regexp-match* rx:prize-datum normalized)]
         [remaining       (for/fold ([remaining normalized])
                                    ([datum-str prize-data-str*])
                            (string-replace remaining datum-str ""))]
         [prize*          (map parse-prize prize-data-str*)]
         [state^          (for/fold ([state state])
                                    ([prize prize*])
                            (match-let ([(cons place earnings) prize])
                              (hash-set state place earnings)))])
    (print-unrecognized (string-trim remaining))
    (values '() state^)))

(define (parse-prize str)
  (cond
    [(regexp-match rx:prize-datum str)
     => (match-lambda [(list _ place-str earnings-str)
                       (cons (parse-place place-str) (parse-earnings earnings-str))])]))

(define rx:prize-datum #px"((?i:[0-9]+(?:st|nd|rd|th)?|purse|rg|rgc|reserve grand champion|reserve grand|gc|grand champion|rc|rch|reserve champion|reserve|ch|champion|hm|honorable mention|win|place|show)) *(?:\\p{P})+ *\\$([0-9,]*)")

(define (parse-place str)
  (match (string-trim str)
    [(regexp #rx"^(?i:purse)$") 'PURSE]
    [(regexp #rx"^(?i:(rg|rgc|reserve grand|reserve grand champion))$") 'RG]
    [(regexp #rx"^(?i:(gc|grand champion))$")                           'GC]
    [(regexp #rx"^(?i:(rc|rch|reserve|reserve champion))$")             'RCH]
    [(regexp #rx"^(?i:(ch|champion))$")                                 'CH]
    [(regexp #rx"^(?i:(hm|honorable mention))$")                        'HM]
    [(regexp #rx"^(?i:win)$")   1]
    [(regexp #rx"^(?i:place)$") 2]
    [(regexp #rx"^(?i:show)$")  3]
    [(regexp #rx"^[0-9]+(?i:st|nd|rd|th)?$") (string->number (car (regexp-match #rx"[0-9]*" str)))]))

(define (parse-earnings str) (string->number (string-replace str #rx"," "")))

(define (parse-results-block str state)
  (let*-values ([(line*)             (string-split str "\n")]
                [(line*^)            (skip-until-header line*)]
                [(class entry-count) (parse-heading (car line*^))]
                [(result*)           (for/list ([line (cdr line*^)])
                                               (mk-result state class entry-count line))])
    (values (filter (λ (x) x) result*) state)))

(define (skip-until-header str*)
  (cond
    [(regexp-match? rx:entry-count (car str*)) str*]
    [else (print-unrecognized (car str*))
          (skip-until-header (cdr str*))]))

(define (parse-heading str)
  (define prefix-rx* (let ([rx:letter-number       #px"^[a-zA-Z][0-9]+(?:\\p{P})*\\s*"]
                           [rx:number-letter?      #px"^[0-9]+[a-zA-Z]?(?:\\p{P})*\\s*"]
                           [rx:race-letter-number  #px"^Race [a-zA-Z][0-9]+(?:\\p{P})*\\s*"]
                           [rx:race-number-letter? #px"^Race [0-9]+[a-zA-Z]?(?:\\p{P})*\\s*"])
                       (list rx:letter-number rx:number-letter? rx:race-letter-number rx:race-number-letter?)))
  (match (parse-entry-count str)
    [#f #f]
    [`(,entry-count-str . ,entry-count)
     (let* ([str^       (string-trim str entry-count-str #:left? #f)]
            [class-name (for/fold ([str str^])
                                  ([prefix-rx prefix-rx*])
                          (string-replace str prefix-rx ""))])
       (values class-name entry-count))]))

(define (parse-entry-count str)
  (let ([match* (regexp-match rx:entry-count str)])
    (if match*
        (cons (car match*) (string->number (cadr match*)))
        #f)))

(define rx:entry-count #px"(?i:\\s*\\(\\s*([0-9]+)\\s+(?:entry|entries)\\s*\\)\\s*)")

(define (mk-result state class entry-count line)
  (let* ([date  (hash-ref state 'date)]
         [event (hash-ref state 'event)])
    (match (parse-result line)
      [#f #f]
      [`(,place ,name ,owner)
       (match (hash-ref state place #f)
         [#f (eprintf "No earnings data for place \"~a\" in result: \"~a\"\n" place line)
             #f]
         [earnings (let ([points (mk-points place entry-count)])
                     (unless points (eprintf "Could not calculate points for: ~a, Place: ~a\n" name place))
                     (hash 'owner owner 'name name 'date date 'event event 'class class
                           'entry-count entry-count 'place place 'points points 'earnings earnings))])])))

(define (parse-result str)
  (match (regexp-match #px"^([a-zA-Z0-9 ]*)(?:\\p{P})+\\s*(.*) (?:owned|leased) by (.*) from .*$" str)
    [(list _ place-str name owner) (list (parse-place place-str) name owner)]
    [else (print-unrecognized str) #f]))

(define (mk-points place entry-count)
  (let* ([winner-count (min entry-count max-winners)]
         [points (match place
                   ['GC         6]
                   ['RGC        5]
                   ['CH         (+ 3 winner-count)]
                   ['RCH        (+ 2 winner-count)]
                   ['HM         (+ 1 winner-count)]
                   [(? number?) (- winner-count place -1)]
                   [_           #f])])
    points))