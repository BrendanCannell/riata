#lang racket

(provide (all-defined-out))

(define cancelled-str "CANCELLED")

(define (select* key* ht) (for/list ([key key*]) (select key ht)))

(define (select key ht) (hash-ref ht key (λ () (error "missing field ~a from result ~a" key ht))))

#|
parse-show goes block by block, using heuristic tests to classify blocks and dispatch them to handlers. Handlers return zero or more result tuples and a (possibly updated) state. The state contains the show name, date, and prize data.
 |#

(define (parse-show str)
  (let*-values ([(block*)     (listing->block* str)]
                [(init-state) (parse-show-header (car block*))]
                [(result* _)  (for/fold ([result* '()] [state init-state])
                                        ([block (cdr block*)])
                                (match-let ([`(,res* ,state^) (parse-block block state)])
                                  (values (append res* result*) state^)))])
    result*))

(define (print-unrecognized str) (eprintf "Unrecognized: ~s\n" str))

(define (listing->block* str) (regexp-split #rx"\n *\n" (string-trim str)))

(define (parse-show-header str)
  (let*-values ([(line*) (string-split str "\n")]
                [(show date unrecognized*)
                 (for/fold ([show #f] [date #f] [unrecognized* '()])
                           ([line line*])
                   (define (! rx) (regexp-match rx line))
                   (cond [(! #px"^.*?(?i:show name).\\s*(.*)$")
                          => (match-lambda [`(,_ ,show^) (values show^ date unrecognized*)])]
                         [(! #px"^.*?(?i:show date).\\s*(.*)$")
                          => (match-lambda [`(,_ ,date^) (values show date^ unrecognized*)])]
                         [(or (! #px"^.*?(?i:show host).\\s*(.*)$")
                              (! #px"^.*?(?i:association/season).\\s*(.*)$")
                              (! #px"^.*?(?i:notes?).\\s*(.*)$"))
                          (values show date unrecognized*)]
                         [else (values show date (cons line unrecognized*))]))])
    (cond [(not show) (error "Could not find show name in header: " str)]
          [(not date) (error "Could not find show date in header: " str)]
          [else       (map print-unrecognized unrecognized*)
                      (hash 'show show 'date date)])))

(define (parse-block block state)
  (cond [(cancelled-block? block) `(() ,state)]
        [(results-block? block)   (parse-results-block block state)]
        [(prize-block? block)     (parse-prize-block block state)]
        [else (print-unrecognized block)
              `(() ,state)]))

(define (cancelled-block? str) (string-contains? str "CANCELLED"))
(define (results-block? str)   (regexp-match? rx:header str))
(define (prize-block? str)     (string-contains? str "$"))

(define (parse-prize-block str state)
  (let* ([normalized      (string-normalize-spaces str)]
         [prize-data-str* (regexp-match* rx:prize-datum normalized)]
         [remaining       (for/fold ([remaining normalized])
                                    ([datum-str prize-data-str*])
                            (string-replace remaining datum-str ""))]
         [prize*          (map parse-prize prize-data-str*)]
         [state^          (for/fold ([state (clear-prize-data state)])
                                    ([prize prize*])
                            (match-let ([`(,place ,earnings) prize])
                              (hash-set state place earnings)))])
    (print-unrecognized (string-trim remaining))
    `(() ,state^)))

(define (clear-prize-data state)
  (for/hash ([(key value) (in-hash state)]
             #:unless (or (number? key) (member key '(PURSE RG GC RCH CH HM))))
    (values key value)))

(define (parse-prize str)
  (match str
    [(regexp rx:prize-datum `(,_ ,place-str ,earnings-str))
     `(,(place-str->place place-str) ,(parse-earnings earnings-str))])
  #;(cond
    [(regexp-match rx:prize-datum str)
     => (match-lambda [(list _ place-str earnings-str)
                       `(,(place-str->place place-str) ,(parse-earnings earnings-str))])]))

(define rx:prize-datum #px"((?i:[0-9]+(?:st|nd|rd|th)?|purse|rg|rgc|reserve grand champion|reserve grand|gc|grand champion|rc|rch|reserve champion|reserve|ch|champion|hm|honorable mention|win|place|show)) *(?:\\p{P})+ *\\$([0-9,]*)")

(define (place-str->place str)
  (match (string-trim str)
    [(regexp #rx"^[0-9]+(?i:st|nd|rd|th)?$") (string->number (car (regexp-match #rx"[0-9]*" str)))]
    [(regexp #rx"^(?i:purse)$") 'PURSE]
    [(regexp #rx"^(?i:rg|rgc|reserve grand|reserve grand champion)$") 'RG]
    [(regexp #rx"^(?i:gc|grand champion)$")                           'GC]
    [(regexp #rx"^(?i:rc|rch|reserve|reserve champion)$")             'RCH]
    [(regexp #rx"^(?i:ch|champion)$")                                 'CH]
    [(regexp #rx"^(?i:hm|honorable mention)$")                        'HM]
    [(regexp #rx"^(?i:win)$")   1]
    [(regexp #rx"^(?i:place)$") 2]
    [(regexp #rx"^(?i:show)$")  3]
    [else #f]))

(define (parse-earnings str) (string->number (string-replace str #rx"," "")))

(define (parse-results-block str state)
  (match-let* ([line*                 (string-split str "\n")]
               [`(,header . ,result*) (skip-until-header line*)]
               [`(,class ,paired?)    (parse-header header)]
               [paired-data?* (for/list ([result result*])
                                (->result-data state class paired? result))]
               [result-data?* (flatten paired-data?*)]
               [result-data*  (filter (λ (x) x) result-data?*)])
    `(,result-data* ,state)))

(define (skip-until-header str*)
  (cond
    [(regexp-match? rx:header (car str*)) str*]
    [else (print-unrecognized (car str*))
          (skip-until-header (cdr str*))]))

(define rx:header #px"^\\s*(?i:(?:race\\s+)?(?:[a-z][0-9]+|[0-9]+[a-z]?))?\\P{L}*(.*?)(?:\\s*\\(\\s*[0-9]+\\s+(?i:entry|entries)\\s*\\)\\s*)")

(define (parse-header str)
  (match-let ([`(,_ ,class) (regexp-match rx:header str)]
              [paired?      (regexp-match? #px"\\b(?i:pair|pairs|pas de deux)\\b" str)])
    `(,class ,paired?)))

#;(define (parse-header str)
  (match-let* ([`(,entry-count-str ,entry-count) (parse-entry-count str)]
               [class-name (header->class-name str entry-count-str)]
               [paired?    (regexp-match? #px"\\b(?i:pair|pairs|pas de deux)\\b" str)])
    `(,class-name ,entry-count ,paired?)))

#;(define (parse-entry-count str)
  (match (regexp-match rx:entry-count str)
    [#f #f]
    [`(,entry-count-str ,entry-count-digits)
     `(,entry-count-str ,(string->number entry-count-digits))]))

#;(define rx:entry-count #px"(?i:\\s*\\(\\s*([0-9]+)\\s+(?:entry|entries)\\s*\\)\\s*)")

#;(define (header->class-name header entry-count-str)
  (define prefix-rx* (let ([rx:letter-number       #px"^[a-zA-Z][0-9]+(?:\\p{P})*\\s*"]
                           [rx:number-letter?      #px"^[0-9]+[a-zA-Z]?(?:\\p{P})*\\s*"]
                           [rx:race-letter-number  #px"^Race [a-zA-Z][0-9]+(?:\\p{P})*\\s*"]
                           [rx:race-number-letter? #px"^Race [0-9]+[a-zA-Z]?(?:\\p{P})*\\s*"])
                       `(,rx:letter-number ,rx:number-letter? ,rx:race-letter-number ,rx:race-number-letter?)))
  (let* ([header/no-entry-count
          (string-trim header entry-count-str #:left? #f)]
         [class-name (for/fold ([str header/no-entry-count])
                               ([prefix-rx prefix-rx*])
                       (string-replace str prefix-rx ""))])
    class-name))



(define (->result-data state class paired? line)
  (let* ([date (hash-ref state 'date)]
         [show (hash-ref state 'show)])
    (match (parse-result line)
      [#f (print-unrecognized line)
          #f]
      [`(,place ,horse ,rider? ,owner? ,farm? ,score?)
       (let ([->datum (λ (horse earnings)
                        (hash 'date  date   'show  show   'class    class
                              'place place  'horse horse  'earnings earnings
                              'rider rider? 'owner owner? 'farm     farm? 'score score?))]
             [prize?  (->earnings state place)])
         (cond
           [(not prize?) (eprintf "No earnings data for place \"~a\" in result: \"~a\"\n" place line)
                         #f]
           [(not paired?)  (->datum horse prize?)]
           [else (match (parse-paired horse)
                   [#f (eprintf "Couldn't parse pair into individuals in pair \"~a\" in result: \"~a\"\n"
                                horse line)
                       #f]
                   [`(,horse1 ,horse2)
                    (let* ([split-earnings (/ prize? 2.0)]
                           [->datum        (λ (horse) (->datum horse split-earnings))])
                      `(,(->datum horse1) ,(->datum horse2)))])]))])))

;; Default to zero for numerical places only
(define (->earnings state place)
  (let ([earnings? (hash-ref state place #f)])
    (cond [earnings?       earnings?]
          [(number? place) 0]
          [else            #f])))

(define (parse-paired horses)
  (match (regexp-match #px"^([^,]*?)\\s*,\\s*(.*)$" horses)
    [#f #f]
    [`(,_ ,horse1 ,horse2) `(,horse1 ,horse2)]))

;; Works backward the end of the string.
(define (parse-result str)
  (match-let*
      ([`(,score? ,rest)         (parse-score str)]
       [`(,owner? ,farm1? ,rest) (parse-owner/farm rest)]
       [`(,rider? ,farm2? ,rest) (parse-rider/farm rest)]
       [`(,place-str? ,horse?)   (parse-place/horse rest)])
    (and place-str? horse?
         ;; I'm told that at most one of farm1 and farm2 will be present.
         (let ([farm? (or farm1? farm2?)]
               [place (place-str->place place-str?)])
           `(,place ,horse? ,rider? ,owner? ,farm? ,score?)))))

(define rxs:sep "[,\\s]+")

(define (->rx$ rxs) (pregexp (++ rxs "$")))
(define ++ string-append)
(define (+sep+ . str*) (string-join str* rxs:sep))

(define rxs:owned-by-x  (+sep+ "(?i:owned|leased)" "(?i:by)" "(.*?)"))
(define rxs:ridden-by-x (+sep+ "(?i:ridden|handled)" "(?i:by)" "(.*?)"))

(define (postpend-of-x rxs)
  (+sep+ rxs "(?i:of|from)" "(.*)"))
(define rxs:owned-by-x-of-y  (postpend-of-x rxs:owned-by-x))
(define rxs:ridden-by-x-of-y (postpend-of-x rxs:ridden-by-x))

(define (->and?-rx$ rxs) (->rx$ (+sep+ "(?i:" "and)?" rxs)))
(define rx:and?-owned-by-x       (->and?-rx$ rxs:owned-by-x))
(define rx:and?-owned-by-x-of-y  (->and?-rx$ rxs:owned-by-x-of-y))
(define rx:and?-ridden-by-x      (->and?-rx$ rxs:ridden-by-x))
(define rx:and?-ridden-by-x-of-y (->and?-rx$ rxs:ridden-by-x-of-y))

(define (parse-score str)
  (match (regexp-match #px"\\P{L}*?(?<![\\$\\.0-9])(?:([0-9]+(?:\\.[0-9]+)?%?)\\P{L}*?)?$" str)
    [`(,all ,score) `(,score ,(remove-suffix str all))]))

(define (parse-owner/farm str)
  (match str
    [(regexp rx:and?-owned-by-x-of-y `(,all ,owner ,farm))
     `(,owner ,farm ,(remove-suffix str all))]
    [(regexp rx:and?-owned-by-x      `(,all ,owner))
     `(,owner #f    ,(remove-suffix str all))]
    [else `(#f #f ,str)]))

(define (parse-rider/farm str)
  (match str
    [(regexp rx:and?-ridden-by-x-of-y `(,all ,rider ,farm))
     `(,rider ,farm ,(remove-suffix str all))]
    [(regexp rx:and?-ridden-by-x      `(,all ,rider))
     `(,rider #f    ,(remove-suffix str all))]
    [else `(#f #f ,str)]))

(define (parse-place/horse str)
  (match (regexp-match #px"^(\\P{P}*)(?:\\p{P})+\\s*(.*)$" str)
    [`(,_ ,place ,horse) `(,place ,horse)]
    [else `(#f #f)]))

(define (remove-suffix str suffix)
  (string-trim str suffix #:left? #f))

;; A place composed of any number of non-punctuation characters, a separator of at least
;; one punctuation character, "<horse> owned/etc by <human>", optional "of/etc <any>",
;; optional punctuation and score, ending with any number of non-letter characters.
#;(define rx:result #px"^(\\P{P}*)(?:\\p{P})+\\s*(.*?)\\s+(?i:owned|leased|ridden|handled)\\s+by\\s+(.*?)(?:\\s+(?i:of|from|owned) .*?)?(?:\\s+\\p{P}+\\s+((?<!\\$)[0-9]+(?:\\.[0-9]+)?%?))?\\P{L}*?$")

#;(define (parse-result str)
  (match (regexp-match rx:result str)
    [`(,_ ,place-str ,horse ,human ,score) `(,(place-str->place place-str) ,horse ,human ,score)]
    [else #f]))

#;(define (mk-points place entry-count)
    (let* ([winner-count (min entry-count (max-winners))]
           [points (match place
                     ['GC         6]
                     ['RGC        5]
                     ['CH         (+ 3 winner-count)]
                     ['RCH        (+ 2 winner-count)]
                     ['HM         (+ 1 winner-count)]
                     [(? number?) (- winner-count place -1)]
                     [_           #f])])
      points))