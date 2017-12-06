#lang racket

#|

Usage:

(!
#<<END
(sub "Subclass1" "Subclass2" "Subclass3")
Class 1
Entry 1
Entry 2
...
Entry n-1
Entry n


(sub "SubclassX" "SubclassY" "SubclassZ")
Class 2
Entry 1
Entry 2
...
Entry m-1
Entry m


...
END
)

If you want to change the maximum division size and/or maximum number of winners
from the defaults of 20 and 6, respectively, you can include the optional
parameters #:max-division-size and/or #:max-winners, respectively. So instead of

(!
...
)

you would type

(! #:max-division-size 30 #:max-winners 10
...
)

The subclass clause can be omitted when a class has no subclasses, in which case
the program won't generate a champion or reserve champion. Also, you can suppress
CH/RC generation by adding a "no-champs" clause before a block like so:

"...

(no-champs)
(sub "Subclass1" "Subclass2" "Subclass3")
Class 1
..."

In addition, you can generate additional champions and reserve champions by
inserting "super" clauses:

"...
(super "Title" of "Class 1" "Class 2")
..."

|#

(require (only-in "shared/main.rkt" cancelled-str)
         srfi/2)

(provide (rename-out [! run-show]))

;;; CONSTANTS/DEFAULTS

(define max-winners       (make-parameter 6))
(define max-division-size (make-parameter 20))

(define (! str #:max-division-size [mdv (max-division-size)] #:max-winners [mw (max-winners)])
  (parameterize ([max-division-size mdv]
                 [max-winners       mw])
    (let* ([block*            (listing->block* str)]
           [block+class*      (for/list ([block block*])
                                (if (class-block? block)
                                  (class-block->class block)
                                  block))]
           [class*            (filter class? block+class*)]
           [superclass+class* (for/list ([block/class block+class*])
                                (if (class? block/class)
                                  block/class
                                  (superclass-block->superclass class* block/class)))]
           [output-str*       (map format-output superclass+class*)]
           [output            (string-append* (add-between output-str* "\n"))])
      (display output))))

(define/match (class-block->class block)
  [((class-block command* name entry*))
   (define (filter-car-eq? x) (filter (λ (ls) (eq? (car ls) x)) command*))
   (let ([subclass-name*? (match (filter-car-eq? 'sub)
                            [`((sub . ,subclass-name*)) subclass-name*]
                            ['()                        #f]
                            [else (error "bad block: " (~a block))])]
         [mk-champs?      (match (filter-car-eq? 'no-champs)
                            ['((no-champs)) #f]
                            ['()            ((length entry*) . > . 1)]
                            [else (error "bad block: " (~a block))])])
     (mk-class name entry* subclass-name*? mk-champs?))])

(define (superclass-block->superclass class* block)
  (match block
    [(superclass-block name cl*)
     (let*-values ([(matching)      (filter (λ (cl) (member (class-name cl) cl*)) class*)]
                   [(entry*)        (apply set-union (map class-entry* matching))]
                   [(entry-count)   (length (set->list entry*))]
                   [(winner*)       (append* (map class->winner* matching))]
                   [(champ r-champ) (winner*->champs winner*)])
       (let ([matching-name* (map class-name matching)])
         (for ([cl cl*] #:unless (member cl matching-name*))
           (error (format "Could not find class '~a' when generating superclass '~a'" cl name))))
       (superclass name entry-count champ r-champ))]))

(define/match (class->winner* cl)
  [((class/simple _ _ _ winner*))       winner*]
  [((class/divisions _ _ _ division*))  (append* (map division->winner* division*))]
  [((class/subclasses _ _ _ subclass*)) (append* (map subclass-winner* subclass*))])

(define/match (division->winner* div)
  [((division/simple _ _ _ winner*))       winner*]
  [((division/subclasses _ _ _ subclass*)) (append* (map subclass-winner* subclass*))])


;;; STRUCTS

(define-struct class (name entry* entry-count) #:transparent)
(define-struct (class/cancelled         class)            () #:transparent)
(define-struct (class/simple            class)            (winner*) #:transparent)
(define-struct (class/divisions         class)            (division*) #:transparent)
(define-struct (class/subclasses        class)            (subclass*) #:transparent)
(define-struct (class/subclasses+champs class/subclasses) (champ r-champ) #:transparent)

(define-struct division (ix entry* entry-count) #:transparent)
(define-struct (division/simple            division)            (winner*) #:transparent)
(define-struct (division/subclasses        division)            (subclass*) #:transparent)
(define-struct (division/subclasses+champs division/subclasses) (champ r-champ) #:transparent)

(define-struct subclass (name winner*) #:transparent)

(define-struct winner (place entry entry-count) #:transparent)

(define-struct superclass (name entry-count champ r-champ) #:transparent)


;;; RESULTS CONSTRUCTORS

(define (mk-class name entry* subclass-name*? mk-champs?)
  (let* ([shuffled*   (list->set (shuffle entry*))]
         [entry-count (set-count shuffled*)])
    (cond [(equal? cancelled-str (car entry*))
           #||#   (class/cancelled name (set) 0)]
          [(entry-count . > . (max-division-size))
           #||#   (mk-class/divisions name shuffled* entry-count subclass-name*? mk-champs?)]
          [subclass-name*?
           #||#   (let ([constructor (if mk-champs? mk-class/subclasses+champs mk-class/subclasses)])
                    (constructor name shuffled* entry-count subclass-name*?))]
          [else
           #||#   (mk-class/simple name shuffled* entry-count)])))

(define (mk-class/subclasses+champs name entry* entry-count subclass-name*)
  (let*-values ([(subclass*)     (mk-subclasses entry* entry-count subclass-name*)]
                [(champ r-champ) (subclass*->champs subclass*)])
    (class/subclasses+champs name entry* entry-count subclass* champ r-champ)))

(define (mk-class/subclasses name entry* entry-count subclass-name*)
  (let ([subclass* (mk-subclasses entry* entry-count subclass-name*)])
    (class/subclasses name entry* entry-count subclass*)))

(define (mk-class/simple name entry* entry-count)
  (let ([winner* (mk-winner* entry* entry-count)])
    (class/simple name entry* entry-count winner*)))

(define (mk-class/divisions name entry* entry-count subclass-name*? mk-champs?)
  (let* ([size*     (mk-partition-size* (max-division-size) entry-count)]
         [entry**   (split-set size* entry*)]
         [division* (mk-division* size* entry** subclass-name*? mk-champs?)])
    (class/divisions name entry* entry-count division*)))

;; Some tricky arithmetic here to make the sizes as consistent as possible while
;; not exceeding the max. For example, if max = 20, a list of n = 66 entries
;; should split into groups of size 17, 17, 16, and 16. Wish I could explain the
;; algorithm!
(define (mk-partition-size* max n)
  (let*-values ([(q r)   (quotient/remainder n max)]
                [(delta) (- max r)]
                [(q^ r^) (quotient/remainder delta (+ q 1))]
                [(size1) (- max q^)]  [(count1) (- (+ 1 q) r^)]
                [(size2) (- size1 1)] [(count2) (/ (- n (* size1 count1)) size2)])
    (append (build-list count1 (λ (_) size1))
            (build-list count2 (λ (_) size2)))))

(define (split-set size* st)
  (let*-values ([(lst) (set->list st)]
                [(subset* _) (for/fold ([subset* '()] [lst^ lst])
                                       ([size (reverse size*)])
                               (let-values ([(before after) (split-at lst^ size)])
                                 (values (cons (list->set before) subset*) after)))])
    subset*))

(define (mk-division* size* entry** subclass-name*? mk-champs?)
  (for/list ([size   size*]
             [entry* entry**]
             [ix     (in-naturals)])
    (mk-division ix entry* size subclass-name*? mk-champs?)))

(define (mk-division ix entry* entry-count subclass-name*? mk-champs?)
  (cond [subclass-name*?
         (let ([subclass* (mk-subclasses entry* entry-count subclass-name*?)])
           (if mk-champs?
             (let-values ([(champ r-champ) (subclass*->champs subclass*)])
               (division/subclasses+champs ix entry* entry-count subclass* champ r-champ))
             (division/subclasses ix entry* entry-count subclass*)))]
        [else
         (let ([winner* (mk-winner* entry* entry-count)])
           (division/simple ix entry* entry-count winner*))]))

(define (mk-subclasses entry* entry-count subclass-name*)
  (for/list ([name subclass-name*])
    (let ([winner* (mk-winner* entry* entry-count)])
      (subclass name winner*))))

(define (mk-winner* entry* entry-count)
  (let ([shuffled* (shuffle (set->list entry*))])
    (for/list ([entry shuffled*] [place (in-range 1 (+ 1 (max-winners)))])
      (winner place entry entry-count))))

(define (subclass*->champs subclass*)
  (let ([winner* (append* (map subclass-winner* subclass*))])
    (winner*->champs winner*)))

(define (winner*->champs winner*)
  (let* ([shuffled*      (shuffle winner*)] ;; To keep tie-breaking sufficiently random
         [entry->points  (make-hash)]
         [_              (for ([winner shuffled*])
                           (let ([points (winner->points winner)])
                             (hash-update! entry->points (winner-entry winner) (λ (x) (+ x points)) 0)))]
         [winner/points* (hash->list entry->points)]
         [sorted         (sort winner/points* > #:key cdr)]
         [champ          (car (car sorted))]
         [r-champ        (car (cadr sorted))])
    (values champ r-champ)))

(define (winner->points wnr)
  (let ([place       (winner-place wnr)]
        [entry-count (winner-entry-count wnr)])
    (+ 1 (- (min entry-count (max-winners)) place))))


;;; PARSING

;; A block is a contiguous group of non-empty lines. Each block corresponds to a
;; class, or else command(s) pertaining to the entire show.
(define-struct block () #:transparent)
(define-struct (class-block block)      (command* name entry*) #:transparent)
(define-struct (superclass-block block) (name class*) #:transparent)

(define (listing->block* str)
  (let ([block* (regexp-split #px"\n[:space:]*\n" (string-trim str))])
    (map parse-block block*)))

(define (read-block* in)
  (for/list ([block? (in-port read-block in)]
             #:break (not block?))
    block?))

;; Skip initial empty lines, then read any number of non-empty lines.
(define (read-block in)
  (and-let* ([first-line (first-non-blank-line in)]
             [line*      (non-blank-lines in)])
    (string-append* (add-between (cons first-line line*) "\n"))))

(define (first-non-blank-line in)
  (for/first ([line    (in-port read-line in)]
              #:unless (blank-line? line))
    (if (eof-object? line) #f line)))

(define (non-blank-lines in)
  (for/list ([line   (in-lines in)]
             #:break (blank-line? line))
    line))

(define (blank-line? str) (andmap char-whitespace? (string->list str)))

(define (parse-block block-str)
  (with-handlers ([exn? (λ (_) (error "failed while parsing block:\n\n" block-str))])
    (let* ([in          (open-input-string block-str)]
           [command*    (read-command* in)]
           [class-name? (let ([line (read-line in)])
                          (if (eof-object? line) #f (string-trim line)))]
           [entry*      (for/list ([line (in-lines in)]) (string-trim line))])
      (cond
        [(and (not class-name?) (= 1 (length command*)) (eq? 'super (caar command*)))
         (match (car command*)
           [(list 'super name 'of class0 class* ...)
            (superclass-block name (cons class0 class*))])]
        [(and class-name? (1 . <= . (length entry*)))
         (class-block command* class-name? entry*)]))))

;; Read (as sexpressions) any number of lines starting with '('
(define (read-command* in)
  (for/list ([char   (in-port peek-char in)]
             #:break (not (eq? char #\()))
    (let ([command (read in)])
      (skip-to-next-line in)
      command)))

(define (skip-to-next-line in)
  (for ([char   (in-port read-char in)]
        #:break (eq? char #\newline))
    (void)))


;;; FORMATTING

(define/match (format-output x)
  [((? superclass?)) (format-superclass x)]
  [((? class?))      (format-class x)])

(define/match (format-superclass sc)
  [((superclass name entry-count champ r-champ))
   (format-champs name #f entry-count champ r-champ)])

(define/match (format-class cl)
  [((class/cancelled name _ _))
   #||#   (format "[b]~a[/b]\n~a\n" name cancelled-str)]
  
  [((class/simple name _ entry-count winner*))
   #||#   (format-block name #f #f entry-count winner*)]
  
  [((class/divisions name _ _ division*))
   #||#   (let ([division-str* (map (curry format-division name) division*)])
            (string-append* (add-between division-str* "\n")))]
  
  [((class/subclasses+champs name _ entry-count subclass* champ r-champ))
   #||#   (format-subclasses+champs name #f entry-count subclass* champ r-champ)]
  
  [((class/subclasses name _ entry-count subclass*))
   #||#   (format-subclasses name #f entry-count subclass*)])

(define (format-division class-name div)
  (match div
    [(division/simple ix _ entry-count winner*)
     (format-block class-name ix #f entry-count winner*)]
    [(division/subclasses+champs ix _ entry-count subclass* champ r-champ)
     (format-subclasses+champs class-name ix entry-count subclass* champ r-champ)]
    [(division/subclasses ix _ entry-count subclass*)
     (format-subclasses class-name ix entry-count subclass*)]))

(define (format-subclasses class-name ix? entry-count subclass*)
  (let ([subclass-str* (map (curry format-subclass class-name ix? entry-count) subclass*)])
    (string-append* (add-between subclass-str* "\n"))))

(define (format-subclasses+champs class-name ix? entry-count subclass* champ r-champ)
  (let ([subclass*-str (format-subclasses class-name ix? entry-count subclass*)]
        [champs-str    (format-champs class-name ix? entry-count champ r-champ)])
    (string-append subclass*-str "\n" champs-str)))

(define (format-subclass class-name ix entry-count sub)
  (match-let* ([(subclass name winner*) sub])
    (format-block class-name ix name entry-count winner*)))

(define (format-block class-name ix subclass-name entry-count winner*)
  (let ([header      (format-header class-name ix subclass-name entry-count)]
        [winner-str* (map format-winner winner*)])
    (string-append* header winner-str*)))

(define (format-champs class-name div-ix? entry-count champ r-champ)
  (let ([header      (format-header class-name div-ix? #f entry-count)]
        [champ-str   (format "CH: ~a\n" champ)]
        [r-champ-str (format "RC: ~a\n" r-champ)])
    (string-append header champ-str r-champ-str)))

(define (format-header class-name div-ix? subclass? entry-count)
  (let ([div-str       (if div-ix? (format " Div. ~a" (div-ix->char div-ix?)) "")]
        [subclass-str  (if subclass? (format " ~a" subclass?) "")]
        [entr-y/ies (if (= 1 entry-count) "Entry" "Entries")])
    (format "[b]~a~a~a[/b] (~a ~a)\n" class-name div-str subclass-str entry-count entr-y/ies)))

(define (div-ix->char div-ix)
  (define char* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (when (div-ix . > . 25)
    (error "Too many divisions. Each class must split into no more than 26 divisions."))
  (string-ref char* div-ix))

(define/match (format-winner wnr)
  [((winner place entry _)) (format "~a - ~a\n" (nat->ordinal place) entry)])

(define (nat->ordinal n)
  (let* ([base   (format "~a" n)]
         [rem    (remainder n 10)]
         [suffix (if (and (10 . < . n) (n . < . 20))
                   "th"
                   (case rem
                     [(1) "st"] [(2) "nd"] [(3) "rd"]
                     [(0 4 5 6 7 8 9) "th"]))])
    (string-append base suffix)))