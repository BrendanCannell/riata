#lang racket

#|

Usage:
(!               "<spreadsheet column>" "<show>")
(! #:verbose? #t "<spreadsheet column>" "<show>") to show extra info

The program will first list all show winners in each class, if any, that do not appear in that class in the spreadsheet column. You'll need to add rows to the spreadsheet for each one. The name comparison is case-sensitive, and it can also get tripped up if, for example, a name is spelled with a curved apostrophe (’) in one place and with a single quote (') in the other. After that, the program will print out the points data, with the entrants in each class listed in alphabetical order. Once the new winners are added, you should be able to paste the points data directly into the spreadsheet and have it line up.

Especially the first time you use the program, I recommend using the following procedure:
1. Run the program in verbose mode, which will print the name of each entrant beside the points they earned.
2. Add any new entrants to the spreadsheet.
3. Insert two new columns on the far left.
4. Copy/paste the points data (with names) into the left column; this will insert the points in the left column and the entrant names in the right column.
5. Copy the far-left column only.
6. Go down the right column and compare the name given by the program and the name next to it that you typed in. In this way you can see if you've mis-ordered any entrants and thus avoid mistakenly assigning points to the wrong ones.
7. Fix any such errors.
8. Delete the two left columns.
9. Paste the points column, which you copied in step 5, in the appropriate place.

|#

(require "shared/main.rkt"
         profile)

(define verbose? (make-parameter #f))

(define (! horse-column-str show-str #:verbose? [-verbose? (verbose?)])
  (parameterize ([verbose? -verbose?])
    (let* ([class?*      (parse-entrant-column horse-column-str)]
           [class*       (filter (λ (x) x) class?*)]
           [result*      (parse-show show-str)]
           [new-entry**  (get-new-entry* class* result*)]
           [column-str   (mk-column result* class?*)])
      (when (not (null? new-entry**))
        (displayln "New Entries:\n")
        (for ([new-entry* new-entry**])
             (for ([line new-entry*])
                  (displayln line))
             (displayln "")))
      (displayln "Points Column:\n")
      (display column-str))))

(define (get-new-entry* class* result*)
  (filter (λ (x) x)
          (for/list ([cl class*])
                    (match-let* ([(class cl-name known-entry*) cl]
                                 [res*           (filter (λ (res) (string-prefix? (select 'class res) cl-name)) result*)]
                                 [current-entry* (for/list ([res res*]) (select 'name res))]
                                 [new*           (remove-duplicates (remove* known-entry* current-entry*))])
                      (if (not (null? new*))
                          (cons (format "~a:" cl-name) (sort new* string<?))
                          #f)))))

(define-struct class (name entry*) #:transparent)

(define (mk-column result* class?*)
  (let ([output-str**
         (for/list ([cl? class?*])
                   (if (not cl?)
                       '("")
                       (match-let* ([(class cl-name known-entry*) cl?]
                                    [res*           (filter (λ (res) (string-prefix? (select 'class res) cl-name)) result*)]
                                    [name->points   (make-hash)]
                                    [_              (for ([res res*])
                                                         (let ([name   (select 'name res)]
                                                               [points (select 'points res)])
                                                           (hash-update! name->points name (λ (x) (+ x points)) 0)))]
                                    [current-entry* (for/list ([res res*]) (select 'name res))]
                                    [all-entry*     (remove-duplicates (append known-entry* current-entry*))]
                                    [heading        (if (verbose?) (string-append "\t" cl-name) "")]
                                    [points-str*    (for/list ([entry (sort all-entry* string-ci<?)])
                                                              (let ([points (hash-ref name->points entry 0)])
                                                                (if (verbose?)
                                                                    (format "~a\t~a" points entry)
                                                                    (~a points))))])
                         (cons heading points-str*))))])
    (string-append* (add-between (append* output-str**) "\n"))))

(define (parse-entrant-column str)
  (define (isolate pred lst)
    (let*-values ([(acc current) (for/fold ([acc '()] [current '()])
                                           ([elem lst])
                                   (if (pred elem)
                                       (if (null? current)
                                           (values (list* (list elem) acc) '())
                                           (values (list* (list elem) (reverse current) acc) '()))
                                       (values acc (cons elem current))))]
                  [(rev-result) (if (null? current) acc (cons (reverse current) acc))])
      (reverse rev-result)))
  (let* ([line*  (string-split str "\n")]
         [block* (isolate (λ (s) (regexp-match? #rx"^[:blank:]*$" s)) line*)])
    (for/list ([block block*])
              (if (regexp-match? #px"^[:blank:]*$" (car block))
                  #f
                  (class (car block) (cdr block))))))