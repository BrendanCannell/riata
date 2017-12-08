#lang racket

#|

Usage:  (! <selectors> "<input>")
Selector keywords:  #:horse, #:owner, #:rider, #:farm
Output CSV:  horse,date,show,class,place,earnings[,rider][,score]

Each selector keyword can be followed be either a string, or a parenthesized list of strings preceded by a single-quote:

  #:horse "Alice"
  #:horse '("Alice" "Bob")

The first selector above will select only results for horses named Alice, while the second will select results for horses named either Alice or Bob. If multiple selectors are present, only results matching all of them are returned, so

  (! #:horse '("Alice" "Bob") #:owner '("Carol" "Dan") "<input>")

would return results for horses named either Alice or Bob owned by either Carol or Dan. For convenience, the plural forms of each selector keyword (#:horses, #:owners, #:riders, #:farms) are aliases of the singular. Selectors can be used in any order, and each can be used at most once.

The program will print the total earnings of all returned results. It will also print whatever part of the input text it didn't recognize, if any. Be sure to examine this. In particular, if any prize amounts went unrecognized, the earnings for the relevant result outputs may be wrong.

To simplify the task, I've put some requirements on the input format that may sometimes necessitate that you do some minor pre-processing by hand:

* The main show data (name and date in particular) must be in a single block before anything else.

* Each block of results must feature a class header line (of the form "<Optional-Prefix?><Class Title> (<Number> Entries)") followed by the individual results. Any lines before the header line will be ignored/unrecognized.

* Any block that is not a results block and contains at least one "$" is assumed to be prize data. The program will look for dollar amounts preceded by known names for places, allowing for separating punctuation, e.g., "1st: $500" or "Champion - $1,200". Any other text will be reported as unrecognized.

* Prize data must come before the results that it applies to. When the program encounters a prize block, it uses that data to calculate earnings for subsequent results until it encounters another prize block, then it uses that data until it encounters another one, and so on. So if the beginning of the listing has multiple prize data blocks for different parts of the show, you would have to copy and paste them before the relevant sections.

Troubleshooting:
 * If you're selecting for a name and you get a "<name>: undefined" error, you need to put the name in double-quotes.
 * If you're using a selector with a list of strings and you get an "application, not a procedure", "application: no case matching <number> non-keyword arguments", or "arity mismatch" error, make sure you have the list in parentheses and preceded by a single-quote.

|#

(require "shared/main.rkt")

(define (! str
           #:horse [horse? #f] #:horses [horses? #f]
           #:owner [owner? #f] #:owners [owners? #f]
           #:rider [rider? #f] #:riders [riders? #f]
           #:farm  [farm?  #f] #:farms  [farms?  #f])
  (let* ([horses? (merge-selectors horse? horses? "horse")]
         [owners? (merge-selectors owner? owners? "owner")]
         [riders? (merge-selectors rider? riders? "rider")]
         [farms?  (merge-selectors farm?  farms?  "farm")]
         [result*     (parse-show str)]
         [result*^    (filter-field? 'horse horses? result*)]
         [result*^^   (filter-field? 'owner owners? result*^)]
         [result*^^^  (filter-field? 'rider riders? result*^^)]
         [result*^^^^ (filter-field? 'farm farms? result*^^^)]
         [value** (let ([output-field* '(horse date show class place earnings rider score)])
                    (for/list ([res result*^^^^]) (select* output-field* res)))])
    (display-csv* value**)
    (printf "Total earnings: $~a\n" (->total-earnings result*^^^^))))

(define (filter-field? field value*? input*)
  (match value*?
    [#f      input*]
    [value*  (define (value-in-value*? value)
               (member (select field value) value*))
             (filter value-in-value*? input*)]))

(define (merge-selectors singular plural singular-str)
  (define (fail-both)
    (let* ([plural-str  (string-append singular-str "s")]
           [singular-kw (string-append "#:" singular-str)]
           [plural-kw   (string-append "#:" plural-str)])
      (error "error: cannot use both" singular-kw "and" plural-kw)))
  (define (validate arg)
    (cond
      [(string? arg) `(,arg)]
      [(list? arg)   (for ([subarg arg] #:unless (string? subarg))
                       (error "error: member" subarg "of selector list" arg "is not a string."
                              (if (symbol? subarg) "Check for proper double-quotes." "")))
                     arg]
      [else (error "error: selector argument" arg "is not a string or list of strings.")]))
  (cond
    [(and singular plural) (fail-both)]
    [singular (validate singular)]
    [plural   (validate plural)]
    [else     #f]))

(define (display-csv* v**)
  (for ([v* v**])
    (let* ([v-str* (for/list ([v v*] #:when v)
                     (if (and (string? v) (string-contains? v ","))
                       (~s v)
                       (~a v)))]
           [csv    (string-join v-str* ",")])
      (displayln csv))))

(define (->total-earnings result*)
  (for/sum ([result result*])
    (select 'earnings result)))