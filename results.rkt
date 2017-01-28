#lang racket

#|

Usage:
(! "<input>")
Returns results for all horses.
Format: owner,name,date,event,class,place,earnings

(! #:owner "<owner>" "<input>")
Returns results for all horses owned by <owner>.
Format: name,date,event,class,place,earnings

(! #:horse "<horse>" "<input>")
Returns results for all horses named <horse>.
Format: owner,date,event,class,place,earnings

(! #:owner "<owner>" #:horse "<horse>" "<input>")
Returns results for all horses owner by <owner> and named <horse>.
Format: date,event,class,place,earnings

What was formerly (! "<input>") is now (! #:owner "Phoebe" "<input>"), and what was formerly (!* "<input>") is now (! "<input>").

The program will print the total earnings of all returned results. It will also print whatever part of the input text it didn't recognize, if any. Be sure to examine this. In particular, if any prize numbers went unrecognized, the earnings for the relevant result outputs may be wrong.

To simplify the task, I've put some requirements on the input format that may sometimes necessitate that you do some minor pre-processing by hand:

* The main show data (name and date in particular) must be in a single block before anything else.

* Any block containing at least one "$" is assumed to be prize data. The program will look for dollar amounts preceded by known names for places, allowing for separating punctuation, e.g., "1st: $500" or "Champion - $1,200". Any other text will be reported as unrecognized.

* Prize data must come before the results that it applies to. When the program encounters a prize block, it uses that data to calculate earnings for subsequent results until it encounters another prize block, then it uses that data until it encounters another one, and so on. So if the beginning of the listing has multiple prize data blocks for different parts of the show, you would have to copy and paste them before the relevant sections.

* Each block of results must feature a class header line (i.e., "<Prefix?><Class Title> (<Number> Entries)") followed by the individual results. Any lines before the header line will be ignored/unrecognized.

|#

(require "shared/main.rkt")

(define (! str #:owner [owner? #f] #:horse [horse? #f])
  (let* ([result*   (parse-show str)]
         [result*^  (if owner? (filter-field 'owner owner? result*) result*)]
         [result*^^ (if horse? (filter-field 'name horse? result*^) result*^)]
         [value**   (let* ([removed-field* (append (if owner? '(owner) '()) (if horse? '(name) '()))]
                           [field*         (remove* removed-field* '(owner name date event class place earnings))])
                         (for/list ([res result*^^]) (select* field* res)))])
    (display-csv* value**)
    (printf "Total earnings: $~a\n" (get-total-earnings result*^^))))

(define (display-csv* v**)
  (for ([v* v**])
       (let ([str* (for/list ([v v*])
                             (if (and (string? v) (string-contains? v ","))
                                 (~s v)
                                 (~a v)))])
            (displayln (string-append* (add-between str* ","))))))

(define (filter-field field value result*)
  (filter (λ (res) (equal? value (select field res))) result*))

(define (get-total-earnings result*) (apply + (map (λ (res) (select 'earnings res)) result*)))