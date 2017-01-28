#lang racket

(define (! str)
  (let*-values ([(winner*) (class-results->winner* str)]
                [(champ r-champ) (winner*->champs winner*)])
    (display (format "CH: ~a\nRC: ~a\n" champ r-champ))))

(define (class-results->winner* str)
  (let* ([block* (regexp-split #px"\n[:space:]*\n" str)]
         [winner* (append* (for/list ([block block*])
                                     (cdr (string-split block "\n"))))])
    (for/list ([winner winner*])
              (match-let  ([(list _ place entry) (regexp-match #px"^(\\d).. - (.*)" winner)])
                (cons entry (string->number place))))))

(define (winner*->champs winner*)
  (let* ([max-place      (apply max (map cdr winner*))]
         [entry->points  (for/fold ([e->p (hash)])
                                   ([winner winner*])
                           (let ([points (+ 1 (- max-place (cdr winner)))])
                             (hash-update e->p (car winner) (λ (x) (+ x points)) 0)))]
         [winner/points* (hash->list entry->points)]
         [sorted         (sort winner/points* > #:key cdr)]
         [champ          (car (car sorted))]
         [r-champ        (car (cadr sorted))])
    (values champ r-champ)))

(define test-str
  "Class X Div. A Subclass 1
1st - Entry 1
2nd - Entry 2
3rd - Entry 3

Class X Div. A Subclass 2
1st - Entry 1
2nd - Entry 2
3rd - Entry 3

Class X Div. B Subclass 1
1st - Entry 1
2nd - Entry 2
3rd - Entry 3

Class X Div. B Subclass 2
1st - Entry 1
2nd - Entry 2
3rd - Entry 3")