# AssignmentDataAbstractionsScheme
Masters Asssignemnt to use scheme meant for Final Semester Project

#lang racket

(require data-science-master)
(require plot)
(require math)
(require json)
(require srfi/19)
(require racket/stream)



; This function reads line-oriented JSON and packages it into an array. For very large data sets.

(define (json-lines->json-array #:head [head #f])
  (let loop ([number 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= number head)))
        (jsexpr->string json-array)
        (loop (add1 number) (cons record json-array)
              (read-json (current-input-port))))))

; To Normalize, remove URLs and punctuation plus spaces
; from each tweet. This function takes a list of words and returns a
; preprocessed subset of words/tokens as a list

(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x))) #:websafe? #t))
       lst))

; Reading the twitter base)

(define tweets (string->jsexpr
                (with-input-from-file "bobi_tweets.json" (λ () (json-lines->json-array)))))

; Remove just the tweet text and source from each tweet

(define t
  (let ([tmp (map (λ (x) (list (hash-ref x 'text))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)

    ))

; t is a list of  strings. am going to use Tail recursion to extract each string and append
; it into one big one.

(define join-tweets
    (local[
           (define (joinx teelist dd)
             (cond [(empty? teelist) dd]
                   [else (joinx (rest teelist) (string-join (list dd "\n " (first(first teelist)))))]
                   )
             )
           ](joinx t "")) )

; I am going to extract each unique word
; and its frequency of appearing
(define words (document->tokens join-tweets #:sort? #t))

; I am using the nrc lexicon and I am labeling each (non stop-word) with an
; affection label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;The sentiment below, consists of a list of triplets of the pattern
;(token sentiment freq) for each token in the document.
(take sentiment 6)


; Since Many words will have 
; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))


;to visualize the results I used a bar graph histogram (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 700))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "Red"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affection"
	  #:y-label "Frequency")))
