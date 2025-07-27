#lang racket
(define (rotates str)
  (let ([n (string-length str)])
    (if (= n 0)
        '()
        (let loop ([i 0] [rotations '()])
          (if (= i n)
              (reverse rotations)
              (let ([rotated-str (string-append (substring str i n) (substring str 0 i))])
                (loop (+ i 1) (cons rotated-str rotations))))))))

(define (arrangeStrList lst)
  (string-append
   "("
   (string-join (map (lambda (s) (string-append "\"" s "\"")) lst) " ")
   ")"))
(define (arrangeChList lst)
  (string-append
   "("
   (string-join (map (lambda (c) (string-append "#\\" (string c))) lst) "")
   ")"))
(define (charSum lst)
  (if (null? lst)
      ""
      (string-join (map (lambda (c) (string-append "\"" (string c) "\"")) lst) "+")))
(define (arrangeStrt s)
  (string-append "\"" s "\""))
(display "Input: ")
(define inputStr (read-line))
(if (eof-object? inputStr)
    (display "\nNo entry has found.Try entering something\n")
    (if (string=? inputStr "")
        (display "\nNothing has entered.Try enter something\n")
        (begin
          (display "\"")
          (display inputStr)
          (display "\"\n")

          (let* ([rotations (rotates inputStr)]
                 [sortedRotation (sort rotations string<?)]
                 [n(string-length inputStr)]
                 [last-chars (map(lambda (rot) (string-ref rot (- n 1))) sortedRotation)]
                 [bwtString(list->string last-chars)]
                 [largestStr(last sortedRotation)]
                 [smallest-string(car sortedRotation)]
                 [combined(string-append smallest-string largestStr)])

            (display "Step 1:Rotations-> ")
            (display (arrangeStrList rotations))
            (newline)
            (display "Step 2:Sorted-> ")
            (display (arrangeStrList sortedRotation))
            (newline)
            (display "Step 3:Last chars-> ")
            (display (arrangeChList last-chars))
            (newline)
            (display "Step 4: Concatenate last chars: ")
            (display (charSum last-chars))
            (display " = ")
            (display (arrangeStrt bwtString))
            (newline)
            (display "Step 5: Return the lexicographically largest string -> \"")
            (display largestStr)
            (display "\"\n")
            (display "Step 6: Concatenate the lexicographically smallest and the largest strings -> \"")
            (display combined)
            (display "\"\n")
          )
        )
    ))