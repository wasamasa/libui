(use (prefix libui ui:))

(ui:margined? #t)
(ui:padded? #t)

(ui:init!)

(define out #f)
(define cur-val 0)
(define cur-op #f)
(define last-op #f)

(define (do-operation button)
  (let ((op (car (string->list (ui:button-text button)))))
    (if (and (char<=? #\0 op) (char<=? op #\9))
        (begin
          (if (and last-op (char<=? #\0 last-op) (char<=? last-op #\9)
                   (not (equal? (ui:entry-text out) "0")))
              (set! (ui:entry-text out) (format "~a~c" (ui:entry-text out) op))
              (set! (ui:entry-text out) (format "~c" op)))
          (set! last-op op))
        (begin
          (set! last-op op)
          (case cur-op
            ((#\+) (set! (ui:entry-text out) (number->string (+ cur-val (string->number (ui:entry-text out))))))
            ((#\-) (set! (ui:entry-text out) (number->string (- cur-val (string->number (ui:entry-text out))))))
            ((#\*) (set! (ui:entry-text out) (number->string (* cur-val (string->number (ui:entry-text out))))))
            ((#\/) (set! (ui:entry-text out) (number->string (/ cur-val (string->number (ui:entry-text out))))))
            ((#\=) #f))
          (if (memv op '(#\+ #\- #\* #\/))
              (begin
                (set! cur-op op)
                (set! cur-val (string->number (ui:entry-text out))))
              (begin
                (set! cur-val 0)
                (set! cur-op #f)
                (when (eqv? op #\C)
                  (set! (ui:entry-text out) "0"))))))))

(ui:widgets
 `(window
   (@ (id main)
      (title "Calculator")
      (width 240)
      (height 240)
      (closing ,(lambda (_window) (ui:quit!))))
   (grid
    (item
     (@ (left 0) (top 0)
        (xspan 4))
     (entry
      (@ (id out)
         (text "0")
         (read-only? #t))))

    (item
     (@ (left 0) (top 1))
     (button (@ (text "7") (clicked ,do-operation))))
    (item
     (@ (left 1) (top 1))
     (button (@ (text "8") (clicked ,do-operation))))
    (item
     (@ (left 2) (top 1))
     (button (@ (text "9") (clicked ,do-operation))))
    (item
     (@ (left 3) (top 1))
     (button (@ (text "/") (clicked ,do-operation))))

    (item
     (@ (left 0) (top 2))
     (button (@ (text "4") (clicked ,do-operation))))
    (item
     (@ (left 1) (top 2))
     (button (@ (text "5") (clicked ,do-operation))))
    (item
     (@ (left 2) (top 2))
     (button (@ (text "6") (clicked ,do-operation))))
    (item
     (@ (left 3) (top 2))
     (button (@ (text "*") (clicked ,do-operation))))

    (item
     (@ (left 0) (top 3))
     (button (@ (text "1") (clicked ,do-operation))))
    (item
     (@ (left 1) (top 3))
     (button (@ (text "2") (clicked ,do-operation))))
    (item
     (@ (left 2) (top 3))
     (button (@ (text "3") (clicked ,do-operation))))
    (item
     (@ (left 3) (top 3))
     (button (@ (text "-") (clicked ,do-operation))))

    (item
     (@ (left 0) (top 4))
     (button (@ (text "C") (clicked ,do-operation))))
    (item
     (@ (left 1) (top 4))
     (button (@ (text "0") (clicked ,do-operation))))
    (item
     (@ (left 2) (top 4))
     (button (@ (text "=") (clicked ,do-operation))))
    (item
     (@ (left 3) (top 4))
     (button (@ (text "+") (clicked ,do-operation)))))))

(set! out (ui:widget-by-id 'out))
(ui:control-show! (ui:->control (ui:widget-by-id 'main)))
(ui:main)
