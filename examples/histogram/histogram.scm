(use (prefix libui ui:)
     extras srfi-1)

(import (prefix libui-draw draw:))

(ui:margined? #t)
(ui:padded? #t)

(define (ints->floats ints)
  (map (lambda (x) (/ x 255)) ints))

(define black (ints->floats '(0 0 0 255)))
(define white (ints->floats '(255 255 255 255)))
(define dodger-blue (ints->floats '(30 144 255 255)))

(define x-off-left 20)
(define y-off-top 20)
(define x-off-right 20)
(define y-off-bottom 20)
(define point-radius 5)

(define pi (* 2 (asin 1)))

(define current-point #f)
(define data-points (make-vector 10 #f))

(define (point-locations width height)
  (let ((xincr (/ width 9))
        (yincr (/ height 100))
        (xs (make-vector 10 #f))
        (ys (make-vector 10 #f)))
    (let loop ((i 0))
      (if (< i 10)
          (let* ((value (ui:spinbox-value (vector-ref data-points i)))
                 (n (- 100 value)))
            (vector-set! xs i (* xincr i))
            (vector-set! ys i (* yincr n))
            (loop (add1 i)))
          (values xs ys)))))

(define (construct-graph width height extend?)
  (let-values (((xs ys) (point-locations width height)))
    (let ((path (draw:new-path)))
      (draw:path-new-figure! path (vector-ref xs 0) (vector-ref ys 0))
      (let loop ((i 1))
        (when (< i 10)
          (draw:path-line-to! path (vector-ref xs i) (vector-ref ys i))
          (loop (add1 i))))
      (when extend?
        (draw:path-line-to! path width height)
        (draw:path-line-to! path 0 height)
        (draw:path-close-figure! path))
      (draw:path-end! path)
      path)))

(define (graph-size client-width client-height)
  (let ((width (- client-width x-off-left x-off-right))
        (height (- client-height y-off-top y-off-bottom)))
    (values width height)))

(define color-button #f)
(define histogram #f)

(define (handler-draw area-handler area params)
  (let ((path #f)
        (brush #f)
        (color (ui:color-button-color color-button))
        (stroke-params (draw:new-stroke-params))
        (matrix (draw:new-matrix))
        (context (ui:draw-params-context params))
        (area-width (ui:draw-params-area-width params))
        (area-height (ui:draw-params-area-height params))
        (graph-width -1)
        (graph-height -1))
    (set! brush (apply draw:new-solid-brush white))
    (set! path (draw:new-path))
    (draw:path-add-rectangle! path 0 0 area-width area-height)
    (draw:path-end! path)
    (draw:fill! context path brush)

    (set!-values (graph-width graph-height) (graph-size area-width area-height))

    (set! brush (apply draw:new-solid-brush black))
    (set! path (draw:new-path))
    (draw:path-new-figure! path x-off-left y-off-top)
    (draw:path-line-to! path x-off-left (+ y-off-top graph-height))
    (draw:path-line-to! path (+ x-off-left graph-width) (+ y-off-top graph-height))
    (draw:path-end! path)
    (draw:stroke! context path brush stroke-params)

    (draw:matrix-identity-set! matrix)
    (draw:matrix-translate! matrix x-off-left y-off-top)
    (draw:transform! context matrix)

    (set! brush (apply draw:new-solid-brush color))

    (set! path (construct-graph graph-width graph-height #t))
    (draw:brush-a-set! brush (/ (last color) 2))
    (draw:fill! context path brush)

    (set! path (construct-graph graph-width graph-height #f))
    (draw:brush-a-set! brush (last color))
    (draw:stroke! context path brush stroke-params)

    (when current-point
      (let-values (((xs ys) (point-locations graph-width graph-height)))
        (set! path (draw:new-path))
        (draw:path-new-figure-with-arc! path
                                           (vector-ref xs current-point)
                                           (vector-ref ys current-point)
                                           point-radius
                                           0 (* 2 pi))
        (draw:path-end! path)
        (draw:fill! context path brush)))))

(define (in-point? x y xtest ytest)
  (let ((x (- x x-off-left))
        (y (- y y-off-top)))
    (and (>= x (- xtest point-radius))
         (<= x (+ xtest point-radius))
         (>= y (- ytest point-radius))
         (<= y (+ ytest point-radius)))))

(define (handler-mouse-event area-handler area event)
  (let*-values (((graph-width graph-height)
                 (graph-size (ui:mouse-event-area-width event)
                             (ui:mouse-event-area-height event)))
                ((xs ys) (point-locations graph-width graph-height)))
    (let loop ((i 0))
      (if (< i 10)
          (if (in-point? (ui:mouse-event-x event) (ui:mouse-event-y event)
                         (vector-ref xs i) (vector-ref ys i))
              (set! current-point i)
              (loop (add1 i)))
          (set! current-point #f))))
  (ui:area-queue-redraw-all! histogram))

(define (handler-mouse-crossed area-handler area left?)
  ;; do nothing
  #f)

(define (handler-drag-broken area-handler area)
  ;; do nothing
  #f)

(define (handler-key-event area-handler area event)
  ;; reject all keys
  #f)

(define (on-data-point-changed _spinbox)
  (ui:area-queue-redraw-all! histogram))

(define (on-color-changed _button)
  (ui:area-queue-redraw-all! histogram))

(define main-window #f)

(define (on-closing _window)
  (ui:control-destroy! (ui:->control main-window))
  (ui:quit!)
  #f)

(define (on-should-quit)
  (ui:control-destroy! (ui:->control main-window))
  #t)

(define handler (ui:new-area-handler handler-draw
                                     handler-mouse-event
                                     handler-mouse-crossed
                                     handler-drag-broken
                                     handler-key-event))

(ui:init!)

(ui:handler-set! #f 'should-quit on-should-quit)

(set! main-window (ui:new-window "libui Histogram Example" 640 480 #t))
(ui:handler-set! main-window 'closing on-closing)

(define hbox (ui:new-horizontal-box))
(ui:window-child-set! main-window (ui:->control hbox))

(define vbox (ui:new-vertical-box))
(ui:box-append! hbox (ui:->control vbox))

(let loop ((i 0))
  (when (< i 10)
    (let ((spinbox (ui:new-spinbox 0 100)))
      (ui:spinbox-value-set! spinbox (random 101))
      (ui:handler-set! spinbox 'changed on-data-point-changed)
      (ui:box-append! vbox (ui:->control spinbox))
      (vector-set! data-points i spinbox))
    (loop (add1 i))))

(set! color-button (ui:new-color-button))
(define brush (apply draw:new-solid-brush dodger-blue))
(ui:color-button-color-set! color-button
                            (draw:brush-r brush)
                            (draw:brush-g brush)
                            (draw:brush-b brush)
                            (draw:brush-a brush))
(ui:handler-set! color-button 'changed on-color-changed)
(ui:box-append! vbox (ui:->control color-button))

(set! histogram (ui:new-area handler))
(ui:box-append! hbox (ui:->control histogram) #t)

(ui:control-show! (ui:->control main-window))
(ui:main)
(ui:uninit!)
