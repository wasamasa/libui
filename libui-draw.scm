(module libui-draw
  (new-solid-brush brush-r brush-r-set! brush-g brush-g-set! brush-b brush-b-set! brush-a brush-a-set!
   new-stroke-params
   new-matrix matrix-identity-set! matrix-translate! transform!
   new-path path-free! path-new-figure! path-new-figure-with-arc!
   path-line-to! path-close-figure! path-add-rectangle! path-end!
   stroke! fill!)

(import chicken scheme foreign)
(use lolevel)

;;; headers

#> #include "ui.h" <#

;;; enums

;; uiDrawLineCap
(define uiDrawLineCapFlat (foreign-value "uiDrawLineCapFlat" unsigned-int))
(define uiDrawLineCapRound (foreign-value "uiDrawLineCapRound" unsigned-int))
(define uiDrawLineCapSquare (foreign-value "uiDrawLineCapSquare" unsigned-int))

(define (cap->int cap)
  (case cap
    ((flat) uiDrawLineCapFlat)
    ((round) uiDrawLineCapRound)
    ((square) uiDrawLineCapSquare)
    (else
     (abort (usage-error "Invalid cap" 'cap->int)))))

;; uiDrawLineJoin
(define uiDrawLineJoinMiter (foreign-value "uiDrawLineJoinMiter" unsigned-int))
(define uiDrawLineJoinRound (foreign-value "uiDrawLineJoinRound" unsigned-int))
(define uiDrawLineJoinBevel (foreign-value "uiDrawLineJoinBevel" unsigned-int))

(define (join->int join)
  (case join
    ((miter) uiDrawLineJoinMiter)
    ((round) uiDrawLineJoinRound)
    ((bevel) uiDrawLineJoinBevel)
    (else
     (abort (usage-error "Invalid join" 'join->int)))))

;; uiDrawFillMode
(define uiDrawFillModeWinding (foreign-value "uiDrawFillModeWinding" unsigned-int))
(define uiDrawFillModeAlternate (foreign-value "uiDrawFillModeAlternate" unsigned-int))

;;; typedefs
(define-foreign-type uiDrawContext* (nonnull-c-pointer (struct "uiDrawContext")))
(define-foreign-type uiDrawPath* (nonnull-c-pointer (struct "uiDrawPath")))
(define-foreign-type uiDrawMatrix* (nonnull-c-pointer (struct "uiDrawMatrix")))
(define-foreign-type uiDrawBrush* (nonnull-c-pointer (struct "uiDrawBrush")))
(define-foreign-type uiDrawStrokeParams* (nonnull-c-pointer (struct "uiDrawStrokeParams")))

(define-foreign-type uiEnum unsigned-int)

;;; auxiliary records

(define-record context pointer)
(define-record path pointer)

(define-record brush storage)
(define-record matrix storage)
(define-record stroke-params storage)

;;; struct helpers

;; brush

(define (brush-pointer brush)
  (make-locative (brush-storage brush)))

(define uiDrawBrush-size (foreign-type-size (struct "uiDrawBrush")))

(define (new-solid-brush r g b a)
  (let* ((brush (make-brush (make-blob uiDrawBrush-size)))
         (brush* (brush-pointer brush)))
    ((foreign-lambda* void ((uiDrawBrush* br) (double r) (double g) (double b) (double a))
       "br->Type = uiDrawBrushTypeSolid, br->R = r, br->G = g, br->B = b, br->A = a;")
     brush* r g b a)
    brush))

(define (brush-r brush)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* double ((uiDrawBrush* br)) "C_return(br->R);") brush*)))

(define (brush-r-set! brush r)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* void ((uiDrawBrush* br) (double r)) "br->R = r;") brush* r)))

(define (brush-g brush)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* double ((uiDrawBrush* br)) "C_return(br->G);") brush*)))

(define (brush-g-set! brush g)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* void ((uiDrawBrush* br) (double g)) "br->G = g;") brush* g)))

(define (brush-b brush)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* double ((uiDrawBrush* br)) "C_return(br->B);") brush*)))

(define (brush-b-set! brush b)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* void ((uiDrawBrush* br) (double b)) "br->B = b;") brush* b)))

(define (brush-a brush)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* double ((uiDrawBrush* br)) "C_return(br->A);") brush*)))

(define (brush-a-set! brush a)
  (let ((brush* (brush-pointer brush)))
    ((foreign-lambda* void ((uiDrawBrush* br) (double a)) "br->A = a;") brush* a)))

;; stroke params

(define (stroke-params-pointer stroke-params)
  (make-locative (stroke-params-storage stroke-params)))

(define uiDrawStrokeParams-size (foreign-type-size (struct "uiDrawStrokeParams")))

(define uiDrawDefaultMiterLimit (foreign-value "uiDrawDefaultMiterLimit" double))

(define (new-stroke-params #!key
                           (cap 'flat) (join 'miter) (thickness 2)
                           (miter-limit uiDrawDefaultMiterLimit)
                           (dashes '()) (offset 0))
  (let* ((stroke-params (make-stroke-params (make-blob uiDrawStrokeParams-size)))
         (stroke-params* (stroke-params-pointer stroke-params))
         (cap-value (cap->int cap))
         (join-value (join->int join)))
    ((foreign-lambda* void ((uiDrawStrokeParams* params) (uiEnum cap) (uiEnum join) (double thickness) (double limit))
       "uiDrawStrokeParams *p = params;"
       "p->Cap = cap, p->Join = join, p->Thickness = thickness, p->MiterLimit = limit;"
       ;; TODO: implement non-null dashes
       "p->Dashes = NULL, p->NumDashes = 0, p->DashPhase = 0;")
     stroke-params* cap-value join-value thickness miter-limit)
    stroke-params))

;; matrix

(define (matrix-pointer matrix)
  (make-locative (matrix-storage matrix)))

(define uiDrawMatrix-size (foreign-type-size (struct "uiDrawMatrix")))

(define (new-matrix)
  (make-matrix (make-blob uiDrawMatrix-size)))

;;; foreign functions

(define uiDrawNewPath (foreign-lambda uiDrawPath* "uiDrawNewPath" uiEnum))
(define uiDrawFreePath (foreign-lambda void "uiDrawFreePath" uiDrawPath*))
(define uiDrawPathNewFigure (foreign-lambda void "uiDrawPathNewFigure" uiDrawPath* double double))
(define uiDrawPathNewFigureWithArc (foreign-lambda void "uiDrawPathNewFigureWithArc" uiDrawPath* double double double double double bool))
(define uiDrawPathLineTo (foreign-lambda void "uiDrawPathLineTo" uiDrawPath* double double))
(define uiDrawPathCloseFigure (foreign-lambda void "uiDrawPathCloseFigure" uiDrawPath*))
(define uiDrawPathAddRectangle (foreign-lambda void "uiDrawPathAddRectangle" uiDrawPath* double double double double))
(define uiDrawPathEnd (foreign-lambda void "uiDrawPathEnd" uiDrawPath*))
(define uiDrawStroke (foreign-lambda void "uiDrawStroke" uiDrawContext* uiDrawPath* uiDrawBrush* uiDrawStrokeParams*))
(define uiDrawFill (foreign-lambda void "uiDrawFill" uiDrawContext* uiDrawPath* uiDrawBrush*))

(define uiDrawMatrixSetIdentity (foreign-lambda void "uiDrawMatrixSetIdentity" uiDrawMatrix*))
(define uiDrawMatrixTranslate (foreign-lambda void "uiDrawMatrixTranslate" uiDrawMatrix* double double))

(define uiDrawTransform (foreign-lambda void "uiDrawTransform" uiDrawContext* uiDrawMatrix*))

;;; errors

(define (define-error location message #!rest condition)
  (let ((base (make-property-condition 'exn 'location location 'message message))
        (extra (apply make-property-condition condition)))
    (make-composite-condition base extra)))

(define (usage-error message location)
  (define-error location message 'usage))

;;; API

(define (new-path #!optional alternate?)
  (let* ((flag (if alternate? uiDrawFillModeAlternate uiDrawFillModeWinding))
         (path* (uiDrawNewPath flag)))
    (make-path path*)))

(define (path-free! path)
  (let ((path* (path-pointer path)))
    (uiDrawFreePath path*)))

(define (path-new-figure! path x y)
  (let ((path* (path-pointer path)))
    (uiDrawPathNewFigure path* x y)))

(define (path-new-figure-with-arc! path xcenter ycenter radius start-angle sweep #!optional negative?)
  (let ((path* (path-pointer path)))
    (uiDrawPathNewFigureWithArc path* xcenter ycenter radius start-angle sweep negative?)))

(define (path-line-to! path x y)
  (let ((path* (path-pointer path)))
    (uiDrawPathLineTo path* x y)))

(define (path-close-figure! path)
  (let ((path* (path-pointer path)))
    (uiDrawPathCloseFigure path*)))

(define (path-add-rectangle! path x y width height)
  (let ((path* (path-pointer path)))
    (uiDrawPathAddRectangle path* x y width height)))

(define (path-end! path)
  (let ((path* (path-pointer path)))
    (uiDrawPathEnd path*)))

(define (stroke! context path brush stroke-params)
  (let ((context* (context-pointer context))
        (path* (path-pointer path))
        (brush* (brush-pointer brush))
        (stroke-params* (stroke-params-pointer stroke-params)))
    (uiDrawStroke context* path* brush* stroke-params*)))

(define (fill! context path brush)
  (let ((context* (context-pointer context))
        (path* (path-pointer path))
        (brush* (brush-pointer brush)))
    (uiDrawFill context* path* brush*)))

(define (matrix-identity-set! matrix)
  (let ((matrix* (matrix-pointer matrix)))
    (uiDrawMatrixSetIdentity matrix*)))

(define (matrix-translate! matrix x y)
  (let ((matrix* (matrix-pointer matrix)))
    (uiDrawMatrixTranslate matrix* x y)))

(define (transform! context matrix)
  (let ((context* (context-pointer context))
        (matrix* (matrix-pointer matrix)))
    (uiDrawTransform context* matrix*)))

)
