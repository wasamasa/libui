(module libui
  (init! main quit!
   handler-set!
   new-window window-child-set! window-margined?-set!
   new-button
   new-horizontal-box box-padded?-set! box-append!
   ->control control-show!
   message-box)

(import chicken scheme foreign)
(use srfi-69 matchable)

;;; headers

#> #include "ui.h" <#

;; TODO: bindings to ui_<platform>.h
;; NOTE: requires the right include paths in libui.setup

;;; enums

;;; typedefs
;; NOTE: uiInitOptions doesn't appear to be used in any way...
;; (define-foreign-type uiInitOptions*-or-null (c-pointer (struct "uiInitOptions")))
(define-foreign-type uiControl* (nonnull-c-pointer (struct "uiControl")))
(define-foreign-type uiWindow* (nonnull-c-pointer (struct "uiWindow")))
(define-foreign-type uiButton* (nonnull-c-pointer (struct "uiButton")))
(define-foreign-type uiBox* (nonnull-c-pointer (struct "uiBox")))

;;; auxiliary records
(define-record control pointer)
(define-record window pointer handlers)
(define-record button pointer handlers)
(define-record box pointer)

;;; foreign functions
(define uiInit (foreign-lambda* c-string* ()
                 "uiInitOptions options;"
                 "const char *msg = uiInit(&options);"
                 "if (msg) {"
                 "  char *ret = strdup(msg);"
                 "  uiFreeInitError(msg);"
                 "  C_return(ret);"
                 "}"
                 "C_return(NULL);"))
(define uiMain (foreign-safe-lambda void "uiMain"))
(define uiQuit (foreign-lambda void "uiQuit"))

(define uiOnShouldQuit (foreign-lambda void "uiOnShouldQuit" (function bool (c-pointer)) c-pointer))

(define uiControl (foreign-lambda uiControl* "uiControl" nonnull-c-pointer))
(define uiControlShow (foreign-lambda void "uiControlShow" uiControl*))

(define uiWindowOnClosing (foreign-lambda void "uiWindowOnClosing" uiWindow* (function bool (uiWindow* c-pointer)) c-pointer))
(define uiWindowSetChild (foreign-lambda void "uiWindowSetChild" uiWindow* uiControl*))
(define uiWindowSetMargined (foreign-lambda void "uiWindowSetMargined" uiWindow* bool))
(define uiNewWindow (foreign-lambda uiWindow* "uiNewWindow" c-string int int bool))

(define uiButtonOnClicked (foreign-lambda void "uiButtonOnClicked" uiButton* (function void (uiButton* c-pointer)) c-pointer))
(define uiNewButton (foreign-lambda uiButton* "uiNewButton" c-string))

(define uiNewHorizontalBox (foreign-lambda uiBox* "uiNewHorizontalBox"))
(define uiBoxSetPadded (foreign-lambda void "uiBoxSetPadded" uiBox* bool))
(define uiBoxAppend (foreign-lambda void "uiBoxAppend" uiBox* uiControl* bool))

(define uiMsgBox (foreign-lambda void "uiMsgBox" uiWindow* nonnull-c-string nonnull-c-string))

;;; errors

(define (define-error location message #!rest condition)
  (let ((base (make-property-condition 'exn 'location location 'message message))
        (extra (apply make-property-condition condition)))
    (make-composite-condition base extra)))

(define (libui-error message location)
  (define-error location message 'libui))

(define (usage-error message location)
  (define-error location message 'usage))

;;; generic handlers

;; special top-level handler that doesn't involve any widget

(define should-quit-handler (make-parameter #f))

(define-external (libui_ShouldQuitHandler (c-pointer data)) bool
  (match-let (((handler args) (should-quit-handler)))
    (apply handler args)))

;; widget-specific handlers

(define widget-table (make-hash-table))

(define (dispatch-event! widget* accessor type)
  (match-let* ((widget (hash-table-ref widget-table widget*))
               (handlers (accessor widget))
               ((handler . args) (hash-table-ref handlers type)))
    (apply handler widget args)))

(define-external (libui_WindowClosingHandler (uiWindow* window*) (c-pointer data)) bool
  (dispatch-event! window* window-handlers 'closing))

(define-external (libui_ButtonClickedHandler (uiButton* button*) (c-pointer data)) void
  (dispatch-event! button* button-handlers 'clicked))

;; generic interface

(define (handler-set! widget type proc #!rest args)
  (cond
   ((and (not widget) (eqv? type 'should-quit))
    (uiOnShouldQuit (location libui_ShouldQuitHandler) #f)
    (should-quit-handler (cons proc args)))
   ((and (window? widget) (eqv? type 'closing))
    (hash-table-set! (window-handlers widget) 'closing (cons proc args))
    (uiWindowOnClosing (window-pointer widget) (location libui_WindowClosingHandler) #f))
   ((and (button? widget) (eqv? type 'clicked))
    (hash-table-set! (button-handlers widget) 'clicked (cons proc args))
    (uiButtonOnClicked (button-pointer widget) (location libui_ButtonClickedHandler) #f))
   (else
    (abort (usage-error "Unsupported widget/type combination" 'handler-set!)))))

;;; API

;; setup and tear-down

(define (init!)
  (let ((ret (uiInit)))
    (when ret
      (abort (libui-error ret 'init!)))))

(define main uiMain)

(define quit! uiQuit)

;; widgets

(define (new-widget constructor wrapper #!rest args)
  (let* ((widget* (apply constructor args))
         (handlers (make-hash-table eqv? eqv?-hash))
         (widget (wrapper widget* handlers)))
    (hash-table-set! widget-table widget* widget)
    widget))

(define (new-window title width height #!optional menubar?)
  (new-widget uiNewWindow make-window title width height menubar?))

(define (window-child-set! window child)
  (let ((window* (window-pointer window))
        (child* (control-pointer child)))
    (uiWindowSetChild window* child*)))

(define (window-margined?-set! window margined?)
  (let ((window* (window-pointer window)))
    (uiWindowSetMargined window* margined?)))

(define (new-button text)
  (new-widget uiNewButton make-button text))

;; boxes

(define (new-horizontal-box)
  (let ((box* (uiNewHorizontalBox)))
    (make-box box*)))

(define (box-padded?-set! box padded?)
  (let ((box* (box-pointer box)))
    (uiBoxSetPadded box* padded?)))

(define (box-append! box control #!optional stretchy?)
  (let ((box* (box-pointer box))
        (control* (control-pointer control)))
    (uiBoxAppend box* control* stretchy?)))

;;  controls

(define (->control arg)
  (cond
   ((window? arg)
    (make-control (uiControl (window-pointer arg))))
   ((button? arg)
    (make-control (uiControl (button-pointer arg))))
   ((box? arg)
    (make-control (uiControl (box-pointer arg))))
   (else
    (abort (usage-error "Unsupported type" '->control)))))

(define (control-show! control)
  (let ((control* (control-pointer control)))
    (uiControlShow control*)))

;; dialogs

(define (message-box parent title description)
  (let ((parent* (window-pointer parent)))
    (uiMsgBox parent* title description)))

)
