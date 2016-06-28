(module libui
  (init! uninit! main quit!
   handler-set!
   margined? padded?
   widget? widget-id widget-type widget-handlers
   new-window window-child-set! window-margined?-set!
   new-button
   new-horizontal-box new-vertical-box box-append! box-padded?-set!
   new-checkbox
   new-entry new-password-entry new-search-entry entry-text-set! entry-read-only?-set!
   new-label
   new-tab tab-append! tab-margined?-set!
   new-group group-child-set! group-margined?-set!
   new-spinbox spinbox-value spinbox-value-set!
   new-slider slider-value slider-value-set!
   new-progress-bar progress-bar-value progress-bar-value-set!
   new-horizontal-separator new-vertical-separator
   new-combobox combobox-append!
   new-editable-combobox editable-combobox-append!
   new-radio-buttons radio-buttons-append!
   new-date-time-picker new-date-picker new-time-picker
   new-multiline-entry new-non-wrapping-multiline-entry multiline-entry-append! multiline-entry-read-only?-set!
   new-area area-queue-redraw-all!
   new-area-handler
   mouse-event-x mouse-event-y mouse-event-area-width mouse-event-area-height
   draw-params-context draw-params-area-width draw-params-area-height
   new-font-button
   new-color-button color-button-color color-button-color-set!
   new-form form-append! form-padded?-set!
   new-grid grid-append! grid-padded?-set!
   ->control control-destroy! control-show!
   open-file save-file message-box message-box-error
   widget-by-id widgets)

(import chicken scheme foreign)
(use srfi-69 matchable lolevel srfi-1
     libui-draw)

;;; headers

#> #include "ui.h" <#

;; TODO: bindings to ui_<platform>.h
;; NOTE: requires the right include paths in libui.setup

;;; enums

;; uiAlign
(define uiAlignFill (foreign-value "uiAlignFill" unsigned-int))
(define uiAlignStart (foreign-value "uiAlignStart" unsigned-int))
(define uiAlignCenter (foreign-value "uiAlignCenter" unsigned-int))
(define uiAlignEnd (foreign-value "uiAlignEnd" unsigned-int))

(define (alignment->int alignment)
  (case alignment
    ((fill) uiAlignFill)
    ((start) uiAlignStart)
    ((center) uiAlignCenter)
    ((end) uiAlignEnd)
    (else
     (abort (usage-error "Invalid alignment" 'alignment->int)))))

;;; typedefs

;; NOTE: uiInitOptions doesn't appear to be used in any way...
;; (define-foreign-type uiInitOptions*-or-null (c-pointer (struct "uiInitOptions")))
(define-foreign-type uiControl* (nonnull-c-pointer (struct "uiControl")))
(define-foreign-type uiWindow* (nonnull-c-pointer (struct "uiWindow")))
(define-foreign-type uiButton* (nonnull-c-pointer (struct "uiButton")))
(define-foreign-type uiBox* (nonnull-c-pointer (struct "uiBox")))
(define-foreign-type uiCheckbox* (nonnull-c-pointer (struct "uiCheckbox")))
(define-foreign-type uiEntry* (nonnull-c-pointer (struct "uiEntry")))
(define-foreign-type uiLabel* (nonnull-c-pointer (struct "uiLabel")))
(define-foreign-type uiTab* (nonnull-c-pointer (struct "uiTab")))
(define-foreign-type uiGroup* (nonnull-c-pointer (struct "uiGroup")))
(define-foreign-type uiSpinbox* (nonnull-c-pointer (struct "uiSpinbox")))
(define-foreign-type uiSlider* (nonnull-c-pointer (struct "uiSlider")))
(define-foreign-type uiProgressBar* (nonnull-c-pointer (struct "uiProgressBar")))
(define-foreign-type uiSeparator* (nonnull-c-pointer (struct "uiSeparator")))
(define-foreign-type uiCombobox* (nonnull-c-pointer (struct "uiCombobox")))
(define-foreign-type uiEditableCombobox* (nonnull-c-pointer (struct "uiEditableCombobox")))
(define-foreign-type uiRadioButtons* (nonnull-c-pointer (struct "uiRadioButtons")))
(define-foreign-type uiDateTimePicker* (nonnull-c-pointer (struct "uiDateTimePicker")))
(define-foreign-type uiMultilineEntry* (nonnull-c-pointer (struct "uiMultilineEntry")))
(define-foreign-type uiArea* (nonnull-c-pointer (struct "uiArea")))
(define-foreign-type uiAreaHandler* (nonnull-c-pointer (struct "uiAreaHandler")))
(define-foreign-type uiAreaDrawParams* (nonnull-c-pointer (struct "uiAreaDrawParams")))
(define-foreign-type uiAreaMouseEvent* (nonnull-c-pointer (struct "uiAreaMouseEvent")))
(define-foreign-type uiAreaKeyEvent* (nonnull-c-pointer (struct "uiAreaKeyEvent")))
(define-foreign-type uiDrawContext* (nonnull-c-pointer (struct "uiDrawContext")))
(define-foreign-type uiFontButton* (nonnull-c-pointer (struct "uiFontButton")))
(define-foreign-type uiColorButton* (nonnull-c-pointer (struct "uiColorButton")))
(define-foreign-type uiForm* (nonnull-c-pointer (struct "uiForm")))
(define-foreign-type uiGrid* (nonnull-c-pointer (struct "uiGrid")))

(define-foreign-type uiEnum unsigned-int)
(define-foreign-type double* (nonnull-c-pointer double))

;;; auxiliary records

(define-record control pointer)

(define-record widget id type pointer handlers)
(define-record-printer (widget w out)
  (fprintf out "#<~a: ~a>" (widget-type w) (widget-id w)))

(define-record context pointer)

(define-record draw-params pointer)
(define-record mouse-event pointer)
(define-record key-event pointer)

;;; struct helpers

;; area handler

(define-record area-handler pointer draw mouse-event mouse-crossed drag-broken key-event)

(define uiAreaHandler-size (foreign-type-size (struct "uiAreaHandler")))

;; area mouse events and draw params

(define (mouse-event-x mouse-event)
  (let ((mouse-event* (mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->X);") mouse-event*)))

(define (mouse-event-y mouse-event)
  (let ((mouse-event* (mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->Y);") mouse-event*)))

(define (mouse-event-area-width mouse-event)
  (let ((mouse-event* (mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->AreaWidth);") mouse-event*)))

(define (mouse-event-area-height mouse-event)
  (let ((mouse-event* (mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->AreaHeight);") mouse-event*)))

(define (draw-params-context draw-params)
  (let* ((draw-params* (draw-params-pointer draw-params))
         (context* ((foreign-lambda* uiDrawContext* ((uiAreaDrawParams* params)) "C_return(params->Context);") draw-params*)))
    (make-context context*)))

(define (draw-params-area-width draw-params)
  (let ((draw-params* (draw-params-pointer draw-params)))
    ((foreign-lambda* double ((uiAreaDrawParams* params)) "C_return(params->AreaWidth);") draw-params*)))

(define (draw-params-area-height draw-params)
  (let ((draw-params* (draw-params-pointer draw-params)))
    ((foreign-lambda* double ((uiAreaDrawParams* params)) "C_return(params->AreaHeight);") draw-params*)))

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
(define uiUninit (foreign-lambda void "uiUninit"))
(define uiMain (foreign-safe-lambda void "uiMain"))
(define uiQuit (foreign-lambda void "uiQuit"))

(define uiOnShouldQuit (foreign-lambda void "uiOnShouldQuit" (function bool (c-pointer)) c-pointer))

(define uiControl (foreign-lambda uiControl* "uiControl" nonnull-c-pointer))
(define uiControlDestroy (foreign-lambda void "uiControlDestroy" uiControl*))
(define uiControlShow (foreign-lambda void "uiControlShow" uiControl*))

(define uiNewWindow (foreign-lambda uiWindow* "uiNewWindow" nonnull-c-string int int bool))
(define uiWindowOnClosing (foreign-lambda void "uiWindowOnClosing" uiWindow* (function bool (uiWindow* c-pointer)) c-pointer))
(define uiWindowSetChild (foreign-lambda void "uiWindowSetChild" uiWindow* uiControl*))
(define uiWindowSetMargined (foreign-lambda void "uiWindowSetMargined" uiWindow* bool))

(define uiNewButton (foreign-lambda uiButton* "uiNewButton" nonnull-c-string))
(define uiButtonOnClicked (foreign-lambda void "uiButtonOnClicked" uiButton* (function void (uiButton* c-pointer)) c-pointer))

(define uiNewHorizontalBox (foreign-lambda uiBox* "uiNewHorizontalBox"))
(define uiNewVerticalBox (foreign-lambda uiBox* "uiNewVerticalBox"))
(define uiBoxAppend (foreign-lambda void "uiBoxAppend" uiBox* uiControl* bool))
(define uiBoxSetPadded (foreign-lambda void "uiBoxSetPadded" uiBox* bool))

(define uiNewCheckbox (foreign-lambda uiCheckbox* "uiNewCheckbox" nonnull-c-string))

(define uiNewEntry (foreign-lambda uiEntry* "uiNewEntry"))
(define uiNewPasswordEntry (foreign-lambda uiEntry* "uiNewPasswordEntry"))
(define uiNewSearchEntry (foreign-lambda uiEntry* "uiNewSearchEntry"))
(define uiEntrySetText (foreign-lambda void "uiEntrySetText" uiEntry* nonnull-c-string))
(define uiEntrySetReadOnly (foreign-lambda void "uiEntrySetReadOnly" uiEntry* bool))

(define uiNewLabel (foreign-lambda uiLabel* "uiNewLabel" nonnull-c-string))

(define uiNewTab (foreign-lambda uiTab* "uiNewTab"))
(define uiTabAppend (foreign-lambda void "uiTabAppend" uiTab* nonnull-c-string uiControl*))
(define uiTabNumPages (foreign-lambda int "uiTabNumPages" uiTab*))
(define uiTabSetMargined (foreign-lambda void "uiTabSetMargined" uiTab* int bool))

(define uiNewGroup (foreign-lambda uiGroup* "uiNewGroup" nonnull-c-string))
(define uiGroupSetChild (foreign-lambda void "uiGroupSetChild" uiGroup* uiControl*))
(define uiGroupSetMargined (foreign-lambda void "uiGroupSetMargined" uiGroup* bool))

(define uiNewSpinbox (foreign-lambda uiSpinbox* "uiNewSpinbox" int int))
(define uiSpinboxOnChanged (foreign-lambda void "uiSpinboxOnChanged" uiSpinbox* (function void (uiSpinbox* c-pointer)) c-pointer))
(define uiSpinboxValue (foreign-lambda int "uiSpinboxValue" uiSpinbox*))
(define uiSpinboxSetValue (foreign-lambda void "uiSpinboxSetValue" uiSpinbox* int))

(define uiNewSlider (foreign-lambda uiSlider* "uiNewSlider" int int))
(define uiSliderOnChanged (foreign-lambda void "uiSliderOnChanged" uiSlider* (function void (uiSlider* c-pointer)) c-pointer))
(define uiSliderValue (foreign-lambda int "uiSliderValue" uiSlider*))
(define uiSliderSetValue (foreign-lambda void "uiSliderSetValue" uiSlider* int))

(define uiNewProgressBar (foreign-lambda uiProgressBar* "uiNewProgressBar"))
(define uiProgressBarValue (foreign-lambda int "uiProgressBarValue" uiProgressBar*))
(define uiProgressBarSetValue (foreign-lambda void "uiProgressBarSetValue" uiProgressBar* int))

(define uiNewHorizontalSeparator (foreign-lambda uiSeparator* "uiNewHorizontalSeparator"))
(define uiNewVerticalSeparator (foreign-lambda uiSeparator* "uiNewVerticalSeparator"))

(define uiNewCombobox (foreign-lambda uiCombobox* "uiNewCombobox"))
(define uiComboboxAppend (foreign-lambda void "uiComboboxAppend" uiCombobox* nonnull-c-string))

(define uiNewEditableCombobox (foreign-lambda uiEditableCombobox* "uiNewEditableCombobox"))
(define uiEditableComboboxAppend (foreign-lambda void "uiEditableComboboxAppend" uiEditableCombobox* nonnull-c-string))

(define uiNewRadioButtons (foreign-lambda uiRadioButtons* "uiNewRadioButtons"))
(define uiRadioButtonsAppend (foreign-lambda void "uiRadioButtonsAppend" uiRadioButtons* nonnull-c-string))

(define uiNewDateTimePicker (foreign-lambda uiDateTimePicker* "uiNewDateTimePicker"))
(define uiNewDatePicker (foreign-lambda uiDateTimePicker* "uiNewDatePicker"))
(define uiNewTimePicker (foreign-lambda uiDateTimePicker* "uiNewTimePicker"))

(define uiNewMultilineEntry (foreign-lambda uiMultilineEntry* "uiNewMultilineEntry"))
(define uiNewNonWrappingMultilineEntry (foreign-lambda uiMultilineEntry* "uiNewNonWrappingMultilineEntry"))
(define uiMultilineEntryAppend (foreign-lambda void "uiMultilineEntryAppend" uiMultilineEntry* nonnull-c-string))
(define uiMultilineEntrySetReadOnly (foreign-lambda void "uiMultilineEntrySetReadOnly" uiMultilineEntry* bool))

(define uiNewArea (foreign-lambda uiArea* "uiNewArea" uiAreaHandler*))
(define uiAreaQueueRedrawAll (foreign-lambda void "uiAreaQueueRedrawAll" uiArea*))

(define uiNewFontButton (foreign-lambda uiFontButton* "uiNewFontButton"))

(define uiNewColorButton (foreign-lambda uiColorButton* "uiNewColorButton"))
(define uiColorButtonOnChanged (foreign-lambda void "uiColorButtonOnChanged" uiColorButton* (function void (uiColorButton* c-pointer)) c-pointer))
(define uiColorButtonColor (foreign-lambda void "uiColorButtonColor" uiColorButton* double* double* double* double*))
(define uiColorButtonSetColor (foreign-lambda void "uiColorButtonSetColor" uiColorButton* double double double double))

(define uiNewForm (foreign-lambda uiForm* "uiNewForm"))
(define uiFormAppend (foreign-lambda void "uiFormAppend" uiForm* nonnull-c-string uiControl* bool))
(define uiFormSetPadded (foreign-lambda void "uiFormSetPadded" uiForm* bool))

(define uiNewGrid (foreign-lambda uiGrid* "uiNewGrid"))
(define uiGridAppend (foreign-lambda void "uiGridAppend" uiGrid* uiControl* int int int int bool uiEnum bool uiEnum))
(define uiGridSetPadded (foreign-lambda void "uiGridSetPadded" uiGrid* bool))

#>
char *libuiFileDialog(uiWindow* parent, char *(*f)(uiWindow* parent)) {
  char *filename = f(parent);
  if (filename) {
    char *ret = strdup(filename);
    uiFreeText(filename);
    C_return(ret);
  }
  C_return(NULL);
}
<#

(define uiOpenFile (foreign-lambda* c-string* ((uiWindow* parent)) "C_return(libuiFileDialog(parent, uiOpenFile));"))
(define uiSaveFile (foreign-lambda* c-string* ((uiWindow* parent)) "C_return(libuiFileDialog(parent, uiSaveFile));"))
(define uiMsgBox (foreign-lambda void "uiMsgBox" uiWindow* nonnull-c-string nonnull-c-string))
(define uiMsgBoxError (foreign-lambda void "uiMsgBoxError" uiWindow* nonnull-c-string nonnull-c-string))

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

(define (dispatch-event! widget* type)
  (match-let* ((widget (hash-table-ref widget-table widget*))
               (handlers (widget-handlers widget))
               ((handler . args) (hash-table-ref handlers type)))
    (apply handler widget args)))

(define-external (libui_WindowClosingHandler (uiWindow* window*) (c-pointer data)) bool
  (dispatch-event! window* 'closing))

(define-external (libui_ButtonClickedHandler (uiButton* button*) (c-pointer _data)) void
  (dispatch-event! button* 'clicked))

(define-external (libui_SpinboxChangedHandler (uiSpinbox* spinbox*) (c-pointer _data)) void
  (dispatch-event! spinbox* 'changed))

(define-external (libui_SliderChangedHandler (uiSlider* slider*) (c-pointer _data)) void
  (dispatch-event! slider* 'changed))

(define-external (libui_ColorButtonChangedHandler (uiColorButton* color-button*) (c-pointer _data)) void
  (dispatch-event! color-button* 'changed))

;; generic interface

(define (handler-set! widget type proc #!rest args)
  (if (and (not widget) (eqv? type 'should-quit))
      (begin
        (uiOnShouldQuit (location libui_ShouldQuitHandler) #f)
        (should-quit-handler (cons proc args)))
      (let ((widget-type (widget-type widget))
            (handlers (widget-handlers widget))
            (widget* (widget-pointer widget))
            (value (cons proc args)))
        (cond
         ((and (eqv? widget-type 'window) (eqv? type 'closing))
          (hash-table-set! handlers 'closing value)
          (uiWindowOnClosing widget* (location libui_WindowClosingHandler) #f))
         ((and (eqv? widget-type 'button) (eqv? type 'clicked))
          (hash-table-set! handlers 'clicked value)
          (uiButtonOnClicked widget* (location libui_ButtonClickedHandler) #f))
         ((and (eqv? widget-type 'spinbox) (eqv? type 'changed))
          (hash-table-set! handlers 'changed value)
          (uiSpinboxOnChanged widget* (location libui_SpinboxChangedHandler) #f))
         ((and (eqv? widget-type 'slider) (eqv? type 'changed))
          (hash-table-set! handlers 'changed value)
          (uiSliderOnChanged widget* (location libui_SliderChangedHandler) #f))
         ((and (eqv? widget-type 'color-button) (eqv? type 'changed))
          (hash-table-set! handlers 'changed value)
          (uiColorButtonOnChanged widget* (location libui_ColorButtonChangedHandler) #f))
         (else
          (abort (usage-error "Unsupported widget/type combination" 'handler-set!)))))))

;; area handler

;; NOTE: this wouldn't work with locatives to blobs as they can change...
(define area-table (make-hash-table))

(define (dispatch-area-event! area-handler* area* accessor #!rest args)
  (define (find-area area*)
    (find
     (lambda (widget)
       (and (eqv? (widget-type widget) 'area)
            (equal? (widget-pointer widget) area*)))
     (hash-table-values widget-table)))

  (let* ((area-handler (hash-table-ref area-table area-handler*))
         (proc (accessor area-handler))
         (area (find-area area*)))
    (apply proc area-handler area args)))

(define-external (libui_AreaDrawHandler (uiAreaHandler* area-handler*) (uiArea* area*) (uiAreaDrawParams* draw-params*)) void
  (dispatch-area-event! area-handler* area* area-handler-draw
                        (make-draw-params draw-params*)))

(define-external (libui_AreaMouseEventHandler (uiAreaHandler* area-handler*) (uiArea* area*) (uiAreaMouseEvent* mouse-event*)) void
  (dispatch-area-event! area-handler* area* area-handler-mouse-event
                        (make-mouse-event mouse-event*)))

(define-external (libui_AreaMouseCrossedHandler (uiAreaHandler* area-handler*) (uiArea* area*) (bool left?)) void
  (dispatch-area-event! area-handler* area* area-handler-mouse-crossed left?))

(define-external (libui_AreaDragBrokenHandler (uiAreaHandler* area-handler*) (uiArea* area*)) void
  (dispatch-area-event! area-handler* area* area-handler-drag-broken))

(define-external (libui_AreaKeyEventHandler (uiAreaHandler* area-handler*) (uiArea* area*) (uiAreaKeyEvent* key-event*)) bool
  (dispatch-area-event! area-handler* area* area-handler-key-event
                        (make-key-event key-event*)))

(define (new-area-handler draw-handler mouse-event-handler mouse-crossed-handler drag-broken-handler key-event-handler)
  (let* ((area-handler* (allocate uiAreaHandler-size))
         (_ ((foreign-lambda* void ((uiAreaHandler* handler))
               "uiAreaHandler *h = handler;"
               "h->Draw = libui_AreaDrawHandler;"
               "h->MouseEvent = libui_AreaMouseEventHandler;"
               "h->MouseCrossed = libui_AreaMouseCrossedHandler;"
               "h->DragBroken = libui_AreaDragBrokenHandler;"
               "h->KeyEvent = libui_AreaKeyEventHandler;")
             area-handler*))
         (area-handler (make-area-handler area-handler* draw-handler mouse-event-handler mouse-crossed-handler drag-broken-handler key-event-handler)))
    (hash-table-set! area-table area-handler* area-handler)
    (set-finalizer! area-handler free)))

;;; API

;; setup and tear-down

(define (init!)
  (let ((ret (uiInit)))
    (when ret
      (abort (libui-error ret 'init!)))))

(define (uninit!)
  ;; run all pending finalizers
  (gc #t)
  (uiUninit))

(define main uiMain)

(define quit! uiQuit)

;; widgets

(define margined? (make-parameter #f))
(define padded? (make-parameter #f))

(define (define-widget type constructor #!rest args)
  (let* ((widget* (apply constructor args))
         (handlers (make-hash-table eqv? eqv?-hash))
         (widget (make-widget #f type widget* handlers)))
    (hash-table-set! widget-table widget* widget)
    widget))

(define (new-window title width height #!optional menubar?)
  (let ((window (define-widget 'window uiNewWindow title width height menubar?)))
    (window-margined?-set! window (margined?))
    window))

(define (window-child-set! window child)
  (let ((window* (widget-pointer window))
        (child* (control-pointer child)))
    (uiWindowSetChild window* child*)))

(define (window-margined?-set! window margined?)
  (let ((window* (widget-pointer window)))
    (uiWindowSetMargined window* margined?)))

(define (new-button text)
  (define-widget 'button uiNewButton text))

(define (new-checkbox text)
  (define-widget 'checkbox uiNewCheckbox text))

(define (new-entry)
  (define-widget 'entry uiNewEntry))

(define (new-password-entry)
  (define-widget 'password-entry uiNewPasswordEntry))

(define (new-search-entry)
  (define-widget 'search-entry uiNewSearchEntry))

(define (entry-text-set! entry text)
  (let ((entry* (widget-pointer entry)))
    (uiEntrySetText entry* text)))

(define (entry-read-only?-set! entry read-only?)
  (let ((entry* (widget-pointer entry)))
    (uiEntrySetReadOnly entry* read-only?)))

(define (new-label text)
  (define-widget 'label uiNewLabel text))

(define (new-tab)
  (define-widget 'tab uiNewTab))

(define (tab-append! tab text control)
  (let ((tab* (widget-pointer tab))
        (control* (control-pointer control)))
    (uiTabAppend tab* text control*)
    (tab-margined?-set! tab (sub1 (tab-pages-length tab)) (margined?))))

(define (tab-pages-length tab)
  (let ((tab* (widget-pointer tab)))
    (uiTabNumPages tab*)))

(define (tab-margined?-set! tab index margined?)
  (let ((tab* (widget-pointer tab)))
    (uiTabSetMargined tab* index margined?)))

(define (new-group text)
  (let ((group (define-widget 'group uiNewGroup text)))
    (group-margined?-set! group (margined?))
    group))

(define (group-child-set! group child)
  (let ((group* (widget-pointer group))
        (child* (control-pointer child)))
    (uiGroupSetChild group* child*)))

(define (group-margined?-set! group margined?)
  (let ((group* (widget-pointer group)))
    (uiGroupSetMargined group* margined?)))

(define (new-spinbox min max)
  (define-widget 'spinbox uiNewSpinbox min max))

(define (spinbox-value spinbox)
  (let ((spinbox* (widget-pointer spinbox)))
    (uiSpinboxValue spinbox*)))

(define (spinbox-value-set! spinbox value)
  (let ((spinbox* (widget-pointer spinbox)))
    (uiSpinboxSetValue spinbox* value)))

(define (new-slider min max)
  (define-widget 'slider uiNewSlider min max))

(define (slider-value slider)
  (let ((slider* (widget-pointer slider)))
    (uiSliderValue slider*)))

(define (slider-value-set! slider value)
  (let ((slider* (widget-pointer slider)))
    (uiSliderSetValue slider* value)))

(define (new-progress-bar)
  (define-widget 'progress-bar uiNewProgressBar))

(define (progress-bar-value progress-bar)
  (let ((progress-bar* (widget-pointer progress-bar)))
    (uiProgressBarValue progress-bar*)))

(define (progress-bar-value-set! progress-bar value)
  (let ((progress-bar* (widget-pointer progress-bar)))
    (uiProgressBarSetValue progress-bar* value)))

(define (new-horizontal-separator)
  (define-widget 'horizontal-separator uiNewHorizontalSeparator))

(define (new-vertical-separator)
  (define-widget 'vertical-separator uiNewVerticalSeparator))

(define (new-combobox)
  (define-widget 'combobox uiNewCombobox))

(define (combobox-append! combobox text)
  (let ((combobox* (widget-pointer combobox)))
    (uiComboboxAppend combobox* text)))

(define (new-editable-combobox)
  (define-widget 'editable-combobox uiNewEditableCombobox))

(define (editable-combobox-append! editable-combobox text)
  (let ((editable-combobox* (widget-pointer editable-combobox)))
    (uiEditableComboboxAppend editable-combobox* text)))

(define (new-radio-buttons)
  (define-widget 'radio-buttons uiNewRadioButtons))

(define (radio-buttons-append! radio-buttons text)
  (let ((radio-buttons* (widget-pointer radio-buttons)))
    (uiRadioButtonsAppend radio-buttons* text)))

(define (new-date-time-picker)
  (define-widget 'date-time-picker uiNewDateTimePicker))

(define (new-date-picker)
  (define-widget 'date-picker uiNewDatePicker))

(define (new-time-picker)
  (define-widget 'time-picker uiNewTimePicker))

(define (new-multiline-entry)
  (define-widget 'multiline-entry uiNewMultilineEntry))

(define (new-non-wrapping-multiline-entry)
  (define-widget 'non-wrapping-multiline-entry uiNewNonWrappingMultilineEntry))

(define (multiline-entry-append! multiline-entry text)
  (let ((multiline-entry* (widget-pointer multiline-entry)))
    (uiMultilineEntryAppend multiline-entry* text)))

(define (multiline-entry-read-only?-set! multiline-entry read-only?)
  (let ((multiline-entry* (widget-pointer multiline-entry)))
    (uiMultilineEntrySetReadOnly multiline-entry* read-only?)))

(define (new-area area-handler)
  (let ((area-handler* (area-handler-pointer area-handler)))
    (define-widget 'area uiNewArea area-handler*)))

(define (area-queue-redraw-all! area)
  (let ((area* (widget-pointer area)))
    (uiAreaQueueRedrawAll area*)))

(define (new-font-button)
  (define-widget 'font-button uiNewFontButton))

(define (new-color-button)
  (define-widget 'color-button uiNewColorButton))

(define (color-button-color color-button)
  (let ((color-button* (widget-pointer color-button)))
    (let-location ((r double)
                   (g double)
                   (b double)
                   (a double))
      (uiColorButtonColor color-button* (location r) (location g) (location b) (location a))
      (list r g b a))))

(define (color-button-color-set! color-button r g b a)
  (let ((color-button* (widget-pointer color-button)))
    (uiColorButtonSetColor color-button* r g b a)))

(define (new-form)
  (let ((form (define-widget 'form uiNewForm)))
    (form-padded?-set! form (padded?))
    form))

(define (form-append! form text control #!optional stretchy?)
  (let ((form* (widget-pointer form))
        (control* (control-pointer control)))
    (uiFormAppend form* text control* stretchy?)))

(define (form-padded?-set! form padded?)
  (let ((form* (widget-pointer form)))
    (uiFormSetPadded form* padded?)))

(define (new-grid)
  (let ((grid (define-widget 'grid uiNewGrid)))
    (grid-padded?-set! grid (padded?))
    grid))

(define (grid-append! grid control left top xspan yspan hexpand halign vexpand valign)
  (let ((grid* (widget-pointer grid))
        (control* (control-pointer control))
        (halign (alignment->int halign))
        (valign (alignment->int valign)))
    (uiGridAppend grid* control* left top xspan yspan hexpand halign vexpand valign)))

(define (grid-padded?-set! grid padded?)
  (let ((grid* (widget-pointer grid)))
    (uiGridSetPadded grid* padded?)))

;; boxes

(define (new-horizontal-box)
  (let ((box (define-widget 'horizontal-box uiNewHorizontalBox)))
    (box-padded?-set! box (padded?))
    box))

(define (new-vertical-box)
  (let ((box (define-widget 'vertical-box uiNewVerticalBox)))
    (box-padded?-set! box (padded?))
    box))

(define (box-append! box control #!optional stretchy?)
  (let ((box* (widget-pointer box))
        (control* (control-pointer control)))
    (uiBoxAppend box* control* stretchy?)))

(define (box-padded?-set! box padded?)
  (let ((box* (widget-pointer box)))
    (uiBoxSetPadded box* padded?)))

;;  controls

(define (->control arg)
  (when (not (widget? arg))
    (abort (usage-error "Argument must be a widget" '->control)))
  (make-control (uiControl (widget-pointer arg))))

(define (control-destroy! control)
  (let ((control* (control-pointer control)))
    (uiControlDestroy control*)))

(define (control-show! control)
  (let ((control* (control-pointer control)))
    (uiControlShow control*)))

;; dialogs

(define (open-file parent)
  (let ((parent* (widget-pointer parent)))
    (uiOpenFile parent*)))

(define (save-file parent)
  (let ((parent* (widget-pointer parent)))
    (uiSaveFile parent*)))

(define (message-box parent title description)
  (let ((parent* (widget-pointer parent)))
    (uiMsgBox parent* title description)))

(define (message-box-error parent title description)
  (let ((parent* (widget-pointer parent)))
    (uiMsgBoxError parent* title description)))

;; SXML interface

(define (widget-by-id id)
  (find
   (lambda (widget)
     (eqv? (widget-id widget) id))
  (hash-table-values widget-table)))

(define (widgets sxml)
  (define (attribute-ref attribute attributes)
    (let ((value (assv attribute attributes)))
      (and value (cadr value))))

  (define sxml-attributes
    (match-lambda
     ((tag ('@ attributes ...) children ...)
      attributes)
     ((tag children ...)
      '())))

  (define sxml-children
    (match-lambda
     ((tag ('@ attributes ...) children ...)
      children)
     ((tag children ...)
      children)))

  (define (find-handler-pair attributes)
    (find
     (lambda (pair)
       (memv (car pair) '(should-quit closing clicked changed)))
     attributes))

  (define (widget tag attributes children)
    (let* ((id (attribute-ref 'id attributes))
           (margined?-pair (assv 'margined? attributes))
           (padded?-pair (assv 'padded? attributes))
           (text (attribute-ref 'text attributes))
           (value (attribute-ref 'value attributes))
           (read-only? (attribute-ref 'read-only? attributes))
           (min (attribute-ref 'min attributes))
           (max (attribute-ref 'max attributes))
           (handler-pair (find-handler-pair attributes))
           (widget
            (case tag
              ((window)
               (let* ((title (attribute-ref 'title attributes))
                      (width (attribute-ref 'width attributes))
                      (height (attribute-ref 'height attributes))
                      (menubar? (attribute-ref 'menubar? attributes))
                      (window (new-window title width height menubar?)))
                 (when margined?-pair
                   (window-margined?-set! window (cadr margined?-pair)))
                 (window-child-set! window (->control (widgets (car children))))
                 window))
              ((button)
               (new-button text))
              ((font-button)
               (new-font-button))
              ((color-button)
               (let ((color (attribute-ref 'color attributes))
                     (color-button (new-color-button)))
                 (when color
                   (apply color-button-color-set! color))
                 color-button))
              ((checkbox)
               (new-checkbox text))
              ((entry password-entry search-entry)
               (let ((entry ((case tag
                               ((entry) new-entry)
                               ((password-entry) new-password-entry)
                               ((search-entry) new-search-entry)))))
                 (when text
                   (entry-text-set! entry text))
                 (when read-only?
                   (entry-read-only?-set! entry read-only?))
                 entry))
              ((multiline-entry non-wrapping-multiline-entry)
               (let ((entry ((case tag
                               ((multiline-entry) new-multiline-entry)
                               ((non-wrapping-multiline-entry) new-non-wrapping-multiline-entry)))))
                 (when read-only?
                   (multiline-entry-read-only?-set! entry read-only?))
                 entry))
              ((multiline-entry)
               (new-multiline-entry))
              ((non-wrapping-multiline-entry)
               (new-non-wrapping-multiline-entry))
              ((label)
               (new-label text))
              ((spinbox)
               (let ((spinbox (new-spinbox min max)))
                 (when value
                   (spinbox-value-set! spinbox value))
                 spinbox))
              ((slider)
               (let ((slider (new-slider min max)))
                 (when value
                   (slider-value-set! slider value))
                 slider))
              ((progress-bar)
               (let ((progress-bar (new-progress-bar)))
                 (when value
                   (progress-bar-value-set! progress-bar value))
                 progress-bar))
              ((horizontal-separator)
               (new-horizontal-separator))
              ((vertical-separator)
               (new-vertical-separator))
              ((combobox editable-combobox radio-buttons)
               (let ((constructor (case tag
                                    ((combobox) new-combobox)
                                    ((editable-combobox) new-editable-combobox)
                                    ((radio-buttons) new-radio-buttons)))
                     (appender (case tag
                                 ((combobox) combobox-append!)
                                 ((editable-combobox) editable-combobox-append!)
                                 ((radio-buttons) radio-buttons-append!))))
                 (let ((widget (constructor)))
                   (for-each
                    (lambda (child)
                      (appender widget child))
                    children)
                   widget)))
              ((date-picker)
               (new-date-picker))
              ((time-picker)
               (new-time-picker))
              ((date-time-picker)
               (new-date-time-picker))
              ((area)
               (let ((handler (attribute-ref 'handler attributes)))
                 (new-area handler)))

              ((hbox vbox)
               (let* ((horizontal? (if (eqv? tag 'hbox) #t #f))
                      (box (if horizontal? (new-horizontal-box) (new-vertical-box))))
                 (for-each
                  (lambda (child)
                    (let* ((attributes (sxml-attributes child))
                           (stretchy? (attribute-ref 'stretchy? attributes)))
                      (box-append! box (->control (widgets child)) stretchy?)))
                  children)
                 (when padded?-pair
                   (box-padded?-set! box (cadr padded?-pair)))
                 box))

              ((tab)
               (let ((tab (new-tab)))
                 (for-each
                  (lambda (child index)
                    (let* ((attributes (sxml-attributes child))
                           (text (attribute-ref 'text attributes))
                           (margined?-pair (assv 'margined? attributes))
                           (children (sxml-children child))
                           (item (car children)))
                      (tab-append! tab text (->control (widgets item)))
                      (when margined?-pair
                        (tab-margined?-set! tab index (cadr margined?-pair)))))
                  children (iota (length children)))
                 tab))
              ((group)
               (let ((group (new-group text))
                     (child (car children)))
                 (group-child-set! group (->control (widgets child)))
                 (when margined?-pair
                   (group-margined?-set! group (cadr margined?-pair)))
                 group))

              ((form)
               (let ((form (new-form))
                     (padded?-pair (assv 'padded? attributes)))
                 (for-each
                  (lambda (child)
                    (let* ((attributes (sxml-attributes child))
                           (stretchy? (attribute-ref 'stretchy? attributes))
                           (text (attribute-ref 'text attributes))
                           (children (sxml-children child))
                           (item (car children)))
                      (form-append! form text (->control (widgets item)) stretchy?)))
                  children)
                 (when padded?-pair
                   (form-padded?-set! form (cadr padded?-pair)))
                 form))
              ((grid)
               (let ((grid (new-grid))
                     (padded?-pair (assv 'padded? attributes)))
                 (for-each
                  (lambda (child)
                    (let* ((attributes (sxml-attributes child))
                           (left (attribute-ref 'left attributes))
                           (top (attribute-ref 'top attributes))
                           (xspan (or (attribute-ref 'xspan attributes) 1))
                           (yspan (or (attribute-ref 'yspan attributes) 1))
                           (hexpand (attribute-ref 'hexpand attributes))
                           (halign (or (attribute-ref 'halign attributes) 'fill))
                           (vexpand (attribute-ref 'vexpand attributes))
                           (valign (or (attribute-ref 'valign attributes) 'fill))
                           (children (sxml-children child))
                           (item (car children)))
                      (grid-append! grid (->control (widgets item)) left top
                                    xspan yspan hexpand halign vexpand valign)))
                  children)
                 (when padded?-pair
                   (grid-padded?-set! grid (cadr padded?-pair)))
                 grid))

              (else
               (abort (usage-error (format "Unimplemented widget tag name: ~a" tag)
                                   'widgets))))))
        (when id
          (widget-id-set! widget id))
        (when handler-pair
          (handler-set! widget (car handler-pair) (cadr handler-pair)))
        widget))

  (match sxml
    ((tag ('@ attributes ...) children ...)
     (widget tag attributes children))
    ((tag children ...)
     (widget tag '() children))
    (_ (abort (usage-error "Invalid SXML syntax" 'widgets)))))

)
