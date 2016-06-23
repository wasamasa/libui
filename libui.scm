(module libui
  (init! uninit! main quit!
   handler-set!
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
   new-multiline-entry new-non-wrapping-multiline-entry
   new-area area-queue-redraw-all!
   new-area-handler
   area-mouse-event-x area-mouse-event-y area-mouse-event-area-width area-mouse-event-area-height
   area-draw-params-context area-draw-params-area-width area-draw-params-area-height
   new-font-button
   new-color-button color-button-color color-button-color-set!
   new-form form-append! form-padded?-set!
   new-grid grid-append! grid-padded?-set!
   ->control control-destroy! control-show!
   open-file save-file message-box message-box-error)

(import chicken scheme foreign)
(use srfi-69 matchable lolevel
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
(define-record window pointer handlers)
(define-record button pointer handlers)
(define-record checkbox pointer handlers)
(define-record entry pointer handlers)
(define-record label pointer)
(define-record tab pointer)
(define-record group pointer)
(define-record spinbox pointer handlers)
(define-record slider pointer handlers)
(define-record progress-bar pointer)
(define-record separator pointer)
(define-record combobox pointer handlers)
(define-record editable-combobox pointer handlers)
(define-record radio-buttons pointer handlers)
(define-record date-time-picker pointer)
(define-record multiline-entry pointer handlers)
(define-record context pointer)
(define-record area pointer)
(define-record area-draw-params pointer)
(define-record area-mouse-event pointer)
(define-record area-key-event pointer)
(define-record font-button pointer handlers)
(define-record color-button pointer handlers)
(define-record form pointer)
(define-record grid pointer)
(define-record box pointer)

;;; struct helpers

;; area handler

(define-record area-handler pointer draw mouse-event mouse-crossed drag-broken key-event)

(define uiAreaHandler-size (foreign-type-size (struct "uiAreaHandler")))

;; area mouse event

(define (area-mouse-event-x mouse-event)
  (let ((mouse-event* (area-mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->X);") mouse-event*)))

(define (area-mouse-event-y mouse-event)
  (let ((mouse-event* (area-mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->Y);") mouse-event*)))

(define (area-mouse-event-area-width mouse-event)
  (let ((mouse-event* (area-mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->AreaWidth);") mouse-event*)))

(define (area-mouse-event-area-height mouse-event)
  (let ((mouse-event* (area-mouse-event-pointer mouse-event)))
    ((foreign-lambda* double ((uiAreaMouseEvent* e)) "C_return(e->AreaHeight);") mouse-event*)))

;; area draw params

(define (area-draw-params-context draw-params)
  (let* ((draw-params* (area-draw-params-pointer draw-params))
         (context* ((foreign-lambda* uiDrawContext* ((uiAreaDrawParams* params)) "C_return(params->Context);") draw-params*)))
    (make-context context*)))

(define (area-draw-params-area-width draw-params)
  (let ((draw-params* (area-draw-params-pointer draw-params)))
    ((foreign-lambda* double ((uiAreaDrawParams* params)) "C_return(params->AreaWidth);") draw-params*)))

(define (area-draw-params-area-height draw-params)
  (let ((draw-params* (area-draw-params-pointer draw-params)))
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

(define (dispatch-event! widget* accessor type)
  (match-let* ((widget (hash-table-ref widget-table widget*))
               (handlers (accessor widget))
               ((handler . args) (hash-table-ref handlers type)))
    (apply handler widget args)))

(define-external (libui_WindowClosingHandler (uiWindow* window*) (c-pointer data)) bool
  (dispatch-event! window* window-handlers 'closing))

(define-external (libui_ButtonClickedHandler (uiButton* button*) (c-pointer _data)) void
  (dispatch-event! button* button-handlers 'clicked))

(define-external (libui_SpinboxChangedHandler (uiSpinbox* spinbox*) (c-pointer _data)) void
  (dispatch-event! spinbox* spinbox-handlers 'changed))

(define-external (libui_SliderChangedHandler (uiSlider* slider*) (c-pointer _data)) void
  (dispatch-event! slider* slider-handlers 'changed))

(define-external (libui_ColorButtonChangedHandler (uiColorButton* color-button*) (c-pointer _data)) void
  (dispatch-event! color-button* color-button-handlers 'changed))

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
   ((and (spinbox? widget) (eqv? type 'changed))
    (hash-table-set! (spinbox-handlers widget) 'changed (cons proc args))
    (uiSpinboxOnChanged (spinbox-pointer widget) (location libui_SpinboxChangedHandler) #f))
   ((and (slider? widget) (eqv? type 'changed))
    (hash-table-set! (slider-handlers widget) 'changed (cons proc args))
    (uiSliderOnChanged (slider-pointer widget) (location libui_SliderChangedHandler) #f))
   ((and (color-button? widget) (eqv? type 'changed))
    (hash-table-set! (color-button-handlers widget) 'changed (cons proc args))
    (uiColorButtonOnChanged (color-button-pointer widget) (location libui_ColorButtonChangedHandler) #f))

   (else
    (abort (usage-error "Unsupported widget/type combination" 'handler-set!)))))

;; area handler

;; NOTE: this wouldn't work with locatives to blobs as they can change...
(define area-table (make-hash-table))

(define-external (libui_AreaDrawHandler (uiAreaHandler* area-handler*) (uiArea* area*) (uiAreaDrawParams* draw-params*)) void
  (let* ((area-handler (hash-table-ref area-table area-handler*))
         (proc (area-handler-draw area-handler))
         (area (make-area area*))
         (draw-params (make-area-draw-params draw-params*)))
    (proc area-handler area draw-params)))

(define-external (libui_AreaMouseEventHandler (uiAreaHandler* area-handler*) (uiArea* area*) (uiAreaMouseEvent* mouse-event*)) void
  (let* ((area-handler (hash-table-ref area-table area-handler*))
         (proc (area-handler-mouse-event area-handler))
         (area (make-area area*))
         (mouse-event (make-area-mouse-event mouse-event*)))
    (proc area-handler area mouse-event)))

(define-external (libui_AreaMouseCrossedHandler (uiAreaHandler* area-handler*) (uiArea* area*) (bool left?)) void
  (let* ((area-handler (hash-table-ref area-table area-handler*))
         (proc (area-handler-mouse-crossed area-handler))
         (area (make-area area*)))
    (proc area-handler area left?)))

(define-external (libui_AreaDragBrokenHandler (uiAreaHandler* area-handler*) (uiArea* area*)) void
  (let* ((area-handler (hash-table-ref area-table area-handler*))
         (proc (area-handler-drag-broken area-handler))
         (area (make-area area*)))
    (proc area-handler area)))

(define-external (libui_AreaKeyEventHandler (uiAreaHandler* area-handler*) (uiArea* area*) (uiAreaKeyEvent* key-event*)) bool
  (let* ((area-handler (hash-table-ref area-table area-handler*))
         (proc (area-handler-key-event area-handler))
         (area (make-area area*))
         (key-event (make-area-key-event key-event*)))
    (proc area-handler area key-event)))

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

(define uninit! uiUninit)

(define main uiMain)

(define quit! uiQuit)

;; widgets

(define (define-widget constructor wrapper #!rest args)
  (let* ((widget* (apply constructor args))
         (handlers (make-hash-table eqv? eqv?-hash))
         (widget (wrapper widget* handlers)))
    (hash-table-set! widget-table widget* widget)
    widget))

(define (define-widget* constructor wrapper #!rest args)
  (let ((widget* (apply constructor args)))
    (wrapper widget*)))

(define (new-window title width height #!optional menubar?)
  (define-widget uiNewWindow make-window title width height menubar?))

(define (window-child-set! window child)
  (let ((window* (window-pointer window))
        (child* (control-pointer child)))
    (uiWindowSetChild window* child*)))

(define (window-margined?-set! window margined?)
  (let ((window* (window-pointer window)))
    (uiWindowSetMargined window* margined?)))

(define (new-button text)
  (define-widget uiNewButton make-button text))

(define (new-checkbox text)
  (define-widget uiNewCheckbox make-checkbox text))

(define (new-entry)
  (define-widget uiNewEntry make-entry))

(define (new-password-entry)
  (define-widget uiNewPasswordEntry make-entry))

(define (new-search-entry)
  (define-widget uiNewSearchEntry make-entry))

(define (entry-text-set! entry text)
  (let ((entry* (entry-pointer entry)))
    (uiEntrySetText entry* text)))

(define (entry-read-only?-set! entry read-only?)
  (let ((entry* (entry-pointer entry)))
    (uiEntrySetReadOnly entry* read-only?)))

(define (new-label text)
  (define-widget* uiNewLabel make-label text))

(define (new-tab)
  (define-widget* uiNewTab make-tab))

(define (tab-append! tab text control)
  (let ((tab* (tab-pointer tab))
        (control* (control-pointer control)))
    (uiTabAppend tab* text control*)))

(define (tab-margined?-set! tab index margined?)
  (let ((tab* (tab-pointer tab)))
    (uiTabSetMargined tab* index margined?)))

(define (new-group text)
  (define-widget* uiNewGroup make-group text))

(define (group-child-set! group child)
  (let ((group* (group-pointer group))
        (child* (control-pointer child)))
    (uiGroupSetChild group* child*)))

(define (group-margined?-set! group margined?)
  (let ((group* (group-pointer group)))
    (uiGroupSetMargined group* margined?)))

(define (new-spinbox min max)
  (define-widget uiNewSpinbox make-spinbox min max))

(define (spinbox-value spinbox)
  (let ((spinbox* (spinbox-pointer spinbox)))
    (uiSpinboxValue spinbox*)))

(define (spinbox-value-set! spinbox value)
  (let ((spinbox* (spinbox-pointer spinbox)))
    (uiSpinboxSetValue spinbox* value)))

(define (new-slider min max)
  (define-widget uiNewSlider make-slider min max))

(define (slider-value slider)
  (let ((slider* (slider-pointer slider)))
    (uiSliderValue slider*)))

(define (slider-value-set! slider value)
  (let ((slider* (slider-pointer slider)))
    (uiSliderSetValue slider* value)))

(define (new-progress-bar)
  (define-widget* uiNewProgressBar make-progress-bar))

(define (progress-bar-value progress-bar)
  (let ((progress-bar* (progress-bar-pointer progress-bar)))
    (uiProgressBarValue progress-bar*)))

(define (progress-bar-value-set! progress-bar value)
  (let ((progress-bar* (progress-bar-pointer progress-bar)))
    (uiProgressBarSetValue progress-bar* value)))

(define (new-horizontal-separator)
  (define-widget* uiNewHorizontalSeparator make-separator))

(define (new-vertical-separator)
  (define-widget* uiNewVerticalSeparator make-separator))

(define (new-combobox)
  (define-widget uiNewCombobox make-combobox))

(define (combobox-append! combobox text)
  (let ((combobox* (combobox-pointer combobox)))
    (uiComboboxAppend combobox* text)))

(define (new-editable-combobox)
  (define-widget uiNewEditableCombobox make-editable-combobox))

(define (editable-combobox-append! editable-combobox text)
  (let ((editable-combobox* (editable-combobox-pointer editable-combobox)))
    (uiEditableComboboxAppend editable-combobox* text)))

(define (new-radio-buttons)
  (define-widget uiNewRadioButtons make-radio-buttons))

(define (radio-buttons-append! radio-buttons text)
  (let ((radio-buttons* (radio-buttons-pointer radio-buttons)))
    (uiRadioButtonsAppend radio-buttons* text)))

(define (new-date-time-picker)
  (define-widget* uiNewDateTimePicker make-date-time-picker))

(define (new-date-picker)
  (define-widget* uiNewDatePicker make-date-time-picker))

(define (new-time-picker)
  (define-widget* uiNewTimePicker make-date-time-picker))

(define (new-multiline-entry)
  (define-widget uiNewMultilineEntry make-multiline-entry))

(define (new-non-wrapping-multiline-entry)
  (define-widget uiNewNonWrappingMultilineEntry make-multiline-entry))

(define (new-area area-handler)
  (let ((area-handler* (area-handler-pointer area-handler)))
    (define-widget* uiNewArea make-area area-handler*)))

(define (area-queue-redraw-all! area)
  (let ((area* (area-pointer area)))
    (uiAreaQueueRedrawAll area*)))

(define (new-font-button)
  (define-widget uiNewFontButton make-font-button))

(define (new-color-button)
  (define-widget uiNewColorButton make-color-button))

(define (color-button-color color-button)
  (let ((color-button* (color-button-pointer color-button)))
    (let-location ((r double)
                   (g double)
                   (b double)
                   (a double))
      (uiColorButtonColor color-button* (location r) (location g) (location b) (location a))
      (list r g b a))))

(define (color-button-color-set! color-button r g b a)
  (let ((color-button* (color-button-pointer color-button)))
    (uiColorButtonSetColor color-button* r g b a)))

(define (new-form)
  (define-widget* uiNewForm make-form))

(define (form-append! form text control #!optional stretchy?)
  (let ((form* (form-pointer form))
        (control* (control-pointer control)))
    (uiFormAppend form* text control* stretchy?)))

(define (form-padded?-set! form padded?)
  (let ((form* (form-pointer form)))
    (uiFormSetPadded form* padded?)))

(define (new-grid)
  (define-widget* uiNewGrid make-grid))

(define (grid-append! grid control left top xspan yspan hexpand halign vexpand valign)
  (let ((grid* (grid-pointer grid))
        (control* (control-pointer control))
        (halign (alignment->int halign))
        (valign (alignment->int valign)))
    (uiGridAppend grid* control* left top xspan yspan hexpand halign vexpand valign)))

(define (grid-padded?-set! grid padded?)
  (let ((grid* (grid-pointer grid)))
    (uiGridSetPadded grid* padded?)))

;; boxes

(define (new-horizontal-box)
  (let ((box* (uiNewHorizontalBox)))
    (make-box box*)))

(define (new-vertical-box)
  (let ((box* (uiNewVerticalBox)))
    (make-box box*)))

(define (box-append! box control #!optional stretchy?)
  (let ((box* (box-pointer box))
        (control* (control-pointer control)))
    (uiBoxAppend box* control* stretchy?)))

(define (box-padded?-set! box padded?)
  (let ((box* (box-pointer box)))
    (uiBoxSetPadded box* padded?)))

;;  controls

(define (->control arg)
  (cond
   ((window? arg)
    (make-control (uiControl (window-pointer arg))))
   ((button? arg)
    (make-control (uiControl (button-pointer arg))))
   ((checkbox? arg)
    (make-control (uiControl (checkbox-pointer arg))))
   ((entry? arg)
    (make-control (uiControl (entry-pointer arg))))
   ((label? arg)
    (make-control (uiControl (label-pointer arg))))
   ((tab? arg)
    (make-control (uiControl (tab-pointer arg))))
   ((group? arg)
    (make-control (uiControl (group-pointer arg))))
   ((spinbox? arg)
    (make-control (uiControl (spinbox-pointer arg))))
   ((slider? arg)
    (make-control (uiControl (slider-pointer arg))))
   ((progress-bar? arg)
    (make-control (uiControl (progress-bar-pointer arg))))
   ((separator? arg)
    (make-control (uiControl (separator-pointer arg))))
   ((combobox? arg)
    (make-control (uiControl (combobox-pointer arg))))
   ((editable-combobox? arg)
    (make-control (uiControl (editable-combobox-pointer arg))))
   ((radio-buttons? arg)
    (make-control (uiControl (radio-buttons-pointer arg))))
   ((date-time-picker? arg)
    (make-control (uiControl (date-time-picker-pointer arg))))
   ((multiline-entry? arg)
    (make-control (uiControl (multiline-entry-pointer arg))))
   ((area? arg)
    (make-control (uiControl (area-pointer arg))))
   ((font-button? arg)
    (make-control (uiControl (font-button-pointer arg))))
   ((color-button? arg)
    (make-control (uiControl (color-button-pointer arg))))
   ((form? arg)
    (make-control (uiControl (form-pointer arg))))
   ((grid? arg)
    (make-control (uiControl (grid-pointer arg))))
   ((box? arg)
    (make-control (uiControl (box-pointer arg))))
   (else
    (abort (usage-error "Unsupported type" '->control)))))

(define (control-destroy! control)
  (let ((control* (control-pointer control)))
    (uiControlDestroy control*)))

(define (control-show! control)
  (let ((control* (control-pointer control)))
    (uiControlShow control*)))

;; dialogs

(define (open-file parent)
  (let ((parent* (window-pointer parent)))
    (uiOpenFile parent*)))

(define (save-file parent)
  (let ((parent* (window-pointer parent)))
    (uiSaveFile parent*)))

(define (message-box parent title description)
  (let ((parent* (window-pointer parent)))
    (uiMsgBox parent* title description)))

(define (message-box-error parent title description)
  (let ((parent* (window-pointer parent)))
    (uiMsgBoxError parent* title description)))

)
