(use (prefix libui ui:))

(define (on-closing _window)
  (ui:quit!)
  #t)

(define (on-should-quit window)
  (ui:control-destroy! (ui:->control window))
  #t)

(define (make-basic-controls-page)
  (let ((vbox #f)
        (hbox #f)
        (group #f)
        (form #f))

    (set! vbox (ui:new-vertical-box))
    (ui:box-padded?-set! vbox #t)

    (set! hbox (ui:new-horizontal-box))
    (ui:box-padded?-set! hbox #t)
    (ui:box-append! vbox (ui:->control hbox))

    (ui:box-append! hbox (ui:->control (ui:new-button "Button")))
    (ui:box-append! hbox (ui:->control (ui:new-checkbox "Checkbox")))

    (ui:box-append! vbox (ui:->control (ui:new-label "This is a label.  Right now, labels can only span one line.")))
    (ui:box-append! vbox (ui:->control (ui:new-horizontal-separator)))

    (set! group (ui:new-group "Entries"))
    (ui:group-margined?-set! group #t)
    (ui:box-append! vbox (ui:->control group) #t)

    (set! form (ui:new-form))
    (ui:form-padded?-set! form #t)
    (ui:group-child-set! group (ui:->control form))

    (ui:form-append! form "Entry" (ui:->control (ui:new-entry)))
    (ui:form-append! form "Password Entry" (ui:->control (ui:new-password-entry)))
    (ui:form-append! form "Search Entry" (ui:->control (ui:new-search-entry)))
    (ui:form-append! form "Multiline Entry" (ui:->control (ui:new-multiline-entry)) #t)
    (ui:form-append! form "Multiline Entry No Wrap" (ui:->control (ui:new-non-wrapping-multiline-entry)) #t)

    (ui:->control vbox)))

(define spinbox #f)
(define slider #f)
(define progress-bar #f)

(define (on-spinbox-changed s)
  (ui:slider-value-set! slider (ui:spinbox-value s))
  (ui:progress-bar-value-set! progress-bar (ui:spinbox-value s)))

(define (on-slider-changed s)
  (ui:spinbox-value-set! spinbox (ui:slider-value s))
  (ui:progress-bar-value-set! progress-bar (ui:slider-value s)))

(define (make-numbers-page)
  (let ((hbox #f)
        (group #f)
        (vbox #f)
        (in-progress #f)
        (combobox #f)
        (editable-combobox #f)
        (radio-buttons #f))

    (set! hbox (ui:new-horizontal-box))
    (ui:box-padded?-set! hbox #t)

    (set! group (ui:new-group "Numbers"))
    (ui:group-margined?-set! group #t)
    (ui:box-append! hbox (ui:->control group) #t)

    (set! vbox (ui:new-vertical-box))
    (ui:box-padded?-set! vbox #t)
    (ui:group-child-set! group (ui:->control vbox))

    (set! spinbox (ui:new-spinbox 0 100))
    (set! slider (ui:new-slider 0 100))
    (set! progress-bar (ui:new-progress-bar))
    (ui:handler-set! spinbox 'changed on-spinbox-changed)
    (ui:handler-set! slider 'changed on-slider-changed)
    (ui:box-append! vbox (ui:->control spinbox))
    (ui:box-append! vbox (ui:->control slider))
    (ui:box-append! vbox (ui:->control progress-bar))

    (set! in-progress (ui:new-progress-bar))
    (ui:progress-bar-value-set! in-progress -1)
    (ui:box-append! vbox (ui:->control in-progress))

    (set! group (ui:new-group "Lists"))
    (ui:group-margined?-set! group #t)
    (ui:box-append! hbox (ui:->control group) #t)

    (set! vbox (ui:new-vertical-box))
    (ui:box-padded?-set! vbox #t)
    (ui:group-child-set! group (ui:->control vbox))

    (set! combobox (ui:new-combobox))
    (ui:combobox-append! combobox "Combobox Item 1")
    (ui:combobox-append! combobox "Combobox Item 2")
    (ui:combobox-append! combobox "Combobox Item 3")
    (ui:box-append! vbox (ui:->control combobox))

    (set! editable-combobox (ui:new-editable-combobox))
    (ui:editable-combobox-append! editable-combobox "Editable Item 1")
    (ui:editable-combobox-append! editable-combobox "Editable Item 2")
    (ui:editable-combobox-append! editable-combobox "Editable Item 3")
    (ui:box-append! vbox (ui:->control editable-combobox))

    (set! radio-buttons (ui:new-radio-buttons))
    (ui:radio-buttons-append! radio-buttons "Radio Button 1")
    (ui:radio-buttons-append! radio-buttons "Radio Button 2")
    (ui:radio-buttons-append! radio-buttons "Radio Button 3")
    (ui:box-append! vbox (ui:->control radio-buttons))

    (ui:->control hbox)))

(define main-window #f)

(define (on-open-file-clicked _button entry)
  (let ((filename (ui:open-file main-window)))
    (if filename
        (ui:entry-text-set! entry filename)
        (ui:entry-text-set! entry "(cancelled)"))))

(define (on-save-file-clicked _button entry)
  (let ((filename (ui:save-file main-window)))
    (if filename
        (ui:entry-text-set! entry filename)
        (ui:entry-text-set! entry "(cancelled)"))))

(define (on-message-box-clicked _button)
  (ui:message-box main-window
                  "This is a normal message box."
                  "More detailed information can be shown here."))

(define (on-message-box-error-clicked _button)
  (ui:message-box-error main-window
                        "This message box describes an error."
                        "More detailed information can be shown here."))

(define (make-data-choosers-page)
  (let ((hbox #f)
        (vbox #f)
        (grid #f)
        (button #f)
        (entry #f)
        (message-grid #f))

    (set! hbox (ui:new-horizontal-box))
    (ui:box-padded?-set! hbox #t)

    (set! vbox (ui:new-vertical-box))
    (ui:box-padded?-set! vbox #t)
    (ui:box-append! hbox (ui:->control vbox))

    (ui:box-append! vbox (ui:->control (ui:new-date-picker)))
    (ui:box-append! vbox (ui:->control (ui:new-time-picker)))
    (ui:box-append! vbox (ui:->control (ui:new-date-time-picker)))

    (ui:box-append! vbox (ui:->control (ui:new-font-button)))
    (ui:box-append! vbox (ui:->control (ui:new-color-button)))

    (ui:box-append! hbox (ui:->control (ui:new-vertical-separator)))

    (set! vbox (ui:new-vertical-box))
    (ui:box-padded?-set! vbox #t)
    (ui:box-append! hbox (ui:->control vbox) #t)

    (set! grid (ui:new-grid))
    (ui:grid-padded?-set! grid #t)
    (ui:box-append! vbox (ui:->control grid))

    (set! button (ui:new-button "Open File"))
    (set! entry (ui:new-entry))
    (ui:entry-read-only?-set! entry #t)
    (ui:handler-set! button 'clicked on-open-file-clicked entry)
    (ui:grid-append! grid (ui:->control button) 0 0 1 1 #f 'fill #f 'fill)
    (ui:grid-append! grid (ui:->control entry) 1 0 1 1 #t 'fill #f 'fill)

    (set! button (ui:new-button "Save File"))
    (set! entry (ui:new-entry))
    (ui:entry-read-only?-set! entry #t)
    (ui:handler-set! button 'clicked on-save-file-clicked entry)
    (ui:grid-append! grid (ui:->control button) 0 1 1 1 #f 'fill #f 'fill)
    (ui:grid-append! grid (ui:->control entry) 1 1 1 1 #t 'fill #f 'fill)

    (set! message-grid (ui:new-grid))
    (ui:grid-padded?-set! message-grid #t)
    (ui:grid-append! grid (ui:->control message-grid) 0 2 2 1 #f 'center #f 'start)

    (set! button (ui:new-button "Message Box"))
    (ui:handler-set! button 'clicked on-message-box-clicked)
    (ui:grid-append! message-grid (ui:->control button) 0 0 1 1 #f 'fill #f 'fill)
    (set! button (ui:new-button "Error Box"))
    (ui:handler-set! button 'clicked on-message-box-error-clicked)
    (ui:grid-append! message-grid (ui:->control button) 1 0 1 1 #f 'fill #f 'fill)

    (ui:->control hbox)))

(ui:init!)

(set! main-window (ui:new-window "libui Control Gallery" 640 480 #t))
(ui:handler-set! main-window 'closing on-closing)
(ui:handler-set! #f 'should-quit on-should-quit main-window)

(define tab (ui:new-tab))
(ui:window-child-set! main-window (ui:->control tab))
(ui:window-margined?-set! main-window #t)

(define basic-controls-page (make-basic-controls-page))
(ui:tab-append! tab "Basic Controls" basic-controls-page)
(ui:tab-margined?-set! tab 0 #t)

(define numbers-page (make-numbers-page))
(ui:tab-append! tab "Numbers and Lists" numbers-page)
(ui:tab-margined?-set! tab 1 #t)

(define data-choosers-page (make-data-choosers-page))
(ui:tab-append! tab "Data Choosers" data-choosers-page)
(ui:tab-margined?-set! tab 2 #t)

(ui:control-show! (ui:->control main-window))
(ui:main)
