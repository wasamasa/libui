(use (prefix libui ui:))

(ui:margined? #t)
(ui:padded? #t)

(define (on-closing _window)
  (ui:quit!)
  #t)

(define (on-should-quit window)
  (ui:control-destroy! (ui:->control window))
  #t)

(define spinbox #f)
(define slider #f)
(define progress-bar #f)

(define (on-spinbox-changed s)
  (ui:slider-value-set! slider (ui:spinbox-value s))
  (ui:progress-bar-value-set! progress-bar (ui:spinbox-value s)))

(define (on-slider-changed s)
  (ui:spinbox-value-set! spinbox (ui:slider-value s))
  (ui:progress-bar-value-set! progress-bar (ui:slider-value s)))

(define main-window #f)
(define open-file-entry #f)
(define save-file-entry #f)

(define (on-open-file-clicked _button)
  (let ((filename (ui:open-file main-window)))
    (if filename
        (ui:entry-text-set! open-file-entry filename)
        (ui:entry-text-set! open-file-entry "(cancelled)"))))

(define (on-save-file-clicked _button)
  (let ((filename (ui:save-file main-window)))
    (if filename
        (ui:entry-text-set! save-file-entry filename)
        (ui:entry-text-set! save-file-entry "(cancelled)"))))

(define (on-message-box-clicked _button)
  (ui:message-box main-window
                  "This is a normal message box."
                  "More detailed information can be shown here."))

(define (on-message-box-error-clicked _button)
  (ui:message-box-error main-window
                        "This message box describes an error."
                        "More detailed information can be shown here."))

(ui:init!)

(define basic-controls-page
  '(vbox
    (hbox
     (button (@ (text "Button")))
     (checkbox (@ (text "Checkbox"))))
    (label (@ (text "This is a label.  Right now, labels can only span one line.")))
    (horizontal-separator)
    (group
     (@ (text "Entries")
        (stretchy? #t))
     (form
      (item
       (@ (text "Entry"))
       (entry))
      (item
       (@ (text "Password Entry"))
       (password-entry))
      (item
       (@ (text "Search Entry"))
       (search-entry))
      (item
       (@ (text "Multiline Entry")
          (stretchy? #t))
       (multiline-entry))
      (item
       (@ (text "Multiline Entry No Wrap")
          (stretchy? #t))
       (non-wrapping-multiline-entry))))))

(define numbers-page
  `(hbox
    (group
     (@ (text "Numbers")
        (stretchy? #t))
     (vbox
      (spinbox
       (@ (id spinbox)
          (min 0)
          (max 100)
          (changed ,on-spinbox-changed)))
      (slider
       (@ (id slider)
          (min 0)
          (max 100)
          (changed ,on-slider-changed)))
      (progress-bar
       (@ (id progress-bar)))
      (progress-bar
       (@ (id in-progress)
          (value -1)))))
    (group
     (@ (text "Lists")
        (stretchy? #t))
     (vbox
      (combobox "Combobox Item 1"
                "Combobox Item 2"
                "Combobox Item 3")
      (editable-combobox "Editable Item 1"
                         "Editable Item 2"
                         "Editable Item 3")
      (radio-buttons "Radio Button 1"
                     "Radio Button 2"
                     "Radio Button 3")))))

(define data-choosers-page
  `(hbox
    (vbox
     (date-picker)
     (time-picker)
     (date-time-picker)
     (font-button)
     (color-button))
    (vertical-separator)
    (vbox
     (@ (stretchy? #t))
     (grid
      (item
       (@ (left 0) (top 0))
       (button
        (@ (text "Open File")
           (clicked ,on-open-file-clicked))))
      (item
       (@ (left 1) (top 0)
          (hexpand #t))
       (entry
        (@ (id open-file-entry)
           (read-only? #t))))
      (item
       (@ (left 0) (top 1))
       (button
        (@ (text "Save File")
           (clicked ,on-save-file-clicked))))
      (item
       (@ (left 1) (top 1)
          (hexpand #t))
       (entry
        (@ (id save-file-entry)
           (read-only? #t))))
      (item
       (@ (left 0) (top 2)
          (xspan 2)
          (halign center)
          (valign start))
       (grid
        (item
         (@ (left 0) (top 0))
         (button
          (@ (text "Message Box")
             (clicked ,on-message-box-clicked))))
        (item
         (@ (left 1) (top 0))
         (button
          (@ (text "Error Box")
             (clicked ,on-message-box-error-clicked))))))))))

(ui:widgets
 `(window
   (@ (id main)
      (title "libui Control Gallery")
      (width 640)
      (height 480)
      (menubar? #t)
      (closing ,on-closing))
   (tab
    (item
     (@ (text "Basic Controls"))
     ,basic-controls-page)
    (item
     (@ (text "Numbers and Lists"))
     ,numbers-page)
    (item
     (@ (text "Data Choosers"))
     ,data-choosers-page))))

(set! spinbox (ui:widget-by-id 'spinbox))
(set! slider (ui:widget-by-id 'slider))
(set! progress-bar (ui:widget-by-id 'progress-bar))
(set! in-progress (ui:widget-by-id 'in-progress))

(set! main-window (ui:widget-by-id 'main))
(set! open-file-entry (ui:widget-by-id 'open-file-entry))
(set! save-file-entry (ui:widget-by-id 'save-file-entry))
(ui:control-show! (ui:->control main-window))
(ui:main)
