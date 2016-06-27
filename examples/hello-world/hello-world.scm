(use (prefix libui ui:))

(ui:margined? #t)
(ui:padded? #t)

(ui:init!)

(define window #f)

(define (greet _button)
  (ui:message-box window "Greeting" "Hello World!"))

(ui:widgets
 `(window
   (@ (id main)
      (title "Hello World")
      (width 180)
      (height 60)
      (menubar? #t)
      (closing ,(lambda (_window) (ui:quit!))))
   (hbox
    (button
     (@ (stretchy? #t)
        (text "Click Me!")
        (clicked ,greet)))
    (button
     (@ (stretchy? #t)
        (text "Quit")
        (clicked ,(lambda (_button) (ui:quit!))))))))

(set! window (ui:widget-by-id 'main))
(ui:control-show! (ui:->control window))
(ui:main)
