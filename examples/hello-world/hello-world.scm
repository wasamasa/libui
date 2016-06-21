(use (prefix libui ui:))

(ui:init!)

(define window (ui:new-window "Hello World" 180 60 #t))
(ui:handler-set! window 'closing (lambda (_window) (ui:quit!)))

(define (greet _button)
  (ui:message-box window "Greeting" "Hello World!"))

(define greet-button (ui:new-button "Click Me!"))
(ui:handler-set! greet-button 'clicked greet)

(define quit-button (ui:new-button "Quit"))
(ui:handler-set! quit-button 'clicked (lambda (_button) (ui:quit!)))

(define hbox (ui:new-horizontal-box))
(ui:box-padded?-set! hbox #t)
(ui:box-append! hbox (ui:->control greet-button) #t)
(ui:box-append! hbox (ui:->control quit-button) #t)

(ui:window-child-set! window (ui:->control hbox))
(ui:window-margined?-set! window #t)
(ui:control-show! (ui:->control window))
(ui:main)
