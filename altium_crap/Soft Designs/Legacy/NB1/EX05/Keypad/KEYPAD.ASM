;...............................................................................
CountInner    .equ     0x000E         ; Loop variable
CountOuter    .equ     0x000F         ; Loop variable
RowBits       .equ     0x0010         ; Bits read from row, each indicates a col
KeyValue      .equ     0x0014         ; Value of key pressed
KeyPressed    .equ     0x0017         ; 1 if a key is pressed, otherwise 0
;...............................................................................

    .Section Text, Code

;...............................................................................
MainProc
;...............................................................................
    MOVLW   0b00011000 
    MOVWF   STATUS     
    MOVLW   0x00
    MOVWF   PORTB                     ; clear port B
    MOVLW   0x00
    MOVWF   PORTC                     ; clear port C
    
;...............................................................................
MainLoop
;...............................................................................
    CALL    WaitForKeyPress           
    MOVLW   0x01                      
    MOVWF   PORTC                     ; write 1 to port C to indicate key down
    MOVF    KeyValue, W   
    MOVWF   PORTB                     ; write key value to port B
    CALL    WaitForNoKeysDown
    MOVLW   0x00
    MOVWF   PORTC                     ; clear port C to indicate no key down
    GOTO    MainLoop
;...............................................................................

;...............................................................................
WaitForKeyPress
;...............................................................................
    CALL    GetKeyValue               ; determine if key down and get value
    CALL    LongDelay             
    MOVF    KeyPressed, F             
    BTFSS   STATUS, 2
    GOTO    GotSomething              ; if key pressed then jump out of loop
    GOTO    WaitForKeyPress           ; loop back if no keypressed
  GotSomething:
    CALL    GetKeyValue               ; get value again (bouncing should have settled)
    RETLW   0
;...............................................................................
    
;...............................................................................
WaitForNoKeysDown
;...............................................................................
    CALL    GetKeyValue               ; determine if key down
    MOVF    KeyPressed, F            
    BTFSC   STATUS, 2
    GOTO    GotNothing                ; if no key down jump out of loop
    CALL    LongDelay
    GOTO    WaitForNoKeysDown         ; loop if key still down
  GotNothing:
    RETLW   0
;...............................................................................

; GetKeyValue determines if a key is down and finds its value. These values
; are set in the KeyPressed and KeyValue registers.
;...............................................................................
GetKeyValue                           
;...............................................................................
    MOVLW   0
    MOVWF   KeyPressed                ; Clear keypressed   
    ; Check first row
    MOVLW   0b11111110                
    MOVWF   PORTA                     ; Put first row low, all others high
    MOVF    PORTA, W                     
    MOVWF   RowBits                   ; Put read columns into variable, pressed cols will be low
    BTFSC   RowBits, 0                ; check bit 0, if low return 1. Continue for other row/ cols ...
    GOTO    CheckRow0Col1
    MOVLW   0x01
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow0Col1:
    BTFSC   RowBits, 1
    GOTO    CheckRow0Col2
    MOVLW   0x02
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow0Col2:
    BTFSC   RowBits, 2
    GOTO    CheckRow0Col3
    MOVLW   0x03
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow0Col3:
    BTFSC   RowBits, 3
    GOTO    CheckRow1
    MOVLW   0x0C
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow1:
    MOVLW   0b11111101
    MOVWF   PORTA
    MOVF    PORTA, W
    MOVWF   RowBits
    BTFSC   RowBits, 0                
    GOTO    CheckRow1Col1
    MOVLW   0x04
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow1Col1:
    BTFSC   RowBits, 1
    GOTO    CheckRow1Col2
    MOVLW   0x05
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow1Col2:
    BTFSC   RowBits, 2
    GOTO    CheckRow1Col3
    MOVLW   0x06
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow1Col3:
    BTFSC   RowBits, 3
    GOTO    CheckRow2
    MOVLW   0x0D
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow2:
    MOVLW   0b11111011
    MOVWF   PORTA
    MOVF    PORTA, W
    MOVWF   RowBits
    BTFSC   RowBits, 0                
    GOTO    CheckRow2Col1
    MOVLW   0x07
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow2Col1:
    BTFSC   RowBits, 1
    GOTO    CheckRow2Col2
    MOVLW   0x08
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow2Col2:
    BTFSC   RowBits, 2
    GOTO    CheckRow2Col3
    MOVLW   0x09
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow2Col3:
    BTFSC   RowBits, 3
    GOTO    CheckRow3
    MOVLW   0x0E
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow3:
    MOVLW   0b11110111
    MOVWF   PORTA
    MOVF    PORTA, W
    MOVWF   RowBits
    BTFSC   RowBits, 0                
    GOTO    CheckRow3Col1
    MOVLW   0x0A
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow3Col1:
    BTFSC   RowBits, 1
    GOTO    CheckRow3Col2
    MOVLW   0x00
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow3Col2:
    BTFSC   RowBits, 2
    GOTO    CheckRow3Col3
    MOVLW   0x0B
    MOVWF   KeyValue
    GOTO    SomethingDown
  CheckRow3Col3:
    BTFSC   RowBits, 3
    GOTO    NothingDown
    MOVLW   0x0F
    MOVWF   KeyValue
  SomethingDown:
    MOVLW   1
    MOVWF   KeyPressed
  NothingDown:
    RETLW   0
;...............................................................................

;...............................................................................
LongDelay
;...............................................................................
    MOVLW   0x10
    MOVWF   CountOuter

  LoopOuter:
    MOVLW   0x10
    MOVWF   CountInner

  LoopInner:
    DECFSZ  CountInner,F
    GOTO    LoopInner

    DECFSZ  CountOuter,F
    GOTO    LoopOuter

    RETLW 0
;...............................................................................


END
