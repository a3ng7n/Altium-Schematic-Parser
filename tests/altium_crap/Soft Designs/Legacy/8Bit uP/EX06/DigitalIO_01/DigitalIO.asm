;...............................................................................
    .section Data, Data

MainCountLow    .dsb    1
MainCountHigh   .dsb    1
CountInner      .dsb    1
CountOuter      .dsb    1

;...............................................................................
    .section Text, Code, inpage
    .global __start
__start:
    GOTO    MainProc

;...............................................................................
MainProc:
;...............................................................................
    CLRF    MainCountLow
    CLRF    MainCountHigh

    CLRW
    TRIS    PORTA                   ; Set Port A as output
    TRIS    PORTB                   ; Set Port B as Output
    TRIS    PORTC                   ; Set Port C as output

    MOVLW   27H
    MOVWF   PORTA

    MOVLW   19H
    MOVWF   PORTB

    MOVLW   49H
    MOVWF   PORTC

MainLoop:

    GOTO    MainLoop
;...............................................................................

    .END
