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
IncrementCounter:
;...............................................................................
    INCFSZ  MainCountLow,F          ; increment MainCountLow
    RETLW   0                       ; return if MainCountLow did not roll over

    INCF    MainCountHigh,F         ; else increment MainCountHigh as well
    RETLW   0

;...............................................................................
DisplayCounter:
;...............................................................................
    MOVF    MainCountLow,W
    MOVWF   PORTC

    MOVF    MainCountHigh,W
    MOVWF   PORTB                   ; Write to display
    RETLW   0

;...............................................................................
LongDelay:
;...............................................................................
    MOVLW   200
    MOVWF   CountOuter

LoopOuter:
        MOVLW   99
        MOVWF   CountInner

    LoopInner:
            NOP
            NOP
            DECFSZ  CountInner,F
            GOTO    LoopInner

        DECFSZ  CountOuter,F
        GOTO    LoopOuter

    RETLW 0

;...............................................................................
MainProc:
;...............................................................................
    CLRF    MainCountLow
    CLRF    MainCountHigh

    CLRW
    TRIS    PORTA                   ; Set Port A as output
    TRIS    PORTB                   ; Set Port B as Output
    TRIS    PORTC                   ; Set Port C as output

MainLoop:
    CALL    LongDelay
    CALL    IncrementCounter
    CALL    DisplayCounter

    GOTO    MainLoop
;...............................................................................

    .END
