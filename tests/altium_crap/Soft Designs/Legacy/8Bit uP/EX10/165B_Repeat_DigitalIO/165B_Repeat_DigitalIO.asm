;...............................................................................
    .Section Data, Data
MainCountLow    .dsb    1
MainCountHigh   .dsb    1
CountInner      .dsb    1
CountOuter      .dsb    1
CountLonger     .dsb    1

;...............................................................................
    .Section Text, Code, inpage
__start:
    GOTO    MainProc

;...............................................................................
IncrementCounter:
;...............................................................................
    INCFSZ  MainCountLow,f
    RETLW   0
    INCF    MainCountHigh,f
    RETLW   0

;...............................................................................
DisplayCounter:
;...............................................................................
    MOVF    MainCountLow,W
    MOVWF   PORTC
    MOVF    MainCountHigh,W
    MOVWF   PORTB
    RETLW 0

;...............................................................................
LongDelay:
;...............................................................................
    MOVLW   100
    MOVWF   CountOuter

    LoopOuter:
        MOVLW   99
        MOVWF   CountInner

        LoopInner:
            NOP
            NOP
            DECFSZ  CountInner,f
        GOTO    LoopInner

        DECFSZ  CountOuter,f
    GOTO    LoopOuter

    RETLW 0

;...............................................................................
VeryLongDelay:
;...............................................................................
    .GEN FOR CountLonger = #0 TO #100
        CALL LongDelay
    .GEN ENDFOR
    RETLW 0

;...............................................................................
MainProc:
;...............................................................................
    CLRF   MainCountLow
    CLRF   MainCountHigh

    CLRW
    TRIS    PORTA
    TRIS    PORTB
    TRIS    PORTC

MainLoop:
    CALL    VeryLongDelay
    CALL    IncrementCounter
    CALL    DisplayCounter
    GOTO    MainLoop
;...............................................................................
    .END
