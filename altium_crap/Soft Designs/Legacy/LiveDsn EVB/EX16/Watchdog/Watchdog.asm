;...............................................................................
MainCountLow  .equ     0x000C
MainCountHigh .equ     0x000D
CountInner    .equ     0x000E
CountOuter    .equ     0x000F
LPORTA        .equ     0x0010

F_Z           .equ   2               ; Workaround to Z not working
;...............................................................................

    .Section Text, Code

;...............................................................................
MainProc
;...............................................................................
    MOVLW   0x00
    MOVWF   MainCountLow
    MOVWF   MainCountHigh

    MOVLW   0x00     ; Set Port A as output
    TRIS    PORTA

    MOVLW   0x00     ; Set Port B as Output
    TRIS    PORTB

    MOVLW   0x00     ; Set Port C as output
    TRIS    PORTC

    MOVLW   0x00
    MOVWF   PORTB
    MOVWF   PORTC

    GOTO    MainLoop
;...............................................................................


;...............................................................................
IncrementCounter
;...............................................................................
    IncFSZ  MainCountLow,F
    Goto    DontIncrementHigh
    IncF    MainCountHigh,F
DontIncrementHigh
    RETLW 0
;...............................................................................


;...............................................................................
DisplayCounter
;...............................................................................
    MovF    MainCountLow,W
    MovWF   PORTC
    MovF    MainCountHigh,W
    MovWF   PORTB            ;Write to display
    RETLW 0
;...............................................................................


;...............................................................................
MainLoop
;...............................................................................
    MOVLW   0b00011000 ; Page 0
    MOVWF   STATUS      ; Init STATUS
    CLRWDT
CheckPortA7For1
    CALL    VeryLongDelay
    CALL    IncrementCounter
    CALL    DisplayCounter
    MovLW   080H
    TRIS    PORTA                   ;Set bit7 of PortA as input
    MovF    PORTA,W                 ;Get the new value of PortA
    MOVWF   LPORTA
    MovLW   080H
    ANDWF   LPORTA,1
    MovLW   080H
    SUBWF   LPORTA,W
    BTFSC   STATUS,F_Z
    GOTO    CheckPortA7For1			; skip watchdog clear if PortA7 equal 1
    GOTO    MainLoop
;...............................................................................

;...............................................................................
LongDelay
;...............................................................................
    MOVLW   0xFF
    MOVWF   CountOuter

LoopOuter
        MOVLW   0xFF
        MOVWF   CountInner
LoopInner
            DECFSZ  CountInner,F
        GOTO    LoopInner

        DECFSZ  CountOuter,F
    GOTO    LoopOuter

    RETLW 0
;...............................................................................


;...............................................................................
VeryLongDelay
;...............................................................................
    CALL    LongDelay
    CALL    LongDelay
    CALL    LongDelay
    CALL    LongDelay
    RETLW 0
;...............................................................................

END
