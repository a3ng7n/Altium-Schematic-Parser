    .extern  __lc_es
;...............................................................................
    .section   ports, sfr, at( 0000H )

PORT0OUT      .dsb   1
PORT1OUT      .dsb   1
;...............................................................................
    .section   counters, data

MainCountLow  .dsb   1
MainCountHigh .dsb   1
;...............................................................................
    .section text1, code, at( 0000H )

    JP      MainProc
;...............................................................................
    .section text2, code, at (0100H)

;...............................................................................
MainProc:
;...............................................................................
    LD      SP,__lc_es-1
;    LD      SP,0x200
    LD      A, 0x00
    LD      (MainCountLow), A
    LD      (MainCountHigh), A

    JP    MainLoop
;...............................................................................

;...............................................................................
IncrementCounter:
;...............................................................................
    LD      BC, (MainCountLow)
    INC     C
    JP      NZ,DontIncrementHigh
    INC     B
DontIncrementHigh
    LD      (MainCountLow), BC
    RET
;...............................................................................

;...............................................................................
DisplayCounter:
;...............................................................................    LD      A,(MainCountLow)
    LD      A,(MainCountLow)
    OUT     (PORT0OUT),A
    LD      A,(MainCountHigh)
    OUT     (PORT1OUT),A
    RET
;...............................................................................

;...............................................................................
MainLoop:
;...............................................................................
    CALL    VeryLongDelay
    CALL    IncrementCounter
    CALL    DisplayCounter

    JP    MainLoop
;...............................................................................

;...............................................................................
LongDelay:
;...............................................................................
    PUSH    BC
    LD      C, 0x0FF
LoopOuter:
        LD  B, 0x0FF
LoopInner:
            DEC     B
            JP      NZ,LoopInner
        ;DJNZ   LoopInner
        DEC     C
    JP      NZ,LoopOuter
    POP     BC
    RET
;...............................................................................

;...............................................................................
VeryLongDelay:
;...............................................................................
    CALL    LongDelay
    CALL    LongDelay
    CALL    LongDelay
    CALL    LongDelay
    RET
;...............................................................................

END
